------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: adagio-socket.adb,v 1.14 2004/03/29 19:13:33 Jano Exp $

with Adagio.BW_usage;
with Adagio.Globals.Options;
with Adagio.Os.Socket;
with Adagio.Security;
with Adagio.Statistics; 
with Adagio.Statistics.Integers;
with Adagio.Trace;

with Agpl.Strings.Fields;

with Strings.Fields;

with Gnat.Sockets; use Gnat.Sockets;
with Gnat.Sockets.Extra;

with Ada.Calendar; use Ada;
with Ada.Unchecked_deallocation;

package body Adagio.Socket is

   Conn_period : Duration; 

   Buffer_size : Positive renames Globals.Options.Network_BufferSize;

   Stat_alive_sockets : constant String := "Network - Alive sockets";

   -------------------
   -- Conn_throttle --
   -------------------
   protected Conn_Throttle is

      procedure Request_connect;

   private

      Next_conn : Calendar.Time;

   end Conn_throttle;

   protected body Conn_throttle is

      procedure Request_connect is
         use Calendar;
      begin
         if Clock < Next_conn then
            delay until Next_conn;
         else
            Next_conn := Clock + Conn_period;
         end if;
      end Request_connect;

   end Conn_throttle;

   -- Marks a socket as created and counts it in statistics:
   procedure Mark_created (This : in out Object) is
   begin
      This.Created.Add;
      Statistics.Object.Update (
         Stat_alive_sockets,
         Statistics.Integers.Increment'Access,
         Statistics.Integers.Create (1));
   end Mark_created;

   -- Sets the buffer size of a socket:
   procedure Set_Buffer_size (
      This : in out Object; Size : in Positive := Buffer_size) is
   begin
      Set_Socket_Option (
         this.Socket, 
         Option => (Name => Sockets.Send_buffer, Size => Size));
      Set_Socket_Option (
         this.Socket, 
         Option => (Name => Sockets.Receive_buffer, Size => Size));
   end Set_Buffer_size;

   -- Creates a TCP socket:
   procedure Create_stream(this: out Object) is
   begin
      Sockets.Create_socket(this.Socket);
      This.Stream := new Stream_type (Sockets.Stream (this.Socket));
      This.Stream.Sock := This.Socket;
      Mark_created (This);

      --  Allow reuse of local addresses.
      Set_Socket_Option(this.Socket, Option => (Sockets.Reuse_Address, True));
      -- Prevent delaying of data:
      -- Set_Socket_Option(this.Socket, Option => (Sockets.No_delay, True));
      -- Change buffering:
      Set_buffer_size (This);
   end Create_stream;

   -- Creates a UDP socket:
   procedure Create_datagram(this: out Object) is
   begin 
      Sockets.Create_socket
        (this.Socket, Mode => Sockets.Socket_datagram);
      This.Stream := new Stream_type (Sockets.Stream (This.Socket));
      This.Stream.Sock := This.Socket;
      Mark_created (This);

      --  Allow reuse of local addresses.
      Set_Socket_Option(this.Socket, Option => (Sockets.Reuse_Address, True));
      -- Prevent delaying of data:
      -- Set_Socket_Option(this.Socket, Option => (Sockets.No_delay, True));
      -- Change buffering:
      Set_buffer_size (This);
   end Create_datagram;

   -- Shutdowns a socket (both ways):
   procedure Shutdown(this: in Object) is
   begin
      Sockets.Shutdown_socket(this.Socket);
   exception
      when Socket_error => 
         null;
         -- Trace.Log ("Socket.Shutdown: " & Trace.Report (E), Trace.Debug);
         -- No report. The usual exception is SOCKET_NOT_CONNECTED.
         -- Will forget about that ;-)
   end Shutdown;

   -- Deletes a socket (frees mem)
   procedure Close (this: in out Object) is
      procedure Free is new Unchecked_deallocation (
         Streams.Root_stream_type'Class, Sockets.Stream_access);
      procedure Free is new Unchecked_deallocation (
         Stream_type, Stream_access);
      Aux : Sockets.Stream_access;
   begin
      if This.Created.Val > 0 then
         This.Created.Reset;
         Statistics.Object.Update (
            Stat_alive_sockets,
            Statistics.Integers.Increment'Access,
            Statistics.Integers.Create (-1));
         if this.Stream /= null and then not this.Stream.Parent_Freed then
            This.Stream.Parent_Freed := true;
            Aux := This.Stream.Sock_stream;
            Free (Aux);
         end if;
         Free (this.Stream);
      end if;
      Trace.Log (
         "Socket.Close: Closing: " & Image (Get_Peer_Name (This)), Trace.Never);
      Sockets.Close_socket(this.Socket);
   exception
      when Socket_error => 
         null;
         -- Trace.Log ("Socket.Close: " & Trace.Report (E), Trace.Debug);
         -- No report. The usual exception is SOCKET_NOT_CONNECTED.
         -- Will forget about that ;-)
   end Close;

    -- Bind the socket with a dotted address:
   procedure Bind(this: Object; Address: String; Port: Natural) is
      Addr: Sockets.Sock_addr_type;
   begin
      if Address /= "0.0.0.0" then
         Addr.Addr:= Sockets.Inet_addr(Address);
      else
         Addr.Addr:= Sockets.Any_inet_addr;
      end if;
      Addr.Port:= Sockets.Port_type(Port);
      Sockets.Bind_socket(this.Socket, Addr);
   end Bind;

   -- Set the socket as a listening one:
   procedure Listen(this: Object; Length: Positive:= 15) is
   begin
      Sockets.Listen_socket(this.Socket, Length);
   end Listen;

   -- Wait for incoming connections:
   procedure Accept_socket(this: Object; New_socket: out Object) is
      Addr: Sockets.Sock_addr_type;
      Sock: Sockets.Socket_type;
   begin
      Sockets.Accept_socket(this.Socket, Sock, Addr);
      New_socket.Socket := Sock;
      New_socket.Refs.Reset (Val => 1);
      if Security.Is_banned (Get_peer_name (Sock).Addr) then
         Sockets.Close_socket (Sock);
         raise Security_ban;
      end if;
      New_socket.Stream  := new Stream_type (Sockets.Stream (Sock));
      New_socket.Stream.Sock := Sock;
      Mark_created (New_socket);

      -- Change buffering:
      Set_Buffer_size (New_socket);
   end Accept_socket;

   -- Connect to a server:
   procedure Connect(this: in Object; Address: String; Port: Natural) is
      Addr: Sockets.Sock_addr_type;
   begin
      Conn_throttle.Request_connect;

      Addr.Addr:= Sockets.Inet_addr(Address);
      Addr.Port:= Sockets.Port_type(Port);
      if Security.Is_banned (Addr.Addr) then
         raise Security_ban;
      end if;
      Sockets.Connect_socket(this.Socket, Addr);
   end Connect;

   -- Joint address:port
   procedure Connect(this: in Object; Address: String) is
      use Agpl.Strings.Fields;
   begin
      Connect (
         This, 
         Select_Field (Address, 1, ':'),
         Natural'Value (Select_Field (Address, 2, ':')));
   end Connect;

   -- Get a stream for a connected socket:
   function Stream(this: Object) return Stream_access is
   begin
      if Security.Is_banned (Get_peer_name (This.Socket).Addr) then
         raise Security_ban;
      end if;
      return This.Stream;
   end;

   -- Set blocking mode:
   procedure Set_blocking_io(this: in Object; Yes: Boolean:= true) is
      Request: Sockets.Request_type(Sockets.Non_blocking_io);
   begin
      Request.Enabled:= not Yes;
      Sockets.Control_socket(this.Socket, Request);
   end Set_blocking_io;

   -- Check available data:
   function Available(this: in Object) return Natural is
      Request: Sockets.Request_type(Sockets.N_bytes_to_read);
   begin
      Sockets.Control_socket(this.Socket, Request);
      return Request.Size;
   end Available;

   function Available (This : Sockets.Socket_type) return Natural is
      Request: Sockets.Request_type(Sockets.N_bytes_to_read);
   begin
      Sockets.Control_socket(This, Request);
      return Request.Size;
   end Available;

   -- Controlling
   procedure Initialize (This : in out Object) is
   begin
      This.Created := new Agpl.Counter.Object;
      This.Refs    := new Agpl.Counter.Object (Initial_Value => 1);
   end Initialize;

   procedure Adjust     (This : in out Object) is
   begin
      This.Refs.Add;
      Trace.Log ("[Adj] Socket " & Image (Get_Peer_Name (This)) & " has" &
         Integer'Image (This.Refs.Val) & " references.", Trace.Never);
   end Adjust;

   procedure Finalize   (this : in out Object) is
   begin
      This.Refs.Add (-1);
      Trace.Log ("[Fin] Socket " & Image (Get_Peer_Name (This)) & " has" &
         Integer'Image (This.Refs.Val) & " references.", Trace.Never);
      -- Just in case:
      if This.Refs.Val = 0 and then This.Created.Val > 0 then
         Trace.Log ("[Fin] Socket " & Image (Get_Peer_Name (This)) & 
            " is being closed automatically", Trace.Never);
         Close(this);
      end if;
      if This.Refs.Val = 0 then
         declare
            procedure Free is new Unchecked_Deallocation (
               Agpl.Counter.Object, Agpl.Counter.Object_Access);
         begin
            Free (This.Refs);
            Free (This.Created);
         end;
      end if;
   exception
      when E: others =>
         Trace.Log("Socket.Finalize: " & Trace.Report(E), Trace.Debug);
   end Finalize;

   procedure Receive
     (this   : in Object;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      From   : out Sock_Addr_Type) is
      use type Ada.Streams.Stream_element_offset;
   begin
      Sockets.Receive_socket (
         this.Socket, 
         Item,
         Last,
         Sockets.Sock_addr_type (From));
      BW_usage.Add_in (Natural (Last - Item'First + 1), BW_usage.UDP);
   end Receive;

   -- Write to an address (for UDP):
    procedure Send
     (this   : in Object;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : Sock_Addr_Type) is
    begin
      if Security.Is_banned (To.Addr) then
         raise Security_ban;
      end if;
      Sockets.Send_socket (
          this.Socket,
          Item,
          Last,
          Sockets.Sock_addr_type (To));
      BW_usage.Add_out (Item'Length, BW_usage.UDP);
    end Send;

   -- Returns the address in xx.xx.xx.xx:nnn format
   function Image (this : in Sock_addr_type) return String is
   begin
      return Sockets.Image (Sockets.Sock_addr_type (this));
   end Image;

   -- Inverse of the previous
   function To_address (this : in String) return Sock_addr_type is
      use Strings.Fields;
   begin
      return (
         Family => Sockets.Family_inet, 
         Addr => Sockets.Inet_addr (Select_field (this, 1, ':')),
         Port => Port_type'Value     (Select_field (this, 2, ':')));
   end To_address;

   function Raw (this : in Object) return Gnat.Sockets.Socket_type is
   begin
      return this.Socket;
   end Raw;
   function Raw (this : in Stream_type) return Gnat.Sockets.Socket_type is
   begin
      return this.Sock;
   end Raw;

   ------------------------------------------------------------------------
   -- Is_alive                                                           --
   ------------------------------------------------------------------------
   -- Check if a connection is still alive
   function Is_alive (this : in Object) return Boolean is
   begin
      return not (
         Sockets.Extra.Check_read_socket (this.Socket) and then
         Available (this) = 0);
   exception
      when E : others =>
         Trace.Log ("Socket.Is_alive: " & Trace.Report (E), Trace.Debug);
      return false;
   end Is_alive;

   ------------------------------------------------------------------------
   -- Is_writable                                                       --
   ------------------------------------------------------------------------
   -- Check if a socket is ready for writing
   function Is_writable (this : in Object) return Boolean is
   begin
      return Sockets.Extra.Check_write_socket (This.Socket);
   exception
      when E : others =>
         Trace.Log ("Socket.Is_writable: " & Trace.Report (E), Trace.Warning);
         return false;
   end Is_writable;

   ------------------------------------------------------------------------
   -- Connection_failed                                                  --
   ------------------------------------------------------------------------
   function Connection_failed (this : in Object) return Boolean is
      Opt_error : Sockets.Option_type := 
         Get_socket_option (This.Socket, Name => Sockets.Error);
   begin
      case Opt_error.Error is
         when Sockets.Connection_refused =>
            return true;
         when others =>
            return false;
      end case;
   end Connection_failed;

   ------------------------------------------------------------------------
   -- Get_error                                                     --
   ------------------------------------------------------------------------
   -- Get last socket error:
   function Get_error (E : in Exception_occurrence) return Error_type is
   begin
      case Os.Socket.Get_error (E) is
         when Os.Socket.Operation_would_block =>
            return Operation_would_block;
         when Os.Socket.Socket_is_not_connected =>
            return Socket_is_not_connected;
         when others =>
            return Unknown_error;
      end case;
   end Get_error;

   ------------------------------------------------------------------------
   -- Get_peer_name                                                      --
   ------------------------------------------------------------------------
   -- Get the remote address of a socket
   function Get_peer_name (This : in Object) return Sock_addr_type is
   begin
      return Sock_addr_type (Sockets.Get_peer_name (This.Socket));
   exception
      when Socket_Error =>
         declare
            Dummy : Sock_Addr_Type;
         begin
            Dummy.Port := 0;
            return Dummy;
         end;
   end Get_peer_name;

   ------------------------------------------------------------------------
   -- Set_send_buffer_size                                               --
   ------------------------------------------------------------------------
   procedure Set_send_buffer_size (This : in Object; Size : in Natural) is
   begin
      Sockets.Set_socket_option (This.Socket, 
         Option => (Name => Sockets.Send_buffer, Size => Size));
   end Set_send_buffer_size;

   -----------------
   -- Stream_type --
   -----------------
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
      use type Ada.Streams.Stream_element_offset;
   begin
      Ada.Streams.Read (Stream.Sock_stream.all, Item, Last);
      BW_usage.Add_in (Natural (Last - Item'First + 1), BW_usage.TCP);
   end Read;

	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Streams.Write (Stream.Sock_stream.all, Item);
      BW_usage.Add_out (Item'Length, BW_usage.TCP);
   end Write;

   function Available (This : in Stream_type) return Natural is
   begin
      return Available (This.Sock);
   end Available;

begin
   -- Initialization of the Gnat library:
   Sockets.Initialize;

   -- Connection throttle:
   Conn_period := Duration (1.0 / Float (
      Globals.Options.Network_ConnectionsPerSecond));

   -- Stats:
   Statistics.Object.Set (
      Stat_alive_sockets, Statistics.Integers.Create (0));
end Adagio.Socket;
