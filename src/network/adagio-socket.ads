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
--  $Id: adagio-socket.ads,v 1.7 2004/01/21 21:05:39 Jano Exp $

with Adagio.Ip_address;

with Agpl.Counter;

with Gnat.Sockets;
use  Gnat;

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Finalization; use Ada;
with Ada.Streams;

package Adagio.Socket is

   pragma Elaborate_body;

   -- Exception for errors:
   Socket_error : exception renames Sockets.Socket_error;

   -- Security exceptions:
   Security_ban : exception;

   type Object is private;
   type Object_access is access all Object;

   subtype Sock_addr_type is Sockets.Sock_addr_type;
   subtype Inet_addr_type is Ip_address.Inet_addr_type;

   -----------------
   -- Stream_type --
   -----------------
   type Stream_type (<>) is new Ada.Streams.Root_stream_type with private;
   type Stream_access is access all Stream_type;
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array);
   function Available (This : in Stream_type) return Natural;

   type Error_Type is (
      Operation_Would_block,
      Socket_is_not_connected,
      Unknown_error);

   -- Creates a TCP socket (takes mem):
   procedure Create_stream(this: out Object);

   -- Creates a UDP socket:
   procedure Create_datagram(this: out Object);

   -- Deletes a socket (frees mem)
   procedure Close(this : in out Object);

   -- Shutdowns a socket (both ways):
   procedure Shutdown(this: in Object);

   -- Bind the socket with a dotted address and port:
   -- Use 0.0.0.0 for IN_ADDR_ANY
   procedure Bind(this: Object; Address: String; Port: Natural);

   -- Set the socket as a listening one:
   procedure Listen(this: Object; Length: Positive:= 15);

   -- Wait for incoming connections:
   procedure Accept_socket(this: Object; New_socket: out Object);

   -- Connect to a server:
   -- Separate address and port
   procedure Connect(this: in Object; Address: String; Port: Natural);
   -- Joint address:port
   procedure Connect(this: in Object; Address: String);

   -- Get a stream for a connected socket:
   function Stream (this: Object) return Stream_access;

   -- Set blocking mode:
   procedure Set_blocking_io (this: in Object; Yes: Boolean:= true);

   -- Check available data:
   function Available(this: in Object) return Natural;

   -- Read taking sender address (for UDP primarily):
   procedure Receive
     (this   : in  Object;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      From   : out Sock_Addr_Type);

   -- Write to an address (for UDP):
    procedure Send
     (this   : in Object;
      Item   : Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : Sock_Addr_Type);

   ------------------------------------------------------------------------
   -- Set_send_buffer_size                                               --
   ------------------------------------------------------------------------
   procedure Set_send_buffer_size (This : in Object; Size : in Natural);

   ------------------------------------------------------------------------
   -- Get_peer_name                                                      --
   ------------------------------------------------------------------------
   -- Get the remote address of a socket
   function Get_peer_name (This : in Object) return Sock_addr_type;

   -- Get error from exception:
   function Get_error (E: Exception_occurrence) return Error_type;

   -- Returns the address in xx.xx.xx.xx:nnn format
   function Image (this : in Sock_addr_type) return String;

   function Image (this : in Inet_addr_type) return String
      renames Sockets.Image;

   -- Inverse of the previous
   -- Takes a xx.xx.xx.xx:n string
   function To_address (this : in String) return Sock_addr_type;

   ------------------------------------------------------------------------
   -- Is_alive                                                           --
   ------------------------------------------------------------------------
   -- Check if a connection is still alive
   function Is_alive (this : in Object) return Boolean;
   pragma Inline (Is_alive);

   ------------------------------------------------------------------------
   -- Is_writable                                                       --
   ------------------------------------------------------------------------
   -- Check if a socket is ready for writing
   function Is_writable (this : in Object) return Boolean;
   pragma Inline (Is_writable);

   ------------------------------------------------------------------------
   -- Connection_failed                                                  --
   ------------------------------------------------------------------------
   function Connection_failed (this : in Object) return Boolean;
      
   -----------
   -- debug --
   -----------
   function Raw (this : in Object) return Gnat.Sockets.Socket_type;
   function Raw (this : in Stream_type) return Gnat.Sockets.Socket_type;

private

   subtype Option_name is Gnat.Sockets.Option_name;
   subtype Option_type is Gnat.Sockets.Option_type;

   type Object is new Finalization.Controlled with
      record
         Socket  : Sockets.Socket_type;
         Stream  : Stream_access;                 -- To avoid the mem-leak in Gnat.
         Created : Agpl.Counter.Object_Access;    -- To track open sockets
         Refs    : Agpl.Counter.Object_Access;    -- Reference counting.
      end record;

   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);
   procedure Finalize   (This : in out Object);

   Stream_id : aliased String := "Network - Alive socket streams";

   type Stream_type (Sock_stream : Sockets.Stream_access) 
   is new Ada.Streams.Root_stream_type with 
   record
      Sock         : Sockets.Socket_type;
      Parent_Freed : Boolean := false;
   end record;

end Adagio.Socket;
