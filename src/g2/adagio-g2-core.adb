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
--  $Id: adagio-g2-core.adb,v 1.26 2004/03/29 19:13:30 Jano Exp $

with Adagio.Connect.Peer_Manager;
with Adagio.Debug;
with Adagio.G2.Bandwidth;
with Adagio.G2.Local_query;
with Adagio.G2.Mesh;
with Adagio.GUID;
with Adagio.GWCache2;
with Adagio.Library;
with Adagio.Mmap.Strings;
with Adagio.Misc;
with Adagio.Network.Endian;
with Adagio.Network_settings;
use  Adagio.Network_settings;
with Adagio.Resolver_Tcp;
with Adagio.Routing;
with Adagio.Socket;
with Adagio.Socket.IP;
with Adagio.Statistics;
with Adagio.Statistics.Booleans;
with Adagio.Statistics.Durations;
with Adagio.Statistics.Integers;
with Adagio.Streams_alias;
with Adagio.Time_t;
with Adagio.Trace;
with Adagio.Traffic;
with Adagio.Unicode;
with Adagio.Upload.Queue.Manager;
with Adagio.Zutil;

with Agpl.Geoip;

with Strings.Fields;
with Zlib;
with Zlib.Streams.Extra;

with Ada.Characters.Handling;
with Ada.Streams;                use Ada.Streams;
use Ada;

with Text_io;
with Adagio.Xml.Utils;

package body Adagio.G2.Core is

   Stat_longest : constant String := "Servers - G2 - Longest connection";
   Stat_servers : constant String := "Servers - G2 - New servers";

   use type Calendar.Time;

   package ACH renames Ada.Characters.Handling;

   -- Auxiliaries for average uptime queue:
   function Average (Left : in Duration; Right : in Integer) return float is
   begin
      if Right = 0 then
         return 0.0;
      else
         return float (Left) / float (Right);
      end if;
   end Average;

   -------------------
   -- Network stuff --
   -------------------

   -- Gives the network identifier
   function Id (this: in Network_type) return String is
      pragma Unreferenced (This);
   begin
      return Network_id;
   end Id;

   -- Connect to that network. Will get servers and connect them as needed.
   procedure Connect(this: in out Network_type) is
   begin
      -- Local port
      this.Port:= Globals.Options.G2_port;
      this.Status:= Network.Connecting;

      -- Restore mesh:
      if Use_mesh then
         G2.Mesh.Object.Restore (
            Globals.Data_Folder & "mesh." & G2.Mesh.Object.Id.all & ".dat");
         G2.Mesh.Object.Configure (
            Globals.Options.library_mesh_sources,
            Globals.Options.library_mesh_TTL);
      end if;

      -- Create connector:
      this.Connector:= new Connector_type;
      this.Connector.Start(this'Unrestricted_access);

      -- Create polling task:
      this.Polling:= new Polling_type;
      this.Polling.Start (this'Unrestricted_access);

      -- Listener
      G2.Listener.Start(this.Listener, this.Port);

      -- Transceiver
      this.Transceiver := new G2.Transceiver.Object;
      G2.Transceiver.Set_BW_limits (
         This.Transceiver.all,
         BW_in  => Globals.Options.G2_UdpBandwidthIn,
         BW_out => Globals.Options.G2_UdpBandwidthOut);
      G2.Transceiver.Start (
         this.Transceiver.all,
         G2.Listener.Get_udp (this.Listener),
         this.Inbound'Unrestricted_access);

      -- Sender
      this.Send_udp := new Sender_udp;
      this.Send_udp.Start (this'Unrestricted_access);

      -- Searcher
      if Globals.Options.Download_Active then
         This.Searcher := new G2.Search.Object;
         G2.Search.Start (
            This.Searcher,
            This.Outbound'Unchecked_Access,
            This.Transceiver);
      end if;

   end Connect;

   -- Disconnect:
   procedure Disconnect(this: in out Network_type) is
      use type G2.Search.Object_Access;
   begin
      -- Transceiver out:
      G2.Transceiver.Shutdown (this.Transceiver.all);

      -- Listener shutdown:
      G2.Listener.Shutdown(this.Listener);

      -- Ensure all tasks termination:
      this.Servers.Disconnect_all;

      -- Save mesh:
      if Use_mesh then
         G2.Mesh.Object.Save (
            Globals.Data_Folder & "mesh." & G2.Mesh.Object.Id.all & ".dat");
      end if;

      -- Stop searcher:
      if This.Searcher /= null then
         G2.Search.Shutdown (This.Searcher.all);
      end if;

      this.Status:= Network.Disconnected;
   end Disconnect;

   -- Says status of the network.
   function Status(this: in Network_type) return Network.Network_status is
   begin
      return this.Status;
   end Status;

   -- Obtain search handler. Can return null if the network is not to be
   -- searched:
   function Get_Search_Handler (This : in Network_Type)
      return Searches.Handler.Object_Access
   is
   begin
      return Searches.Handler.Object_Access (This.Searcher);
   end Get_Search_Handler;

   ------------------
   -- Server stuff --
   ------------------

   -- Get a unique id to identify it:
   function Id(this: in Server_type) return String is
      P: constant String:= Natural'Image(this.Port);
   begin
      return To_string(this.Address) & ":" & P(P'first + 1 .. P'last);
   end Id;

   -- Get description
   function Describe (this: in Server_type) return String is
      function QRT_status return String is
      begin
         if This.QRT_status = Sending then
            return Misc.To_lower (QRT_Status_type'Image (This.QRT_status)) &
               " (" & Misc.To_string (This.QRT_packets_sent) & "/" &
               Misc.To_string (This.QRT_packets + 1) & ")";
         else
            return Misc.To_lower (QRT_Status_type'Image (This.QRT_status));
         end if;
      end;
      UA : Ustring;
   begin
      if This.Slot /= null then
         UA := This.Slot.User_agent;
      else
         UA := U ("unknown agent");
      end if;
      if UA = U ("") then
         UA := U ("unknown agent");
      end if;
      return Id (this) & "; Nick: " &
         Xml.Get_attribute ("identity/handle", "primary",
         this.User_profile, "Anonymous") & " ("
         & S (UA) & "); Load:" &
         this.Num_leaves'Img & "/" & Misc.To_string (this.Max_leaves) &
         "; QRT: " & QRT_status &
         "; Rating: " & Misc.To_string (Rate (This), 2);
   end Describe;

   -- Get network it belongs:
   function Net(this: in Server_type) return String is
      pragma Unreferenced (This);
   begin
      return Network_id;
   end Net;

   -- Evaluate its goodness to be connected:
   function Rate(this: in Server_type) return Server.Rating is
   begin
      return This.Score;
   end Rate;

   ------------------------------------------------------------------------
   -- Add_score                                                          --
   ------------------------------------------------------------------------
   -- Adds score to a server ensuring no overflow
   procedure Add_score (This : in out Server_type; Score : in Server.Rating)
   is
   begin
      This.Score := This.Score + Score;
   exception
      when Constraint_error =>
         This.Score := Server.Rating'Last;
   end Add_score;

   ------------------------------------------------------------------------
   -- Add_hub                                                            --
   ------------------------------------------------------------------------
   -- Creates a hub and adds it to the cache, if possible
   procedure Add_hub (
      Address : in String; Port : in Natural; Score : in Float := 1000000.0)
   is
      Serv : Server_access := new Server_type;
   begin
      Create (Serv.all, The_network, Address, Port);
      Serv.Score := Score;
      Server.List.Add (Server.Object_access (Serv));
   exception
      when Server.Server_already_cached =>
         null;
   end Add_hub;

   -----------------------------------------------------------------------
   -- Parse_ultrapeers                                                  --
   -----------------------------------------------------------------------
   -- Parse from a standard X-Try-Ultrapeers and add to server cache
   procedure Parse_ultrapeers (Net : in Network_access; S : in String) is
      use Strings.Fields;
      Discarded  : Natural := 0;
   begin
      if S'Length < 1 then
         return;
      end if;
      if S (S'Last) = ',' then
         Parse_ultrapeers (Net, S (S'First .. S'Last - 1));
         return;
      end if;
      for N in 1 .. Count_Fields (S, ',') loop
         declare
            Ultrapeer  : String := Select_field (S, N, ',');
            New_server : Server_access;
         begin
            New_server := new Server_type;
            Create (
               New_server.all,
               Net,
               Select_field (Ultrapeer, 1, ':'),
               Natural'Value (
                  Select_field (Select_field (Ultrapeer, 2, ':'), 1, ' ')));
            begin
               if Reachable (New_server.all) then
                  Server.List.Add (Server.Object_access (New_server));
               else
                  Server.Free (Server.Object_access (New_server));
                  Discarded := Discarded + 1;
               end if;
            exception
               when others =>
                  Discarded := Discarded + 1;
            end;
         end;
      end loop;
      if Count_fields (S, ',') > 0 then
         Trace.Log ("Added" &
            Natural'Image (Count_Fields (S, ',') - Discarded)
            & " servers from X-Try-Ultrapeers header.");
         Trace.Log ("Discarded" & Discarded'Img
            & " servers from X-Try-Ultrapeers header.");
      end if;
   exception
      when E : others =>
         Trace.Log ("G2.Core.Parse_ultrapeers: " & Trace.Report (E),
            Trace.Error);
   end Parse_ultrapeers;

   procedure Fail (this : in out Server_type) is
   begin
      this.Failures := this.Failures + 1;
      this.Score    := this.Score / 2.0;
   end Fail;

   ---------------------
   -- Prepare_connect --
   ---------------------
   -- Prepare everything for a new server connection attempt:
   procedure Prepare_connect (This : in out Server_type) is
   begin
      -- Clean queue:
      This.Slot.Outbound.Clear;

      -- QRT
      This.QRT_timestamp := Past_aeons;
      This.Checked_QRP   := Past_aeons;
      This.QRT_reset     := false;
      This.QRT_status    := Not_sent;

      -- Deflate:
      This.Slot.Deflate  := false;
      This.Slot.ZCreated := false;
      Circular_stream.Reset (This.Slot.CStream_in);
      Circular_stream.Reset (This.Slot.CStream_out);
      This.Slot.OBuffer_used := false;
      Chronos.Reset (This.Slot.ZCron);
      This.Slot.ZFlushed := true;
   end Prepare_connect;

   -----------------------------------
   -- Connect                       --
   -----------------------------------
   -- Establish a connection:
   procedure Connect(this: in out Server_type) is
      use type Socket.Error_type;
   begin
      if not Reachable (This) then
         Fail (This);
         Disconnect (This);
         Trace.Log ("G2.Core.Connect: Discarding unreachable server: " &
            Id (This));
         return;
      end if;

      case This.Connection_stage is
         -- STARTING --
         when Starting =>
            This.Last_try_connect := Calendar.Clock;
            This.Connection_stage := Connecting;
            Trace.Log ("G2.Server.Connect: connecting to " & Id(this) &
               " (rating: " & Misc.To_string (Float (Rate (this))) & ")...");

            -- Initialization of vars:
            this.QRT_reset  := false;
            this.QRT_status := Not_sent;

            -- Socket things:
            this.Network.Servers.Set_status (
               this'Unrestricted_access, Connecting);
            -- Socket startup:
            Socket.Create_stream (This.Slot.Socket);
            Socket.Set_blocking_io (This.Slot.Socket, false); -- From now on.
            begin
               -- Do it!
               Socket.Connect(
                  This.Slot.Socket, To_string(this.Address), this.Port);
	            -- Should have been raised Operation_would_block!
	            raise Constraint_error;
            exception
               when E : Socket.Socket_error =>
                  if Socket.Get_error (E) = Socket.Operation_would_block then
                     -- Normal connection in progress...
                     This.Connection_stage := Connecting;
                  else
                     Fail (this);
                     Clear (this);
                     this.Network.Servers.Set_status
                       (this'Unrestricted_access, Disconnected);
                     raise;
                  end if;
               when Socket.Security_ban =>
                  Fail (this);
                  Clear (this);
                  this.Network.Servers.Set_status
                    (this'Unrestricted_access, Disconnected);
                  Trace.Log ("Connection with " & Id (This) & " failed " &
                     "[security ban]", Trace.Informative);
               when E : others =>
                  Fail (this);
                  Clear (this);
                  this.Network.Servers.Set_status
                    (this'Unrestricted_access, Disconnected);
                  Trace.Log ("Connection with " & Id (This) & " failed: " &
                     Trace.Report (E), Trace.Error);
            end;
         -- CONNECTING --
         when Connecting =>
            if Calendar.Clock - This.Connection_start >
               Globals.Options.G2_ConnectTimeout
            then
               Fail (this);
               Clear (this);
               this.Network.Servers.Set_status
                 (this'Unrestricted_access, Disconnected);
               Trace.Log ("Connection with " & Id (This) &
	       	  " failed [timeout].");
               return;
            end if;
            -- success
            if Socket.Is_writable (This.Slot.Socket) then
               -- Step to handshaking:
               This.Handshake_start  := Calendar.Clock;
               This.Connection_stage := Handshake_preparing;
               this.Network.Servers.Set_status
                 (this'Unrestricted_access, Handshaking);
               Trace.Log ("Connection with " & Id (This) & " started.");
            -- Failure
            elsif (not Socket.Is_alive (This.Slot.Socket)) or else
               Socket.Connection_failed (This.Slot.Socket)
            then
               Fail (this);
               Clear (this);
               this.Network.Servers.Set_status
                 (this'Unrestricted_access, Disconnected);
               Trace.Log ("Connection with " & Id (This) &
	          " failed [refused].");
               return;
            end if;
         when others =>
            raise Constraint_error;
      end case;
   end Connect;

   -----------------------------------
   -- Handshake                     --
   -----------------------------------
    -- Do the handshaking:
   procedure Handshake(this: in out Server_type) is
      Ask: String:= S (Globals.Options.G2_HandshakeAsk);
      Ack: String:= S (Globals.Options.G2_HandshakeAnswer);
      Head : Http.Header.Set renames This.Slot.Head;
      procedure Drop(Reason: String) is
      begin
         String'Write(this.Slot.Stream, Ack & " 500 " & Reason &
            Http.CRLF & Http.CRLF);
         Disconnect(this);
      end Drop;
      use type Socket.Error_type;
   begin
      if Calendar.Clock - This.Handshake_start >
         Globals.Options.G2_HandshakeTimeout then
         Fail (This);
         Disconnect (This);
         return;
      end if;
      if not Socket.Is_alive (This.Slot.Socket) then
         Fail (This);
         Disconnect (This);
         return;
      end if;
      case This.Connection_stage is
         -- PREPARING --
         when Handshake_preparing =>
            this.Last_seen := Calendar.Clock;
            this.Network.Servers.Set_status (
               this'Unrestricted_access, Handshaking);
            this.Slot.Stream :=
               Tcp_slot.Stream_access (Socket.Stream (this.Slot.Socket));
            -- Prepare headers:
            Http.Header.Clear (Head);
            case Socket.IP.Kind(To_string(this.Address)) is
               when Socket.IP.Local =>
                  Http.Header.Add(Head, "Listen-IP",
                     "127.0.0.1:" & Misc.To_string(this.Local_port));
               when Socket.IP.Internal =>
                  Http.Header.Add(Head, "Listen-IP",
                     Socket.IP.Get_IP(false) & ":" &
                     Misc.To_string(this.Local_port));
               when Socket.IP.Public =>
                  Http.Header.Add(Head, "Listen-IP",
                     Socket.IP.Get_IP(Internet_route = Direct) & ":" &
                     Misc.To_string(this.Local_port));
            end case;
            Http.Header.Add (Head, "Remote-IP", To_string(this.Address));
            Http.Header.Add (Head, "User-Agent", User_agent);
            Http.Header.Add (Head, "Accept", Content_type);
            Http.Header.Add (Head, "X-Ultrapeer", "False");
            Http.Header.Add (Head, "X-Ultrapeer-Needed", "True");
            if Globals.Options.G2_CompressedLink then
               Http.Header.Add (Head, "Accept-Encoding", "deflate");
            end if;
            Http.Header.Set_response (Head, Ask);
            This.Connection_stage := Handshake_sending_first;
         -- SENDING FIRST --
         when Handshake_sending_first =>
            if Socket.Is_writable (This.Slot.Socket) then
               begin
                  Http.Header.Write (Head, this.Slot.Stream.all, true, true);
                  Trace.Log("G2.Handshaking (1): Sent: " &
                     Http.Header.Write(Head), File => To_string(Logfile));
                  This.Connection_stage := Handshake_receiving;
                  Http.Header.Parser.Reset (This.Slot.Http_parser);
               exception
                  when E : Socket.Socket_error =>
                     if Socket.Get_error (E) /=
                        Socket.Operation_would_block
                     then
                        raise;
                     end if;
               end;
            end if;
         -- RECEIVING --
         when Handshake_receiving =>
            -- Read data
            if not Http.Header.Parser.Completed (This.Slot.Http_parser) then
               Http.Header.Parser.Check (
                  This.Slot.Http_parser, This.Slot.Socket);
            end if;

            -- Check for completion
            if Http.Header.Parser.Completed (This.Slot.Http_parser) then
               Http.Header.Parser.Get_headers (This.Slot.Http_parser, Head);
               Trace.Log("G2.Handshaking (2): Read: " &
                  Http.Header.Write(Head), File => To_string(Logfile));
            else
               return;  --<---------------------- EARLY EXIT POINT!
            end if;

            -- Parse ultrapeers:
            if ACH.To_lower (Http.Header.Get (Head, "Content-Type")) =
               G2.Content_type
               or else
               Misc.Contains (Misc.To_lower(
                  Http.Header.Get (Head, "User-Agent")), "Shareaza")
            then
               Parse_ultrapeers (
                  this.Network, Http.Header.Get (Head, "X-Try-Ultrapeers"));
            end if;

            -- Check for G2:
            if ACH.To_lower (Http.Header.Get (Head, "Content-Type")) /=
               G2.Content_type
            then
               Trace.Log ("G2.Handshake: Dropping: Content-Type: " &
                  Http.Header.Get (Head, "Content-Type"));
               Fail (this);
               Drop ("Searching for G2 hubs");
               Trace.Log (
                  Http.Header.Write(Head), File => To_string (Logfile));
               return;
            end if;

            -- Check for deflate:
            if ACH.To_lower (Http.Header.Get (Head, "Content-Encoding")) =
               "deflate" and then Globals.Options.G2_CompressedLink
            then
               This.Slot.Deflate := true;
            end if;

            -- If not success, disconnect:
            if Http.Header.Get_response (Head)(14 .. 16) /= "200" then
               Trace.Log("G2.Handshake: Disconnecting from " & Id(this) &
                  " because: " & Http.Header.Get_response (Head));
               Fail       (this);
               Disconnect (this);
               return;
            end if;

            Trace.Log ("G2.Handshake: " & Http.Header.Get(Head, "Listen-IP") &
               " is a " & Http.Header.Get(Head, "User-Agent"));
            -- Check for ultrapeer:
            if ACH.To_lower (Http.Header.Get(Head, "X-Ultrapeer")) /= "true"
            then
               Trace.Log ("G2.Handshake: Dropping: X-Ultrapeer: " &
                  Http.Header.Get (Head, "X-Ultrapeer"));
               Fail (this);
               Drop ("Searching for ultrapeers");
               Trace.Log (
                  Http.Header.Write(Head), File => To_string (Logfile));
               return;
            end if;

            -- Keep some data
            This.Slot.User_agent := U (Http.Header.Get (Head, "User-Agent"));

            -- Public visible IP in case of Nat forwarding:
            if Internet_Route = NatForward then
               declare
                  NATF_Address : constant String := Http.Header.Get (Head, "Remote-IP");
               begin
                  if NATF_Address'length > 0 then
                     Network_Settings.Set_NATF_Address (NATF_Address);
                  end if;
               end;
            end if;

            -- Prepare final ack:
            Http.Header.Clear (Head);
            Http.Header.Set_response (Head, ack & " 200 OK");
            Http.Header.Add   (Head, "Accept",       Content_type);
            Http.Header.Add   (Head, "Content-Type", Content_type);
            Http.Header.Add   (Head, "X-Ultrapeer",  "False");
            if This.Slot.Deflate then
               Http.Header.Add (Head, "Content-Encoding", "deflate");
            end if;

            This.Connection_stage := Handshake_sending_last;
         -- SENDING LAST --
         when Handshake_sending_last =>
            if Socket.Is_writable (This.Slot.Socket) then
               begin
                  Http.Header.Write (Head, this.Slot.Stream.all, true, true);
                  -- Beyond this point, we have sucessfully sent the headers
                  Trace.Log("G2.Handshaking (3): Sent: " &
                     Http.Header.Write(Head), File => To_string(Logfile));
                  Http.Header.Clear (Head);

                  -- My first G2 packet, sparks!
                  this.Failures         := 0;
                  this.Successes        := this.Successes + 1;
                  this.Last_packet_time := Calendar.Clock;
                  this.Last_ping_time   := Calendar.Clock;
                  this.Connection_start := Calendar.Clock;
                  this.Slot.Outbound.Clear;

                  G2.Packet.Parsing.Create (
                     this.Slot.Packet_parser,
                     this.Slot.CStream_in'Access,
                     Available_cstream'Access);

                  -- Create ZStream:
                  if This.Slot.Deflate then
                     This.Slot.ZCreated := true;
                     Zlib.Streams.Create (
                        This.Slot.ZStream_in,
                        Zlib.Streams.Out_stream,
                        This.Slot.CStream_in'Access,
                        Back_compressed => false);
                     Zlib.Streams.Create (
                        This.Slot.ZStream_out,
                        Zlib.Streams.Out_stream,
                        This.Slot.CStream_out'Access,
                        Back_compressed => true);
                     Trace.Log ("Connection with " & Id (This) &
                        " is compressed (deflate)");
                  end if;

                  -- Connected!
                  this.Network.Servers.Set_status
                     (this'Unrestricted_access, Connected);
                  this.Network.Status := Network.Connected;
               exception
                  when E : Socket.Socket_error =>
                     if Socket.Get_error (E) /=
                        Socket.Operation_would_block
                     then
                        raise;
                     end if;
               end;
            end if;
         when others =>
            Trace.Log ("G2.Server.Handshake: Unexpected stage: " &
               This.Connection_stage'Img, Trace.Error);
            raise Constraint_error;
      end case;
   exception
      when E: others =>
         Trace.Log("G2.Server.Handshake: Failed to " & Id(this) & ": " &
            Trace.Report(E));
         Fail (This);
         Disconnect (This);
   end Handshake;

   -----------------------------------------------------------------------
   -- Disconnect                                                        --
   -----------------------------------------------------------------------
   procedure Disconnect(this: in out Server_type) is
   begin
      Disconnect2 (This);
   end Disconnect;
   procedure Disconnect2(
      this: in out Server_type; Spare : in Boolean := false)
   is
      Uptime : Duration := Calendar.Clock - this.Connection_start;
   begin
      This.Network.Servers.Set_status
         (this'Unrestricted_access, Disconnecting);

      Clear (This);

      if not Spare then
         Average_uptime.Push (this.Uptimes, Uptime);
      end if;

      this.Last_try   := this.Last_try_connect;

      begin
         Trace.Log ("Disconnected from " & Id (this));
         Trace.Log ("  -- Uptime:" & Misc.Image (Uptime));
         Trace.Log ("  -- Avg up:" &
            Misc.Image (Duration (Average_uptime.Average (this.Uptimes))));
      exception
         when Average_uptime.No_data =>
            null;
      end;

      This.Network.Servers.Set_status
         (this'Unrestricted_access, Disconnected);
   exception
      when others =>
         This.Network.Servers.Set_status (
            this'Unrestricted_access, Disconnected);
   end Disconnect2;

   -- Disconnects and lowers rating to 0!
   procedure Disconnect_hub(this: in String) is
      Serv : Server_access;
   begin
      The_network.Servers.Get (This, Connected, Serv);
      if Serv /= null then
         Serv.Score := 1.0;
         Disconnect2 (Serv.all, Spare => true);
      end if;
   end Disconnect_hub;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear (This : in out Server_type) is
   begin
      Socket.Shutdown (this.Slot.Socket);
      Socket.Close    (this.Slot.Socket);

      if This.Slot.ZCreated then
         begin
            This.Slot.ZCreated := false;
            -- Abort fluxes:
            Zlib.Streams.Extra.Close_abort (This.Slot.ZStream_in);
            Zlib.Streams.Extra.Close_abort (This.Slot.ZStream_out);
         exception
            when E : others =>
               Trace.Log ("G2.Close: " & Trace.Report (E), Trace.Error);
         end;
      end if;

      Circular_stream.Reset (This.Slot.CStream_in);
      Circular_stream.Reset (This.Slot.CStream_out);

      this.Slot.Outbound.Clear;
      this.Slot.User_agent := Null_ustring;

      this.QRT_timestamp := Past_aeons;
      this.Checked_QRP   := Past_aeons;

      this.Connection_stage := Starting;
   end Clear;

   ------------------------------------------------------------------------
   -- Dropable                                                           --
   ------------------------------------------------------------------------
   -- True when the server is to be purged:
   function Dropable (this : in Server_type) return Boolean is
   begin
      return
         (not This.Is_root) and then
         Calendar.Clock - this.Last_seen >
            Globals.Options.G2_ConfidencePeriod and then
         this.Failures >= Globals.Options.G2_Retries;
   end Dropable;

   ------------------------------------------------------------------------
   -- Is_Ready                                                           --
   ------------------------------------------------------------------------
   -- True when ready to connect
   function Is_Ready (This : in Server_Type) return Boolean is
   begin
      if this.Failures >= Globals.Options.G2_Retries then
         return false;
      elsif this.Failures > 0 and then
         Calendar.Clock - this.Last_try < Globals.Options.G2_RestPeriod
      then
         return false;
      else
         return true;
      end if;
   end Is_Ready;

   ------------------------------------------------------------------------
   -- Reachable                                                          --
   ------------------------------------------------------------------------
   -- Check against connection settings:
   function Reachable (This : in Server_type) return Boolean is
   begin
      return Routing.TCP_Reachable (Id (This));
   end;

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : Server_access) return Boolean is
   begin
      return Id (L.all) = Id (R.all);
   end Equal;

   ------------------------------------------------------------------------
   -- Serialize                                                          --
   ------------------------------------------------------------------------
   -- Dump:
   procedure Serialize
     (Stream: access Streams.Root_stream_type'Class;
      this: in Server_type) is
   begin
      String'Output                (Stream, To_string(this.Address));
      Natural'Output               (Stream, this.Port);
      Natural'Output               (Stream, this.Failures);
      Natural'Output               (Stream, this.Successes);
      Average_uptime.Object'Output (Stream, this.Uptimes);
      Server.Rating'Output         (Stream, this.Score);
      Calendar.Time'Output         (Stream, this.Last_seen);
      Calendar.Time'Output         (Stream, this.Last_try);
   end Serialize;

   ------------------------------------------------------------------------
   -- Restore                                                            --
   ------------------------------------------------------------------------
   -- Recover:
   function Restore
     (Stream: access Streams.Root_stream_type'Class) return Server_type is
      S: Server_type;
   begin
      S.Address     := To_ustring (String'Input    (Stream));
      S.Port        := Natural'Input               (Stream);
      S.Failures    := Natural'Input               (Stream);
      S.Successes   := Natural'Input               (Stream);
      S.Uptimes     := Average_uptime.Object'Input (Stream);
      S.Score       := Server.Rating'Input         (Stream);
      S.Last_seen   := Calendar.Time'Input         (Stream);
      S.Last_try    := Calendar.Time'Input         (Stream);
      return S;
   end Restore;

   ------------------------------------------------------------------------
   -- Check_Pipes                                                        --
   ------------------------------------------------------------------------
   -- Check for data to read:
   Stat_cron : Chronos.Object;
   procedure Check_pipes(this: in out Server_type) is
      P     : G2.Packet.Object := G2.Packet.Null_packet;
      Item  : G2.Packet.Queue.Item_type;
      Dur   : Duration;
      Max   : Duration;
      Avail : Ada.Streams.Stream_element_count;
      Allow : Natural;
      use type G2.Packet.Object;

      -- Get available bandwidth for reading
      function Get_bandwidth return Natural is
         Aw, Awx : Natural := 0;
         Needed  : Natural;
      begin
         if This.Slot.Deflate then
            Needed := Natural'Min (
               Socket.Available (This.Slot.Socket),
               Circular_stream.Available_write (This.Slot.CStream_in) / 2 +1);
         else
            Needed := Natural'Min (
               Socket.Available (This.Slot.Socket),
               Circular_stream.Available_write (This.Slot.CStream_in));
         end if;
         Bandwidth.Server_link.Commit (Needed, Aw, Extra => false);
         if Aw < Needed then
            Bandwidth.Server_link.Commit (Needed - Aw, Awx, Extra => true);
         end if;
         return Aw + Awx;
      end Get_bandwidth;

   begin
      loop
         -- Check alive:
         if not Socket.Is_alive (this.Slot.Socket) then
            Trace.Log (
               "G2 server connection lost (" &
               Id (this) & ")", Trace.Informative);
            Disconnect (this);
            exit;
         else
            this.Last_seen := Calendar.Clock;

            if Chronos.Elapsed (Stat_cron) > 0.8 then
               Chronos.Reset (Stat_cron);
               Dur := this.Last_seen - this.Connection_start;
               begin
                  Max := Statistics.Durations.Value (
                     Statistics.Durations.Duration_value (
                        Statistics.Object.Get (Stat_longest)));
               exception
                  when Statistics.Value_not_defined =>
                     Max := 0.0;
               end;
               if Dur > Max then
                  Statistics.Object.Set (Stat_longest,
                     Statistics.Durations.Create (Dur));
               end if;
            end if;
         end if;

         Allow := Get_bandwidth;
         if Allow = 0 then
            return; -- <-- EARLY EXIT BY BANDWIDTH THROTTLE
         end if;
         Avail := Stream_element_count (Allow);

         -- Try to pass compressed data to the uncompressed stream:
         if This.Slot.Deflate then
            if Circular_stream.Available_write (This.Slot.CStream_in) <
               Natural'(Packet.Max_packet_size)
            then
               null; -- Can't read still...
            else
               declare
                  Buff : Ada.Streams.Stream_element_array (1 .. Avail);
                  Last : Ada.Streams.Stream_element_offset;
               begin
                  Ada.Streams.Read (This.Slot.Stream.all, Buff, Last);
                  if Last /= Avail then
                     raise Constraint_error;
                  else
                     -- Decompression to the in stream
                     Zlib.Streams.Write (This.Slot.ZStream_in, Buff);
                  end if;
               end;
            end if;
         else
            -- Pass uncompressed data to the stream
            declare
               Buff : Ada.Streams.Stream_element_array (1 .. Avail);
               Last : Ada.Streams.Stream_element_offset;
            begin
               Ada.Streams.Read (This.Slot.Stream.all, Buff, Last);
               if Last /= Avail then
                  raise Constraint_error;
               else
                  -- Pass to the in stream
                  Circular_stream.Write (This.Slot.CStream_in, Buff);
               end if;
            end;
         end if;

         -- Try to acquire packets:
         G2.Packet.Parsing.Check (this.Slot.Packet_parser, Result => P);

         exit when P = G2.Packet.Null_packet;

         this.Last_packet_time := Calendar.Clock;
         this.Last_seen        := Calendar.Clock;

         -- Stat received packets
         G2.Packets_received := Natural'Min (
            G2.Packets_received + 1, Natural'Last - 1);
         This.Packets_in     := Natural'Min (
            This.Packets_in     + 1, Natural'Last - 1);

         -- Do something with it
         Item.Source := G2.Packet.Queue.Server;
         Item.Tcp_id := U (id (this));
         Item.Packet := P;
         this.Network.Inbound.Put (Item);

         -- Mark traffic
         Traffic.Add ((
            Arrival  => Calendar.Clock,
            Protocol => Protocol_descr,
            Way      => Traffic.Incoming,
            From     => Item.Tcp_id,
            Name     => U (G2.Packet.Name (Item.Packet)),
            Data     => U (G2.Packet.To_hex (Item.Packet))));
      end loop;
   exception
      when E : others =>
         Trace.Log ("G2.Core.Check_pipes [" & S (This.Slot.User_agent) & "]: " &
            Trace.Report (E),
            Trace.Error);
         Disconnect (this);
   end Check_pipes;

   ------------------------------------------------------------------------
   -- Send_Pending                                                       --
   ------------------------------------------------------------------------
   -- Send pending data:
   -- First try to send any already buffered, then try to get new data
   procedure Send_pending (This : in out Server_type) is
      Awarded : Natural;
      -- Get available bandwidth for sending
      function Get_bandwidth return Natural is
         Aw, Awx : Natural := 0;
         Needed  : Natural;
      begin
         Needed := Natural'Min (
            This.Slot.OBuffer'Length,
            Circular_stream.Available_read (This.Slot.CStream_out));
         Bandwidth.Server_link.Commit (Needed, Aw, Extra => false);
         if Aw < Needed then
            Bandwidth.Server_link.Commit (Needed - Aw, Awx, Extra => true);
         end if;
         return Aw + Awx;
      end Get_bandwidth;
   begin
      -- Flush buffer if necessary:
      if Chronos.Elapsed (This.Slot.ZCron) > 5.0 then
         Chronos.Reset (This.Slot.ZCron);
         if This.Slot.Deflate and then not This.Slot.ZFlushed then
            Zlib.Streams.Flush (This.Slot.ZStream_out);
            This.Slot.ZFlushed := true;
         end if;
      end if;

      while
         This.Slot.OBuffer_used or else
         Circular_stream.Available_read (This.Slot.CStream_out) > Natural'(0)
      loop
         if This.Slot.OBuffer_used then
            -- Check alive:
            if not Socket.Is_alive (this.Slot.Socket) then
               Trace.Log (
                  "G2 server connection lost (" &
                  Id (this) & ")", Trace.Informative);
               Disconnect (this);
               return; -- <-- EXIT BY DEAD SERVER
            end if;
            if not Socket.Is_writable (this.Slot.Socket) then
               Trace.Log ("G2.Send_pending: Soft delayed for " &
                  Id (This), Trace.Never);
               return; -- <-- EXIT BY CONNECTION CONGESTION
            end if;
            begin
               Write (
                  This.Slot.Stream.all,
                  This.Slot.OBuffer (1 .. This.Slot.OLast));
               -- Write successful:
               This.Slot.OBuffer_used := false;
            exception
               when E : Socket.Socket_error =>
                  case Socket.Get_error (E) is
                     when Socket.Operation_would_block =>
                        Trace.Log ("G2.Send_pending: Hard delayed for " &
                           Id (This));
                        return; -- <-- EXIT BY CONNECTION CONGESTION
                     when others =>
                        raise;
                  end case;
            end;
         end if;
         if Circular_stream.Available_read (This.Slot.CStream_out) >
               Natural'(0)
            and then not This.Slot.OBuffer_used
         then
            -- Bandwidth things:
            Awarded := Get_bandwidth;
            if Awarded > 0 then
               -- Reading:
               Circular_stream.Read (
                  This.Slot.CStream_out,
                  This.Slot.OBuffer (
                     1 .. Ada.Streams.Stream_element_offset (Awarded)),
                  This.Slot.OLast);
               This.Slot.OBuffer_used := true;
            else
               return; -- <-- EXIT BY BANDWIDTH THROTTLE
            end if;
         end if;
      end loop;
   exception
      when E : others =>
         Trace.Log ("G2.Core.Send_pending: " & Trace.Report (E),
            Trace.Error);
         Disconnect (this);
   end Send_pending;

   -- Create a new server from a dotted adress:port
   procedure Create (
      this     : out Server_type;
      Net      : in  Network_access;
      Address  : in  String;
      Port     : in  Natural;
      Seen     : in  Calendar.Time := Calendar.Clock) is
   begin
      this.Address   := To_ustring(Address);
      this.Port      := Port;
      this.Network   := Net;
      this.Last_seen := Seen;
   end Create;

   -----------------------
   -- Available_cstream --
   -----------------------
   -- Helper for availables:
   function Available_cstream (
      This : access Ada.Streams.Root_stream_type'Class)
      return Natural is
   begin
      return Circular_stream.Available_read (
         Circular_stream.Object_access (This).all);
   end Available_cstream;

   ------------------
   -- Surveillance --
   ------------------

   -- Monitors that we are really connected.
   task body Connector_type is

      Net : Network_access;   -- Network object it belongs...
      Active_servers : Natural renames Globals.Options.G2_ActiveServers;
      Try_servers    : Natural renames Globals.Options.G2_TryServers;

   begin
      -- Start connecting
      accept Start(this: in Network_access) do
         Net:= this;
         Net.Status:= Network.Connecting;
      end;
      Main: loop
         declare
            Curr_active_servers  : Natural := 0;
            Serv : Server_access;
         begin
            -- End?
            exit when Globals.Requested_exit;
            -- Sleep.
            delay Globals.Options.G2_PollPeriod;
            -- Check to how many servers we are connected:
            Curr_active_servers  := Net.Servers.Status_count (Connected);
            if Curr_active_servers > 0 then
               Net.Status := Network.Connected;
            else
               Net.Status := Network.Connecting;
            end if;

            -- Establish new connections as needed:
            if Curr_active_servers < Active_servers and then
               Net.Servers.Count < Try_servers then
               -- Get new servers and try to connect:
               declare
                  -- Cached:
                  Cached : Server.Object_access_array :=
                     Server.List.Get_best
                       (Network_id, Try_servers - Net.Servers.Count);
                  -- From webcache only if we had not enough cached ones:
                  GWCached : GWCache2.Network_node_array :=
                     GWCache2.Query_any (Network_id,
                        Try_servers - Net.Servers.Count - Cached'Length);
                  New_server : Server_access;
                  Discarded  : Natural := 0;
               begin
                  -- Start connection sequence:
                  -- And add to our list of servers:
                  for n in Cached'Range loop
                     New_server := Server_access (Cached (n));
                     -- Other data:
                     New_server.Network          := Net;
                     New_server.Local_port       := Net.Port;
                     New_server.Connection_start := Calendar.Clock;
                     New_server.Profile_requested:= false;
                     New_server.Connection_stage := Starting;
                     -- Allocate in slots:
                     Net.Servers.Add (New_server);
                     -- Start connection sequence:
		     -- Not necessary because the poll task will do that:
                  end loop;
                  -- Create new G2 servers and add to cache:
                  for n in GWCached'Range loop
                     New_server := new Server_type;
                     Create (New_server.all, Net,
                        To_string (GWCached (n).Address), GWCached (n).Port);
                     begin
                        Server.List.Add (Server.Object_access (New_server));
                     exception
                        when Server.Server_already_cached =>
                           Discarded := Discarded + 1;
                     end;
                  end loop;
                  if GWCached'Length > 0 then
                     Trace.Log ("Added" & Natural'Image (
                        GWCached'Length - Discarded) &
                        " servers from GWebCache2");
                     Trace.Log ("Discarded" & Discarded'Img &
                        " servers from GWebCache2");
                  end if;
               exception
                  when E: others =>
                     Trace.Log ("G2.Connector [Adding servers]: " &
                        Trace.Report (E), Trace.Error);
               end;
            end if;
            while Curr_active_servers > Active_servers loop
               -- We should disconnect newers :
               Net.Servers.Get_newest (Serv);
               exit when Serv = null;
               Disconnect2 (Serv.all, Spare => true);
               Curr_active_servers := Curr_active_servers - 1;
            end loop;
         exception
            when E: others =>
               Trace.Log ("G2.Connector_type [Main loop]: " &
                  Trace.Report (E), Trace.Error);
         end;
      end loop Main;
      Trace.Log ("G2.Connector_type exited");
   exception
      when E: others =>
         Trace.Log ("G2.Connector_type [Body]: " & Trace.Report (E),
            Trace.Error);
   end Connector_type;

   task body Polling_type is

      Net               : Network_access;
      P                 : Duration renames Globals.Options.G2_PollConnection;
      Delta_score       : Server.Rating;
      Serv              : Server_access;
      Serv_aux          : Server_access;
      Serv_next         : Server_access;
      Connected_servers : Natural renames Globals.Options.G2_ActiveServers;
      Must_drop         : Boolean;

   begin
      accept Start (this : in Network_access) do
         Net := this;
      end;
      Delta_score := Server.Rating (P);
      Main: loop
         begin
            exit when Globals.Requested_exit;

            ----------------
            -- DISPATCHER --
            ----------------
            Dispatcher (Net);
            -------------
            -- SERVERS --
            -------------
            Must_drop :=
               Connected_servers <= Net.Servers.Status_count(Connected);
            Net.Servers.Get_first (Serv);
            while Serv /= null loop
               case Serv.Network.Servers.Status (Serv) is
                  when Disconnected =>
                     -- Drop packets pending outbound
                     Serv.Slot.Outbound.Clear;

                     Serv_aux  := Serv;
                     Serv_next := Serv;
                     Net.Servers.Get_next (Serv_next);
                     Net.Servers.Remove (Serv);
                     Serv := Serv_aux;
                     Server.List.Check_in (Server.Object_access (Serv));
                     Serv := Serv_next;
                  when Disconnecting =>
                     null; -- Still cleaning up
                  when Connecting =>
                     if Must_drop then
                        Disconnect (Serv.all);
                     else
                        Connect (Serv.all);
                     end if;
                  when Handshaking =>
                     if Must_drop then
                        Disconnect (Serv.all);
                     else
                        Handshake (Serv.all);
                     end if;
                  when Connected =>
                     -- Increase score:
                     Add_score (Serv.all, Delta_score);
                     -- Check pipes for each connected server:
                     Check_pipes (Serv.all);
                     -- Send outbound packtes
                     if Net.Servers.Status (Serv) = Connected then
                        Sender (Serv);
                     end if;
                     -- General maintenance
                     if Net.Servers.Status (Serv) = Connected then
                        Maintenance (Serv);
                     end if;
               end case;
               if Serv /= null then
                  Net.Servers.Get_next (Serv);
               end if;
            end loop;
         exception
            when E: others =>
               Trace.Log ("G2.Polling_type [Main loop]: " & Trace.Report (E),
                  Trace.Error);
               if Serv /= null then
                  Trace.Log ("G2.Polling_type [Main loop]: " & Id (Serv.all) &
                  ": " & Server_Status'Image (
                     Serv.Network.Servers.Status (Serv)) & ": " &
                     Serv.Connection_stage'Img, Trace.Error);
                  Disconnect (Serv.all);
               end if;
         end;
         -- Sleep
         delay P;
         if Debug.Debug_statistics_enabled then
            Statistics.Object.Set ("Tasking - Server poll",
               Statistics.Booleans.Create (true));
         end if;
      end loop Main;
      Trace.Log ("G2.Polling_type exited");
   exception
      when E: others =>
         Trace.Log("G2.Polling_type [Body]: " & Trace.Report(E), Trace.Error);
   end Polling_type;

   ------------------
   -- Servers pool --
   ------------------
   protected body Server_pool is separate;

   -----------------
   -- Maintenance --
   -----------------
   procedure Maintenance (This : Server_access) is separate;

   ----------------
   -- Dispatcher --
   ----------------
   procedure Dispatcher (Net : access Network_type) is separate;
   ------------
   -- Sender --
   ------------
   task body Sender_udp is separate;
   procedure Sender (This : access Server_type) is separate;

   ------------------------
   -- Processing packets --
   ------------------------
   procedure Process_packet (
      Net    : in Network_access;
      Source : in Server_access;
      Item   : in Packet.Queue.Item_type)
   is separate;

   ----------------------
   -- Creating packets --
   ----------------------

   -- Create a LNI packet with info about us, in respect to the given server.
   function Create_LNI (
      Net: in Network_access;
      Destination : in Server_access) return Packet.Object is

      P        : Packet.Object;   -- Root
      C        : Packet.Object;   -- Children
      Payload  : UString;         -- Payload

   begin
      P := Packet.Create ("LNI");

      -- NA child
      case Socket.IP.Kind (To_string (Destination.Address)) is
         when Socket.IP.Local =>
            Payload := U (To_string (To_address ("127.0.0.1")));
         when Socket.IP.Internal =>
            Payload := U (To_string (To_address (Socket.IP.Get_IP(false))));
         when Socket.IP.Public =>
            Payload := U (To_string (To_address (
               Socket.IP.Get_IP(Internet_route = Direct))));
      end case;

      Payload := Payload & U (To_string (Network.Endian.Convert (
         Net.Port, 2, Packet.Big_endian (P))));

      C := Packet.Create ("NA", S (Payload));
      Packet.Add_child (P, C);

      -- GU child (GUID)
      C := Packet.Create ("GU", GUID.To_char_array (GUID.My_GUID));
      Packet.Add_child (P, C);

      -- V child (vendor code)
      C := Packet.Create ("V", Vendor_code);
      Packet.Add_child (P, C);

      -- LS child (library statistics)
      C := Packet.Create (
         "LS",
         To_string (Network.Endian.Convert (Library.Object.Num_files, 4,
            Packet.Big_endian (P))) &
         To_string (Network.Endian.Convert (Library.Object.Size_files, 4,
            Packet.Big_endian (P))));
      Packet.Add_child (P, C);

      return P;
   end Create_LNI;

   ------------------------------------------------------------------------
   -- Create_KHL                                                         --
   ------------------------------------------------------------------------
   function Create_KHL (Net : in Network_access) return Packet.Object is
      pragma Unreferenced (Net);
      P       : Packet.Object := Packet.Create ("KHL");
      C       : Packet.Object;
      Data    : Ustring;
      Servers : Server.Object_access_array :=
         Server.List.Get_best (G2.Network_id, 20);
   begin
      for N in Servers'Range loop
         Server.List.Check_in (Servers (N));
      end loop;
      C := Packet.Create ("TS",
         To_string (Time_t.Clock, Packet.Big_endian (P)));
      Packet.Add_child (P, C);
      for N in Servers'Range loop -- NO SERVERS REPORTED!!!
         exit;
         Data := U (To_string (To_address (S (
               Server_access (Servers (N)).Address))));
         Data := Data &
            To_string (Network.Endian.Convert (
               Server_access (Servers (N)).Port, 2,
               Packet.Big_endian (P))) &
            String'(1 .. 4 => Character'Val (0));
         C := Packet.Create ("CH", S (Data));
         Packet.Add_child (P, C);
      end loop;
      return P;
   end Create_KHL;

   ------------
   -- Report --
   ------------
   function Report (Net : Network_type) return Report_array is
   begin
      return Net.Servers.Report;
   end Report;

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet via TCP
   procedure Send (
      this           : in     Server_access;
      P              : in     Packet.Object;
      In_response_to : in     Packet.Object := Packet.Null_packet) is

      I : Packet.Queue.Item_type;
      use Packet.Safe_child;
   begin
      if This = null then
         return;
      end if;

      I.Packet := P;
      I.Tcp_id := U (Id (This.all));
      I.Source := Packet.Queue.Server;
      if In_response_to /= Packet.Null_packet then
         I.In_response_to := In_response_to;
      end if;

      if this.Slot /= null then
         this.Slot.Outbound.Put (I);
      end if;
   end Send;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Initialize (this : in out Server_type) is
      pragma Unreferenced (This);
   begin
      null;
--      begin
--         Statistics.Object.Update (
--            Stat_servers,
--            Statistics.Integers.Increment'Access,
--            Statistics.Integers.Create (1));
--      exception
--         when others => null;
--      end;
   end Initialize;
   procedure Adjust (this : in out Server_type) is
      pragma Unreferenced (This);
   begin
      null;
--      begin
--         Statistics.Object.Update (
--            Stat_servers,
--            Statistics.Integers.Increment'Access,
--            Statistics.Integers.Create (1));
--      exception
--         when others => null;
--      end;
   end Adjust;
   procedure Finalize (this : in out Server_type) is
      use type Xml.Node;
   begin
      if this.User_profile /= null then
         Xml.Delete (this.User_profile);
         this.User_profile := null;
      end if;
--     Trace.Log ("<----");
--      begin
--         Statistics.Object.Update (
--            Stat_servers,
--            Statistics.Integers.Increment'Access,
--            Statistics.Integers.Create (-1));
--      exception
--         when others => null;
--      end;
      Adagio.Server.Finalize (Adagio.Server.Object (This));
   end Finalize;

   --------------------
   -- Connected hubs --
   --------------------
   function Connected_hubs return Natural is
   begin
      return The_network.Servers.Status_count (Connected);
   end Connected_hubs;

   ------------------------------------------------------------------------
   -- Hubs_http_handler                                                  --
   ------------------------------------------------------------------------
   procedure Hubs_http_handler (
      Data : out Agpl.Http.Server.Sort_Handler.Data_set)
   is
      Serv  : Server_access;
      Score : Natural;
   begin
      The_network.Servers.Get_first (Serv);
      while Serv /= null loop
         begin
            Score := Natural (Serv.Score);
         exception
            when Constraint_error =>
               Score := 0;
         end;
         declare
            use Agpl.Http.Server.Sort_handler;
            use Ada.Calendar;
            Row    : Data_row;
            CC     : Agpl.Geoip.Country_code :=
               Agpl.Geoip.Country_code_from_addr (Id (Serv.all));
            Uptime : Duration := Clock - Serv.Connection_start;
         begin
            -- Country code flag
            if CC = "??" then
               Append (Row, (U ("unknown"), Null_ustring));
            else
               Append (Row, (U (CC), Null_ustring));
            end if;
            -- Country
            if CC /= "??" then
               Append (Row, (
                  U (Agpl.Geoip.Country_name_from_code (CC)),
                  U (Agpl.Geoip.Country_name_from_code (CC))));
            else
               Append (Row, (
                  U (Agpl.Geoip.Country_name_from_code (CC)),
                  U ("Zz")));
            end if;
            -- IP:port
            Append (Row, (U (Id (Serv.all)), U (Id (Serv.all))));
            -- Status
            Append (Row, (
               U (Server_status'Image (The_network.Servers.Status (Serv))),
               U (Server_status'Image (The_network.Servers.Status (Serv)))));
            -- Nick
            Append (Row, (
               U (Xml.Get_attribute ("identity/handle", "primary",
                  Serv.User_profile, "Anonymous")),
               U (Xml.Get_attribute ("identity/handle", "primary",
                  Serv.User_profile, "Anonymous"))));
            -- User_agent
            Append (Row, (Serv.Slot.User_agent, Serv.Slot.User_agent));
            -- Leaf load
            Append (Row, (
               U (Misc.To_string (Serv.Num_leaves) & "/" &
                  Misc.To_string (Serv.Max_leaves)),
               Rpad (Serv.Num_leaves)));
            -- Qrp status
            if Serv.QRT_status /= Sending then
               Append (Row, (
                  U (Serv.QRT_status'Img), U (Serv.QRT_status'Img)));
            else
               Append (Row, (
                  U (Serv.QRT_status'Img & " (" &
                     Misc.To_string (Serv.QRT_packets_sent) & "/" &
                     Misc.To_string (Serv.QRT_packets + 1) & ")"),
                  U (Serv.QRT_status'Img & S (RPad (Serv.QRT_packets_sent))))
                  );
            end if;
            -- Rating
            Append (Row, (
               U (Misc.To_string (Serv.Score, 0)),
               Rpad (Score, 6)));
            -- Uptime
            Append (Row, (
               U (Misc.Image (Uptime)),
               Rpad (Natural (Uptime))));
            -- Packets in
            Append (Row, (
               U (Misc.To_string (Serv.Packets_in)),
               Rpad (Serv.Packets_in)));
            -- Packets out
            Append (Row, (
               U (Misc.To_string (Serv.Packets_out)),
               Rpad (Serv.Packets_out)));

            -- Row
            Append (Data, Row);
         end;
         The_network.Servers.Get_next (Serv);
      end loop;
   end Hubs_http_handler;

begin

   The_network := new G2.Core.Network_type;
   Network.List.Add (Network.Object_access (The_network));

   Trace.Log ("G2_server size: " &
      Integer'Image (Server_type'size / 8));

--   Statistics.Object.Set (Stat_servers,
--      Statistics.Integers.Create (0));

   Hardcoded_servers:
      declare
         Serv  : Server_access;
         Nodes : Xml.Node_array := Xml.Get_all ("network/Gnutella2/root",
            Globals.Config);
         use Strings.Fields;
      begin
         for N in Nodes'Range loop
            declare
               Full_addr : String renames
                  Xml.Get_attribute (Nodes (N), "address", "");
               Addr : String := Select_field (Full_addr, 1, ':');
               Port : String := Select_field (Full_addr, 2, ':');
            begin
               if Addr /= "" and then Port /= "" then
                  Serv := new Server_type;
                  Create (Serv.all, The_network, Addr, Natural'Value (Port));
                  Serv.Score := Server.Rating (
                     Xml.Utils.Get_num (Nodes (N), "rating", Natural'Last));
                  Serv.Is_root := true;
                  Server.List.Add (Server.Object_access (Serv));
                  Trace.Log ("Added root Gnutella2 server: " & Full_addr);
               else
                  Trace.Log ("Cannot add invalid Gnutella2 server: " &
                     Full_addr, Trace.Warning);
               end if;
            end;
         end loop;
      end Hardcoded_servers;

end Adagio.G2.Core;
