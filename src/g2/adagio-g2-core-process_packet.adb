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
--  $Id: adagio-g2-core-process_packet.adb,v 1.8 2004/02/24 15:26:10 Jano Exp $

separate (Adagio.G2.Core)
procedure Process_packet (
   Net    : in Network_access; 
   Source : in Server_access;
   Item   : in Packet.Queue.Item_type) is
   
   P     : G2.Packet.Object renames Item.Packet;
   Serv  : Server_access;

   use type G2.Search.Object_Access;
   use type Packet.Object;
   use type Packet.Queue.Source_type;
   use type Xml.Node;

   package Conv renames Adagio.Network.Endian;

   ------------------------------------------------------------------------
   -- Broadcast_packet                                                   --
   ------------------------------------------------------------------------
   -- Echoes a packet to all connected servers except supplied one:
   -- R = In_response_to
   procedure Broadcast_packet (
      P      : in Packet.Object; 
      R      : in Packet.Object := Packet.Null_packet;
      Server : in Server_access := null) is

      Serv : Server_access;
   begin
      Net.Servers.Get_first (Serv);
      while Serv /= Null loop
         if Serv /= Server and then Net.Servers.Status (Serv) = Connected then
            Send (Serv, P, R);
         end if;
         Net.Servers.Get_next (Serv);
      end loop;
   end Broadcast_packet;

   ------------------------------------------------------------------------
   -- Process_PI_UDP                                                     --
   ------------------------------------------------------------------------
   procedure Process_PI_UDP is
      Server : Server_access;
      C      : Packet.Object := Packet.Create ("RELAY");
   begin
      Packet.Add_child (P, C);
      Net.Servers.Get_first (Server);
      while Server /= null loop
         if Id (Server.all) /= Item.Tcp_Id and then
            Net.Servers.Status (Server) = Connected
         then
            declare
               I : Packet.Queue.Item_type;
            begin
               Packet.Queue.Send (
                  Net.Outbound,
                  P,
                  Socket.To_address (Id (Server.all)),
                  Safe => true,
                  In_response_to => I.Packet);
            end;
         end if;
         Net.Servers.Get_next (Server);
      end loop;
   end Process_PI_UDP;

   ------------------------------------------------------------------------
   -- Process_KHL                                                        --
   ------------------------------------------------------------------------
   procedure Process_KHL is
      Hubs  : Packet.Object_array := Packet.Get_children (Item.Packet, "CH");
      Hubs2 : Packet.Object_array := Packet.Get_children (Item.Packet, "NH");
      Discarded   : Natural := 0;
      Remote_time : Time_t.Time_t;
      Has_times   : Boolean;
      use type Time_t.Time_t;
   begin
      if Packet.Is_a (Item.Packet, "/KHL/TS") then
         Has_times := true;
         Remote_time := To_time_t (Packet.Payload (Packet.Get_child ( 
               Item.Packet, "TS")), Packet.Big_endian (
                  Packet.Get_child (Item.Packet, "TS")));
      end if;
      for N in Hubs2'Range loop
         declare
            Address : String := Packet.Payload (Hubs2 (N));
            Sock    : Socket.Sock_addr_type;
            Server  : Server_access;
         begin
            if Address'Length /= 10 and then Address'Length /= 6 then
               raise Unimplemented; -- Not an IPv4 address
            end if;
            Sock := To_address (
               Address (Address'First .. Address'First + 5), 
               Packet.Big_endian (Item.Packet));

            Server := new Server_type;
            Server.Score := 250.0; -- Below GWebCached2 ones but above CH
            Create (Server.all, Net, Socket.Image (Sock.Addr), 
               Natural (Sock.Port));
            if Reachable (Server.all) then
               Adagio.Server.List.Add (Adagio.Server.Object_access (Server));
            else
               Adagio.Server.Free (Adagio.Server.Object_access (Server));
               Discarded := Discarded + 1;
            end if;
         exception
            when Adagio.Server.Server_already_cached =>
               Discarded := Discarded + 1;
            when E : others =>
               Trace.Log ("G2.Core.Process_packet.Process_KHL/NH: " &
                  Trace.Report (E), Trace.Warning);
         end;
      end loop;
      for N in Hubs'Range loop
         declare
            Address : String := Packet.Payload (Hubs (N));
            Sock    : Socket.Sock_addr_type;
            Server  : Server_access;
            Seen    : Time_t.Time_t := To_time_t (
               Address (Address'Last - 3 .. Address'Last),
                     Packet.Big_endian (Item.Packet));
            Seen2   : Calendar.Time;
         begin
            if Address'Length /= 10 and then Address'Length /= 6 then
               exit;
               raise Unimplemented; -- Not an IPv4 address
            end if;
            Sock := To_address (
               Address (Address'First .. Address'First + 5), 
               Packet.Big_endian (Item.Packet));
            if Has_times then
               Seen2 := 
                  Calendar.Clock - Time_t.To_duration (Remote_time - Seen);
            else
               Seen2 := Calendar.Clock;
            end if;

            Server := new Server_type;
            Server.Score := 200.0; -- Below GWebCached2 ones and NH ones
            Create (Server.all, Net, Socket.Image (Sock.Addr), 
               Natural (Sock.Port), Seen2);
            if Reachable (Server.all) then
               Adagio.Server.List.Add (Adagio.Server.Object_access (Server));
            else
               Adagio.Server.Free (Adagio.Server.Object_access (Server));
               Discarded := Discarded + 1;
            end if;
         exception
            when Adagio.Server.Server_already_cached =>
               Discarded := Discarded + 1;
            when E : others =>
               Trace.Log ("G2.Core.Process_packet.Process_KHL/CH: " &
                  Trace.Report (E), Trace.Warning);
         end;
      end loop;
      Trace.Log ("G2.Core.Process_packet.Process_KHL: Added" & 
         Natural'Image (Hubs'Length + Hubs2'Length - Discarded) & 
         " servers from KHL.");
      Trace.Log ("G2.Core.Process_packet.Process_KHL: Discarded" & 
         Natural'Image (Discarded) & " servers from KHL.");
   end Process_KHL;

   ------------------------------------------------------------------------
   -- Process_Q2                                                         --
   ------------------------------------------------------------------------
   procedure Process_Q2 is
      Neighbours : Ustring_array := Net.Servers.Address_list;
      Last : Natural := Neighbours'Last;
      Tcp_queue : Packet.Queue.Object_access;
   begin
      if Item.Source = Packet.Queue.Listener_UDP then
         Trace.Log ("G2.Core.Process_packet: Query received via UDP", 
            Trace.Warning);
         return; -- Only hubs should receive that.
      end if;

      if Source /= null then
         Tcp_queue := Source.Slot.Outbound'Access;
      end if;

      -- Check against our databases:
      -- DN queries:
      if Packet.Is_a (Item.Packet, "/Q2/DN") then
         G2.Local_query.DN (
            Unicode.G2_to_string (Packet.Payload (Packet.Get_child (
               Item.Packet, "DN")), Packet.Big_endian (Item.Packet)),
            Item,
            Net.Outbound'Access, 
            Tcp_queue,
            Net.Port,
            Neighbours (1 .. Last));
      end if;
      -- URN queries:
      if Packet.Is_a (Item.Packet, "/Q2/URN") then
         G2.Local_query.URN (
            Item, 
            Net.Outbound'Access,
            Tcp_queue,
            Net.Port,
            Neighbours (1 .. Last));
      end if;
      -- MD queries:
      if Packet.Is_a (Item.Packet, "/Q2/MD") then
         G2.Local_query.MD (
            Item, 
            Net.Outbound'Access,
            Tcp_queue,
            Net.Port,
            Neighbours (1 .. Last));
      end if;
   exception
      when Unicode.Invalid_encoding =>
         Trace.Log ("G2.Process_packet (Q2): Received query with unsupported"&
            " language.");
   end Process_Q2;

   ------------------------------------------------------------------------
   -- Process_PUSH                                                       --
   ------------------------------------------------------------------------
   procedure Process_PUSH is
      Addr : Socket.Sock_addr_type;
   begin
      Addr := To_address (
         Packet.Payload (Item.Packet), Packet.Big_endian (Item.Packet));
      Adagio.Connect.Peer_Manager.Object.Add (Resolver_Tcp.Create_Pushed (Addr));
   end Process_PUSH;
begin
   -----------
   -- /?/TO --
   -----------
   if Packet.Get_child (P, "TO") /= Packet.Null_packet and then
      Guid.To_char_array (Guid.My_guid) /= 
         Packet.Payload (Packet.Get_child (P, "TO")) then
      Trace.Log ("G2.Core.Process_packet: /?/TO received for " &
         Packet.Payload (Packet.Get_child (P, "TO")));
      -- Shouldn't get these being a leaf!
      raise Unimplemented;
   -- /PI --
   ---------
   elsif Packet.Is_a (P, "/PI") then
      declare
         Po : G2.Packet.Object;
      begin
         Po := G2.Packet.Create ("PO");

         if Item.Source = Packet.Queue.Server then
            Send (Source, Po, Item.Packet);
         else
            Packet.Queue.Send (
               Net.Outbound, 
               Po, 
               Item.Udp_source,
               Safe => true,
               In_response_to => Item.Packet);
         end if;
      end;
      -------------
      -- /PI/UDP --
      -------------
      if Packet.Is_a (P, "/PI/UDP") and not Packet.Is_a (P, "/PI/RELAY") then
         Process_PI_UDP;
      elsif Packet.Is_a (P, "/PI/UDP") and Packet.Is_a (P, "/PI/RELAY") then
         -- Should not receive relays being a leaf node!
         -- Trace.Log ("Process_Packet: /PI/RELAY received while being leaf.", Trace.Warning);
         null;
      elsif not Packet.Is_a (P, "/PI/UDP") then
         null; -- regular PI
      else -- other combinations not valid:
         Trace.Log ("G2.Core.Process_packet: Invalid packet received:", 
            Trace.Informative);
         Trace.Log (Packet.To_hex (P));
         Packet.Parsing.Trace_tree (P);
      end if;
   ---------
   -- /PO --
   ---------
   elsif Packet.Is_a (P, "/PO") then
      case Item.Source is
         when Packet.Queue.Server =>
            null;
         when others =>
            null;
      end case;
   ----------
   -- /LNI --
   ----------
   elsif Packet.Is_a (P, "/LNI") then
      -- Get statistics of server:
      if Packet.Is_a (P, "/LNI/HS") then
         if Item.Source = Packet.Queue.Server then
            Net.Servers.Get (S (Item.Tcp_Id), Connected, Serv);
            if Serv /= null then
               declare
                  Data : String := Packet.Payload (
                     Packet.Get_child (P, "HS"));
               begin
                  -- Update statistics
                  Serv.Num_leaves := Conv.Convert (
                     Conv.To_byte_array (Data (Data'First .. Data'First + 1)),
                     Packet.Big_endian (P));
                  Serv.Max_leaves := Conv.Convert (
                     Conv.To_byte_array (
                        Data (Data'First + 2 .. Data'First + 3)),
                     Packet.Big_endian (P));
               end;
            end if;
         end if;
      end if;
   ------------
   -- /UPROC --
   ------------
   elsif Packet.Is_a (P, "/UPROC") then
      declare
         I     : Packet.Queue.Item_type := Item;
      begin
         I.Packet          := Core.Create_UPROD;
         I.In_response_to  := Item.Packet;
         I.Udp_destination := Item.Udp_source;
         Send (Source, I.Packet, Item.Packet);
      end;
   ------------
   -- /UPROD --
   ------------
   elsif Packet.Is_a (P, "/UPROD") then
      -- Parse profile
      if Packet.Is_a (P, "/UPROD/XML") then
         Net.Servers.Get (Item.Tcp_Id, Connected, Serv);
         if Serv /= null then
            declare
               Data : String := Unicode.G2_to_String (
                  Packet.Payload (Packet.Get_child (Item.Packet, "XML")),
                  Packet.Big_endian (Item.Packet));
               Doc  : Xml.Document;
            begin
               begin
                  Doc  := Xml.From_string (Data);
               exception
                  when others =>
                     Trace.Log (Data, Trace.Error);
                     raise;
               end;
               if Serv.User_profile /= null then
                  Xml.Delete (Serv.User_profile);
               end if;
               Serv.User_profile := Doc;
            end;
         end if;
      end if;
   ---------
   -- /Q2 --
   ---------
   elsif Packet.Is_a (P, "/Q2") then
--      Trace.Log ("Query received:");
--      Packet.Parsing.Trace_tree (P);
      Process_Q2;
   -------------
   -- /HAW    --
   -------------
   elsif Packet.Is_a (P, "/HAW") then
      null;
   -------------
   -- /KHL    --
   -------------
   elsif Packet.Is_a (P, "/KHL") then
      Process_KHL;
   -------------
   -- /PUSH   --
   -------------
   elsif Packet.Is_a (P, "/PUSH") then
      Process_PUSH;
   --------------------
   -- SEARCH RELATED --
   --------------------
   elsif Packet.Is_a (P, "/QKA") or else
         Packet.Is_a (P, "/QA") or else
         Packet.Is_a (P, "/QH2")
   then
      if Net.Searcher /= null then
         G2.Search.Process_Search_Packet (Net.Searcher, Item);
      end if;
   ------------------
   -- DISCARDABLES --
   ------------------
   elsif Packet.Is_a (P, "/Q1") or else
         Packet.Is_a (P, "/QKR")
   then
      null;
   -------------
   -- UNKNOWN --
   -------------
   else
      Trace.Log ("Discarded unknown packet: " & G2.Packet.To_hex (P), 
         Trace.Informative);
      Packet.Parsing.Trace_tree (P);
   end if;
exception
   when E: others =>
      Trace.Log ("G2.Core.Process_packet: " & Trace.Report (E), Trace.Error);
      Trace.Log ("G2.Core.Process_packet: " & Packet.To_hex (Item.Packet), 
         Trace.Error);
end Process_packet;
