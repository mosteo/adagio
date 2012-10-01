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
--  $Id: adagio-g2-core-maintenance.adb,v 1.4 2004/01/21 21:05:25 Jano Exp $

separate (Adagio.G2.Core)
procedure Maintenance (This : Server_access) is
   Net             : Network_access := This.Network;

   type Reset_payload is record
      Command  : Byte     := 0;
      Entries  : Positive;
      Infinity : Byte     := 1;
   end record;
   for Reset_payload use record
      Command  at 0 range 0 .. 7;
      Entries  at 1 range 0 .. 31;
      Infinity at 5 range 0 .. 7;
   end record;
   for Reset_payload'Size use 8 * 6;

   ------------------------------------------------------------------------
   -- Update_QRT                                                         --
   ------------------------------------------------------------------------
   procedure Reset_QRT (Server : in Server_access) is
      P     : Packet.Object;
      Reset : Reset_payload := (0, 2 ** Library.QRP_size, 1);
      S     : Mmap.Strings.Object := Mmap.Strings.Create (Reset'Address);
   begin
      P := Packet.Create ("QHT", S.all (S.all'First .. S.all'First + 5));
      Send (Server, P);
   end Reset_QRT;

   procedure Update_QRT (Server : in Server_access) is
      Patch_size : Constant Natural := 256; -- in bytes, for fragments


      type Send_payload is record
         Command        : Byte := 1;
         Fragment_no    : Byte;
         Fragment_count : Byte;
         Compression    : Byte := 1;
         Bits           : Byte := 1;
      end record;
      for Send_payload use record
         Command        at 0 range 0..7;
         Fragment_no    at 1 range 0..7;
         Fragment_count at 2 range 0..7;
         Compression    at 3 range 0..7;
         Bits           at 4 range 0..7;
      end record;
      for Send_payload'Size use 5 * 8;

      P     : Packet.Object;
   begin
      if Server.QRT_timestamp < Library.Object.Get_QRP_timestamp then
         Server.QRT_timestamp    := Calendar.Clock;
         -- Send table
         declare
            Table  : String (1 .. 2 ** Library.QRP_size / 8) :=
               Library.Object.Get_QRP;
            Patch  : Stream_element_array (1 .. 2 ** Library.QRP_size / 8);
            for Patch'Address use Table'Address;
            Pragma Import (Ada, Patch);
            ZPatch : Stream_element_array := Zutil.Deflate (Patch);
            Num_frags : Natural := (ZPatch'Length + 1) / Patch_size + 1;
            Send   : Send_payload;
            SMap   : Mmap.Strings.Object :=
               Mmap.Strings.Create (Send'Address);
            Pos    : Stream_element_offset := ZPatch'First;
         begin
            Server.QRT_packets_sent := 0;
            Server.QRT_packets      := Num_frags;
            Server.QRT_status       := Sending;

            -- Check sizes.
            if Num_frags > Natural (Byte'Last) then
               Trace.Log (
                  "G2.Core.Maintenance_task.Send_QRP: Table too large:" &
                  Natural'Image (Num_frags), Trace.Warning);
               return;
            end if;
            -- Send reset
            Reset_QRT (Server);

            -- Send fragments
            Send.Fragment_count := Byte (Num_frags);
            for N in 1 .. Num_frags loop
               Send.Fragment_no := Byte (N);
               P := Packet.Create ("QHT",
                  SMap.all (1 .. Send_payload'Size / 8) &
                  Misc.To_string (ZPatch (Pos .. Stream_element_offset'Min (
                     Pos + Stream_element_offset (Patch_size) - 1,
                     ZPatch'Last))));
               Pos := Pos + Stream_element_offset (Patch_size);
               Core.Send (Server, P);
            end loop;
         end;
         P := Packet.Create ("PO");
         Core.Send (Server, P);
      else
         -- Mark it as sent anyways just in case it's empty and never will be sent:
         Server.QRT_Status := Sent;
      end if;
   end Update_QRT;

begin
   declare
      Now  : Calendar.Time := Calendar.Clock; -- Now time for reference.
      Serv : Server_access := This;
   begin
      ---------------------------------
      -- Check connection starvation --
      ---------------------------------
      if Now - Serv.Last_packet_time >
         Globals.Options.G2_PingTimeout
      then
         -- Disconnect from server:
         Disconnect (Serv.all);
         Trace.Log ("G2.Core.Maintenance: Dropping server " &
            Id (Serv.all) & " because of timeout",
            Trace.Informative);
      elsif Now - Serv.Last_packet_time >
         Globals.Options.G2_PingDelay then
         if Now - Serv.Last_ping_time > 5.0 then
            -- Mark time
            Serv.Last_ping_time := Now;
            -- Send a ping.
            declare
               P    : Packet.Object := Packet.Create ("PI");
            begin
               -- TCP ping
               Send (Serv, P);
               -- UDP ping
               Packet.Queue.Send (
                  Net.Outbound,
                  P,
                  Socket.To_address (
                     S (Serv.Address) & ":" &
                     Misc.To_string (Serv.Port)),
                  Safe => true);
            end;
         end if;
      end if;
      -----------------------------
      -- Update node information --
      -----------------------------
      if Now - Serv.Last_update > Globals.Options.G2_ServerInfoDelay then
         Serv.Last_update := Now;
         -- LNI
         Send (Serv, Create_LNI (Net, Serv));
         -- KHL
         Send (Serv, Create_KHL (Net));
      end if;
      ---------------------
      -- Request profile --
      ---------------------
      if not Serv.Profile_requested then
         declare
            P    : Packet.Object := Packet.Create ("UPROC");
         begin
            Send (Serv, P);
            Serv.Profile_requested := true;
         end;
      end if;
      ----------------
      -- Update QRP --
      ----------------
      if not Serv.QRT_reset then
         Reset_QRT (Serv);
         Serv.QRT_Reset := true;
      end if;
      if Now - Serv.Checked_QRP >
         Globals.Options.G2_QRTUpdatePeriod and then
         Now - Serv.Connection_start > Globals.Options.G2_QRTDelay
      then
         Serv.Checked_QRP := Now;
         Update_QRT (Serv);
      end if;
      --------------------
      -- Apt for search --
      --------------------
      if Globals.Options.Download_Active and then
         (not Serv.Apt_For_Search) and then
         Now - Serv.Connection_Start > 15.0
      then
         Serv.Apt_For_Search := true;
         G2.Search.Set_Queues (Net.Searcher, Net.Servers.Get_Queues_And_Addresses);
         G2.Search.Set_Start_Nodes (Net.Searcher, Net.Servers.Address_List);
      end if;

   exception
      when E : others =>
         Trace.Log (
            "G2.Core.Maintenance: " & Trace.Report (E), Trace.Error);
   end;
end Maintenance;
