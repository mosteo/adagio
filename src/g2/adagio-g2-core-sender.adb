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
--  $Id: adagio-g2-core-sender.adb,v 1.14 2004/03/01 00:41:57 Jano Exp $

separate (Adagio.G2.Core)
procedure Sender (This : access Server_type) is
   Net         : Network_access;
   Latency     : Duration;
   Stall       : Boolean := false;

   ---------------
   -- To_server --
   ---------------
   procedure To_server (
      Serv : in Server_access; I : in Packet.Queue.Item_type) is
      Dummy   : Packet.Queue.Item_type;
      Size    : Natural := Packet.Full_length (I.Packet);
      Success : Boolean;
   begin
      -- Server is delayed?
      if Calendar.Clock < Serv.Delayed_send then
         Stall := true;
         return;
      end if;

      -- Packet too large?
      if Size > Packet.Max_packet_size then
         Trace.Log ("G2.Core.Sender.To_server: Dropping packet too large",
            Trace.Warning);
         Net.Outbound.Get (Dummy);
         return;
      end if;

      if not Socket.Is_alive (Serv.Slot.Socket) then
         Stall := true;
         Disconnect (Serv.all);
         Trace.Log ("G2.Core.Sender: Connection reset by peer: " 
            & Id (Serv.all));
      elsif not Socket.Is_writable (Serv.Slot.Socket) then
         -- Trace.Log ("G2.Core.Sender: Packet deferred for server " &
         --   Id (Serv.all) & " (Link full)");
         Stall := true;
         return;
      else -- Sending!
         -- Deflate or not deflate:
         if not Serv.Slot.Deflate then
            Success :=
               Circular_stream.Available_write (Serv.Slot.CStream_out) 
               >= Size;
            if Success then
               Packet.Write (Serv.Slot.CStream_out'Access, I.Packet);
            end if;
         else -- DEFLATE
            Success := -- With security margin:
               Circular_stream.Available_write (Serv.Slot.CStream_out) >
               2 * Size;
            if Success then
               Packet.Write (
                  Serv.Slot.ZStream_out'Unrestricted_access, I.Packet);
               Serv.Slot.ZFlushed := false;
            end if;
         end if;
         if Success then
            -- Stat sent packets
            G2.Packets_sent  := Natural'Min (
               G2.Packets_sent  + 1, Natural'Last - 1);
            Serv.Packets_out := Natural'Min (
               Serv.Packets_out + 1, Natural'Last - 1);

            -- Mark traffic
            Traffic.Add ((
               Arrival  => Calendar.Clock,
               Protocol => Protocol_descr,
               Way      => Traffic.Outgoing,
               From     => I.Tcp_id,
               Name     => U (G2.Packet.Name (I.Packet)),
               Data     => U (G2.Packet.To_hex (I.Packet))));

            Trace.Log ("  --> TCP/" & S (I.Tcp_Id) & " " &
               Packet.To_hex (I.Packet) & " (Latency:" &
               Duration'Image (Latency) & ") (Size:" & Size'Img & ")", 
               File => S (Logfile));
            if Serv.QRT_status = Sending then
               if Packet.Is_a (I.Packet, "/QHT") then
                  Serv.QRT_packets_sent := Serv.QRT_packets_sent + 1;
                  Add_score (Serv.all, 2.0);
                  if Serv.QRT_packets_sent > Serv.QRT_packets then
                     Serv.QRT_status := Sent;
                     Add_score (Serv.all, 300.0);
                  end if;
               end if;
            end if;
            Serv.Slot.Outbound.Get (Dummy);
         else
            Stall := true;
            Serv.Last_delay := 
               Duration'Min (Serv.Last_delay * 1.5, 5.0);
            Serv.Delayed_send := Calendar.Clock + Serv.Last_delay;
            Trace.Log ("G2.Core.Sender: Packet deferred for server " 
               & Id (Serv.all) & 
               " (Socket full or cbuffer exhausted)", Trace.Debug);
         end if;
      end if;
   exception
      when E : others =>
         Trace.Log ("G2.Core.Sender.To_server: " & Trace.Report (E),
            Trace.Error);
         Disconnect (Serv.all);
         Stall := true;
   end To_server;

begin
   Net := This.Network;
   declare
      Item       : Packet.Queue.Item_type;
      Success    : Boolean;
      use type Packet.Object;
      use type Packet.Queue.Source_type;
   begin
      -- TCP PACKETS / DEFLATE LINKS
      declare
         Server : Server_access := Server_access (This);
      begin
         Globals.Main_throttle.Start_work;
         loop
            -- Pending packets
            Server.Slot.Outbound.Peek (Item, Success);

            if Success then
               if Item.In_response_to /= Packet.Null_packet then
                  Latency := Calendar.Clock - 
                     Packet.Arrival_time (Item.In_response_to);
               else
                  Latency := Calendar.Clock - 
                     Packet.Arrival_time (Item.Packet);
               end if;

               case Item.Source is
                  when Packet.Queue.Server =>
                     To_server (Server, Item);
                  when others =>
                     Raise_exception (Constraint_error'Identity,
                        "Unexpected " & Item.Source'Img &
                        " packet in TCP queue");
               end case;
            end if;

            -- Send pending data
            Send_pending (Server.all);
            exit when Stall or else Server.Slot.Outbound.Is_empty;
            Globals.Main_throttle.Cycle_work;
         end loop;
         Globals.Main_throttle.End_work;

      exception
         when E : others =>
            Trace.Log (
               "G2.Core.Sender: " & Trace.Report (E), 
               Trace.Error);
      end;
   exception
      when E : others =>
         Trace.Log (
            "G2.Core.Sender: " & Trace.Report (E), 
            Trace.Error);
   end;
end Sender;
