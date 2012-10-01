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
--  $Id: adagio-g2-core-dispatcher.adb,v 1.6 2004/02/29 20:36:41 Jano Exp $

separate (Adagio.G2.Core)
procedure Dispatcher (Net : access Network_type) is
begin
   declare
      Item : Packet.Queue.Item_type;
      use type Packet.Queue.Source_type;
      Serv : Server_access;
   begin
      -- Get a packet and process it:
      Globals.Main_throttle.Start_work;
      while not Net.Inbound.Is_empty loop
         Net.Inbound.Get (Item);

         if Item.Source = Packet.Queue.Server then
            Trace.Log ("  <-- TCP/" & S (Item.Tcp_Id) & " " &
               Packet.To_hex (Item.Packet), File => S (Logfile));
            Net.Servers.Get (S (Item.Tcp_id), Connected, Serv);
         elsif Item.Source = Packet.Queue.Listener_udp then
            Trace.Log ("  <-- UDP/" & Socket.Image (
               Item.Udp_source) & " " &
               Packet.To_hex (Item.Packet), File => S (Logfile));
            Net.Servers.Get (
               Socket.Image (Item.Udp_source), Connected, Serv);
            if Serv /= null then
               Serv.Last_packet_time := Calendar.Clock;
            end if;
         end if;

         Process_packet (Network_access (Net), Serv, Item);
         if not Net.Inbound.Is_empty then
            Globals.Main_throttle.Cycle_work;
         end if;
      end loop;
      Globals.Main_throttle.End_work;
   exception
      when E : others =>
         if Serv /= null then
            Trace.Log ("G2.Core.Dispatcher: " & S (Serv.Slot.User_agent),
            Trace.Error);
         end if;
         Trace.Log (
            "G2.Core.Dispatcher: " & Trace.Report (E), 
            Trace.Error);
   end;
end Dispatcher;
