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
--  $Id: adagio-g2-core-sender_udp.adb,v 1.7 2004/01/21 21:05:26 Jano Exp $

separate (Adagio.G2.Core)
task body Sender_udp is
   Net         : Network_access;
   Latency     : Duration;

   ------------
   -- To_udp --
   ------------
   procedure To_udp (
      Network : in Network_access; 
      I       : in Packet.Queue.Item_type) is
      Dummy   : Packet.Queue.Item_type;
   begin
      -- Remove the peeked packet
      Net.Outbound.Get (Dummy);
      -- Here it goes!
      G2.Transceiver.Send (Network.Transceiver.all, I);
      
      Trace.Log ("  --> UDP/" & Socket.Image (
         I.Udp_destination) & " " &
         Packet.To_hex (I.Packet) & " (Latency:" &
         Duration'Image (Latency) & ") (Size:" &
         Packet.Full_length (I.Packet)'Img & ")", 
         File => S (Logfile));
   exception
      when others =>
         raise;
   end To_udp;

begin
   -- Wait for start
   select
      accept Start (Network : in Network_access) do
         Net := Network;
      end;
   or
      terminate;
   end select;

   -- Forever loop
   while not Globals.Requested_exit loop
      declare
         Empty_item : Packet.Queue.Item_type;
         Item       : Packet.Queue.Item_type;
         Success    : Boolean;
         use type Packet.Object;
         use type Packet.Queue.Source_type;
      begin
         Item := Empty_item;
         delay Globals.Options.G2_SendPeriod;

         -- UDP PACKETS
         -- Get an UDP packet and dispatch it:
         Net.Outbound.Peek (Item, Success);

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
                  Raise_exception (Constraint_error'Identity,
                     "Unexpected TCP packet in UDP queue");
               when Packet.Queue.Listener_udp =>
                  To_udp    (Net, Item);
               when others =>
                  raise Unimplemented;
            end case;
         end if;
      exception
         when E : others =>
            Trace.Log (
               "G2.Core.Sender_udp: " & Trace.Report (E), 
               Trace.Error);
      end;
   end loop;
   Trace.Log ("G2.Core.Sender_udp exited", Trace.Informative);
end Sender_udp;
