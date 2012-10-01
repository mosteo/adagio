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
--  $Id: adagio-g2-core-stats.adb,v 1.1 2004/03/29 19:13:31 Jano Exp $

with Adagio.G2.Transceiver;

package body Adagio.G2.Core.Stats is

   function Pending_udp_in  return Natural is -- not completely received
   begin
      return G2.Transceiver.Get_stat (
         The_network.Transceiver.all, 
         G2.Transceiver.Pending_in);
   end Pending_udp_in;

   function Pending_udp_out return Natural is -- not ACKed
   begin
      return G2.Transceiver.Get_stat (
         The_network.Transceiver.all, 
         G2.Transceiver.Pending_out);
   end Pending_udp_out;

   function Pending_udp_out_throttle return Natural is -- not sent
   begin
      return G2.Transceiver.Get_stat (
         The_network.Transceiver.all, 
         G2.Transceiver.Throttled_out);
   end Pending_udp_out_throttle;

   function Pending_udp_out_latency return Natural is -- Penaly due to throttling
   begin
      return 
         Natural (G2.Transceiver.Get_Outbound_Udp_Delay (The_network.Transceiver.all));
   end Pending_udp_out_latency;

   function Remote_Searching_Latency return Duration is
      use type G2.Search.Object_Access;
   begin
      if The_Network.Searcher /= null then
         return G2.Search.Get_Latency (The_Network.Searcher);
      else
         return 0.0;
      end if;
   end Remote_Searching_Latency;

   function Get_Searching_Candidate_Hubs return Natural is
      use type G2.Search.Object_Access;
   begin
      if The_Network.Searcher /= null then
         return G2.Search.Get_Tracked_Hubs (The_Network.Searcher);
      else
         return 0;
      end if;
   end Get_Searching_Candidate_Hubs;
   
   function Get_Searching_Hubs return Natural is
      use type G2.Search.Object_Access;
   begin
      if The_Network.Searcher /= null then
         return G2.Search.Get_Hubs (The_Network.Searcher);
      else
         return 0;
      end if;
   end Get_Searching_Hubs;

   function Get_Searching_Leaves return Natural is
      use type G2.Search.Object_Access;
   begin
      if The_Network.Searcher /= null then
         return G2.Search.Get_Leaves (The_Network.Searcher);
      else
         return 0;
      end if;
   end Get_Searching_Leaves;

end Adagio.G2.Core.Stats;
