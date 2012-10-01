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
--  $Id: adagio-g2-packet-queue.adb,v 1.8 2004/02/29 20:36:42 Jano Exp $

-- Queue for inbound G2 packets:
with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Trace;

package body Adagio.G2.Packet.Queue is

   use type Packet.Object;

   Stat_item : constant String := "Network - G2 - Queued packets";

   ----------------------
   -- Controlled queue --
   ----------------------
   protected body Object is

      -- Add a packet to the queue:
      procedure Put (Item : in Item_type) is
      begin
         if BQueue.Length (Data) > Max_pending_packets then
            Trace.Log ("G2.Packet.Queue: Queue full, dropping packet " &
               To_hex (Item.Packet), Trace.Warning);
         else
            if Is_a (Item.Packet, "/PO") then
               BQueue.Prepend (Data, Item);
            else
               BQueue.Append (Data, Item);
            end if;
            Statistics.Object.Update (
               Stat_item,
               Statistics.Integers.Increment'Access,
               Statistics.Integers.Create (1));
         end if;
      end Put;

      -- Remove a packet:
      entry Get (Item : out Item_type) when not BQueue.Is_empty (Data) is
      begin
         Item := BQueue.Element (BQueue.First (Data));
         BQueue.Delete_first (Data);
         if Item.Packet = Packet.Null_packet then 
            Trace.Log ("******* null in get");
         end if;
         Statistics.Object.Update (
            Stat_item,
            Statistics.Integers.Increment'Access,
            Statistics.Integers.Create (-1));
      end Get;

      -- Look at the first packet:
      procedure Peek (Item : out Item_type; Success : out Boolean) is
      begin
         if BQueue.Is_empty (Data) then
            Success := false;
         else
            Item := BQueue.Element (BQueue.First (Data));
            if Item.Packet = Packet.Null_packet then 
               Trace.Log ("****** null in G2.Packet.Queue.Peek", Trace.Error);
               Success := false;
            else
               Success := true;
            end if;
         end if;
      end Peek;

      -- Defer a packet from a server:
      procedure Defer (Tcp_id : in String) is
         use BQueue;
         I    : Iterator_type := First (Data);
         Item : Item_type;
      begin
         while I /= Back (Data) loop
            Item := Element (I);
            if Item.Source = Listener_tcp and then
               Item.Tcp_id /= Tcp_id then
               -- Move this packet to front:
               Delete (Data, I);
               Prepend (Data, Item);
               return;
            else
               I := Succ (I);
            end if;
         end loop;
      end Defer;

      -----------
      -- Clear --
      -----------
      procedure Clear is
      begin
         Statistics.Object.Update (
            Stat_item,
            Statistics.Integers.Increment'Access,
            Statistics.Integers.Create (-BQueue.Length (Data)));
         BQueue.Clear (Data);
      end Clear;
      
      -- Says if empty
      function Is_empty return Boolean is
      begin
         return BQueue.Is_empty (Data);
      end Is_empty;

      -- Says length
      function Length return Natural is
      begin
         return BQueue.Length (Data);
      end Length;

   end Object;

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet via UDP
   procedure Send (
      this           : in out Object;
      P              : in     Packet.Object;
      Destination    : in     Socket.Sock_addr_type;
      Safe           : in     Boolean := false;
      In_response_to : in     Packet.Object := Packet.Null_packet) is

      I : Item_type;
      use Packet.Safe_child;
   begin
      I.Source          := Listener_udp;
      I.Packet          := P;
      I.Udp_destination := Destination;
      I.Udp_safe        := Safe;
      if In_response_to /= Null_packet then
         I.In_response_to := In_response_to;
      end if;

      this.Put (I);
   end Send;

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet via TCP
   procedure Send (
      this           : in out Object;
      P              : in     Packet.Object;
      Tcp_id         : in     String;
      In_response_to : in     Packet.Object := Packet.Null_packet) is

      I : Item_type;
      use Packet.Safe_child;
   begin
      I.Source := Server;
      I.Packet := P;
      I.Tcp_id := U (Tcp_id);
      if In_response_to /= Null_packet then
         I.In_response_to := In_response_to;
      end if;

      this.Put (I);
   end Send;

   ------------------------------------------------------------------------
   -- Control                                                            --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Item_type) is
      pragma Unreferenced (This);
   begin
      null;
--      Statistics.Object.Update (
--         Stat_item,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (1));
   end Initialize;
   procedure Adjust     (This : in out Item_type) is
      pragma Unreferenced (This);
   begin
      null;
--      Statistics.Object.Update (
--         Stat_item,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (1));
   end Adjust;
   procedure Finalize   (This : in out Item_type) is
      pragma Unreferenced (This);
   begin
      null;
--      Statistics.Object.Update (
--         Stat_item,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (-1));
   end Finalize;

begin
   Statistics.Object.Set (
      Stat_item,
      Statistics.Integers.Create (0)); -- There are five static items around
end Adagio.G2.Packet.Queue;
