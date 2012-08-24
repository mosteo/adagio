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
--  $Id: adagio-g2-packet-queue.ads,v 1.9 2004/02/29 20:36:42 Jano Exp $

-- Queues for inbound/outbound G2 packets:

with Adagio.Globals.Options;
with Adagio.Socket;

with Ada.Finalization;           use Ada;

with Charles.Lists.Double.Unbounded;

package Adagio.G2.Packet.Queue is

   pragma Elaborate_body;

   type Object;
   type Object_access is access all Object;
   type Object_array is array (Positive range <>) of Object_access;

   -- Pairs Address/Queue for various uses
   type Address_Queue is record
      Address : Ustring;
      Queue   : Packet.Queue.Object_Access;
   end record;

   type Address_Queue_Array is array (Positive range <>) of Address_Queue;
   type Address_Queue_Array_Access is access all Address_Queue_Array;

   -- For safety reasons, we'll not store more that these:
   -- Same value is used for inbound/outbound limits:

   Max_pending_packets : Natural renames Globals.Options.G2_MaxPacketQueueLength;

   type Source_type is (Listener_tcp, Listener_udp, Server);

   type Item_type is new Finalization.Controlled with record
      Source          : Source_type;         -- Source type

      Tcp_Id          : Ustring;             -- Source server id for tcp

      Udp_source      : Socket.Sock_addr_type;
      Udp_destination : Socket.Sock_addr_type;
      Udp_safe        : Boolean := false;    -- Should be ACKed?

      Packet          : G2.Packet.Object :=  -- The packet
         Null_packet;
      In_response_to  : G2.Packet.Object :=  -- Packet being answered
         Null_packet;
   end record;
   procedure Initialize (This : in out Item_type);
   procedure Adjust     (This : in out Item_type);
   procedure Finalize   (This : in out Item_type);

   ---------------------
   -- protected queue --
   ---------------------

   -- helper things:
   package BQueue is new Charles.Lists.Double.Unbounded (Item_type, "=");

   protected type Object is
      -- Add a packet to the queue:
      -- As a special case, POs will go to the head of the queue
      procedure Put (Item : in Item_type);

      -- Remove a packet:
      entry Get (Item : out Item_type);

      -- Look at the first packet:
      -- Success will be false if there aren't packets
      procedure Peek (Item : out Item_type; Success : out Boolean);

      -- Defer a packet from a server:
      procedure Defer (Tcp_id : in String);

      -- Forget all data
      procedure Clear;

      -- Says if empty
      function Is_empty return Boolean;

      -- Says length
      function Length return Natural;

   private

      -- The true queue:
      Data : BQueue.Container_type;

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
      In_response_to : in     Packet.Object := Packet.Null_packet);

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet via TCP
   procedure Send (
      this           : in out Object;
      P              : in     Packet.Object;
      Tcp_id         : in     String;
      In_response_to : in     Packet.Object := Packet.Null_packet);

end Adagio.G2.Packet.Queue;
