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
--  $Id: adagio-g2-transceiver_types.ads,v 1.7 2004/03/29 19:13:31 Jano Exp $

with Adagio.Globals.Options;
with Adagio.Socket;

with Agpl.Event_queues;
with Agpl.Event_queues.Calendar;
with Agpl.Streams;

with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;
with Charles.Maps.Sorted.Unbounded;
with Charles.Sets.Sorted.Unbounded;

with Ada.Calendar;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Streams;  use Ada.Streams;
with Ada.Unchecked_deallocation;
use  Ada;

with Interfaces; use Interfaces;

package Adagio.G2.Transceiver_types is

   pragma Elaborate_body;

   -- Maximum inbound/outbound pending packets:
   Max_packets : Natural renames Globals.Options.G2_UdpBuffers;

   package Event_Queue renames Agpl.Event_Queues.Calendar;

private


   Protocol_descr : constant Ustring := U ("G2 UDP");
   Safe_descr     : constant array (Boolean) of Ustring :=
      (true => U ("(Safe) "), false => Null_Ustring);

   Stat_alive_packets : constant String :=
      "Network - G2 - UDPT - Alive packets";

   -- Maximum fragments for a packet:
   Max_fragments     : constant := 256;

   -- Max size in bytes for acceptable inbound fragments:
   Max_fragment_size : constant := 1024;

   -- Max size for a full G2 packet:
   Max_packet_size   : constant := Max_Fragments * Max_Fragment_Size;

   -- Size for outbound fragments:
   Fragment_size     : constant := 500;

   -- Time between retransmissions:
   Retry_timeout     : constant := 8.0;

   -- Maximum life time for pending packets:
   Transmit_timeout  : constant := 26.0;

   -- Maximum time an incomplete inbound is stored:
   Receive_timeout   : constant := 30.0;

   Gnd_tag : constant String := "GND";

   -- Flags:
   Flag_deflate : constant Unsigned_8 := 2#0000_0001#; -- 1
   Flag_ack     : constant Unsigned_8 := 2#0000_0010#; -- 2

   -- HEADER
   type Packet_header is record
      szTag     : String (1 .. 3);
      nFlags    : Interfaces.Unsigned_8;
      nSequence : Interfaces.Unsigned_16;
      nPart     : Interfaces.Unsigned_8;
      nCount    : Interfaces.Unsigned_8;
   end record;
   for Packet_header use record
      szTag     at 0 range 0 .. 23;
      nFlags    at 3 range 0 .. 7;
      nSequence at 4 range 0 .. 15;
      nPart     at 6 range 0 .. 7;
      nCount    at 7 range 0 .. 7;
   end record;
   for Packet_header'Size use 8 * 8;
   Header_size : constant := 8;

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   function To_string (H : Packet_header) return String;

   -- FRAGMENT
   type Fragment_type is new Ada.Finalization.Controlled with record
      Data      : Agpl.Streams.Stream_Element_Array_Access;
      Valid     : Boolean := false; -- Ack for sents, received for incomings

      Timeout   : Event_queue.Event_type;
   end record;
   type Fragment_access is access all Fragment_type;

   procedure Adjust   (This : in out Fragment_Type);
   procedure Finalize (This : in out Fragment_Type);
   procedure Set_Data (This : in out Fragment_Type;
                       Data : in     Stream_Element_Array);
   --  Allocate if needed and copy Data to the fragment.

   type Fragment_array is array (Integer range <>) of Fragment_type;

   -- PACKET
   type Packet_type (Count : Natural) is new
      Finalization.Limited_Controlled with
   record
      Header      : Packet_header;
      Fragments   : Fragment_array (1 .. Count);
      Arrived     : Calendar.Time := Calendar.Clock;
      Source      : Socket.Sock_addr_type;
      Destination : Socket.Sock_addr_type;
      Safe        : Boolean := false;
   end record;
   type Packet_access is access all Packet_type;

   procedure Initialize (This : in out Packet_type);
--   procedure Adjust     (This : in out Packet_type);
   procedure Finalize   (This : in out Packet_type);
-- for Packet_access'Storage_pool use Debug_pool;

   -- Identify inbound packets:
   function Id_inbound (This : in Packet_type) return String;

   -- Functions for inbound packet lists
   function Older (Left, Right : in Packet_access) return Boolean;

   package Packet_list_by_arrival is new
      Charles.Sets.Sorted.Unbounded (Packet_access, Older, "=");
   function Equal (Left, Right : in Packet_list_by_arrival.Iterator_type)
      return Boolean;
   package Packet_list_by_source is new
      Charles.Maps.Hashed.Strings.Unbounded (
         Packet_list_by_arrival.Iterator_type,
         Charles.Hash_string, "=", Equal);
   package PLbA renames Packet_list_by_arrival;
   package PLbS renames Packet_list_by_source;

   function Soft_Equal (Left, Right : in Packet_access) return Boolean;
   package Fragment_list is new Charles.Maps.Sorted.Unbounded (
      Interfaces.Unsigned_16, Packet_access, Interfaces."<", Soft_Equal);

   -- Helper for retransmissions
   type Pending_retransmission is record
      nSequence : Interfaces.Unsigned_16;
      nPart     : Interfaces.Unsigned_8;
      Failures  : Natural := 0;
   end record;

   -----------------------------------------------------------------------
   -- Is_Complete                                                       --
   -----------------------------------------------------------------------
   function Is_complete (this : in Packet_type) return Boolean;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new Unchecked_deallocation (Packet_type, Packet_access);

   ------------------------------------------------------------------------
   -- Cache of sending data                                              --
   ------------------------------------------------------------------------
   type Udp_message (Last : Ada.Streams.Stream_Element_Offset) is record
      Data : Ada.Streams.Stream_element_array (1 .. Last);
      Dest : Socket.Sock_addr_type;
      Date : Calendar.Time;
   end record;

   ------------
   -- Create --
   ------------
   function Create (
      Data : in Stream_element_array; Dest : Socket.Sock_addr_type)
      return Udp_message;
   pragma Inline (Create);

   package Udp_message_list is new Ada.Containers.Indefinite_Doubly_Linked_Lists (
      Udp_message, "=");

end Adagio.G2.Transceiver_types;
