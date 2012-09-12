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
--  $Id: adagio-g2-transceiver_types-prot.ads,v 1.7 2004/03/29 19:13:31 Jano Exp $

With
Interfaces,
Ada.Calendar,
Adagio.Constants,
Adagio.Types,
Adagio.G2.Packet.Queue,
Adagio.Os.Memory,
Adagio.Socket;

Use
Adagio.Constants,
Adagio.Types;


package Adagio.G2.Transceiver_types.Prot is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is limited private;
   type Object_access is access all Object;

   ------------------------------------------------------------------------
   -- Get_Outbound_Udp_Delay                                             --
   ------------------------------------------------------------------------
   -- Returns the estimated time to deplete the outbound udp data queue
   function Get_Outbound_Udp_Delay (This : in Object) return Duration;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Activate it!
   -- Receives a binded udp socket which will be used for
   --  incoming and outcoming data.
   -- Receives a packet queue wherein arrived packets will be added.
   procedure Start (
      this  : in out Object;
      S     : in Socket.Object_access;
      Queue : in Packet.Queue.Object_access);

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet, securely or not:
   procedure Send (
      This : in out Object;
      Item : in     G2.Packet.Queue.Item_type);

   ------------------------------------------------------------------------
   -- Set_BW_limits                                                      --
   ------------------------------------------------------------------------
   procedure Set_BW_limits (
      This   : in out Object;
      BW_in  : in     Speed;
      BW_out : in     Speed);

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   -- The end.
   procedure Shutdown (this : in out Object);

   -----------
   -- Stats --
   -----------
   type Stats is (Pending_in, Pending_out, Throttled_out);

   function Get_stat (This : in Object; Stat : in Stats) return Natural;

private

   ------------------------------------------------------------------------
   -- Core_type                                                          --
   ------------------------------------------------------------------------
   protected type Core_type (Parent : access Object) is
      ---------------------
      -- Add_safe_packet --
      ---------------------
      procedure Add_safe_packet (Packet : access Packet_type);
      ----------------------
      -- Allocate_inbound --
      ----------------------
      procedure Allocate_inbound (
         From  : in     Packet_access;
         P     : out    Packet_access;
         Frags : in     Natural);
      -----------------------
      -- Allocate_outbound --
      -----------------------
      procedure Allocate_outbound (P : out Packet_access; Frags : in Natural);
      ---------------
      -- Available --
      ---------------
      -- Packets available for reading
      function Available return Boolean;
      --------------------------------
      -- Available_inbound/outbound --
      --------------------------------
      function Available_inbound  return Natural;
      function Available_outbound return Natural;
      --------------
      -- Complete --
      --------------
      procedure Complete (Packet : in out Packet_access);
      ---------------------
      -- Drop_out_packet --
      ---------------------
      procedure Drop_out_packet (
         nSequence  : in Interfaces.Unsigned_16;
         Success    : in Boolean);
      -----------------------
      -- Get_older_inbound --
      -----------------------
      procedure Get_older_inbound (P : out Packet_access);
      ------------------
      -- Get_rcv_time --
      ------------------
      function Get_rcv_time return Calendar.Time;
      -------------------
      -- Get_send_time --
      -------------------
      function Get_send_time return Calendar.Time;
      ----------------------------
      -- Get_Outbound_Udp_Delay --
      ----------------------------
      -- Says the remaining time to empty the outbound queue
      function Get_Outbound_Udp_Delay return Duration;
      -------------
      -- Lengths --
      -------------
      function Length_by_arrival return Natural;
      function Length_by_source return Natural;
      function Length_outbound return Natural;
      function Length_throttled_out return Natural;
      --------------------
      -- Receive_packet --
      --------------------
      procedure Receive_packet;
      --------------------
      -- Remove_inbound --
      --------------------
      procedure Remove_inbound (Id : in String);
      --------------------------
      -- Remove_older_inbound --
      --------------------------
      procedure Remove_older_inbound;
      --------------------
      -- Retry_fragment --
      --------------------
      procedure Retry_fragment (Part : in out Pending_retransmission);
      --------------
      -- Send_ack --
      --------------
      procedure Send_ack (P : in Packet_access);
      ----------
      -- Send --
      ----------
      procedure Send (Item : in G2.Packet.Queue.Item_type);
      -- Debug:
      procedure Send (Item : in G2.Packet.Queue.Item_type; Date : in Calendar.Time);
      -------------------
      -- Send_fragment --
      -------------------
      -- Only enqueues it
      procedure Send_fragment (
         Packet : in out Packet_access;
         Part   : in     Interfaces.Unsigned_8);
      --------------------
      -- Send_fragments --
      --------------------
      procedure Send_fragments (Packet : in out Packet_access);
      ----------------------------------
      -- Send_next_fragment_to_socket --
      ----------------------------------
      -- Sends it for real and sets next sending time.
      -- Also says if there was something to be sent.
      procedure Send_next_fragment_to_socket (Sent : out Boolean);
      -----------
      -- Start --
      -----------
      procedure Start (
         S     : in Socket.Object_access;
         Queue : in G2.Packet.Queue.Object_access);
   private

      Udp      : Socket.Object_access;
      G2_queue : G2.Packet.Queue.Object_access;

      Packets_in_by_arrival : PLbA.Container_type;
      Packets_in_by_source  : PLbS.Container_type;
      Last_inbound          : Calendar.Time := Past_aeons;

      Packets_out   : Fragment_list.Container_type;

      Next_packet : Unsigned_16 := 1;  -- Counter

      Next_receive : Calendar.Time := Calendar.Clock;
      Next_sending : Calendar.Time := Calendar.Clock;
      Udp_msgs     : Udp_message_list.List;
      Udp_msgs_size: Natural := 0; -- To estimate remaining time.

   end Core_type;

   ------------------------------------------------------------------------
   -- Dispatcher_task                                                    --
   ------------------------------------------------------------------------
   -- Periodically peeks for new packets and processes timeous
   task type Dispatcher_task (Parent : access Object) is
      pragma Storage_Size (1024 * 1024);
      entry Start;
   end Dispatcher_task;

   ------------------------------------------------------------------------
   -- Sender_task                                                        --
   ------------------------------------------------------------------------
   task type Sender_task (Parent : access Object) is
      pragma Storage_Size (Os.Memory.Max_Stack_Size);
      entry Start;
   end Sender_task;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is limited record
      Core       : Core_type (Object'Access);
      Dispatcher : Dispatcher_task (Object'Access);
      Sender     : Sender_task (Object'Access);
      Timeouts   : Event_queue.Object (
         Stack_size => 128 * 1024);

      BW_in      : Speed := Speed'Last; -- For throttling
      BW_out     : Speed := Speed'Last;
   end record;

   ------------------------------------------------------------------------
   -- Types for retransmission event queue                               --
   ------------------------------------------------------------------------
   type Retry_context is new Agpl.Event_Queues.Context_type with record
      Retry       : Pending_retransmission;
      Transceiver : Object_access;
   end record;
   type Retry_access is access all Retry_context;

   ------------------------------------------------------------------------
   -- Queue_retry                                                        --
   ------------------------------------------------------------------------
   procedure Queue_retry (Context : in Agpl.Event_Queues.Context_Type'Class);

end Adagio.G2.Transceiver_types.Prot;
