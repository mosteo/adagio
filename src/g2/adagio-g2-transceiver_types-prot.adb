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
--  $Id: adagio-g2-transceiver_types-prot.adb,v 1.18 2004/03/29 19:13:30 Jano Exp $

With
Ada.Exceptions,
Ada.Streams,
Ada.Unchecked_conversion,
Adagio.Trace,
Adagio.Chronos,
Adagio.G2.Packet,
Adagio.G2.Packet.Parsing,
Adagio.Globals.Options,
Adagio.Memory_stream_constrained,
Adagio.Misc,
Adagio.Network_Settings,
Adagio.Security,
Adagio.Socket.IP,
Adagio.Statistics,
Adagio.Statistics.Integers,
Adagio.Traffic,
Adagio.Zutil,
Zlib;


use  Ada;
use Ada.Exceptions;
use Ada.Streams;


package body Adagio.G2.Transceiver_types.Prot is

   use type Calendar.Time;
   use type Fragment_list.Iterator_type;

   ------------------------------------------------------------------------
   -- Get_Outbound_Udp_Delay                                             --
   ------------------------------------------------------------------------
   -- Returns the estimated time to deplete the outbound udp data queue
   function Get_Outbound_Udp_Delay (This : in Object) return Duration is
   begin
      return This.Core.Get_Outbound_Udp_Delay;
   end Get_Outbound_Udp_Delay;

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
      Queue : in Packet.Queue.Object_access) is
   begin
      This.Core.Start (S, Queue);
      This.Dispatcher.Start;
      This.Sender.Start;
   end Start;

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet, securely or not:
   procedure Send (
      This : in out Object;
      Item : in     G2.Packet.Queue.Item_type) is
      use type Network_Settings.Routings;
   begin
      if Network_Settings.Internet_Route = Network_Settings.None and then
         Socket.IP.Is_Public (Socket.Image (Item.Udp_Destination))
      then
         Trace.Log ("G2.Transceiver.Send: Dropped unroutable public packet while firewalled.", Trace.Debug);
         return; -- <-- EARLY EXIT, DROPPED UNROUTABLE PACKET
      end if;

      --declare
      --   Aux : Calendar.Time := Calendar.Clock;
      --begin
      --   This.Core.Send (Item, Aux);
      --end;

      This.Core.Send (Item);
      G2.Packets_sent := Natural'Min (
         G2.Packets_sent + 1, Natural'Last - 1);

      -- Mark traffic
      Traffic.Add ((
         Arrival  => Calendar.Clock,
         Protocol => Protocol_descr,
         Way      => Traffic.Outgoing,
         From     => U (Socket.Image (Item.Udp_destination)),
         Name     => U (G2.Packet.Name (Item.Packet)),
         Data     => Safe_descr (Item.Udp_safe) &
                     U (G2.Packet.To_hex (Item.Packet))));
   end Send;

   ------------------------------------------------------------------------
   -- Set_BW_limits                                                      --
   ------------------------------------------------------------------------
   procedure Set_BW_limits (
      This   : in out Object;
      BW_in  : in     Speed;
      BW_out : in     Speed) is
   begin
      This.BW_in  := BW_in;
      This.BW_out := BW_out;
   end Set_BW_limits;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   -- The end.
   procedure Shutdown (this : in out Object) is
   begin
      Event_queue.Shutdown (This.Timeouts);
   end Shutdown;

   -----------
   -- Stats --
   -----------
   function Get_stat (This : in Object; Stat : in Stats) return Natural is
   begin
      case Stat is
         when Pending_in =>
            return This.Core.Length_by_arrival;
         when Pending_out =>
            return This.Core.Length_outbound;
         when Throttled_out =>
            return This.Core.Length_throttled_out;
      end case;
   end Get_stat;

   ------------------------------------------------------------------------
   -- Core_type                                                          --
   ------------------------------------------------------------------------
   protected body Core_type is
      procedure Debug_Out is
         I : Fragment_list.Iterator_type := Fragment_list.First (Packets_out);
         J : Fragment_list.Iterator_type;
      begin
         Trace.log ("PACKETS_OUT CONTENTS");
         while I /= Fragment_list.Back (Packets_out) loop
            Trace.log ("KEY:" & Interfaces.Unsigned_16'Image (
               Fragment_list.Key (I)));
            J := Fragment_list.Find (Packets_out,
               Fragment_list.Key (I));
            Trace.log ("KEYBIS:" & Interfaces.Unsigned_16'Image (
               Fragment_list.Key (J)));
            I := Fragment_list.Succ (I);
         end loop;
	end Debug_out;

	Pragma Unreferenced( Debug_Out );
      ---------------------
      -- Add_safe_packet --
      ---------------------
      -- Success says if the packet is stored in the list
      procedure Add_safe_packet (Packet : access Packet_type) is
         Retry : Retry_context;
      begin
         Retry.Transceiver := Object_access (Parent);
         -- Insert in list of tracked packets:
--         Trace.Log ("INSERTING SAFE PACKET ------------>" &
--            Packet.Header.nSequence'Img);
         Fragment_list.Insert (
            Packets_out, Packet.Header.nSequence, Packet_access (Packet));
--         Debug_out;
         -- Create timeouts:
         Retry.Retry.nSequence := Packet.Header.nSequence;
         for N in 1 .. Integer (Packet.Header.nCount) loop
            Retry.Retry.nPart := Interfaces.Unsigned_8 (N);
            Event_queue.Create (
               Parent.Timeouts,
               Packet.Fragments (N).Timeout,
               Calendar.Clock + Retry_timeout,
               Queue_retry'Access,
               Retry);
         end loop;
         -- Done
      end Add_safe_packet;
      ----------------------
      -- Allocate_inbound --
      ----------------------
      -- Allocates a new packet and drops the oldest one if already at limit
      procedure Allocate_inbound (
         From  : in     Packet_access;
         P     : out    Packet_access;
         Frags : in     Natural)
      is
         It    : PLbA.Iterator_type;
      begin
         if Available_inbound = 0 then
            -- Delete oldest:
            Trace.Log (
               "G2.Transceiver: Dropping oldest inbound packet.",
               Trace.Warning);
            Remove_older_inbound;
         end if;
         P := new Packet_type (Count => Frags);
         P.Header := From.Header;
         P.Source := From.Source;

         -- Ensure no duplicate arrival times in structure:
         -- (Could use a multiset here, shouldn't be a problem but...)
         loop
            if P.Arrived /= Last_inbound then
               Last_inbound := P.Arrived;
               exit;
            end if;
            delay 0.000001;
            P.Arrived := Calendar.Clock;
         end loop;

         -- Add to the two indexes:
         PLbA.Insert (Packets_in_by_arrival, P, It);
         PLbS.Insert (Packets_in_by_source,  Id_inbound (P.all), It);
      end Allocate_inbound;
      -----------------------
      -- Allocate_outbound --
      -----------------------
      -- Allocates a new packet and drops the oldest one if already at limit
      procedure Allocate_outbound (
         P : out Packet_access; Frags : in Natural) is
      begin
         if Available_outbound = 0 then
            P := Fragment_list.Element (Fragment_list.First (Packets_out));
            if P = null then
               Trace.Log ("G2.Transceiver: Empty outbound queue while " &
                  " Available_outbound = 0!", Trace.Warning);
            else
               Drop_out_packet (P.Header.nSequence, Success => false);
               Trace.Log ("G2.Transceiver: Dropping oldest outbound packet",
                  Trace.Warning);
            end if;
         end if;
         P := new Packet_type (Count => Frags);
      end Allocate_outbound;
      ---------------
      -- Available --
      ---------------
      function Available return Boolean is
      begin
         return Socket.Available (Udp.all) > 0;
      end Available;
      --------------------------------
      -- Available_inbound/outbound --
      --------------------------------
      function Available_inbound  return Natural is
      begin
         return Natural (Integer'Max (Max_packets - Length_by_source, 0));
      end Available_inbound;
      function Available_outbound return Natural is
      begin
         return Natural (Integer'Max (Max_packets - Length_outbound, 0));
      end Available_outbound;
      --------------
      -- Complete --
      --------------
      -- Do completion of a packet:
      procedure Complete (Packet : in out Packet_access) is
         function Is_compressed return Boolean is
         begin
            return (Packet.Header.nFlags and Flag_deflate) /= 0;
         end Is_compressed;

         Length : Stream_element_offset := 0;
         Item   : G2.Packet.Queue.Item_type;

      begin
         -- Defaults:
         Item.Source     := G2.Packet.Queue.Listener_udp;
         Item.Udp_source := Packet.Source;

         -- Reconstruct data
         for N in Packet.Fragments'Range loop
            Length := Length + Packet.Fragments (N).Data'Length;
         end loop;
         declare
            Data : Stream_element_array (1 .. Length);
            Pos  : Stream_element_offset := Data'First;
         begin
            for N in Packet.Fragments'Range loop
               Data (Pos .. Pos + Packet.Fragments (N).Data'Length - 1) :=
                  Packet.Fragments (N).Data.all;
               Pos := Pos + Packet.Fragments (N).Data'Length;
            end loop;
            begin
               if Is_compressed then
                  declare
                     ZData : constant Stream_element_array := Zutil.Inflate (Data);
                  begin
                     Item.Packet := G2.Packet.Parsing.From_element_array (ZData);
                  exception
                     when Zlib.Zlib_Error =>
                        Trace.Log ("Transceiver.Complete: Zlib error inflating packet with" &
                           Integer'Image (Packet.Fragments'Length) & " fragments",
                           Trace.Warning);
                     Remove_inbound (Id_inbound (Packet.all));
                     return;
                  end;
               else
                  Item.Packet := G2.Packet.Parsing.From_element_array (Data);
               end if;
            exception
               when G2.Packet.Parse_Error =>
                  Trace.Log ("G2.Transceiver.Complete: Malformed packet from " & Socket.Image (Item.Udp_source),
                     Trace.Warning);
                  Remove_inbound (Id_inbound (Packet.all));
                  raise;
            end;
            G2_queue.Put (Item);
            -- Mark traffic
            Traffic.Add ((
               Arrival  => Calendar.Clock,
               Protocol => Protocol_descr,
               Way      => Traffic.Incoming,
               From     => U (Socket.Image (Item.Udp_source)),
               Name     => U (G2.Packet.Name (Item.Packet)),
               Data     => Safe_descr (
                              (Packet.Header.nFlags and Flag_ack) /= 0)
                           & U (G2.Packet.To_hex (Item.Packet))));
         end;
         -- Trace.Log ("G2.Transceiver: Deleting completed received (" &
         --   Id_inbound (Packet.all) & ")");

         -- Count a complete packet received
         G2.Packets_received := Natural'Min (
            G2.Packets_received + 1, Natural'Last - 1);

         Remove_inbound (Id_inbound (Packet.all));
         -- Supposedly it's the same being deleted from the collection
         -- Free (Packet);
      end Complete;
      ---------------------
      -- Drop_out_packet --
      ---------------------
      -- We'll search a packet and free/delete it.
      -- It could not be there if has been dropped by excess timeout.
      procedure Drop_out_packet (
         nSequence : in Interfaces.Unsigned_16;
         Success   : in Boolean)
      is
         R : Fragment_list.Iterator_type := Fragment_list.Find (
            Packets_out, nSequence);
         P : Packet_access;
      begin
         if R /= Fragment_list.Back (Packets_out)  then
            P := Fragment_list.Element (R);
            -- Cancel timeouts:
            for N in 1 .. Integer (P.Header.nCount) loop
               if not P.Fragments (N).Valid then
                  Event_queue.Cancel (
                     Parent.Timeouts, P.Fragments (N).Timeout);
               end if;
            end loop;
            -- Delete
            if Success then
               -- Trace.Log ("G2.Transceiver: Packet" &
               --   nSequence'Img & " acknowledged.",
               --   File => S (Logfile));
               null;
            else
               --Trace.Log ("G2.Transceiver: Packet" &
               --nSequence'Img & " out dropped.",
               --File => S (Logfile));
               null;
            end if;
            Fragment_list.Delete (Packets_out, R);
            Free (P);
         else
            Trace.Log (
               "G2.Transceiver.Drop_out_packet: Target packet missing ("
               & Misc.To_string (Natural (nSequence)) & ")",
               Trace.Warning);
         end if;
      end Drop_out_packet;
      -----------------------
      -- Get_older_inbound --
      -----------------------
      procedure Get_older_inbound (P : out Packet_access) is
         use PLbA;
         F : constant Iterator_type := First (Packets_in_by_arrival);
      begin
         if F = Back (Packets_in_by_arrival) then
            P := null;
         else
            P := Element (F);
         end if;
      end Get_older_inbound;
      ------------------
      -- Get_rcv_time --
      ------------------
      function Get_rcv_time return Calendar.Time is
      begin
         return Next_receive;
      end Get_rcv_time;
      -------------------
      -- Get_send_time --
      -------------------
      function Get_send_time return Calendar.Time is
      begin
         return Next_sending;
      end Get_send_time;
      ----------------------------
      -- Get_Outbound_Udp_Delay --
      ----------------------------
      -- Says the remaining time to empty the outbound queue
      function Get_Outbound_Udp_Delay return Duration is
      begin
         return Duration (Udp_Msgs_Size) / Duration (Parent.BW_Out);
      end Get_Outbound_Udp_Delay;
      -------------
      -- Lengths --
      -------------
      function Length_by_arrival return Natural is
      begin
         return PLbA.Length (Packets_in_by_arrival);
      end Length_by_arrival;
      function Length_by_source return Natural is
      begin
         return PLbS.Length (Packets_in_by_source);
      end Length_by_source;
      function Length_outbound return Natural is
      begin
         return Fragment_list.Length (Packets_out);
      end Length_outbound;
      function Length_throttled_out return Natural is
      begin
         return Natural (Udp_message_list.Length (Udp_msgs));
      end Length_throttled_out;
      --------------------
      -- Receive_packet --
      --------------------
      procedure Receive_packet is
         Buffer : aliased Stream_element_array
           (1 .. Max_fragment_size + Header_size);
         for Buffer'Alignment use 4;
         Last   : Stream_element_offset;
         Addr   : Socket.Sock_addr_type;
         P      : Packet_access;
         Packet : aliased Packet_type (0);
         Head   : Packet_header;
         for Head'Address use Buffer'Address;
         pragma Import (Ada, Head);
      begin
         begin
            Socket.Receive (Udp.all, Buffer, Last, Addr);

            -- Set next reception time:
            Next_receive := Calendar.Clock +
               Duration (Last) / Duration (Parent.BW_in);
            --Trace.Log ("Scheduled delay for udp rcv : " &
            --   Duration'Image (
            --   Duration (Last) / Duration (Parent.BW_in)),
            --   Trace.Debug);

            -- Stat received packets
            --G2.Packets_received := Natural'Min (
            --   G2.Packets_received + 1, Natural'Last - 1);

            if not Security.Is_allowed (Addr.Addr) then
               Trace.Log ("G2.Transceiver.Receive_packet: Packet dropped " &
                  "from " & Socket.Image (Addr.Addr) & " [Security ban]",
                  Trace.Debug);
               return;
            end if;
         exception
            when Socket.Socket_error =>
   --        Trace.Log ("G2.Transceiver.Receive_packet: " & Trace.Report (E));
               return;
         end;

         -- Packet too large
         if Last = Buffer'Last then
            Raise_Exception (Constraint_Error'Identity, "UDP fragment too large");
         end if;

         -- Don't go further if it's unknown
         if Head.szTag /= GND_tag then
            Trace.Log ("UDP dropped: Unknown tag: " & Head.szTag,
               Trace.Warning, S (Logfile));
            return;
         end if;

         -- Understandable?
         if (Head.nFlags and 2#00001100#) /= 0 then
            Trace.Log ("UDP dropped: Unknown flags:" & Head.nFlags'Img,
               Trace.Warning, S (Logfile));
            return;
         end if;

         -- Search an existing equal:
         Packet.Header := Head;
         Packet.Source := Addr;

         -- ACK?
         if Head.nCount = 0 then
            --Trace.Log ("  <-- ACK " & To_string (Packet.Header) & "/" &
            --   Socket.Image (Packet.Source));
            -- Mark traffic
            --Traffic.Add ((
            --   Arrival  => Calendar.Clock,
            --   Protocol => Protocol_descr,
            --   Way      => Traffic.Incoming,
            --   From     => U (Socket.Image (Packet.Source)),
            --   Name     => U ("ACK"),
            --   Data     => U (To_string (Packet.Header))));
            declare
               Result : constant Fragment_list.Iterator_type := Fragment_list.Find (
                  Packets_out, Head.nSequence);
               nPart  : constant Integer := Integer (Head.nPart);
               R : Packet_access renames Fragment_list.Element (Result);
            begin
               if Result /= Fragment_list.Back (Packets_out) then
                  R.Fragments (nPart).Valid := true;
                  -- Cancel timeout:
                  Event_queue.Cancel (
                     Parent.Timeouts, R.Fragments (nPart).Timeout);
                  -- Completely acked?
                  if Is_complete (R.all) then
                     Drop_out_packet (Head.nSequence, Success => true);
                  end if;
               else
                  -- How can it be? we received an ACK for a packet we have
                  --   not sent.
                  --   Maybe it has been already dropped.
                  --Trace.Log (
                  --   "G2.Transceiver.Receive_packet: ACK for missing " &
                  --   "packet " & To_string (Packet.Header), Trace.Debug);
                  null;
               end if;
            end;
         else -- Not ACK
            declare
               Result : constant PLbS.Iterator_type := PLbS.Find (
                  Packets_in_by_source, Id_inbound (Packet));
               use type PLbS.Iterator_Type;
            begin
               --Trace.Log ("  <-- FRG " & To_string (Packet.Header) & "/" &
               --   Socket.Image (Packet.Source));
               -- Create/update an incoming packet.
               if Result /= PLbS.Back (Packets_in_by_source) then
                  --Trace.Log ("  <-- FND " & Id_inbound (Packet));
                  P := PLbA.Element (
                     PLbS.Element (Result));
               else
                  --Trace.Log ("  <-- NEW " & Id_inbound (Packet));
                  Allocate_inbound (
                     Packet'Unchecked_Access, P, Natural (Head.nCount));
               end if;
               P.Header := Packet.Header;
               P.Source := Packet.Source;
               -- Now actualize data:
               if not P.Fragments (Integer (P.Header.nPart)).Valid then
                  Set_Data (P.Fragments (Integer (P.Header.nPart)),
                            Buffer (Header_Size + 1 .. Last));
                 P.Fragments (Integer (P.Header.nPart)).Valid := true;
               end if;
               -- Ack
               if (Head.nFlags and Flag_ack) /= 0 then
                  Send_ack (P);
               end if;
               -- Complete
               if Is_complete (P.all) then
                  Complete (P);
               end if;
            end;
         end if;
      end Receive_packet;
      --------------------
      -- Remove_inbound --
      --------------------
      procedure Remove_inbound (Id : in String)
      is
         use PLbS;
         P : Packet_access;
         I : Iterator_type := Find (Packets_in_by_source, Id);
         Alias : PLbA.Iterator_type;
      begin
         if I = Back (Packets_in_by_source) then
            Trace.Log ("G2.Transceiver.Remove_inbound: Packet missing: " &
               Id, Trace.Warning);
            return;
         end if;
         Alias := Element (I);
         PLbS.Delete (Packets_in_by_source, I);
         P := PLbA.Element (Alias);
         PLbA.Delete (Packets_in_by_arrival, Alias);
         Free (P);
      end Remove_inbound;
      --------------------------
      -- Remove_older_inbound --
      --------------------------
      procedure Remove_older_inbound is
         use PLbA;
         F : Iterator_type := First (Packets_in_by_arrival);
         P : Packet_access;
      begin
         if F = Back (Packets_in_by_arrival) then
            return;
         end if;

         P := Element (F);
         PLbS.Delete (Packets_in_by_source, Id_inbound (P.all));
         PLbA.Delete (Packets_in_by_arrival, F);
         Free (P);
      end Remove_older_inbound;
      --------------------
      -- Retry_fragment --
      --------------------
      procedure Retry_fragment (Part : in out Pending_retransmission) is
         Retry : Retry_context;
      begin
         Part.Failures := Part.Failures + 1;
         if Part.Failures * Duration'(Retry_timeout) >= Transmit_timeout then
            -- Drop packet!
            Drop_out_packet (Part.nSequence, Success => false);
         else
            declare
               Result : constant Fragment_list.Iterator_type :=
                  Fragment_list.Find (Packets_out, Part.nSequence);
               R : Packet_access;
            begin
               if Result /= Fragment_list.Back (Packets_out) then
                  R := Fragment_list.Element (Result);
                  --Trace.Log (
                  --   "G2.Transceiver: Re-sending fragment because of" &
                  --   Part.Failures'Img & " timeout(s): " &
                  --   To_string (R.Header), File => S (Logfile));
                  -- Retransmit part:
                  Send_fragment (R, Part.nPart);
                  -- Re-create timeout
                  Retry.Retry := Part;
                  Retry.Transceiver := Object_access (Parent);
                  Event_queue.Create (
                     Parent.Timeouts,
                     R.Fragments (Integer (Part.nPart)).Timeout,
                     Calendar.Clock + Retry_timeout,
                     Queue_retry'Access,
                     Retry);
               else
                  Trace.Log (
                     "G2.Transceiver: Missing packet attempting retry:" &
                     Part.nSequence'Img);
               end if;
            end;
         end if;
      end Retry_fragment;
      --------------
      -- Send_ack --
      --------------
      procedure Send_ack (P : in Packet_access) is
         H      : Packet_header := P.Header;
         type Header_array is new Stream_element_array (1 .. Header_size);
         function To_array is new Unchecked_conversion (
            Packet_header, Header_array);
         Last   : Stream_element_offset;
         use type Network_Settings.Routings;
      begin
         -- Not send if firewalled:
         if Network_Settings.Internet_Route = Network_Settings.None and then
            Socket.Ip.Is_Public (Socket.Image (P.Source))
         then
            Trace.Log ("G2.Transceiver.Send_Ack: Dropping ACK because of firewalling.", Trace.Debug);
            return; -- <-- EARLY EXIT, DROPPING ACK WHILE FIREWALLED
         end if;

         -- Stat packets sent
         --G2.Packets_sent := Natural'Min (
         --   G2.Packets_sent + 1, Natural'Last - 1);

         --Trace.Log ("  --> ACK " & To_string (P.Header) &
         --   "/" & Socket.Image (P.Source),
         --   File => S (Logfile));
         H.nFlags := 0;
         H.nCount := 0;
         Socket.Send (
            Udp.all, Stream_element_array (to_array (H)), Last, P.Source);

         -- Mark traffic
         --Traffic.Add ((
         --   Arrival  => Calendar.Clock,
         --   Protocol => Protocol_descr,
         --   Way      => Traffic.Outgoing,
         --   From     => U (Socket.Image (P.Source)),
         --   Name     => U ("ACK"),
         --   Data     => U (To_string (P.Header))));
      exception
         when Socket.Security_ban =>
            Trace.Log ("G2.Transceiver.Send_ack: ACK dropped for " &
               Socket.Image (P.Source.Addr) & " [Security ban]",
               Trace.Debug);
      end Send_ack;
      ----------
      -- Send --
      ----------
      procedure Send (Item : in G2.Packet.Queue.Item_type; Date : in Calendar.Time)
      is
      begin
         if Calendar.Clock - Date > 10.0 then
            Trace.Log ("G2.Transceiver.Send: Abnormal lock wait: " &
               Duration'Image (Calendar.Clock - Date), Trace.Error);
         end if;
         Send (Item);
      end Send;
      procedure Send (Item : in G2.Packet.Queue.Item_type) is
         Buffer : Stream_element_array (1 .. Max_packet_size);
         Offset : Stream_element_offset := Buffer'First;
         Stream : aliased Memory_stream_constrained.Stream_type;
         Parts  : Natural;
         Packet : Packet_access;
         Remain : Stream_element_offset;
         Zdone  : Boolean := false;
      begin
         Memory_stream_constrained.Create
            (Stream, Buffer'Address, Buffer'Length);
         -- Serialize:
         Serialize: begin
            G2.Packet.Write (Stream'Access, Item.Packet);
         exception
            when E : Constraint_Error =>
               Trace.Log ("G2.Transceiver.Send.Serialize: " &
                  Trace.Report (E), Trace.Debug);
               Trace.Log (
                  "G2.Transceiver.Send.Serialize: UDP packet too large.",
                  Trace.Warning);
               return;
         end Serialize;
         -- Try to compress payload:
         begin
            declare
               ZBuffer : constant Stream_element_array := Zutil.Deflate (
                  Buffer (1 .. Memory_stream_constrained.Index (Stream)));
            begin
               Buffer (1 .. ZBuffer'Length) := ZBuffer;
               ZDone := true;
            end;
         exception
            when Zutil.Cannot_deflate =>
               null;
         end;
         -- Create fragments:
         Parts := (Integer (Memory_stream_constrained.Index (Stream)) - 1) /
                  Fragment_size + 1;
         if Parts > Natural (Interfaces.Unsigned_8'Last) then
            Trace.Log ("G2.Transceiver.Send: Packet too big:" &
               Natural'Image (Parts) & " fragments needed", Trace.Warning);
            return;
         end if;

         -- Packet exists allocated only within this frame:
         begin
            Allocate_outbound (Packet, Parts);
            Packet.Header.nSequence := Next_packet;
            Next_packet := Next_packet + 1;
            Packet.Header.szTag  := GND_tag;
            Packet.Header.nFlags := 0;
            Packet.Header.nCount := Unsigned_8 (Parts);
            Packet.Destination   := Item.Udp_destination;
            Packet.Safe := Item.Udp_safe;

            if Packet.Safe then
               Packet.Header.nFlags := Packet.Header.nFlags or Flag_ack;
            end if;
            if ZDone then
               Packet.Header.nFlags := Packet.Header.nFlags or Flag_deflate;
            end if;

            for N in 1 .. Parts loop
               if N /= Parts then
                  Set_Data (Packet.Fragments (N),
                            Buffer (Offset .. Offset + Fragment_size - 1));
                  Offset := Offset + Fragment_size;
               else
                  Remain :=
                     Memory_stream_constrained.Index (Stream) rem
                     Fragment_size;
                  if Remain = 0 then -- A full exactly ended fragment
                     Remain := Fragment_size;
                  end if;
                  Set_Data (Packet.Fragments (N),
                            Buffer (Offset .. Offset + Remain - 1));
               end if;
            end loop;

            -- Send it!
            Send_fragments (Packet);
         exception
            when Socket.Security_ban =>
               Trace.Log ("G2.Transceiver [Send]: Security ban for " &
                  Socket.Image (Packet.Destination), Trace.Debug);
               if Packet /= null then
                  Free (Packet);
               end if;
               return;
            when E : others =>
               Trace.Log ("G2.Transceiver [Send]: " & Trace.Report (E),
                  Trace.Error);
               if Packet /= null then
                  Free (Packet);
               end if;
               return;
         end;
         if Packet.Safe then
            Add_safe_packet (Packet);
         else
            Free (Packet);
         end if;
      exception
         when E : others =>
            Trace.Log ("G2.Transceiver.Send: " & Trace.Report (E),
               Trace.Error);
            if Packet /= null then
               Free (Packet);
            end if;
      end Send;
      -------------------
      -- Send_fragment --
      -------------------
      procedure Send_fragment (
         Packet : in out Packet_access;
         Part   : in     Interfaces.Unsigned_8)
      is
         Buffer : Stream_element_array (1 .. Fragment_size + Header_size);
         Last   : Stream_element_offset;
         M      : aliased Memory_stream_constrained.Stream_type;
         N      : constant Integer := Integer (Part);
      begin
         Memory_stream_constrained.Create (M, Buffer'Address, Buffer'Length);
         Packet.Header.nPart := Part;
         Packet_header'Write (M'Access, Packet.Header);

         Memory_stream_constrained.Write (M, Packet.Fragments (N).Data.all);
         Last := Packet.Fragments (N).Data'Length + Header_size;

         --Trace.Log ("  --> FRG " & To_string (Packet.Header) & "/" &
         --   Socket.Image (Packet.Destination), File => S (Logfile));

         -- Enqueue fragment in LIFO fashion:
         Udp_msgs_size := Udp_msgs_size + Natural (Last);
         Udp_message_list.Prepend (
            Udp_msgs,
            Create (Buffer (1 .. Last), Packet.Destination));
      end Send_fragment;
      --------------------
      -- Send_fragments --
      --------------------
      procedure Send_fragments (Packet : in out Packet_access) is
      begin
         for N in 1 .. Packet.Header.nCount loop
            Send_fragment (Packet, N);
         end loop;
      end Send_fragments;
      ----------------------------------
      -- Send_next_fragment_to_socket --
      ----------------------------------
      -- Sends it for real and sets next sending time
      procedure Send_next_fragment_to_socket (Sent : out Boolean) is
         use Udp_message_list;
         use Ada.Calendar;
         Last    : Stream_element_offset;
         Dropped : Natural := 0;
         Now     : constant Time := Clock;
         I       : Udp_message_list.Cursor;
      begin
         Sent := false;
         if Is_empty (Udp_msgs) then
            Next_sending := Clock + 0.1;
         else
            declare
               Msg : constant Udp_Message := First_Element (Udp_Msgs);
            begin
               Udp_msgs_size := Udp_msgs_size - Natural (Msg.Last);
               Delete_first (Udp_msgs);

               begin
                  Socket.Send (
                               Udp.all,
                               Msg.Data (Msg.Data'First .. Msg.Last),
                               Last,
                               Msg.Dest);
               exception
                  when E : Socket.Socket_Error =>
                     Trace.Log ("G2.Transceiver.Send_next_fragment_to_socket: " &
                                Trace.Report (E), Trace.Warning, S (Globals.Options.Debug_NetLogFile));
                  when Socket.Security_ban =>
                     Trace.Log ("G2.Transceiver.Send_next_fragment_to_socket: " &
                                "Security ban", Trace.Debug);
               end;

               -- Drop timeouted packets:
               loop
                  I := Udp_message_list.Last (Udp_msgs);
                  exit when Is_empty (Udp_msgs) or else
                  Now - Element (I).Date <=
                    Globals.Options.G2_UdpOutboundTimeout;
                  Udp_msgs_size := Udp_msgs_size - Natural (Element (I).Last);
                  Delete_last (Udp_msgs); -- Drop it!
                  Dropped := Dropped + 1;
               end loop;
               if Dropped > 0 then
                  Trace.Log ("G2.Transceiver.Sender_task: Dropped" &
                             Dropped'Img & " too old UDP outbound packets",
                             Trace.Debug);
               end if;

               -- Compute next sending:
               if Parent.BW_out /= Speed'Last then
                  Next_sending := Clock +
                    Duration (Msg.Last) / Duration (Parent.BW_out);
               else
                  Next_Sending := Clock;
               end if;

               Sent := true;

            end;
         end if;
      end Send_next_fragment_to_socket;
      -----------
      -- Start --
      -----------
      procedure Start (
         S     : in Socket.Object_access;
         Queue : in G2.Packet.Queue.Object_access) is
      begin
         Udp      := S;
         G2_queue := Queue;
      end Start;
   end Core_type;

   ------------------------------------------------------------------------
   -- Sender_task                                                        --
   ------------------------------------------------------------------------
   task body Sender_task is
--      Timer   : Agpl.Chronos.Object;
      Sent    : Boolean;
      Num     : Natural := 0;
   begin
      select
         accept Start;
      or
         terminate;
      end select;
      Main : loop
         exit Main when Globals.Requested_exit;
         delay until Parent.Core.Get_send_time;
         begin
            Parent.Core.Send_next_fragment_to_socket (Sent);
            if Sent then
               Num := Num + 1;
            end if;
         exception
            when E : others =>
               Trace.Log ("G2.Transceiver.Sender: " & Trace.Report (E),
                  Trace.Error);
         end;
--           declare
--              use Agpl.Chronos;
--           begin
--              if Elapsed (Timer) >= 1.0 then
--                 Reset (Timer);
--                 Trace.Log ("G2.Transceiver.Sender:" & Num'Img & " msgs sent " &
--                            "in the last second.", Trace.Informative);
--                 Num := 0;
--              end if;
--           end;
      end loop Main;
      Trace.Log ("G2.Transceiver.Sender exited");
   end Sender_task;

   ------------------------------------------------------------------------
   -- Dispatcher_task                                                    --
   ------------------------------------------------------------------------
   task body Dispatcher_task is
      Cron, Cron2, Cron3, Cron4 : Chronos.Object;
      P : Packet_access;
      Num_alive_packets : Natural;
   begin
      select
         accept Start;
      or
         terminate;
      end select;
      Main : loop
         exit Main when Globals.Requested_exit;
         begin
            -- Check pipes:
--            Globals.Main_throttle.Start_work;
            Chronos.Reset (Cron4);
            if Calendar.Clock >= Parent.Core.Get_rcv_time then
               while Parent.Core.Available and then
                  Chronos.Elapsed (Cron4) < 0.9
               loop
                  Parent.Core.Receive_packet;
--                  Globals.Main_throttle.Cycle_work;
               end loop;
            end if;

            -- Drop too old incompletes:
            if Chronos.Elapsed (Cron3) > 15.0 then
               Chronos.Reset (Cron3);
--               Trace.Log ("G2.Transceiver: starting old packets purge");
               loop
                  Parent.Core.Get_older_inbound (P);
                  if P /= null then
                     Trace.Log ("G2.Transceiver: " &
                        "Purging incoming packet with age " &
                        Misc.Image (Calendar.Clock - P.Arrived));
                  end if;
                  exit when P = null or else
                     Calendar.Clock - P.Arrived < Receive_timeout;
                  Parent.Core.Remove_older_inbound;
--                  Globals.Main_throttle.Cycle_work;
               end loop;
            end if;

            -- Report
            if Chronos.Elapsed (Cron) > 60.0 then
               Chronos.Reset (Cron);
               Num_alive_packets := Statistics.Integers.Value (
                  Statistics.Integers.Integer_value (
                     Statistics.Object.Get (Stat_alive_packets)));
               Trace.Log ("******** G2 Transceiver Report ********");
               Trace.Log (" Alive packets: " &
                  Misc.To_string (Num_alive_packets));
               Trace.Log (" Available inbound: " &
                  Misc.To_string (Parent.Core.Available_inbound));
               Trace.Log (" Available outbound: " &
                  Misc.To_string (Parent.Core.Available_outbound));
               Trace.Log (" Pending inbound : " &
                  Misc.To_string (Parent.Core.Length_by_source));
               Trace.Log (" Pending outbound: " &
                  Misc.To_string (Parent.Core.Length_outbound));
               Trace.Log ("***************************************");
            end if;

            -- Stats
            if Chronos.Elapsed (Cron2) > 0.7 then
               Chronos.Reset (Cron2);
               Statistics.Object.Set (
                  "Network - G2 - UDPT - Pending inbound",
                  Statistics.Integers.Create (
                     Parent.Core.Length_by_source));
               Statistics.Object.Set (
                  "Network - G2 - UDPT - Pending outbound",
                  Statistics.Integers.Create (Parent.Core.Length_outbound));
               Statistics.Object.Set (
                  "Network - G2 - UDPT - Available inbound",
                  Statistics.Integers.Create (Parent.Core.Available_inbound));
               Statistics.Object.Set (
                  "Network - G2 - UDPT - Available outbound",
                  Statistics.Integers.Create (
                     Parent.Core.Available_outbound));
            end if;
         exception
            when E : others =>
               Trace.Log ("G2.Transceiver.Dispatcher: " & Trace.Report (E),
                  Trace.Error);
         end;
         delay 0.1;
      end loop Main;
      Trace.Log ("G2.Transceiver.Dispatcher exited.");
   end Dispatcher_task;

   ------------------------------------------------------------------------
   -- Queue_retry                                                        --
   ------------------------------------------------------------------------
   procedure Queue_retry (Context : in Agpl.Event_Queues.Context_Type'Class) is
      R : Retry_Context := Retry_Context (Context);
   begin
      R.Transceiver.Core.Retry_fragment (R.Retry);
   exception
      when E : others =>
         Trace.Log ("G2.Transceiver.Queue_Retry: " & Trace.Report (E), Trace.Error);
   end Queue_retry;

end Adagio.G2.Transceiver_types.Prot;
