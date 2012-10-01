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
--  $Id: adagio-connect-peer_manager.adb,v 1.3 2004/01/21 21:05:36 Jano Exp $

--  For general purpose connections, like chats, browses, etc.

with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Trace;
with Generic_event_queue;

with Ada.Real_time;
use  Ada;

package body Adagio.Connect.Peer_manager is

   Stat_peers : constant String := "Misc - Chat/browse peers";

   use type Slot_list.Iterator_type;

   ------------------------------------------------------------------------
   -- Event system                                                       --
   ------------------------------------------------------------------------
   procedure Process_event (Id : in String) is
   begin
      Object.Process (Id);
   end Process_event;

   package Event_queue is new Generic_event_queue (
      String, Process_event, 1024 * 1024);

   Events : Event_queue.Object;

   procedure Create_event (Id : in String; Sleep : in Duration) is
      Event : Event_queue.Event_type;
      use Real_time;
   begin
      Event_queue.Create (
         Events, Event,
         Real_time.Clock + To_time_span (Sleep), Id);
   end Create_event;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is

      ---------
      -- Add --
      ---------
      --  Enqueue a peer for management.
      procedure Add (This : access Peer.Object'Class) is
      begin
         if Contains (Peer.Id (This.all)) then
            raise Already_connected;
         end if;

         --  Enqueue and first process.
         Slot_list.Insert (
            Peers, Peer.Id (This.all), (Peer => Peer.Object_access (This)));
         Process (Peer.Id (This.all));
      end Add;

      --------------
      -- Contains --
      --------------
      --  Says if some peer is already there.
      function Contains (Id : in String) return Boolean is
      begin
         return Slot_List.Find (Peers, Id) /= Slot_List.Back (Peers);
      exception
         when E : others =>
            Trace.Log ("Connect.Peer_Manager.Contains: " & Trace.Report (E),
               Trace.Error);
            raise;
      end Contains;

      -------------
      -- Process --
      -------------
      --  Do the processing for a given Id (backcalled from timeout queue).
      procedure Process (Id : in String) is
         use Slot_list;
         I : Iterator_type := Find (Peers, Id);
         C : Peer.Context_type;
         P : Peer.Object_access;
      begin
         if I = Back (Peers) then
            Trace.Log ("Peer_manager.Process: Received event for missing " &
               "peer: " & Id, Trace.Warning);
         else
            begin
               P := Element (I).Peer;
               Peer.Process (P.all, C);
            exception
               when E : others =>
                  Trace.Log ("Peer_manager.Process [dispatched process]: " &
                     Trace.Report (E), Trace.Error);
               Trace.Log ("Peer connection with " & Id & " dropped.");
               Peer.Free (P);
               Delete (Peers, Id);
               Statistics.Object.Set (
                  Stat_peers,
                  Statistics.Integers.Create (Length (Peers)));
               return;
            end;
            --  Finished?
            if C.Is_done then
               Trace.Log ("Peer connection with " & Id & " finished.");
               Peer.Free (P);
               Delete (Peers, Id);
            elsif C.Sleep < 0.1 then
               Create_event (Id, 0.1);
            else
               Create_event (id, C.Sleep);
            end if;
         end if;

         --  Stats
         Statistics.Object.Set (
            Stat_peers,
            Statistics.Integers.Create (Length (Peers)));
      end Process;

      ------------
      -- Signal --
      ------------
      --  "Signals" a managed peer to do some synchronous processing.
      procedure Signal (Id : in String; Params : in out Peer.Generic_Params'Class) is
         use Slot_list;
         I : Iterator_type := Find (Peers, Id);
      begin
         if I = Back (Peers) then
            Trace.Log ("Peer_manager.Signal: Signal for missing " &
               "peer: " & Id, Trace.Warning);
         else
            Peer.Signal (Element (I).Peer.all, Params);
         end if;
      end Signal;

      ----------------
      -- Terminated --
      ----------------
      --  Forces Finalize/removing of a peer
      procedure Terminated (Id : in String) is
         use Slot_list;
         I : Iterator_type := Find (Peers, Id);
         P : Peer.Object_access;
      begin
         if I = Back (Peers) then
            Trace.Log ("Peer_manager.Terminated: Missing " &
               "peer: " & Id, Trace.Warning);
         else
            P := Element (I).Peer;
            Trace.Log ("Peer connection with " & Id & " terminated.");
            Peer.Free (P); -- Will cause Finalization
            Delete (Peers, Id);
         end if;

         --  Stats
         Statistics.Object.Set (
            Stat_peers,
            Statistics.Integers.Create (Length (Peers)));
      end Terminated;

   end Object;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Event_queue.Shutdown (Events);
   end Shutdown;

begin
   --  Initial stats
   Statistics.Object.Set (
      Stat_peers,
      Statistics.Integers.Create (0));
end Adagio.Connect.Peer_manager;
