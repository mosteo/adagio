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
--  $Id: adagio-upload-queue-manager.adb,v 1.15 2004/02/29 20:36:46 Jano Exp $

with Adagio.Convert;
with Adagio.Debug;
with Adagio.File;
with Adagio.Socket;
with Adagio.Statistics;
with Adagio.Statistics.Booleans;
with Adagio.Statistics.Integers;
with Adagio.Statistics.Strings;
with Adagio.Trace;
with Adagio.Upload.Client_data;
with Adagio.Upload.Log;
with Adagio.Upload.Queue.Create_from_xml;
with Adagio.Xml;
with Adagio.Xml.Utils;
with Generic_event_queue;
with Sequence;

with Agpl.Geoip;

with Ada.Calendar; use Ada;
with Ada.Real_time;
with Ada.Unchecked_deallocation;
with Interfaces;

package body Adagio.Upload.Queue.Manager is

   Share_bandwidth : Boolean renames Globals.Options.Uploads_ShareBandwidth;
   Throttle        : Float renames Globals.Options.Uploads_throttle;
   Minimum_send_delay : Duration 
      renames Globals.Options.Uploads_MinimumSendDelay;

   Last_client_data_save : Calendar.Time := Calendar.Clock;

   package Client_pool renames Client.Pool.Map;

   -- Sequence to track completed uploads.
   package Positive_seq is new Sequence (Interfaces.Unsigned_32);
   Completed_seq : Positive_seq.Object;
   use type Interfaces.Unsigned_32;

   procedure Free is new Unchecked_deallocation (
      Client_slot, Client_slot_access);

   ------------------------------------------------------------------------
   -- Event system                                                       --
   ------------------------------------------------------------------------
   procedure Process_event (Queue_id : in String) is
   begin
      -- Only to test accessibility of Queue_id
      Trace.Log (" ~~~ Upload event for " & Queue_id, Trace.Never);
      Object.Process_event (Queue_id);
   end Process_event;

   package Event_queue is new Generic_event_queue (
      String, Process_event);

   Events : Event_queue.Object;

   procedure Create_event (Queue_id : in String; Deadline : Real_time.Time) is
      Event : Event_queue.Event_type;
   begin
      Event_queue.Create (Events, Event, Deadline, Queue_id);
   end Create_event;

   ------------------------------------------------------------------------
   -- PROTECTED OBJECT (MANAGER)                                         --
   ------------------------------------------------------------------------
   protected body Object is

   ------------------
   -- Upload_stats --
   ------------------
   procedure Upload_stats is
   begin
      if Chronos.Elapsed (Sent_cron) > 0.8 then
         Chronos.Reset (Sent_cron);
         -- Pending requests
         Statistics.Object.Set (
            "Network - Pending requests",
            Statistics.Integers.Create (Client_pool.Length (Pending)));
         -- Queued clients
         Statistics.Object.Set (
            "Uploads - Queued clients",
            Statistics.Integers.Create (Client_list.Length (Clients)));
         -- Session data sent
         begin
            Statistics.Object.Set (
               Stat_session_upload,
               Statistics.Strings.Create (
                  Long_long_integer'Image (Session_sent) & " B (" &
                  Convert.To_size (Float (Session_sent)) & ")"));
         exception
            when others =>
            Statistics.Object.Set (
               Stat_session_upload,
               Statistics.Strings.Create (
                  Long_long_integer'Image (Session_sent) & " B (>" &
                  Convert.To_size (Float'Last) & ")"));
         end;
         -- Session mean speed
         declare
            use Calendar;
         begin
            Mean_speed := Speed (
               Long_long_float (Session_sent) /
               Long_long_float (Clock - Globals.Adagio_start));
            Statistics.Object.Set (
               Stat_session_speed,
               Statistics.Strings.Create (
                  Convert.To_size (Float (Mean_speed)) & "/s"));
         exception
            when others =>
               Mean_speed := Speed'Last;
               Statistics.Object.Set (
                  Stat_session_speed,
                  Statistics.Strings.Create (">" &
                     Convert.To_size (Float'Last) & "/s"));
         end;
      end if;
   end Upload_stats;

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Frees all memory used by a client
   procedure Remove (Queue_id : in String) is
      Slot : Client_slot_access;
      use Client_list;
      use type Ada.Calendar.Time;
   begin
      Slot := Element (Find (Clients, Queue_id));

      if Slot.Session_sent > 0 then
         Client_data.List.Add_sent (
            Client.Id (Slot.Client.all), Slot.Session_sent);
         if Calendar.Clock - Last_client_data_save > 60.0 then
            Last_client_data_save := Calendar.Clock;
            Client_data.List.Save;
         end if;
      end if;

      Client.Cancel (Slot.Client.all);
      Client.Free (Slot.Client);
      Free (Slot);
      Delete (Clients, Queue_id);
   end Remove;

   ------------------------------------------------------------------------
   -- Enqueue                                                            --
   ------------------------------------------------------------------------
   procedure Enqueue (this : access Client.Object'Class) is
      Aux : Client.Object_access := Client.Object_access (this);
   begin
      -- Add to pending of identification clients
      if Client_pool.Length (Pending) >= Max_unknown then
         Trace.Log ("Upload.Queue.Manager: Cannot enqueue client " &
            Client.Id (This.all) & ", max" &
            " unknown reached.", Trace.Informative);
         Client.Cancel (this.all);
         Client.Free (Aux);
      elsif Client_pool.Is_in (Client.Id (This.all), Pending) then
         Trace.Log ("Upload.Queue.Manager: Cannot enqueue client " &
            Client.Id (This.all) & ", " &
            " unresolved request already pending.", Trace.Informative);
         Client.Cancel (this.all);
         Client.Free (Aux);
      else
         Client_pool.Insert (
            Pending, Client.Id (This.all),
               (Arrival_time => Calendar.Clock,
                Client       => Client.Object_access (This)));
      end if;
   end Enqueue;

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Restore itself from a file
   -- And upload manager simply stores all its clients. When restoring,
   --    each queue searches them there by Id.
   procedure Init (File : in String) is
      Xml_queues : Xml.Node_array :=
         Xml.Get_all ("uploads/queue", Globals.Config);
      New_queue  : Queue.Object_access;
      Priorities : Natural := 0;
   begin
      Path := U (File);
      -- Sum all priorities:
      for N in Xml_queues'Range loop
         if Xml.Get_attribute (Xml_queues (N), "active", "yes") = "yes" then
            Priorities := Priorities + Xml.Utils.Get_num (
               Xml_queues (N), "priority", 1);
         end if;
      end loop;
      -- Create all queues:
      for N in Xml_queues'Range loop
         if Xml.Get_attribute (Xml_queues (N), "active", "yes") = "yes" then
            New_queue := new Queue.Object;
            Queue.Create_from_xml (
               New_queue.all,
               Xml_queues (N),
               Float (Xml.Utils.Get_num (Xml_queues (N), "priority", 1)) /
                 Float (Priorities));
            Queue_list.Insert (
               Queues,
               Xml.Get_attribute (Xml_queues (N), "name", ""),
               New_queue);
         end if;
      end loop;
   end Init;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Processes pending unknowns.
   procedure Process is
      use Client_pool;
      use type Adagio.File.Object;
      use type Ada.Calendar.Time;

      Pos           : Iterator_type := First (Pending);
      Context_in    : Client.Queue_context;
      Context_out   : Client.Client_results;
      Current       : Client.Object_access;
      Arrival       : Calendar.Time;
      Slot          : Client_slot_access;
      Client_queues : Queue_vector.Object (1);
      Success       : Boolean;

      -- Try to enqueue a client in all queues. Return in how many it is.
      procedure Try_queues (
         Current : in Client.Object_access;
         Queued  : out Queue_vector.Object) is

         use type Queue_list.Iterator_type;
         Pos     : Queue_list.Iterator_type := Queue_list.First (Queues);
         Success : Boolean;
      begin
         Queue_vector.Reset (Queued);
         while Pos /= Queue_list.Back (Queues) loop
            Queue_list.Element (Pos).Enqueue (Current, Success);
            if Success then
               Queue_vector.Append (Queued, Queue_list.Element (Pos));
            end if;
            Pos := Queue_list.Succ (Pos);
         end loop;
      end Try_queues;

      use type Upload.Resource.Handle;
   begin
      while Pos /= Back (Pending) loop
         Current := Element (Pos).Client;
         Arrival := Element (Pos).Arrival_time;

         -- Check starvation of request:
         if Calendar.Clock - Arrival >= 60.0 then

            Trace.Log ("Upload.Queue.Manager: Dropping client " &
               Client.Id (Current.all) & " because no request has been made" &
               " in a minute.", Trace.Informative);

            Client.Cancel (Current.all);
            Delete (Pending, Pos);
            Client.Free (Current);

         else

            Context_in.Must_start   := false;
            Context_in.Allowed_up   := File_size'Last;
            Context_in.Allowed_down := File_size'Last;
            begin
               Client.Process (Current.all, Context_in, Context_out);

               -- Enqueue resolved
               if Client.Requested_resource (Current.all) /=
                  Upload.Resource.Null_handle and then 
                  not Context_out.Is_done 
               then

                  if Client_list.Is_in(Client.Queue_id (Current.all), Clients)
                  then
                     Trace.Log ("Upload.Queue.Manager: Dropping request " &
                        Client.Queue_id (Current.all) & ": Already queued");
                     Client.Cancel (Current.all);
                     Delete (Pending, Pos);
                     Client.Free (Current);
                  else
                     Try_queues (Current, Client_queues);
                     if Queue_vector.Length (Client_queues) = 0 then
                        -- Forget it:
                        Client.Reject (Current.all, Client.Busy, Success);
                        Trace.Log ("Upload.Queue.Manager: No queue slots " &
                           "available", Trace.Informative);

                        if Success then
                           Client.Cancel (Current.all);
                           Delete (Pending, Pos);
                           Client.Free (Current);
                           Trace.Log (
                              "Upload.Queue.Manager: Dropping request:" &
                              " No suitable queues or queues are full.",
                              Trace.Informative);
                        end if;
                     else
                        Delete (Pending, Pos);
                        -- Insert it as queued
                        Slot := new Client_slot'(
                           Client        => Current,
                           Queue_id      => U (Client.Queue_id (Current.all)),
                           Position      => Natural'Last,
                           Is_uploading  => false,
                           Start_ack     => false,
                           BW_boost      => 1.0,
                           Last_run      => Calendar.Clock,
                           Active_queue  => null,
                           Queues        => Client_queues,
                           Speed         => (Calendar.Clock, 0, 0),
                           Session_sent  => 0,
                           Session_start => Calendar.Clock);
                        Client_list.Insert (
                           Clients,
                           S (Slot.Queue_id),
                           Slot);
                        Create_event (S (Slot.Queue_id), Real_time.Clock);
                     end if;
                  end if;
               elsif Context_out.Is_done then
               -- Drop failed
                  Trace.Log ("Upload.Queue.Manager: Dropping request for " &
                     Client.Id (Current.all));
                  Client.Cancel (Current.all);
                  Delete (Pending, Pos);
                  Client.Free (Current);
               else
                  Pos := Succ (Pos);
               end if;

            exception
               when Upload.Client.Connection_lost | Socket.Socket_error =>
                  Trace.Log (
                     "Upload.Queue.Manager: Connection lost [resolving] from "
                     & Client.Id (Current.all));
                  Client.Cancel (Current.all);
                  Delete (Pending, Pos);
                  Client.Free (Current);
               when Upload.Client.User_agent_is_banned =>
                  Trace.Log (
                     "Upload.Queue.Manager: Dropping client " &
                     Client.Id (Current.all) & " [security ban]");
                  Client.Cancel (Current.all);
                  Delete (Pending, Pos);
                  Client.Free (Current);
               when Upload.Client.Unknown_request =>
                  Trace.Log (
                     "Upload.Queue.Manager: Dropping client " &
                     Client.Id (Current.all) & " [unknown request]");
                  Client.Cancel (Current.all);
                  Delete (Pending, Pos);
                  Client.Free (Current);
               when E : others =>
                  Trace.Log (
                     "Upload.Queue.Manager: Dropping client " &
                     Client.Id (Current.all) & " because: " &
                     Trace.Report (E), Trace.Error);
                  Client.Cancel (Current.all);
                  Delete (Pending, Pos);
                  Client.Free (Current);
            end;
         end if;
      end loop;
      Upload_stats;
   end Process;

   ------------------------
   -- Lost_from_queues --
   ------------------------
   -- Mark a client lost in all queues except, maybe, a given one:
   procedure Lost_from_queues (Slot : access Client_slot) is
      use Queue_vector;
   begin
      for N in reverse 1 .. Last (Slot.Queues) loop
         Slot.Queues.Vector (N).Lost (S (Slot.Queue_id));
         Delete (Slot.Queues, N);
      end loop;
      pragma Assert (Length (Slot.Queues) = 0);
   end Lost_from_queues;

   ------------------------------------------------------------------------
   -- Process_event                                                      --
   ------------------------------------------------------------------------
   -- Process events for already identified and queued clients
   procedure Process_event (Queue_id : in String) is
      Slot : Client_slot_access;

      ----------------------------
      -- Register_upload_finish --
      ----------------------------
      -- Register an upload that has finished sucessfully
      procedure Register_upload_finish (This : in Client_slot_access) is
         Addr : constant String := Client.Id (This.Client.all);
         Code : Ustring;
         Name : Ustring := U (
            Upload.Resource.Name (
               Upload.Resource.V (
                  Client.Requested_resource (This.Client.all)).all)); 
         Sname : constant String := S (Name);
      begin
         if This.Active_queue = null then
            return;
         end if;
         if Sname'Length > 6 and then
            Sname (Sname'First .. Sname'First + 6) = "TTH of " 
         then 
            Name := U (Sname (Sname'First + 7 .. Sname'Last));
         end if;
         Code := U (Agpl.Geoip.Country_code_from_addr (Addr));
         if S (Code) = "??" then
            Code := U ("unknown");
         end if;
         Upload.Log.Records.Add ((
            Filename  => Name,
            Client    => U (Client.Name (This.Client.all)),
            Last_seen => Calendar.Clock,
            Address   => U (Addr),
            Code      => Code,
            Country   => U (Agpl.Geoip.Country_name_from_addr (Addr)),
            Queue     => U (This.Active_queue.Get_name)));
      end Register_upload_finish;
      ------------------------
      -- Reset_upload_speed --
      ------------------------
      procedure Reset_upload_speed (Slot : access Client_slot) is
         use type Calendar.Time;
         Avg_period : Duration := Slot.Active_queue.Get_avg_period;
      begin
         Slot.Speed.Current_amount := 0;
         Slot.Speed.Next_deadline  :=
            Calendar.Clock + Avg_period;
         Slot.Speed.Target_amount  := File_size (Avg_period) *
            Slot.Active_queue.Get_min_speed;
      end Reset_upload_speed;

      ------------------------
      -- Lost_from_queues --
      ------------------------
      -- Mark a client lost in all queues except, maybe, a given one:
      procedure Lost_from_queues (Except : Queue.Object_access := null) is
         use Queue_vector;
      begin
         for N in reverse 1 .. Last (Slot.Queues) loop
            if Slot.Queues.Vector (N) /= Except then
               Slot.Queues.Vector (N).Lost (Queue_id);
               Delete (Slot.Queues, N);
            end if;
         end loop;
      end Lost_from_queues;

      ------------------------
      -- Remove_from_queues --
      ------------------------
      -- Get a client out of all queues except, maybe, a given one:
      procedure Remove_from_queues (Except : Queue.Object_access := null) is
         use Queue_vector;
      begin
         for N in reverse 1 .. Last (Slot.Queues) loop
            if Slot.Queues.Vector (N) /= Except then
               Slot.Queues.Vector (N).Remove (Queue_id);
               Delete (Slot.Queues, N);
            end if;
         end loop;
         pragma Assert (
            (Length (Slot.Queues) = 1 and then Except /= null) or else
            (Length (Slot.Queues) = 0 and then Except = null));
      end Remove_from_queues;

      ---------------------
      -- Process_waiting --
      ---------------------
      procedure Process_waiting is
         Status     : Queue_slot;
         Data_in    : Client.Queue_context;
         Data_out   : Client.Client_results;
         Max_slots  : Natural := 0;
         Used_slots : Natural := 0;
         Aux        : Interfaces.Unsigned_32;
         use Ada.Real_time;
      begin
         Slot.Last_run := Calendar.Clock;

         -- Extract best queue position and can start:
         Slot.Position := Natural'Last;
         for N in 1 .. Queue_vector.Last (Slot.Queues) loop
            Slot.Queues.Vector (N).Check_client (Queue_id, Status);

            -- Start!
            if Status.Can_start then
               Slot.Is_uploading := true;
               Slot.Active_queue := Slot.Queues.Vector (N);
               Remove_from_queues (Except => Slot.Queues.Vector (N));
               exit;
            else
               Slot.Position := Natural'Min (Slot.Position, Status.Position);
               Max_slots     := Natural'Max (Max_slots, Status.Max_slots);
               Used_slots    := Natural'Max (Used_slots, Status.Used_slots);
            end if;
         end loop;

         -- Defer if upload starting, process cc.
         if Slot.Is_uploading then
            Trace.Log(
               "Upload.Queue.Manager: Starting upload of " & 
                  Upload.Resource.Name (
                     Upload.Resource.V (
                        Client.Requested_resource (Slot.Client.all)).all) &
                  " to " & Client.Id (Slot.Client.all) & " (" &
                  Client.Name (Slot.Client.all) & ")",
               Trace.Informative);
            Create_event (Queue_id, Real_time.Clock);
         else
            Data_in := (
               Position      => Slot.Position,
               Max_slots     => Max_slots,
               Current_slots => Used_slots,
               Must_start    => false,
               Allowed_up    => File_size'Last,
               Allowed_down  => File_size'Last);
            begin
               Upload.Client.Process (Slot.Client.all, Data_in, Data_out);
            exception
               when Upload.Client.Connection_lost | Socket.Socket_error =>
                  Trace.Log ("Queue [waiting]: Connection lost to " & 
                  Queue_id);
                  Lost_from_queues;
                  Remove (Queue_id);
                  return;
               when Upload.Client.Unknown_request =>
                  Trace.Log ("Queue [waiting]: Unknown request from " & 
                  Queue_id);
                  Lost_from_queues;
                  Remove (Queue_id);
                  return;
               when Upload.Client.Client_polled_too_soon =>
                  Trace.Log ("Queue [waiting]: Poll too soon for " & 
                  Queue_id);
                  Lost_from_queues;
                  Remove (Queue_id);
                  return;
               when Upload.Client.Client_missed_poll_deadline =>
                  Trace.Log ("Queue [waiting]: Poll too late for " & 
                  Queue_id);
                  Lost_from_queues;
                  Remove (Queue_id);
                  return;
               when Upload.Client.User_agent_is_banned =>
                  Remove_from_queues;
                  Remove (Queue_id);
                  return;
               when E : others =>
                  Trace.Log ("Upload.Queue.Process_event (queued): Slot " &
                     Queue_id & ": " & Trace.Report (E), Trace.Error);
                  -- Lost!
                  Lost_from_queues;
                  Remove (Queue_id);
                  return;
            end;

            -- Neat ending.
            if Data_out.Is_done then
               Trace.Log ("Upload.Queue.Process_event: Upload " &
                  Upload.Resource.Name (
                     Upload.Resource.V (
                        Client.Requested_resource (Slot.Client.all)).all) &
                  " finished successfully [waiting].", Trace.Informative);
               Register_upload_finish (Slot);
               Remove_from_queues;
               Remove (Queue_id);
               Completed_seq.Get_next (Aux);
               Statistics.Object.Set (Stat_session_completed, 
                  Statistics.Integers.Create (Integer (Aux + 1)));
               return;
            end if;
            -- Check for request change:
            if Client.Queue_id (Slot.Client.all) /= Queue_id then
               Trace.Log ("Upload.Queue.Manager: Client " & Queue_id &
                  " changed " & "request to " &
                  Client.Queue_id (Slot.Client.all) &
                  " while waiting, dropping.", Trace.Informative);
               Lost_from_queues;
               Remove (Queue_id);
               return;
            end if;
            Create_event (Queue_id, Data_out.Awakening);
         end if;
      end Process_waiting;

      -----------------------
      -- Process_uploading --
      -----------------------
      procedure Process_uploading is
         use type Calendar.Time;
         use type Real_time.Time;

         Safe_BW  : File_size;
         Extra_BW : File_size;
         Awarded, Awarded_extra : File_size := 0;

         Elapsed  : Duration := Calendar.Clock - Slot.Last_run;

         Data_in  : Client.Queue_context;
         Data_out : Client.Client_results;

         Aux : Interfaces.Unsigned_32;

         procedure Drop_client is
         begin
            Remove_from_queues;
            Remove (Queue_id);
         end Drop_client;
      begin
         -- BANDWIDTH MANAGEMENT
         begin
            Safe_BW  := File_Size'Min (
               File_size (Float (Slot.Active_queue.Get_slot_bandwidth) *
               Float (Elapsed)),
               File_Size'Last / 2);
         exception
            when Constraint_error =>
               Safe_BW := File_size'Last / 2;
         end;
         begin
            if Share_bandwidth then
               Extra_BW := File_Size'Min (
                  File_size (Float (Slot.Active_queue.Get_slot_bandwidth) *
                  Float (Elapsed) * Slot.BW_boost) - Safe_BW,
                  File_Size'Last / 2);
            else
               Extra_BW := 0;
            end if;
         exception
            when Constraint_error =>
               Extra_BW := File_size'Last / 2;
         end;

         if Extra_BW > 0 then
            Bandwidth.Commit (Natural (Safe_BW), Natural (Awarded));
            Bandwidth.Commit (Natural (Extra_BW), Natural (Awarded_extra), Extra => true);
         else
            Bandwidth.Commit (Natural (Safe_BW), Natural (Awarded));
            Extra_BW := 0;
         end if;

         Slot.Last_run := Calendar.Clock;

         Data_in := (
               Position      => 1,
               Max_slots     => 1,
               Current_slots => 1,
               Must_start    => true,
               Allowed_up    => Awarded + Awarded_extra,
               Allowed_down  => 0);

--         Globals.Main_throttle.Start_work;
         begin
            Upload.Client.Process (Slot.Client.all, Data_in, Data_out);
         exception
            when Upload.Client.Connection_lost | Socket.Socket_error =>
               Trace.Log ("Queue [uploading]: Connection lost to " & 
               Queue_id);
               Register_upload_finish (Slot);
               Completed_seq.Get_next (Aux);
               Statistics.Object.Set (Stat_session_completed, 
                  Statistics.Integers.Create (Integer (Aux + 1)));
               Remove_from_queues;
               Remove (Queue_id);
               return;
            when Upload.Client.Unknown_request =>
               Trace.Log ("Queue [uploading]: Unknown request from " & 
               Queue_id);
               Lost_from_queues;
               Remove (Queue_id);
               return;
            when E : others =>
               Trace.Log ("Upload.Queue.Process_event (Uploading): Slot " &
                  Queue_id & ": " & Trace.Report (E), Trace.Warning);
               -- Drop
               Remove_from_queues;
               Remove (Queue_id);
               return;
         end;
--         Globals.Main_throttle.End_work;

         -- Neat ending.
         if Data_out.Is_done then
            Trace.Log ("Upload.Queue.Process_event: Upload " &
               Upload.Resource.Name (
                  Upload.Resource.V (
                     Client.Requested_resource (Slot.Client.all)).all) &
               " finished successfully [uploading].", Trace.Informative);
            Register_upload_finish (Slot);
            Remove_from_queues;
            Remove (Queue_id);
            Completed_seq.Get_next (Aux);
            Statistics.Object.Set (Stat_session_completed, 
               Statistics.Integers.Create (Integer (Aux + 1)));
            return;
         end if;

         -- Check for request change (not allowed while uploading):
         if Client.Queue_id (Slot.Client.all) /= Queue_id then
            Trace.Log ("Upload.Queue.Manager: Client " & Queue_id &
               " changed " & "request to " &
               Client.Queue_id (Slot.Client.all) &
               " while uploading, dropping.", Trace.Informative);
            Remove_from_queues;
            Remove (Queue_id);
            return;
         end if;

          -- Check speeds:
         if not Slot.Start_ack and Data_out.Is_uploading then
            Slot.Start_ack := true;
            Reset_upload_speed (Slot);
            Slot.Session_start := Calendar.Clock;
         end if;
         if Slot.Start_ack then
            Slot.Speed.Current_amount :=
               Slot.Speed.Current_amount + Data_out.Sent;
            Slot.Session_sent := Slot.Session_sent + Data_out.Sent;
            if Slot.Speed.Current_amount >= Slot.Speed.Target_amount then
               Reset_upload_speed (Slot);
            elsif Slot.Speed.Next_deadline < Calendar.Clock then
               -- Failed, too slow:
               Trace.Log ("Upload.Queue.Manager.Process_event: Upload " &
                  Queue_id & " is too slow, dropping.", Trace.Informative);
               Remove_from_queues;
               Remove (Queue_id);
               return;
            end if;
            -- Stats
            Object.Session_sent := 
               Object.Session_sent + Long_long_integer (Data_out.Sent);
            Upload_stats;
         end if;

         -- Check preemptions
         if Slot.Start_ack then
            declare
               use type Queue.Preemptions;
               Preempt : Queue.Preemptions :=
                  Slot.Active_queue.Get_preemptions;
            begin
               if Preempt.Kind = Time or Preempt.Kind = Both then
                  if Calendar.Clock - Slot.Session_start >= Preempt.Time then
                     Drop_client;
                     Trace.Log ("Upload.Queue.Manager.Process_event: " &
                        "Time preemption for " & Queue_id, Trace.Informative);
                     return;
                  end if;
               end if;
               if Preempt.Kind = Size or Preempt.Kind = Both then
                  if Slot.Session_sent >= Preempt.Size then
                     Drop_client;
                     Trace.Log ("Upload.Queue.Manager.Process_event: " &
                        "Size preemption for " & Queue_id, Trace.Informative);
                     return;
                  end if;
               end if;
            end;
         end if;

         -- Throttling
         if Data_out.Sent = Awarded + Awarded_extra then
            -- Throttle faster if consumed all awarded
            if Awarded + Awarded_extra > 0 then
               if Awarded + Awarded_extra = Extra_BW + Safe_BW then
                  Slot.BW_boost := Float'Max (
                     Slot.BW_boost * (1.0 + (1.0 - Throttle)), 0.1);
               end if;
               -- Maximum throttle:
               Slot.BW_boost := Float'Min (Float'Last / 2.0, Slot.BW_boost);
            end if;
         else
            -- Throttle slower
            Slot.BW_boost := Slot.BW_boost * Throttle;
         end if;
--         Trace.Log ("Upload throttle: " &
--            Misc.To_string (Slot.BW_boost, 2));

         -- Schedule a new event for this slot:
         if Data_out.Awakening <
            Real_time.Clock + Real_time.To_time_span (Minimum_send_delay)
         then
            Data_out.Awakening :=
               Real_time.Clock + Real_time.To_time_span (Minimum_send_delay);
         end if;

         Create_event (Queue_id, Data_out.Awakening);
      end Process_uploading;

   begin
      -- Check applicable:
      if Globals.Requested_exit then
         return;
      elsif not Client_list.Is_in (Queue_id, Clients) then
         Trace.Log ("Upload.Queue.Manager.Process_event: " &
            "Event for missing upload: " & Queue_id, Trace.Warning);
         return;
      else
         Slot := Client_list.Element (Client_list.Find (Clients, Queue_id));
      end if;

      declare
         Cron : Chronos.Object;
      begin
         if Slot.Is_uploading then
            Process_uploading;
            if Chronos.Elapsed (Cron) > 5.0 then
               Trace.Log ("Upload process client too long [uploading]", 
                  Trace.Warning);
            end if;
         else
            Process_waiting;
            if Chronos.Elapsed (Cron) > 5.0 then
               Trace.Log ("Upload process client too long [uploading]", 
                  Trace.Warning);
            end if;
         end if;
      end;

   exception
      when E : others =>
         if Upload.Client."/=" (Slot.Client, null) then
            Remove_from_queues;
            Remove (S (Slot.Queue_id));
         end if;
         Trace.Log ("Upload.Queue.Manager.Process_event: " & Trace.Report (E),
            Trace.Error);
   end Process_event;

   ------------------------------------------------------------------------
   -- Save_queues                                                        --
   ------------------------------------------------------------------------
   -- Dump queues to disk
   procedure Save_queues is
      use Queue_list;
      Pos : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Element (Pos).Serialize (S (Globals.Data_folder));
         Pos := Succ (Pos);
      end loop;
   end Save_queues;

   ------------------------------------------------------------------------
   -- Total_length                                                       --
   ------------------------------------------------------------------------
   -- Sumed length of all queues
   function Total_length return Natural is
      use Queue_list;
      Total : Natural := 0;
      Pos   : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Total := Total + Element (Pos).Get_alive_length;
         Pos   := Succ (Pos);
      end loop;

      return Total;
   end Total_length;

   ------------------------------------------------------------------------
   -- Max_length                                                         --
   ------------------------------------------------------------------------
   -- Max length of any queue
   function Max_length return Natural is
      use Queue_list;
      Total : Natural := 0;
      Pos   : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Total := Natural'Max (Total, Element (Pos).Get_alive_length);
         Pos   := Succ (Pos);
      end loop;

      return Total;
   end Max_length;

   ------------------------------------------------------------------------
   -- Max_active_length                                                  --
   ------------------------------------------------------------------------
   -- Sumed length of allowed active uploads for every queue
   function Max_active_length return Natural is
      use Queue_list;
      Total : Natural := 0;
      Pos   : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Total := Total + Element (Pos).Get_uploads;
         Pos := Succ (Pos);
      end loop;

      return Total;
   end Max_active_length;

   ------------------------------------------------------------------------
   -- Num_active_uploads                                                 --
   ------------------------------------------------------------------------
   -- Total numbr of clients uploading
   function Num_active_uploads return Natural is
      use Queue_list;
      Total : Natural := 0;
      Pos   : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Total := Total + Element (Pos).Get_current_uploads;
         Pos := Succ (Pos);
      end loop;

      return Total;
   end Num_active_uploads;

   ------------------------------------------------------------------------
   -- Num_waiting                                                        --
   ------------------------------------------------------------------------
   -- Total number of clients waiting
   function Num_waiting return Natural is
      use Queue_list;
      Total : Natural := 0;
      Pos   : Iterator_type := First (Queues);
   begin
      while Pos /= Back (Queues) loop
         Total := Total + Element (Pos).Get_current_waiting;
         Pos := Succ (Pos);
      end loop;

      return Total;
   end Num_waiting;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      declare
         use Client_list;
         Pos : Iterator_type := First (Clients);
      begin
         while Pos /= Back (Clients) loop
            Remove (S (Element (Pos).Queue_id));
            Pos := First (Clients);
         end loop;
      end;
      Event_queue.Shutdown (Events);
      Save_queues;
      Client_data.List.Save;
   end Shutdown;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Return queues names:
   function Report return Ustring_array is
      Result : Ustring_array (1 .. Queue_list.Length (Queues));
      use Queue_list;
      I : Iterator_type := First (Queues);
   begin
      for N in Result'Range loop
         Result (N) := U (Element (I).Get_name);
         I := Succ (I);
      end loop;
      return Result;
   end Report;

   ------------------------------------------------------------------------
   -- Report_queue                                                       --
   ------------------------------------------------------------------------
   function Report_queue (
      Name : in String;
      From : in Natural;
      Qty  : in Natural;
      Lost : in Boolean) return Queue.Report_array 
   is
      use Queue_list;
      I         : Iterator_type := Find (Queues, Name);
      No_report : Queue.Report_array (1 .. 0);
   begin
      if I /= Back (Queues) then
         return Element (I).Report (From, Qty, Lost);
      else
         return No_report;
      end if;
   end Report_queue;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (
      Name : in  String;  -- Queue name
      Lost : in  Boolean; -- Show lost ones
      Data : out Agpl.Http.Server.Sort_handler.Data_set) 
   is
      use Queue_list;
      I : Iterator_type := Find (Queues, Name);
   begin
      if I /= Back (Queues) then
         Element (I).Http_report (Lost, Data);
      else
         I := First (Queues);
         while I /= Back (Queues) loop
            Element (I).Http_report (Lost, Data);
            I := Succ (I);
         end loop;
      end if;
   end Http_report;

   ------------------------------------------------------------------------
   -- Members access                                                     --
   ------------------------------------------------------------------------
   -- Get_session_sent
   function Get_session_sent return Long_long_integer is
   begin
      return Session_sent;
   end Get_session_sent;

   -- Get_mean_speed
   function Get_mean_speed return Speed is
   begin
      return Mean_speed;
   end Get_Mean_speed;

   end Object;

   task Manager_Update;

   task body Manager_Update is
   begin
      loop
         exit when Globals.Requested_exit;
         delay 0.25;
         if Debug.Debug_statistics_enabled then
            Statistics.Object.Set ("Tasking - Uploads manager",
               Statistics.Booleans.Create (true));
         end if;
         begin
            Manager.Object.Process;
         exception
            when E : others =>
               Trace.Log ("Upload.Queue_manager.Manager_update: " &
                  Trace.Report (E), Trace.Error);
         end;
      end loop;
      Trace.Log ("Adagio.Upload.Queue.Manager.Manager_update exited.");
   end Manager_Update;

begin
   Client_data.List.Init;
end Adagio.Upload.Queue.Manager;
