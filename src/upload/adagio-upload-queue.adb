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
--  $Id: adagio-upload-queue.adb,v 1.8 2004/02/24 15:26:14 Jano Exp $

With
Adagio.Exceptions,
Adagio.File.Criteria,
Adagio.Globals,
Adagio.Globals.Options,
Adagio.Os,
Adagio.Trace,
Adagio.Unicode,
Adagio.Upload.Active_clients,
Adagio.Upload.Client_data,
Adagio.Upload.Resource.File,
Agpl.Geoip,
Agpl.Types.Ustrings,
Expressions_evaluator,
Strings.Utils,
Gnat.Heap_sort_a,
Gnat.Os_lib,
Ada.Streams,
Ada.Streams.Stream_IO,
Ada.Tags,
Ada.Unchecked_Deallocation;

Use
Ada,
Ada.Tags,
Ada.Streams,
Adagio.Exceptions,
Agpl.Types.Ustrings,
GNAT,
Strings.Utils;
with Ada.Strings.Unbounded;

package body Adagio.Upload.Queue is

   use type Agpl.Types.Ustrings.Ustring;

   use type Upload.Resource.Handle;

   Remember_client_period : Duration
      renames Globals.Options.Uploads_RememberClientPeriod;
   Safe_queues            : Boolean
      renames Globals.Options.Uploads_SafeQueues;

   procedure Free is new Unchecked_deallocation (
      Queue_slot, Queue_slot_access);

   ------------------------------------------------------------------------
   -- Times_client                                                       --
   ------------------------------------------------------------------------
   -- Count how many times is a client in a slot_list
   function Times_client (
      This : in Slot_Vector.Object; Client_id : in String)
      return Natural is
      use Slot_vector;
      Num : Natural := 0;
   begin
      for N in 1 .. Last (This) loop
         if This.Vector (N).Client_id = Client_id then
            Num := Num + 1;
         end if;
      end loop;

      return Num;
   end Times_client;

   ------------------------------------------------------------------------
   -- Trace_queue                                                        --
   ------------------------------------------------------------------------
   procedure Trace_queue (
      This : in Slot_vector.Object; Rated : in Boolean := false) is
      use Slot_vector;
      Trail : Ustring;
   begin
      for N in 1 .. Last (This) loop
         Trail := U (" ");
         if This.Vector (N).Can_start then
            Trail := Trail & U ("U");
         end if;
         if This.Vector (N).Alive then
            Trail := Trail & U ("");
         else
            Trail := Trail & U ("D");
         end if;
         if Rated then
            Trace.Log ("Position" & N'img & "#" &
               Misc.To_string (Float (This.Vector (N).Rating)) & ": " &
               S (This.Vector (N).Queue_Id) & S (Trail));
         else
            Trace.Log ("Position" & N'img & ": " &
               S (This.Vector (N).Queue_Id) & S (Trail));
         end if;
      end loop;
   end Trace_queue;

   protected body Object is

   procedure Purge_dead_slots;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   function Contains (Queue_id : in String) return Boolean is
   begin
      return Id_list.Is_in (Queue_id, Ids);
   end Contains;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This           : in  Object_access;
      Name           : in  String;
      Length         : in  Natural;
      Uploads        : in  Natural;         -- Max queue length.
      Bandwidth      : in  File_size;        -- Base bandwidth.
      Minimum_speed  : in  Speed := 1024; -- That active clients must hold.
      Average_period : in  Duration := 30.0 * 60.0;
      Criteria       : in  String   := "true";
      Preemptive     : in  Preemptions := Default_preemption;
      Ordering       : in  Orderings   := Default_ordering;
      Base_folder    : in  String      := "") is

      -- From file:
      procedure Restore is
         use Ada.Calendar;
         use Ada.Streams.Stream_IO;
         use Slot_vector;

         F    : File_type;
         Path : Ustring;
         Str  : Stream_access;
         Slot : Queue_slot_access;
      begin
         if Base_folder = "" then
            Path := Name & U (".queue.dat");
         elsif Base_folder (Base_folder'Last) /= Os.Folder_separator then
            Path := Base_folder & Os.Folder_separator & Name &
               U (".queue.dat");
         else
            Path := Base_folder & Name & U (".queue.dat");
         end if;

         if not Os_lib.Is_regular_file (S (Path)) then
            Trace.Log ("Upload.Queue.Restore: Queue data not found for " &
               S (Path));
            return;
         end if;

         Open (F, Name => S (Path), Mode => In_file);
         Str := Stream (F);

         -- Loading
         while not End_of_file (F) loop
            Slot := new Queue_slot;
            Slot.Max_slots  := Length;
            Slot.Queue_id   := U (String'Input (Str));
            Slot.Client_id  := U (String'Input (Str));

            Slot.Arrival    := Calendar.Time'Input (Str);
            Slot.Last_seen  := Calendar.Time'Input (Str);
            Slot.Expiration := Calendar.Time'Input (Str);

            Slot.Client_name := Ustring'Input (Str);
            Slot.Client_file := Ustring'Input (Str);
            Slot.Client_ip   := Ustring'Input (Str);

            slot.Can_start  := false;
            Slot.Alive      := false;

            if Times_client (Clients, S (Slot.Client_id)) < Max_per_client
            then
               Append (Clients, Slot);
               Id_list.Insert (Ids, S (Slot.Queue_Id), Slot);
            else
               Free(Slot);
            end if;
         end loop;

         Close (F);
         Trace.Log ("Upload.Queue.Restore: Queue " & Name &
            " loaded correctly.", Trace.Informative);
         Purge_dead_slots;
         Schedule;
      exception
         when E : others =>
            Trace.Log ("Upload.Queue.Restore: Load failed from " & S (Path) &
               ": " & Trace.Report (E), Trace.Error);
            if Is_open (F) then
               Close (F);
            end if;
      end Restore;

   begin
      Object.Self       := This;
      Object.Name       := U (Name);
      Object.Length     := Length;
      Object.Uploads    := Uploads;
      Object.Bandwidth  := Bandwidth;
      Object.Min_speed  := Minimum_speed;
      Object.Avg_period := Average_period;
      Object.Criteria   := U (Criteria);
      Object.Preemptive := Preemptive;
      Object.Order      := Ordering;

      File_base := U (Base_folder);

      Max_per_client := Globals.Options.Uploads_MaxPerClient;

      -- Slot 0 null:
      Slot_vector.Append (Clients, null);

      -- Loading:
      Restore;
   end Create;

   ------------------------------------------------------------------------
   -- Purge_dead_slots                                                   --
   ------------------------------------------------------------------------
   procedure Purge_dead_slots is
      use Slot_vector;
      use type Calendar.Time;
      Now : Calendar.Time := Calendar.Clock;
   begin
      for N in reverse 1 .. Last (Clients) loop
         if (not Clients.Vector (N).Alive) and then
            Clients.Vector (N).Expiration < Now
         then
            Trace.Log ("Upload.Queue.Purge_dead_slots: Dropping too old: "
               & S (Clients.Vector (N).Queue_id));
            Remove (S (Clients.Vector (N).Queue_id));
         end if;
      end loop;
   end Purge_dead_slots;

   ------------------------------------------------------------------------
   -- Enqueue                                                            --
   ------------------------------------------------------------------------
   -- Try to add a new client.
   -- Can only fail if queue is full.
   procedure Enqueue (
      Client  : access Upload.Client.Object'Class;
      Success : out    Boolean) is
      Slot    : Queue_slot_access;

      procedure Save is
      begin
         Serialize (S (File_base));
      end Save;
      -- Requeuing. We'll try to reuse a dead slot of a client for a new
      -- upload. An exact match will be tried first, i.e., if the same file
      -- was requested when lost, that position will be taken.
      -- However, if the client requests a file it had not requested when
      -- lost, its slot will be used.
      procedure Requeue_client (Success : out Boolean) is
         use Slot_vector;
      begin
         for N in 1 .. Last (Clients) loop
            Slot := Clients.Vector (N);
            if (not Slot.Alive) and then
               S (Slot.Queue_Id) = Upload.Client.Queue_id (Client.all) then
               -- Revived!
               Slot.Can_start  := false;
               Slot.Alive      := true;
               Slot.Resource   :=
                  Upload.Client.Requested_resource (Client.all);

               Success := true;
               return;     --<---------------- Exit with slot revived.
            end if;
         end loop;
         -- Try to use another dead slot for the same client.
         for N in 1 .. Last (Clients) loop
            Slot := Clients.Vector (N);
            if (not Slot.Alive) and then
               S (Slot.Client_id) = Upload.Client.Id (Client.all) then
               -- Revived!

               -- Remove from old Id
               Id_list.Delete (Ids, S (Slot.Queue_id));

               Slot.Queue_id   := U (Upload.Client.Queue_id (Client.all));
               Slot.Can_start  := false;
               Slot.Alive      := true;
               Slot.Resource   :=
                  Upload.Client.Requested_resource (Client.all);

               -- Re-insert with correct id
               Id_list.Insert (Ids, S (Slot.Queue_id), Slot);

               Success := true;
               return;     --<---------------- Exit with slot revived.
            end if;
         end loop;
         Success := false;
      end Requeue_client;
   begin
      -- Criteria?
      if not Upload.Resource.Qualify (
         Upload.Resource.V (Upload.Client.Requested_resource (Client.all)).all,
         S (Criteria)) then
         Trace.Log ("Upload.Queue: " & S (Name) &
            ": Rejecting, criteria not meet.");
         Success := false;
         return;
      end if;

      -- Already queued?
      declare
         use type Id_list.Iterator_type;
         Slot : Id_list.Iterator_type :=
            Id_list.Find (Ids, Upload.Client.Queue_id (Client.all));
      begin
         if Slot /= Id_list.Back (Ids) and then
            Id_list.Element (Slot).Alive
         then
            Trace.Log ("Upload.Queue: " & S (Name) &
               ": Rejecting, already queued.");
            Success := false;
            return;
         end if;
      end;

      Purge_dead_slots;
      -- Search for a dead slot of the client:
      Requeue_client (Success);
      if Success then
         Trace.Log ("Upload.Queue: Revived slot " & S (Slot.Queue_Id));
      else
         -- Queue full?
         if Slot_vector.Length (Clients) >= Length then
            Trace.Log ("Upload.Queue: " & S (Name) &
               ": Rejecting, queue is full.");
            Success := false;
            return;
         end if;

         -- Too many appareances in this queue?
         if Times_client (Clients, Upload.Client.Id (Client.all)) >=
            Max_per_client
         then
            Trace.Log ("Upload.Queue: " & S (Name) &
               ": Rejecting, too many uploads for same client.");
            Success := false;
            return;
         end if;

         -- Insert it:
         Slot       := new Queue_slot'(
            Queue_id   => U (Upload.Client.Queue_id (Client.all)),
            Client_id  => U (Upload.Client.Id (Client.all)),
            Resource   => Upload.Client.Requested_resource (Client.all),
            Client     => Upload.Client.Object_access (Client),
            Position   => 0,
            Max_slots  => Length,
            Used_slots => Slot_vector.Last (Clients),
            Rating     => 0.0,
            Arrival    => Calendar.Clock,
            Last_seen  => Calendar.Clock,
            Expiration => Calendar.Clock,
            Can_start  => false,
            Alive      => true,
            Client_name => U (Upload.Client.Name (Client.all)),
            Client_file =>
               U (Upload.Resource.Name (
                  Upload.Resource.V (
                     Upload.Client.Requested_resource (Client.all)).all)),
            Client_ip  => U (Upload.Client.Address (Client.all))
                     );

         Slot_Vector.Append (Clients, Slot);
         Id_list.Insert (Ids, S (Slot.Queue_Id), Slot);
      end if;

      Slot.Client := Upload.Client.Object_access (Client);

      Success    := true;

      -- See if is a starter:
      Schedule;

      Trace.Log ("** Queue: " & S (Name) & " ** ");
      Trace_queue (Clients, Order.Kind = Rated);
      if Safe_queues then
         Save;
      end if;
   end Enqueue;

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Remove an upload by ID
   procedure Remove (Queue_id : in String) is
      use Slot_vector;
   begin
      Id_list.Delete (Ids, Queue_id);
      for N in 1 .. Last (Clients) loop
         if S (Clients.Vector (N).Queue_id) = Queue_id then
            if Clients.Vector (N).Can_start then
               Current_uploads := Current_uploads - 1;
               -- Remove from actives:
               Upload.Active_clients.List.Remove (
                  (Id       => Clients.Vector (N).Client_id,
                   Queue_id => Clients.Vector (N).Queue_id));
            end if;
            Free (Clients.Vector (N));
            Delete (Clients, N);
            -- Optimize (Clients); -- EXPERIMENTAL -- TO BE REMOVED
            exit;
         end if;
      end loop;

      Schedule;
   end Remove;

   ------------------------------------------------------------------------
   -- Lost                                                               --
   ------------------------------------------------------------------------
   -- Mark a slot as lost by ID
   -- Free all resources and wait until reconnection or too old.
   procedure Lost (Queue_id : in String) is
      use Calendar;
      use Id_list;
      Slot : Queue_slot_access;
   begin
      Slot := Element (Find (Ids, Queue_id));
      Slot.Alive      := false;
      Slot.Expiration := Clock + Remember_client_period;
      Slot.Last_seen  := Clock;

      Trace.Log ("Upload.Queue.Lost: Queue id " & Queue_id & " lost.");
      Purge_dead_slots;
      Schedule;
   end Lost;

   ------------------------------------------------------------------------
   -- Schedule                                                           --
   ------------------------------------------------------------------------
   -- Re-sort the queue and marks the slots which should start.
   -- Never should a slot uploading be moved to the not uploading zone.
   procedure Schedule is
      use Ada.Calendar;
      use Slot_vector;

      procedure Move (From, To : Natural) is
      begin
         Clients.Vector (To) := Clients.Vector (From);
      end Move;
      function Less (L, R : Natural) return Boolean is
         SL, SR : Queue_slot_access;
      begin
         SL := Clients.Vector (L);
         SR := Clients.Vector (R);
         if SL.Can_start /= SR.Can_start then
            return SL.Can_start > SR.Can_start;
         elsif SL.Alive = SR.Alive then
            if SL.Alive then
               return SL.Arrival < SR.Arrival;
            else
               return SL.Last_seen > SR.Last_seen;
            end if;
         else
            return SL.Alive > SR.Alive ;
         end if;
      end Less;
      function Less_rated (L, R: Natural) return Boolean is
         SL, SR : Queue_slot_access;
      begin
         SL := Clients.Vector (L);
         SR := Clients.Vector (R);
         if SL.Can_start /= SR.Can_start then
            return SL.Can_start > SR.Can_start;
         elsif SL.Alive = SR.Alive then
            if SL.Alive then
               if SL.Rating = SR.Rating then
                  return Less (L, R);
               else
                  return SL.Rating > SR.Rating;
               end if;
            else
               return SL.Last_seen > SR.Last_seen;
            end if;
         else
            return SL.Alive > SR.Alive;
         end if;
      exception
         when E : others =>
            Trace.Log ("Upload.Queue.Less_rated: " & Trace.Report (E),
               Trace.Error);
            return Less (L, R);
      end Less_rated;
      procedure Rate_slots is
      begin
         for N in 1 .. Last (Clients) loop
            Clients.Vector (N).Rating :=
               Rate (Clients.Vector (N).all, S (Order.Expression));
         end loop;
      end Rate_slots;

      Start   : Calendar.Time := Calendar.Clock;
      N       : Natural;
      Success : Boolean;
   begin
      -- Sort the queue
      case Order.Kind is
         when Fifo =>
            Heap_sort_a.Sort (
               Last (Clients),
               Move'Unrestricted_access,
               Less'Unrestricted_access);
         when Rated =>
            Rate_slots;
            Heap_sort_a.Sort (
               Last (Clients),
               Move'Unrestricted_access,
               Less_rated'Unrestricted_access);
         when others =>
            raise Unimplemented;
      end case;
      Trace.Log ("Upload.Queue.Schedule: " & S (Name) & " sorted in " &
         Misc.To_string (Float (Clock - Start), 5) & "s");

      -- Mark the downloads that must start:
      -- They'll be the N first that are alive
      N := 1;
      while N <= Last (Clients) and then Current_uploads < Uploads loop
         if Clients.Vector (N).Alive and then
            not Clients.Vector (N).Can_start
         then
            -- Try adding it to active clients:
            Upload.Active_clients.List.Add (
               (Id       => Clients.Vector (N).Client_id,
                Queue_id => Clients.Vector (N).Queue_id),
               Max_per_client,
               Success);
            if Success then
               Clients.Vector (N).Can_start := true;
               Current_uploads := Current_uploads + 1;
               declare
                  R : Upload.Resource.Object_access :=
                     +Upload.Client.Requested_resource (
                        Clients.Vector (N).Client.all);
               begin
                  if R.all'Tag = Upload.Resource.File.Object'Tag then
                     Upload.Resource.File.Add_upload (
                        Upload.Resource.File.Object (R.all));
                  end if;
               end;
            end if;
         end if;
         N := N + 1;
      end loop;

      -- Mark positions:
      for N in 1 .. Last (Clients) loop
         Clients.Vector (N).Position := N;
      end loop;
   end Schedule;

   ------------------------------------------------------------------------
   -- Serialize                                                          --
   ------------------------------------------------------------------------
   procedure Serialize (Base : String := "") is
      use Ada.Streams.Stream_IO;
      use Slot_vector;

      F    : File_type;
      Path : Ustring;
      Str  : Stream_access;

      procedure Save_slot (This : in Queue_slot_access) is
         use type Calendar.Time;
      begin
         String'Output (Str, S (This.Queue_Id));
         String'Output (Str, S (This.Client_id));

         Calendar.Time'Output (Str, This.Arrival);
         Calendar.Time'Output (Str, This.Last_seen);
         if This.Alive then
            Calendar.Time'Output (
               Str, Calendar.Clock + Remember_client_period);
         else
            Calendar.Time'Output (Str, This.Expiration);
         end if;

         Ustring'Output (Str, This.Client_name);
         Ustring'Output (Str, This.Client_file);
         Ustring'Output (Str, This.Client_ip);
      end Save_slot;

   begin
      if Base = "" then
         Path := Name & ".queue.dat";
      elsif Base (Base'Last) /= Os.Folder_separator then
         Path := Base & Os.Folder_separator & Name & ".queue.dat";
      else
         Path := Base & Name & ".queue.dat";
      end if;
      Create (F, Name => S (Path), Mode => out_file);
      Str := Stream (F);

      -- Savings
      for N in 1 .. Last (Clients) loop
         Save_slot (Clients.Vector (N));
      end loop;

      Close (F);
      Trace.Log ("Upload.Queue.Serialize: Queue " & S (Name) &
         " saved correctly.", Trace.Debug);
   exception
      when E : others =>
         Trace.Log ("Upload.Queue.Serialize: Save failed to " & S (Path) &
            ": " & Trace.Report (E), Trace.Error);
         if Is_open (F) then
            Close (F);
         end if;
   end Serialize;

   ------------------------------------------------------------------------
   -- Check_client                                                       --
   ------------------------------------------------------------------------
   -- Obtains awareness about a queued client
   procedure Check_client (Queue_id : in String; Data : out Queue_slot) is
      use Id_list;
   begin
      Data := Element (Find (Ids, Queue_id)).all;
      Data.Used_slots := Slot_vector.Last (Clients);
   end Check_client;

   ------------------------------------------------------------------------
   -- Busy                                                               --
   ------------------------------------------------------------------------
   -- Maximum uploads reached
   function Busy return Boolean is
   begin
      return Current_uploads >= Uploads;
   end Busy;

   ------------------------------------------------------------------------
   -- Is_candidate                                                       --
   ------------------------------------------------------------------------
   -- Says if a file satisfy queue entry condition
   function Is_candidate (F : in File.Object) return Boolean is
   begin
      return File.Criteria.Qualify (F, S (Criteria));
   end Is_candidate;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Status of the queue:
   function Report (
      From : in Natural;
      Qty  : in Natural;
      Lost : in Boolean) return Report_array is
      use Slot_vector;
      Result : Report_array (1 .. 1 + Natural'Min (Qty, 10) - 1);
      No_res : Report_array (1 .. 0);
      Slot   : Queue_slot_access;
      Status : Ustring;
      Path   : Ustring;
      Name   : Ustring;
      Spd    : Float;
      use Ada.Calendar;
      use Slot_vector;
      use type File.Object;

      Pos    : Natural := From - 1;
   begin
      if Globals.Requested_exit then
         return No_res;
      end if;

      for N in 1 .. Natural'Min (Qty, 10) loop
         -- Early exit if no more clients:
         -- Trace.Log ("Queue: Pos: " & Pos'Img);
         -- Trace.Log ("Queue: Len: " & Natural'Image (Last (Clients)));
         loop
            Pos := Pos + 1;
            exit when
               Pos > Last (Clients) or else
               Clients.Vector (Pos).Alive or else
               Lost;
         end loop;
         if Pos > Last (Clients) then
            return Result (1 .. N - 1);
         end if;
         -- Data of the next valid client:
         Slot := Clients.Vector (Pos);
         Path := Slot.Client_file;
         Name := Slot.Client_name;
         if not Slot.Alive then
            Status := U ("Lost");
            Spd    := 0.0;
         elsif Slot.Can_start then
            Status := U ("Uploading");
         else
            Status := U ("Waiting");
         end if;
         if Slot.Alive then
            Spd  := Client.Speed (Slot.Client.all);
            ASU.Append (Path, " (Queued since: " &
               Misc.Image (Clock - Slot.Arrival) & ")");
            if Order.Kind = Rated then
               ASU.Append (Path, " (Rating: " & Misc.To_string (
                  Float (Slot.Rating), 5) & ")");
            end if;
         else
            ASU.Append (Path, " (Lost since: " &
               Misc.Image (Clock - Slot.Last_seen) & ")");
         end if;

         Result (N) := (
            Client_id => Slot.Client_id,
            Position  => Slot.Position,
            Status    => Status,
            Speed     => Spd,
            Client    => U (Unicode.To_utf8 (S (Name))),
            File      => U (Unicode.To_utf8 (S (Path))));
      end loop;
      return Result;
   exception
      when E : others =>
         Trace.Log ("Upload.Queue.Report: " & S (Name) & ": " &
            Trace.Report (E), Trace.Error);
         return No_res;
   end Report;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (
      Lost : in  Boolean; -- Show lost ones
      Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      use Ada.Calendar;
      use Slot_vector;
      use type File.Object;

   begin
      if Globals.Requested_exit then
         return;
      end if;
      for N in 1 .. Last (Clients) loop
         exit when
            ((not Clients.Vector (N).Alive) and (not Lost));

         declare
            Row    : Data_row;
            Slot   : Queue_slot_access := Clients.Vector (N);
            Status : Ustring;
            Spd    : Float;
            Now    : Calendar.Time := Clock;
            Code   : Agpl.Geoip.Country_code;
         begin
            -- Queue name
            Append (Row, (Name, Name));

            -- Position
            Append (Row, (U (Misc.To_string (N)), RPad (N, 4)));

            -- Rating
            if Slot.Alive then
               Append (Row, (
                  U (Misc.To_string (Float (Slot.Rating))),
                  Rpad (Float (Slot.Rating), 15)));
            else
               Append (Row, (
                  U ("N/A"),
                  Rpad (Float'Last, 15)));
            end if;

            -- Status
            if not Slot.Alive then
               Status := U ("Lost");
               Spd    := 0.0;
            else
               if Slot.Can_start then
                  Status := U ("Uploading");
               else
                  Status := U ("Waiting");
               end if;
               Spd  := Client.Speed (Slot.Client.all);
            end if;
            Append (Row, (Status, Status));

            -- Filename
            Append (Row, (Slot.Client_file, Slot.Client_file));

            -- Speed
            Append (Row, (
               U (Misc.To_string (Spd)),
               Rpad (Spd, 10)));

            -- Client
            Append (Row, (Slot.Client_name, Slot.Client_name));

            -- Arrival
            Append (Row, (
               U (Misc.Image (Now - Slot.Arrival)),
               Rpad (Float (Now - Slot.Arrival), 15)));

            -- Last seen
            if not Slot.Alive then
               Append (Row, (
                  U (Misc.Image (Now - Slot.Last_seen)),
                  Rpad (Float (Now - Slot.Last_seen), 15)));
            else
               Append (Row, (U ("Now"), U ("0")));
            end if;

            -- Expiration
            if not Slot.Alive then
               Append (Row, (
                  U (Misc.Image (Slot.Expiration - Now)),
                  Rpad (Float (Slot.Expiration - Now), 15)));
            else
               Append (Row, (U ("N/A"), U ("0")));
            end if;

            -- IP
            Append (Row, (Slot.Client_ip, Slot.Client_ip));

            -- Country code
            Code := Agpl.Geoip.Country_code_from_addr (S (Slot.Client_ip));
            if Code = "??" then
               Append (Row, (U ("unknown"), U ("ZZ")));
            else
               Append (Row, (U (Code), U (Code)));
            end if;

            -- Country name
            if Code = "??" then
               Append (Row, (U ("Unknown"), U ("Unknown")));
            else
               Append (Row, (
                  U (Agpl.Geoip.Country_name_from_code (Code)),
                  U (Agpl.Geoip.Country_name_from_code (Code))));
            end if;

            Append (Data, Row);
         end;
      end loop;
   end Http_report;

   ------------------------------------------------------------------------
   -- Members_access                                                     --
   ------------------------------------------------------------------------
   -- Get_name
   function Get_name return String is
   begin
      return S (Name);
   end Get_name;

   -- Get_slot_bandwidt
   function Get_slot_bandwidth return Speed is
   begin
      return Bandwidth / Speed (Uploads);
   end Get_slot_bandwidth;

   -- Get max length
   function Get_max_length return Natural is
   begin
      return Length;
   end Get_max_length;

   -- Get current queued clients:
   function Get_current_length return Natural is
   begin
      return Natural'Max (Slot_vector.Last (Clients), 0);
   end Get_current_length;

   -- Count alive slots:
   function Get_alive_length return Natural is
      Length : Natural := 0;
      use Slot_vector;
   begin
      for N in 1 .. Last (Clients) loop
         if Clients.Vector (N).Alive then
            Length := Length + 1;
         end if;
      end loop;

      return Length;
   end Get_alive_length;

   -- Get average period for speeds
   function Get_avg_period return Duration is
   begin
      return Avg_period;
   end Get_avg_period;

   -- Get minimum speed
   function Get_min_speed return Speed is
   begin
      return Min_speed;
   end Get_min_speed;

   -- Preemptions
   function Get_preemptions return Preemptions is
   begin
      return Preemptive;
   end Get_preemptions;

   -- Max active uploads
   function Get_uploads return Natural is
   begin
      return Uploads;
   end Get_uploads;

   function Get_current_uploads return Natural is -- uploading now
      Length : Natural := 0;
      use Slot_vector;
   begin
      for N in 1 .. Last (Clients) loop
         if Clients.Vector (N).Alive then
            if Clients.Vector (N).Can_start then
               Length := Length + 1;
            else
               return Length;
            end if;
         else
            return Length;
         end if;
      end loop;

      return Length;
   end Get_current_uploads;

   function Get_current_waiting return Natural is -- alive waiting now
      Length : Natural := 0;
      use Slot_vector;
   begin
      for N in 1 .. Last (Clients) loop
         if Clients.Vector (N).Alive then
            if not Clients.Vector (N).Can_start then
               Length := Length + 1;
            end if;
         else
            return Length;
         end if;
      end loop;

      return Length;
   end Get_current_waiting;

   end Object;

   package Eval is new Expressions_evaluator (Rating);

   ------------------------------------------------------------------------
   -- Rate                                                               --
   ------------------------------------------------------------------------
   function Rate (This : in Queue_slot; Expr : in String) return Rating is
      use Ada.Calendar;

      Cooked_expr : Ustring := U ("f = " & Expr);

      Uploads     : Natural;
      Bytes_sent  : File_size;
      Size        : File_size;
      Waited      : Duration;

      E           : Eval.Expressions;
      Result      : Rating;
   begin
      if not This.Alive then
         return Rating'First;
      end if;

      -- Get values:
      declare
         F : Upload.Resource.File.Object_access;
      begin
         F := Upload.Resource.File.Object_access (+This.Resource);

         Uploads := File.Uploads (Upload.Resource.File.File (F.all));
      exception
         when others => -- For example, incorrect tag :)
            Uploads := 0;
      end;

      Bytes_sent := Client_data.List.Get_sent (S (This.Client_id));
      Size       := File_size (
         Upload.Resource.Size (Upload.Resource.V (This.Resource).all));
      Waited     := Clock - This.Arrival;

      -- Do replacements:
      Cooked_expr :=
         U (Replace (S (Cooked_expr), "uploads",
            "(" & Natural'Image (Uploads) & ")"));

      Cooked_expr :=
         U (Replace (S (Cooked_expr), "bytes_sent",
            "(" & File_size'Image (Bytes_sent) & ")"));

      Cooked_expr :=
         U (Replace (S (Cooked_expr), "file_size",
            "(" & File_size'Image (Size) & ")"));

      if Waited < 1.0 then
         Waited := 1.0;
      end if;
      Cooked_expr :=
         U (Replace (S (Cooked_expr), "waited",
            "(" & Duration'Image (Waited) & ")"));

      begin
         E := Eval.Create (S (Cooked_expr));
         Result := Eval.Evaluate (E, Eval.f);
         Eval.Destroy (E);
         return Result;
      exception
         when Ex : others =>
            Trace.Log ("Upload.Ratings: Unable to evaluate " &
               S (Cooked_expr) & ": " & Trace.Report (Ex), Trace.Error);
            Eval.Destroy (E);
            return Rating'First;
      end;
   end Rate;

end Adagio.Upload.Queue;
