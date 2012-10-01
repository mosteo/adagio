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
--  $Id: aenea-walker.adb,v 1.13 2004/03/09 23:46:48 Jano Exp $

with Adagio.Convert;
with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver;
with Adagio.Globals;
with Adagio.Network.Endian;
with Adagio.Os.Memory;
with Adagio.Socket;

with Aenea.Chronos;
with Aenea.Countries;
with Aenea.DB;
with Aenea.Debug;
with Aenea.Globals;
with Aenea.Globals.Options;
with Aenea.Gui.Events;
with Aenea.Hub.Actions;
with Aenea.Hub.Automaton;
with Aenea.Hub.Create_Event;
with Aenea.Hub.Report;
with Aenea.Net;
with Aenea.Os;
with Aenea.Persistent;
with Aenea.Tables;
with Aenea.Trace;

with Agpl.Calendar.Format;
with Agpl.Chronos; use Agpl.Chronos;
with Agpl.Event_Queues.Calendar;
with Agpl.Safe_file;
with Agpl.Strings; use Agpl.Strings;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
use  Agpl;

with Ada.Calendar; use  Ada.Calendar;
--  with Ada.Containers;
with Ada.Strings.Maps; use Ada.Strings;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
use  Ada;

package body Aenea.Walker is

   use type Ada.Calendar.Time;
   use type Hub.Status_Type;
   use type Signals.Answer_Kinds;

   use Hub.Hub_map;
   package Conv renames Adagio.Network.Endian;
   package Event_Queue renames Agpl.Event_Queues.Calendar;

   Hosts_path      : constant String := Globals.Data_folder & "hosts.dat";

   Startup         : constant Time := Clock;

   Wrong_Status    : Boolean := False; -- For debug. When nothing changes.
   pragma Atomic (Wrong_Status);
   pragma Unreferenced (Wrong_Status);

   -- Set for removing spaces and nulls from version strings:
   use type Maps.Character_Set;
   Trim_Set : constant Maps.Character_Set :=
      Maps.To_Set (' ') or Maps.To_Set (Character'Val (0));

   ------------------------------------------------------------------------
   -- Get_leaves_from_HS                                                 --
   ------------------------------------------------------------------------
   procedure Get_leaves_from_HS (
      P          : in G2.Packet.Object;
      Leaves     : out Natural;
      Max_leaves : out Natural)
   is
      Data : constant String := G2.Packet.Payload (P);
   begin
      Leaves := Conv.Convert (
         Conv.To_byte_array (Data (Data'First .. Data'First + 1)),
            G2.Packet.Big_endian (P));
      if Data'Length >= 4 then
         Max_leaves := Conv.Convert (
            Conv.To_byte_array (
               Data (Data'First + 2 .. Data'First + 3)),
            G2.Packet.Big_endian (P));
      else
         Max_leaves := 0;
      end if;
   end Get_leaves_from_HS;

   ------------------------------------------------------------------------
   -- Trace_status                                                       --
   ------------------------------------------------------------------------
   procedure Trace_status is
   begin
      Trace.Log
        ("H:" & Integer'Image (Net.Counter.Hubs_count) &
         "; L:" & Integer'Image (Net.Counter.Leaves_count) &
         "; U:" & Integer'Image (Net.Counter.Unique_Count) &
         "; S:" & Integer'Image (Net.Counter.Total_count), Trace.Informative);
      Trace.Log
        ("Trk:" & Integer'Image (Adder.Count) &
         "; In :" & Integer'Image (Net.Incoming.Length) &
         "; Out:" & Integer'Image (Net.Outgoing.Length) &
         "; Ev:" & Integer'Image (Event_queue.Length (Net.Events)) &
         "; Nx: " & To_String (Float (Event_Queue.Get_Next_Deadline (Net.Events) -
                                      Clock)) &
         "; Mem:" & Adagio.Convert.To_Size (Adagio.Os.Memory.Heap_Usage),
         Trace.Informative);
   end Trace_status;

   ------------------------------------------------------------------------
   -- Reporter                                                           --
   ------------------------------------------------------------------------
   task Reporter;
   task body Reporter is
      H, L, T: Natural := 0;
      NH, NL : Natural;
      NT, NU : Natural;
      NOL, NNOL : Natural := 0;
      EQL, NEQL : Natural := 0;
      NXE, NNXE : Time := Clock;
      Count  : Natural := 0;

      Period   : constant Duration := 1.0;
      Next     : Time := Clock + Period;
      DBPeriod : Duration := 0.0;
      DBStart  : constant Time := Clock;
      Unchange : Agpl.Chronos.Object;
      Timer      : Chronos.Object;
      Long_Timer : Chronos.Object;
      Watchdog   : Chronos.Object;
   begin
      while not Globals.Requested_exit loop
         if Watchdog.Elapsed > 60.0 then
            Debug.Overtimed_Task.Set (+"Reporter");
            Debug.Overtime.Set (Watchdog.Elapsed);
         end if;
         Watchdog.Reset;
         delay until Next;
         Next := Next + Period;
         begin
            Trace_Status;

            NH := Net.Counter.Hubs_count;
            NL := Net.Counter.Leaves_count;
            NT := Net.Counter.Total_count;
            NU := Net.Counter.Unique_count;
            NNOL := Net.Outgoing.Length;
            NEQL := Event_Queue.Length (Net.Events);
            NNXE := Event_Queue.Get_Next_Deadline (Net.Events);
            if NH /= H or else NL /= L or else NT /= T or else
              NOL /= NNOL or else NEQL /= EQL or else NNXE /= NXE
            then
               Wrong_Status := False;
               Agpl.Chronos.Reset (Unchange);
               H := NH;
               L := NL;
               T := NT;
               NOL := NNOL;
               EQL := NEQL;
               NXE := NNXE;
            elsif Event_Queue.Get_Next_Deadline (Net.Events) - Clock < - 10.0 - Globals.Options.Walk_InsertDelay
              or else Agpl.Chronos.Elapsed (Unchange) >= 120.0
            then
               Wrong_Status := True;
               Trace.Log ("WARNING - Events are stalled", Trace.Warning);
               Trace.Log ("Next event due in" & Duration'Image
                            (Event_Queue.Get_Next_Deadline (Net.Events) - Clock), Trace.Warning);
               Trace.Log ("Queue master status is " &
                          Event_Queue.Get_Master_Status (Net.Events)'Img, Trace.Warning);
               Trace.Log ("Queue worker status is " &
                          Event_Queue.Get_Worker_Status (Net.Events)'Img, Trace.Warning);
            end if;

            -- Averages
            if Clock - DBStart >= Globals.Options.Walk_InsertDelay then
               Count := Count + 1;
               Net.Counter.Push_hubs (NH);
               Net.Counter.Push_leaves (NL);
               Net.Counter.Push_Unique (NU);
               Net.Counter.Push_total (NT);
               if Count = 60 then
                  Count := 0;
                  Trace.Log ("******* AVG HUBS  :" & Integer'Image (Net.Counter.Avg_hubs), Trace.Informative);
                  Trace.Log ("******* AVG LEAVES:" & Integer'Image (Net.Counter.Avg_leaves), Trace.Informative);
                  Trace.Log ("******* AVG UNIQUE:" & Integer'Image (Net.Counter.Avg_Unique), Trace.Informative);
                  Trace.Log ("******* AVG TOTAL:" & Integer'Image (Net.Counter.Avg_total), Trace.Informative);
               end if;
            end if;

            -- DB stuff
            DBPeriod := DBPeriod + 1.0;
            if Clock - DBStart >= Globals.Options.Walk_InsertDelay then
               if DBPeriod >= Globals.Options.Walk_InsertPeriod then
                  Trace.Log ("** DB insertions starting ... **", Trace.Informative);
                  DBPeriod := 0.0;
                  Chronos.Reset (Timer);
                  select
                     delay Globals.Options.Walk_InsertAbortPeriod;
                     Trace.Log ("!! DB insertions ABORTED.     !!", Trace.Warning);
                     Tables.By_Insertion_Total_Time.Sum_Key ("Aborted (Timeout)");
                  then abort

                     --  DEBUG
--                       declare
--                          Now          : constant String := "Timestamp";
--                          Versions_Db  : Db.Row_Lists.List;
--                          procedure Insert_Version (Version : in String; Count : in Integer) is
--                          begin
--                             Versions_Db.Append (Db.Rows'(+"version", +Version, Count, +Now));
--                          end Insert_Version;
--                       begin
--                          Hub.By_Version.Iterate (Insert_Version'Unrestricted_Access);
--                          Db.Insert_Rows (Versions_Db);
--                       end;
                     --  /DEBUG

                     -- Amounts crawled
                     DB.Insert_row
                       (Net.Counter.Avg_hubs,
                        Net.Counter.Avg_leaves,
                        Net.Counter.Avg_Unique,
                        Net.Counter.Avg_total,
                        Adder.Count);

                     -- Vendors
                     declare
                        Now : constant String := Db.Get_Timestamp;
                        procedure Insert_Vendor (Vendor : in String; Count : in Integer) is
                        begin
                           Db.Insert_Row ("vendor", Vendor, Count, Now);
                        end Insert_Vendor;
                     begin
                        Hub.By_Vendor.Iterate (Insert_Vendor'Unrestricted_Access);

                        -- Insert versions & countries; minimum 5-minutes delay
                        if Chronos.Elapsed (Long_Timer) >= 300.0 then
                           Trace.Log ("** DB extended insertions...  **", Trace.Informative);
                           Chronos.Reset (Long_Timer);
                           declare
                              Now : constant String := Db.Get_Timestamp;
                           begin
                              declare
                                 Versions_Db  : Db.Row_Lists.List;
                                 procedure Insert_Version (Version : in String; Count : in Integer) is
                                 begin
                                    Versions_Db.Append (Db.Rows'(+"version", +Version, Count, +Now));
                                 end Insert_Version;
                              begin
                                 Hub.By_Version.Iterate (Insert_Version'Unrestricted_Access);
                                 Db.Insert_Rows (Versions_Db);
                              end;

                              declare
                                 Countries_Db : Db.Row_Lists.List;
                                 procedure Insert_Country (Code : in String; Count : in Integer) is
                                 begin
                                    Countries_Db.Append (Db.Rows'(+"country", +Code, Count, +Now));
                                 end Insert_Country;
                              begin
                                 Countries.Iterate (Insert_Country'Access);
                                 Db.Insert_Rows (Countries_Db);
                              end;
                           end;
                        end if;
                     end;

                     --  Update or histogram of insertions time:
                     Tables.By_Insertion_Total_Time.Sum_Key
                       (Tables.Interval_Key (Integer (Chronos.Elapsed (Timer)),
                                             Globals.Options.Db_Histogram_Grain));

                     Trace.Log ("** DB insertions finished.    **", Trace.Informative);
                     Trace.Log ("** DB insertions lasted for " & Chronos.Image (Timer),
                                Trace.Informative);
                  end select;
               end if;
            end if;
         exception
            when E : others =>
               Trace.Log ("Reporter: " & Trace.Report (E), Trace.Error);
               --  Update or histogram of insertions time:
               Tables.By_Insertion_Total_Time.Sum_Key
                 (Agpl.Calendar.Format.Timestamp & " " & Trace.Report (E));
         end;
      end loop;
   end Reporter;

   ----------------------
   -- Process_Incoming --
   ----------------------
   procedure Process_Incoming is
      Watchdog : Chronos.Object;
   begin
      loop
         exit when Adagio.Globals.Requested_Exit;

         if Watchdog.Elapsed > 10.0 then
            Debug.Overtimed_Task.Set (+"Process_Incoming");
            Debug.Overtime.Set (Watchdog.Elapsed);
         end if;
         Watchdog.Reset;

         declare
            Item : G2.Packet.Queue.Item_type;
            Ok   : Boolean := False;
         begin
            select
               Net.Incoming.Get (Item);
               Ok := True;
            or
               delay 1.0;
            end select;

            if Ok and then not Debug_Stop then

               Adder.Process_item (Item);

            end if;
         exception
            when E : others =>
               Trace.Log ("Process_Incoming: " & Trace.Report (E), Trace.Error);
         end;
      end loop;
   end Process_Incoming;

   ----------------------
   -- Process_Outgoing --
   ----------------------
   procedure Process_Outgoing is
      Watchdog : Chronos.Object;
   begin
      loop
         exit when Adagio.Globals.Requested_Exit;

         if Watchdog.Elapsed > 10.0 then
            Debug.Overtimed_Task.Set (+"Process_Outgoing");
            Debug.Overtime.Set (Watchdog.Elapsed);
         end if;
         Watchdog.Reset;

         declare
            Item : G2.Packet.Queue.Item_type;
            Ok   : Boolean := False;
         begin
            select
               Net.Outgoing.Get (Item);
               Ok := True;
            or
               delay 1.0;
            end select;

            if Ok and then not Debug_Stop then
               select
                  delay 10.0;
                  Trace.Log ("Process_Outgoing: Blocked for more than 10 seconds!", Trace.Warning);
               then abort
                  G2.Transceiver.Send (Net.UDPT.all, Item); -- Max BW out
               end select;

               Net.Delayer.Request;                      -- Ad-hoc throttling
            end if;
         end;
      end loop;
   end Process_Outgoing;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   procedure Start is
      W : Worker_Access;
      pragma Unreferenced (W);
   begin
      if Globals.Options.Walk_SaveHubs then
         Adder.Restore_hubs;
      end if;

      --  Create maintenance task:
      W := new Worker (new String'("Maintenance"), Event_Maintenance'Access);
      --  Create packet sender task:
      W := new Worker (new String'("Process_Outgoing"), Process_Outgoing'Access);
      --  Create packet receiver task:
      W := new Worker (new String'("Process_Incoming"), Process_Incoming'Access);
   end Start;

   ------------------------------------------------------------------------
   -- Protected Adder                                                    --
   ------------------------------------------------------------------------
   protected body Adder is

      ------------------------------------------------------------------------
      -- Add                                                                --
      ------------------------------------------------------------------------
      procedure Add_Ready (Address : in String; H : out Hub.Object_Access) is
         I    : constant Iterator_Type := Find (Hubs, Address);
         Cron : Chronos.Object;
      begin
         if I = Back (Hubs) then
            H := new Hub.Object;
            Hub.Create (H.all, Address, Hub.Ready);
            Hub.Create_Event (H.all, (Kind => Signals.Expire), Globals.Options.Walk_RefreshUnit);
            Insert (Hubs, Hub.Id (H.all), H);
            Hub.By_Status.Sum_Key (Hub.Status_Type'Image (Hub.Get_Status (H.all)), +1);
         else
            H := Element (I);
         end if;

         if Cron.Elapsed > 1.0 then
            Trace.Log ("Adder.Add_Ready abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      end Add_Ready;

      procedure Add_Found (Address : in String) is
         H : Hub.Object_Access;
         Cron : Chronos.Object;
      begin
         Add_Ready (Address, H);
         -- Process the convenient signal:
         Hub.Automaton.Process_Signal
           (H.all,
            (Kind => Signals.Found));

         if Cron.Elapsed > 1.0 then
            Trace.Log ("Adder.Add_Found abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      end Add_Found;

      ------------------------------------------------------------------------
      -- Best_Uptimes;                                                      --
      ------------------------------------------------------------------------
      function  Best_Uptimes (Num : in Positive := 10) return Types.Uptimes_Array is
         package Sorted_Hubs renames Types.Sorted_Hubs;
         package Hashed_Hubs renames Types.Hashed_Hubs;
         use type Hashed_Hubs.Cursor;

         Cron: Chronos.Object;
         Set : Sorted_Hubs.Set;
         Map : Hashed_Hubs.Map;
         I   : Iterator_Type := First (Hubs);
         Ant : Hashed_Hubs.Cursor;
         Pos : Hashed_Hubs.Cursor;
         Po2 : Sorted_Hubs.Cursor;
         Ok  : Boolean;
         Now : constant Time := Clock;
         Rec : constant Types.Uptimes_Array := Persistent.Object.Get_Top_Ten_Uptimes;
      begin
--         Hashed_Hubs.Set_Capacity (Map, Ada.Containers.Count_Type (Length (Hubs)));
         -- Insert recorded
         for K in Rec'Range loop
            Hashed_Hubs.Insert (Map, S (Rec (K).Nick), Rec (K), Pos, Ok);
         end loop;
         -- Insert actuals eliminating duplicated nicks
         while I /= Back (Hubs) loop
            if Hub.Is_Alive (Element (I).all) then
               Ant := Hashed_Hubs.Find (Map, Hub.Get_Nick (Element (I).all));
               if Ant = Hashed_Hubs.No_Element or else
                  Hashed_Hubs.Element (Ant).Uptime <
                  Now - Hub.Get_First_Seen (Element (I).all)
               then
                  -- Remove first if already there:
                  if Ant /= Hashed_Hubs.No_Element then
                     Hashed_Hubs.Delete (Map, Ant);
                  end if;
                  Hashed_Hubs.Insert (
                     Map,
                     Hub.Get_Nick (Element (I).all),
                     (Nick    => U (Hub.Get_Nick (Element (I).all)),
                      Uptime  => Now - Hub.Get_First_Seen (Element (I).all),
                      Version => U (Hub.Get_Version (Element (I).all))
                      ),
                     Pos, Ok);
               end if;
            end if;
            I := Succ (I);
         end loop;

         -- Sort these
         Pos := Hashed_Hubs.First (Map);
         while Pos /= Hashed_Hubs.No_Element loop
            Sorted_Hubs.Insert (
               Set,
               Hashed_Hubs.Element (Pos),
               Po2, Ok);
            Hashed_Hubs.Next (Pos);
         end loop;

         declare
            Result : Types.Uptimes_Array (
               1 .. Natural'Min (Num, Natural (Sorted_Hubs.Length (Set))));
            J      : Sorted_Hubs.Cursor := Sorted_Hubs.First (Set);
         begin
            for K in Result'Range loop
               Result (K) := Sorted_Hubs.Element (J);
               Trace.Log (K'Img & " " & S (Result (K).Nick) & " " & Result (K).Uptime'Img,
                  Trace.Debug);
               Sorted_Hubs.Next (J);
            end loop;

            if Cron.Elapsed > 5.0 then
               Trace.Log ("Adder.Best_Uptimes abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
            end if;

            return Result;
         end;
      end Best_Uptimes;

      ------------------------------------------------------------------------
      -- Count                                                              --
      ------------------------------------------------------------------------
      function Count return Natural is
      begin
         return Length (Hubs);
      end Count;

      ------------------------------------------------------------------------
      -- Save_hubs                                                          --
      ------------------------------------------------------------------------
      procedure Save_hubs is
         use Ada.Streams.Stream_IO;
         Cron : Chronos.Object;
         I : Iterator_type := First (Hubs);
         H : Hub.Object_access;
         F : File_type;
         S : Stream_access;
         C : Chronos.Object;
      begin
         Safe_file.Open (F, Out_file, Name => Hosts_path);
         S := Stream (F);
         while I /= Back (Hubs) loop
            H := Element (I);
            Hub.Object'Write (S, H.all);
            I := Succ (I);
         end loop;
         Safe_file.Close (F);
         Trace.Log (
            Integer'Image (Length (Hubs)) & " hosts saved in " &
                    Chronos.Image (C), Trace.Informative);

         if Cron.Elapsed > 10.0 then
            Trace.Log ("Adder.Save_Hubs abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      exception
         when E : others =>
            if Is_open (F) then
               Safe_file.Close (F);
            end if;
            Trace.Log ("Walker.Save_hubs: " & Trace.Report (E), Trace.Error);
      end Save_hubs;

      ------------------------------------------------------------------------
      -- Restore_hubs                                                       --
      ------------------------------------------------------------------------
      procedure Restore_hubs is
         use Ada.Streams.Stream_IO;
         H : Hub.Object_Access;
         F : File_type;
         S : Stream_access;
         C : Chronos.Object;
      begin
         if not Safe_file.Exists_for_reading (Hosts_path) then
            Trace.Log ("No prior hubs found.", Trace.Informative);
            return;
         end if;

         Safe_file.Open (F, In_file, Name => Hosts_path);
         S := Stream (F);
         while not End_of_file (F) loop
            H := new Hub.Object;
            Hub.Object'Read (S, H.all);
            Hub.Set_leaves (H.all, 0);
            Hub.Set_Status (H.all, Hub.Dead);
            Hub.Relocate (H.all);
            Insert (Hubs, Hub.Id (H.all), H);
         end loop;
         Safe_file.Close (F);
         Trace.Log (
            Integer'Image (Length (Hubs)) & " hosts restored in " &
            Chronos.Image (C), Trace.Informative);
      exception
         when E : others =>
            if Is_open (F) then
               Safe_file.Close (F);
            end if;
            Trace.Log ("Walker.Restore_hubs: " & Trace.Report (E), Trace.Error);
      end Restore_hubs;

      ------------------------------------------------------------------------
      -- Report                                                             --
      ------------------------------------------------------------------------
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
         use Agpl.Http.Server.Sort_handler;

         Cron : Chronos.Object;
         I : Iterator_type     := First (Hubs);
         H : Hub.Object_access;
      begin
         while I /= Back (Hubs) loop
            H := Element (I);
            declare
               Row : Data_row;
            begin
               Hub.Report (H.all, Row);
               Append (Data, Row);
            end;
            I := Succ (I);
         end loop;

         if Cron.Elapsed > 5.0 then
            Trace.Log ("Adder.Add_Ready abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      exception
         when E : others =>
            Trace.Log ("Walker.Report: " & Trace.Report (E), Trace.Warning);
      end Report;

      ------------------------------------------------------------------------
      -- Process_item                                                       --
      ------------------------------------------------------------------------
      entry Process_item (Item : in G2.Packet.Queue.Item_type) when True is
         Cron    : Chronos.Object;
         E1, E2,
         E3, E4  : Duration;
         Packet  : G2.Packet.Object renames Item.Packet;
         Signal  : Signals.Object (Kind => Signals.Answer);
         Address : constant String := Adagio.Socket.Image (Item.Udp_source);
         H       : Hub.Object_Access;
         use type Hub.Object_Access;
      begin
         -- Locate hub for later use or create it in ready status
         Add_Ready (Address, H);

         --  Add unique node:
         Net.Counter.Add_Unique (Address);
         E1 := Cron.Elapsed;

         -- Packet identity
         if G2.Packet.Is_A (Packet, "/CRAWLA") then
            Signal.Answer_Kind := Signals.CRAWLA;
         elsif G2.Packet.Is_A (Packet, "/PO") then
            Signal.Answer_Kind := Signals.PO;
         else
            G2.Packet.Parsing.Trace_Tree (Packet, Trace.Warning);
            raise Unexpected_Packet;
         end if;

--         Adagio.G2.Packet.Parsing.Trace_Tree (Packet, Trace.Warning);

         -- Extract data from a CRAWLA
         if Signal.Answer_Kind = Signals.CRAWLA then
            -- Leaves
            Get_leaves_from_HS (
               G2.Packet.Get_child (Packet, "SELF/HS"),
               Signal.Leaves, Signal.Max_Leaves);
            -- Hub?
            Signal.Is_Hub := Signal.Leaves > 0 or else G2.Packet.Is_A (Packet, "/CRAWLA/SELF/HUB");
         else
            Signal.Leaves     := 0;
            Signal.Max_Leaves := 0;
         end if;

         -- Nick
         if G2.Packet.Is_A (Packet, "/CRAWLA/SELF/NAME") then
            Signal.Nick :=
               ASU.Trim (
                  U (G2.Packet.Payload (
                     G2.Packet.Get_Child (Item.Packet, "SELF/NAME"))),
                  Trim_Set, Trim_Set);
         else
            Signal.Nick := U (Hub.Get_Nick (H.all));
         end if;

         -- Version
         if G2.Packet.Is_A (Packet, "/CRAWLA/SELF/CV") then
            Signal.Version :=
               ASU.Trim (
                  U (G2.Packet.Payload (
                     G2.Packet.Get_Child (Packet, "SELF/CV"))),
                  Trim_Set, Trim_Set);
         else
            Signal.Version := U (Hub.Get_Version (H.all));
         end if;

         -- Vendor
         if G2.Packet.Is_A (Packet, "/CRAWLA/SELF/V") then
            Signal.Vendor :=
               ASU.Trim (
                  U (G2.Packet.Payload (
                     G2.Packet.Get_Child (Packet, "SELF/V"))),
                  Trim_Set, Trim_Set);
         else
            Signal.Vendor := U (Hub.Get_Vendor (H.all));
            if S (Signal.Vendor) = Hub.Vendor_UNKN then
               if Agpl.Strings.Contains (
                  Agpl.Strings.To_Upper (S (Signal.Version)), "GNUCDNA")
               then
                  Signal.Vendor := U (Hub.Vendor_GDNA);
               end if;
            end if;
         end if;

         Process_Answer:
         begin
            Hub.Automaton.Process_Signal (H.all, Signal);
         exception
            when E : others =>
               Trace.Log ("Process_Item.Process_Answer: " & Trace.Report (E),
                  Trace.Error);
         end Process_Answer;
         E2 := Cron.Elapsed;

         -- Add new hubs if it has NH children (be it PO or CRAWLA):
         Add_Neighbors:
         declare
            Nodes : G2.Packet.Object_array :=
               G2.Packet.Get_children (Item.Packet, "NH");
         begin
            for N in Nodes'Range loop
               begin
                  declare
                     Addr : constant String := G2.To_address (
                        G2.Packet.Payload (
                           G2.Packet.Get_child (Nodes (N), "NA")),
                        G2.Packet.Big_endian (Item.Packet));
                  begin
                     Add_Found (Addr);
                     Net.Counter.Add_Unique (Addr);
                  end;
               exception
                  when E : others =>
                     Trace.Log ("Process_Item.Add_neighbors: " & Trace.Report (E),
                        Trace.Error);
               end;
            end loop;
         end Add_Neighbors;
         E3 := Cron.Elapsed;

         --  Add unique leaves
         Add_Leaves:
         declare
            Nodes : G2.Packet.Object_array :=
               G2.Packet.Get_children (Item.Packet, "NL");
         begin
            for N in Nodes'Range loop
               begin
                  declare
                     Addr : constant String := G2.To_address (
                        G2.Packet.Payload (
                           G2.Packet.Get_child (Nodes (N), "NA")),
                        G2.Packet.Big_endian (Item.Packet));
                  begin
                     Net.Counter.Add_Unique (Addr);
                  end;
               exception
                  when E : others =>
                     Trace.Log ("Process_Item.Add_Leaves: " & Trace.Report (E),
                        Trace.Error);
               end;
            end loop;
         end Add_Leaves;
         E4 := Cron.Elapsed;

         if Cron.Elapsed > 2.0 then
            Trace.Log ("Adder.Process_Item abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
            Trace.Log ("E1 =" & E1'Img, Trace.Warning);
            Trace.Log ("E2 =" & E2'Img, Trace.Warning);
            Trace.Log ("E3 =" & E3'Img, Trace.Warning);
            Trace.Log ("E4 =" & E4'Img, Trace.Warning);
         end if;
      end Process_item;

      ------------------------------------------------------------------------
      -- Process_Signal                                                     --
      ------------------------------------------------------------------------
      procedure Process_Signal (
         Address  : in String;
         Sequence : in Types.Sequences;
         Signal   : in Signals.Object)
      is
         use type Types.Sequences;
         Cron : Chronos.Object;
         I : constant Iterator_Type := Find (Hubs, Address);
      begin
         if I /= Back (Hubs) then
            if Hub.Sequence (Element (I).all) = Sequence then
               Hub.Automaton.Process_Signal (Element (I).all, Signal);
            else
               Trace.Log (
                  "Process_Signal: Dropped " & Signal.Kind'Img &
                  " for " & Address & " [out of synch]",
                  Trace.Debug);
            end if;
         else
            Trace.Log (
               "Process_Signal: Dropped " & Signal.Kind'Img &
               " for " & Address & " [not tracked]",
               Trace.Debug);
         end if;

         if Cron.Elapsed > 2.0 then
            Trace.Log ("Adder.Process_Signal abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      end Process_Signal;

      ------------------------------------------------------------------------
      -- Obliterate                                                         --
      ------------------------------------------------------------------------
      procedure Obliterate is
         N   : Natural       := 0;
      begin
         Last_Purge := Clock;
         declare
            I : Iterator_Type := First (Hubs);
            J : Iterator_Type;
            Aux : Hub.Object_Access;
            procedure Free is new Unchecked_Deallocation (
               Hub.Object, Hub.Object_Access);
         begin
            while I /= Back (Hubs) loop
               J := Succ (I);
               if True then
                  Aux := Element (I);
                  Hub.Actions.Cancel (Aux.all);
                  Hub.By_Status.Sum_Key (
                     Hub.Status_Type'Image (Hub.Get_Status (Aux.all)), -1);
                  Free (Aux);
                  Delete (Hubs, I);
                  N := N + 1;
               end if;
               I := J;
            end loop;
         end;
         Trace.Log ("Obliterated " & N'Img & " hubs", Trace.Informative);
         Last_Number_Purged := N;
      exception
         when E : others =>
            Trace.Log ("Obliterate: " & Trace.Report (E), Trace.Error);
      end Obliterate;

      ------------------------------------------------------------------------
      -- Purge                                                              --
      ------------------------------------------------------------------------
      procedure Purge is
         use Ada.Calendar;
         Now : constant Time := Clock;
         N   : Natural                := 0;
         Cron: Chronos.Object;
      begin
         if Globals.Options.Walk_Purge_Active and then
            Clock - Last_Purge >= Globals.Options.Walk_Purge_Period
         then
            Last_Purge := Clock;
            declare
               I : Iterator_Type := First (Hubs);
               J : Iterator_Type;
               Aux : Hub.Object_Access;
               procedure Free is new Unchecked_Deallocation (
                  Hub.Object, Hub.Object_Access);
            begin
               while I /= Back (Hubs) loop
                  J := Succ (I);
                  if Hub.Get_Status (Element (I).all) = Hub.Dead and then
                     Hub.Get_Last_Seen (Element (I).all) +
                     Globals.Options.Walk_Purge_Age < Now
                  then
                     Aux := Element (I);
                     Hub.Actions.Cancel (Aux.all);
                     Hub.By_Status.Sum_Key (
                        Hub.Status_Type'Image (Hub.Get_Status (Aux.all)), -1);
                     Free (Aux);
                     Delete (Hubs, I);
                     N := N + 1;
                  end if;
                  I := J;
               end loop;
            end;
            Trace.Log ("Purged" & N'Img & " hubs", Trace.Informative);
            Last_Number_Purged := N;
         end if;
         if Cron.Elapsed > 3.0 then
            Trace.Log ("Adder.Purge abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      exception
         when E : others =>
            Trace.Log ("Purge: " & Trace.Report (E), Trace.Error);
      end Purge;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         if Globals.Options.Globals_Shutdown_Active and then
            Clock - Startup > Globals.Options.Globals_Shutdown_Deadline
         then
            Trace.Log ("Aenea: Controlled shutdown now!!", Trace.Always);
            Os.Kill_Me;
         end if;
      end Shutdown;

   end Adder;

   ------------------------------------------------------------------------
   -- Get_Best_Uptimes                                                   --
   ------------------------------------------------------------------------
   procedure Get_Best_Uptimes is
      Bests : Types.Uptimes_Array := Adder.Best_Uptimes;
   begin
      Trace.Log ("Getting best uptimes...", Trace.Debug);

      if Bests'Length > 0 then
         Persistent.Object.Set_Longest_Uptime_Time (Bests (Bests'First).Uptime);
         Persistent.Object.Set_Longest_Uptime_Nick (Bests (Bests'First).Nick);
      end if;

      Persistent.Object.Set_Top_Ten_Uptimes (Bests);

      Persistent.Object.Save;
   end Get_Best_Uptimes;

   ------------------------------------------------------------------------
   -- Event_Maintenance                                                  --
   ------------------------------------------------------------------------
   procedure Event_Maintenance is
      Cron_Save : Chronos.Object;
      Cron_Best : Chronos.Object;
      Cron_1m   : Chronos.Object;
      Watchdog  : Chronos.Object;
   begin
      loop
         exit when Adagio.Globals.Requested_Exit;

         if Watchdog.Elapsed > 60.0 then
            Debug.Overtimed_Task.Set (+"Event_Maintenance");
            Debug.Overtime.Set (Watchdog.Elapsed);
         end if;
         Watchdog.Reset;

         delay 1.0;
         begin
            if Chronos.Elapsed (Cron_1m) >= 60.0 then
               Chronos.Reset (Cron_1m);

               -- Preventive shutdown
               Adder.Shutdown;
               -- Purges
               Adder.Purge;

               -- Shutdown if connectivity lost (?).
               if Clock - Startup > 60.0 and then Net.Counter.Hubs_Count = 0
                 and then Globals.Options.Globals_Shutdown_OnDisconnect
               then
                  Trace.Log ("LOST CONNECTIVITY: shutting down!", Trace.Always);
                  Os.Kill_Me;
               end if;

               --  Purge unique hubs
               declare
                  Num : Natural;
               begin
                  Net.Counter.Purge_Uniques (Purged => Num);
                  Last_Number_Purged_Unique := Num;
                  Trace.Log ("Purged" & Num'Img & " unique nodes", Trace.Informative);
               end;
            end if;

            -- Save hubs
            if Globals.Options.Walk_SaveHubs then
               if Chronos.Elapsed (Cron_Save) >= Globals.Options.Walk_SavePeriod then
                  Chronos.Reset (Cron_Save);
                  Gui.Events.Trace ("[>] Save_Hubs", 2);
                  Adder.Save_hubs;
                  Gui.Events.Trace ("[<] Save_Hubs", 2);
               end if;
            end if;

            -- Get best uptimes
            if Chronos.Elapsed (Cron_Best) >= Globals.Options.Walk_BestUptimesPeriod then
               Chronos.Reset (Cron_Best);
               Get_Best_Uptimes;
            end if;
         exception
            when E : others =>
               Trace.Log ("Walker.Event_Maintenance: " & Trace.Report (E), Trace.Error);
         end;
      end loop;
   end Event_Maintenance;


   ------------------------------------------------------------------------
   -- Receive_Signal                                                     --
   ------------------------------------------------------------------------
   -- For timed events (signals)
   procedure Receive_Signal (Context : in Agpl.Event_queues.Context_access) is
      C   : Net.Signal_Context := Net.Signal_Context (Context.all);
   begin
      Adder.Process_Signal (S (C.Address), C.Sequence, C.Signal);
   exception
      when E : others =>
         Trace.Log ("Receive_Signal: " & Trace.Report (E), Trace.Error);
   end Receive_Signal;

   ------------
   -- Report --
   ------------
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
   begin
      Adder.Report (Data);
   end Report;

   ------------
   -- Worker --
   ------------
   task body Worker is
   begin
      Trace.Log ("Worker " & Name.all & " activated and running.", Trace.Informative);
      Work.all;
      Trace.Log ("Worker " & Name.all & " finalized.", Trace.Informative);
   exception
      when E : others =>
         Trace.Log ("Worker " & Name.all & ": " & Trace.Report (E), Trace.Error);
   end Worker;

end Aenea.Walker;
