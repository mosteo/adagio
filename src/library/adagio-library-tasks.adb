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
--  $Id: adagio-library-tasks.adb,v 1.8 2004/01/21 21:05:28 Jano Exp $

with
Adagio.File,
Adagio.Globals.Options,
Adagio.Hash,
Adagio.Misc,
Adagio.Statistics,
Adagio.Statistics.Strings,
Adagio.Trace,
Sha1,
TigerTree,
Acf.Hash.Message_Digests,
Acf.Types,
Ada.Real_time;

use
Ada.Real_time;

package body Adagio.Library.Tasks is

   Stat_current_hash : constant String := "Library - Currently hashing";

   task body Folder_maintenance is
      P: Real_time.Time_span;
      Next_start    : Real_time.Time:= Real_time.Clock;
      Last_save     : Real_time.Time := Real_time.Clock;
      Save_interval : Time_span := To_time_span (
         Globals.Options.Library_AutosaveInterval);
   begin
      -- Delayed start
      select
         accept Start(Period: Duration) do
            P:= Real_time.To_time_span(Period);
         end Start;
      or
         terminate;
      end select;
      -- Check a folder each time
      Globals.Hash_throttle.Start_work;
      Main: while not Globals.Requested_exit loop
         begin
            delay until Next_start;
            Library.Object.Process_pending_folder;
            Next_start:= Real_time.Clock + P;
            Globals.Hash_throttle.Cycle_work;
            if Clock - Last_save > Save_interval then
               Last_save := Clock;
               Library.Object.Save;
            end if;
         exception
            when E: others =>
               Trace.Log
                 ("Folder maintenance [main loop]: " & Trace.Report(E),
                  Trace.Error);
         end;
      end loop Main;
      Trace.Log("Folder maintenance exited");
   end Folder_maintenance;

   task body File_maintenance is
      P          : Real_time.Time_span;
      Next_start : Real_time.Time:= Real_time.Clock;
      Hash_speed : Hash.Hash_speeds := Hash.Hash_speeds'Value (S (
         Globals.Options.Library_HashSpeed));
   begin
      -- Delayed start
      select
         accept Start(Period: Duration) do
            P:= Real_time.To_time_span(Period);
         end Start;
      or
         terminate;
      end select;
      -- Get a file, hash it
      Main: while not Globals.Requested_exit loop
      begin
         Hashing: loop
            declare
               use type File.Object;
               F     : File.Object;
               Start : Time:= Clock;
            begin
               Library.Object.Get_pending_file (F);
               if F = File.Null_file then
                  Statistics.Object.Set (
                     Stat_current_hash,
                     Statistics.Strings.Create ("None"));
               end if;
               exit Hashing when
                  F = File.Null_file or else
                  Globals.Requested_exit;
               -- Hash it!
               Trace.Log("Starting hashing of: " & File.Path (F),
                  Trace.Informative);
               Statistics.Object.Set (
                  Stat_current_hash,
                  Statistics.Strings.Create (File.Name (F)));
               File.Compute_hashes (F, Hash_speed);

               -- Add to library as finished:
               File.Refresh            (F);
               Library.Object.Add_file (F);

               Trace.Log ("Last hash: Time: " &
                  Misc.Image (To_duration (Clock - Start)) & "; Speed: " &
                  Misc.To_string (Float (Natural'(File.Size (F))) /
                     Float (To_duration (Clock - Start)) / 1024.0) &
                     " kB/s", Trace.Informative);
               Trace.Log ("bitprint: " &
                  Sha1.To_base32 (File.Sha (F)) & "." &
                  TigerTree.To_base32 (File.TTH (F)), Trace.Informative);
               Trace.Log ("ed2k: " &
                  Acf.Hash.Message_digests.To_hex_string (
                     File.Ed2k (F),
                     Acf.Types.Lower_case), Trace.Informative);
            exception
               when E: others =>
                  Trace.Log("File maintenance [hashing " & File.Path (F) &
                     "]: " & Trace.Report(E));
            end;
         end loop Hashing;

         delay until Next_start;
         Next_start:= Next_start + P;
      exception
         when E: others =>
            Trace.Log ("File maintenance [main loop]: " & Trace.Report(E),
               Trace.Error);
      end;
      end loop Main;
      Trace.Log("File maintenance exited");
   end File_maintenance;

   task Status;

   task body Status is
   begin
      loop
         for N in 1 .. 25 loop
            exit when Globals.Requested_exit;
            delay 1.0;
         end loop;
         exit when Globals.Requested_exit;
         Trace.Log ("***** Library status report *****");
         Trace.Log ("***** Files:" & Object.Num_files'Img);
         Trace.Log ("***** Size: " & Object.Size_files'Img & " kB");
         Trace.Log ("***** QRP:   " &
            Misc.To_string (Object.Get_QRP_Ratio * 100.0, 5) & "%");
         Trace.Log ("*********************************");
      end loop;
   end Status;

   task body Folder_refresh is
   begin
      loop
         for N in 1 .. 5 * 60 loop
            exit when Globals.Requested_exit;
            delay 1.0;
         end loop;
         exit when Globals.Requested_exit;
         Library.Object.Refresh_folders;
      end loop;
   end Folder_refresh;

end Adagio.Library.Tasks;
