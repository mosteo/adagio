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
--  $Id: adagio-os-memory_stats.adb,v 1.9 2004/03/29 19:13:33 Jano Exp $

-- Package with OS dependent functions.

with Adagio.Convert;
with Adagio.Globals;
with Adagio.Misc;
with Adagio.Os.Memory;
with Adagio.Statistics;
with Adagio.Statistics.Strings;
with Adagio.Trace;

package body Adagio.Os.Memory_stats is

   Stat_memory : constant String := "Memory - Heap usage";
   Stat_delta  : constant String := "Memory - Heap delta";
   Stat_high   : constant String := "Memory - High watermark";

   Cached_usage : Natural := 0;
   pragma Atomic (Cached_usage);

   ------------------------------------------------------------------------
   -- Cached_heap_usage                                                  --
   ------------------------------------------------------------------------
   -- Gets the value of heap usage, updated with the below task period
   function Cached_heap_usage return Natural is
   begin
      return Cached_usage;
   end Cached_heap_usage;

   ------------------------------------------------------------------------
   -- Memory_task                                                        --
   ------------------------------------------------------------------------
   task body Memory_task is
      P       : Duration;
      The_end : Boolean := false;
      Last    : Natural := 0;
      Max     : Integer := -1;
   begin
      select 
         accept Start (Period : in Duration := 60.0) do
            P := Period;
         end Start;
      or
         terminate;
      end select;
      Trace.Log ("Memory_task started with period " & 
         Misc.To_string (Float (P), 3));

      loop
         declare
            Size : Natural;
         begin
            Size         := Memory.Heap_usage;
            Cached_usage := Size;
            Trace.Log ("Memory_task: Usage is " & 
               Misc.To_string (Size) & " (" & Convert.To_size (Size) & ")", 
               Trace.Never);
            Trace.Log ("Memory_task: Delta is " & 
               Misc.To_string (Size - Last) & " (" & Convert.To_size (
               Size - Last) & ")", 
               Trace.Never);

            Statistics.Object.Set (
               Stat_memory,
               Statistics.Strings.Create (Convert.To_size (Size, 2)));
            Statistics.Object.Set (
               Stat_delta,
               Statistics.Strings.Create (Convert.To_size (Size - Last, 0)));
            if Size > Max then
               Max := Size;
               Statistics.Object.Set (
                  Stat_high,
                  Statistics.Strings.Create (Convert.To_size (Max, 2)));
            end if;

            Last := Size;

            select 
               accept Shutdown do
                  The_end := true;
               end Shutdown;
            or
               delay P;
            end select;

            exit when The_end;
         exception
            when E : others =>
               Trace.Log ("Memory_task: " & Trace.Report (E), Trace.Error);
         end;
      end loop;
      Trace.Log ("Memory_task exited.");
   end Memory_task;

end Adagio.Os.Memory_stats;
