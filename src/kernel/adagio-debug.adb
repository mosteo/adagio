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
--  $Id: adagio-debug.adb,v 1.7 2004/01/21 21:05:27 Jano Exp $

with Adagio.Globals.Options;
with Adagio.Statistics;
with Adagio.Statistics.Booleans;
with Adagio.Statistics.Integers;

package body Adagio.Debug is

   TSE : Boolean renames Globals.Options.Debug_DebugStatistics;
   
   ----------------------------------------------------------------------
   -- Debug_statistics_enabled                                         --
   ----------------------------------------------------------------------
   function Debug_statistics_enabled return Boolean is
   begin
      return TSE;
   end Debug_statistics_enabled;

   ------------------------------------------------------------------------
   -- Reset_alive_tasks                                                  --
   ------------------------------------------------------------------------
   -- Reset statistics for alive tasks:
   procedure Reset_alive_tasks is
      use Statistics.Booleans;
   begin
      Statistics.Object.Set ("Tasking - Heartbeat", Create (false));
      Statistics.Object.Set ("Tasking - Main", Create (false));
      Statistics.Object.Set ("Tasking - Uploads manager", Create (false));
      Statistics.Object.Set ("Tasking - Server poll", Create (false));
   end Reset_alive_tasks;

   ------------------------------------------------------------------------
   -- Control                                                            --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Tracker_type) is
   begin
      Statistics.Object.Update (
         This.Id.all, 
         Statistics.Integers.Increment'Access,
         Statistics.Integers.Create (1));
   exception
      when Statistics.Value_not_defined =>
         Statistics.Object.Set (This.Id.all, Statistics.Integers.Create (1));
   end Initialize;
   procedure Finalize   (This : in out Tracker_type) is
   begin 
      Statistics.Object.Update (
         This.Id.all, 
         Statistics.Integers.Increment'Access,
         Statistics.Integers.Create (-1));
   exception
      when Statistics.Value_not_defined =>
         Statistics.Object.Set (This.Id.all, Statistics.Integers.Create (0));
   end Finalize;
   
end Adagio.Debug;
