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
--  $Id: adagio-heartbeat.adb,v 1.4 2004/01/21 21:05:27 Jano Exp $

With
Adagio.Debug,
Adagio.Globals.Options,
Adagio.Statistics,
Adagio.Statistics.Booleans,
Adagio.Statistics.Durations,
Adagio.Trace,
Ada.Calendar;

use
Ada.Calendar;

package body Adagio.HeartBeat is

   task body Object is
      Startup : Time     := Clock;
      Period  : Duration renames Globals.Options.debug_HeartBeat;
   begin
      while not Globals.Requested_exit loop
         if Debug.Debug_statistics_enabled then
            Statistics.Object.Set (
               "Misc - Uptime",
               Statistics.Durations.Create (Clock - Startup));
         end if;
         delay until Clock + Period;
         if Debug.Debug_statistics_enabled then
            Statistics.Object.Set (
               "Tasking - Heartbeat", Statistics.Booleans.Create (true));
         end if;
      end loop;
      Trace.Log ("HeartBeat: Exited.");
   end Object;

end Adagio.HeartBeat;
