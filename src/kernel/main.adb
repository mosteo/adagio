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
--  $Id: main.adb,v 1.10 2004/01/21 21:05:27 Jano Exp $

with Ada.Exceptions;

with Adagio.Chronos;
with Adagio.Constants;
with Adagio.Debug;
with Adagio.Everything;
with Adagio.Globals;
with Adagio.Startup;
with Adagio.Statistics;
with Adagio.Statistics.Booleans;
with Adagio.OS;
pragma Warnings (Off);
with Adagio.OS.Interrupts;
pragma Warnings (On);
with Adagio.OS.Memory;
with Adagio.Trace;

use Adagio;

--  Force compilation of needed AWS link files:
pragma Warnings (Off);
with Ssl.Thin;
with Ssl.Wrappers;
pragma Warnings (On);

procedure main is


   task Main_task is
      pragma Storage_size (Os.Memory.Max_Stack_Size);
   end Main_task;

   task body Main_task is
   begin
      declare
         C : Chronos.Object;
      begin
         Trace.Log (Constants.User_agent & " is starting...", Trace.Informative);
         Startup.Init;
         Trace.Log (Constants.User_agent & " started in " & Chronos.Image (C),
            Trace.Informative);
      end;

      while not Adagio.Globals.Requested_exit loop
         delay 1.0;
         if Debug.Debug_statistics_enabled then
            Statistics.Object.Set ("Tasking - Main",
               Statistics.Booleans.Create (True));
         end if;
      end loop;

      select
         delay 10.0;
         Adagio.Trace.Log("Adagio exited (forced)", Trace.Informative);
         Adagio.Os.Kill_Me;
      then abort
         Adagio.Startup.Shutdown;
         Adagio.Trace.Log("Adagio exited normally", Trace.Informative);
      end select;

   exception
      when e: others =>
         Adagio.OS.Message_box
           ("Adagio",
            "Error: " & Ada.Exceptions.Exception_name(e) & ": " &
                        Ada.Exceptions.Exception_message(e));
         --  Force shutdown whenever possible:
         Adagio.Trace.Log("Adagio aborting...", Trace.Always);
         --  Give some time to dump logs:
         delay 2.0;
         Adagio.Trace.Log("Adagio aborted", Trace.Informative);
         Adagio.OS.Kill_me;
   end Main_task;

begin
   null;
end main;
