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
--  $Id: aenea_main.adb,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Aenea;
use  Aenea;
with Aenea.Chronos;
with Aenea.Globals;
with Aenea.Globals.Options; -- Force for initialization.
with Aenea.Interrupts;      -- Force for handlers.
with Aenea.OS;
with Aenea.Startup;
with Aenea.Trace;
with Helpers;

with Ada.Exceptions;
with Text_IO;

procedure Aenea_Main is

   task Main_task is
      pragma Storage_size (1024 * 1024);
   end Main_task;

   task body Main_task is
   begin
      declare
         C : Chronos.Object;
      begin
         Trace.Log (Version & " PID is" & Integer'Image (Helpers.Get_Pid), Trace.Always);
         Text_Io.Put_Line (Version & " PID is" & Integer'Image (Helpers.Get_Pid));
         Trace.Log (Version & " is starting...", Trace.Informative);
         Aenea.Startup.Init;
         Trace.Log (Version & " started in " & Chronos.Image (C),
            Trace.Informative);
      end;

      while not Aenea.Globals.Requested_exit loop
         delay 1.0;
      end loop;

      select
         delay 5.0;
         Aenea.Trace.Log("Aenea exited (forced)", Trace.Informative);
         Aenea.Os.Kill_Me;
      then abort
         Aenea.Startup.Shutdown;
         Aenea.Trace.Log("Aenea exited normally", Trace.Informative);
      end select;

   exception
      when e: others =>
         OS.Message_box
           (Version,
            "Error: " & Ada.Exceptions.Exception_name(e) & ": " &
                        Ada.Exceptions.Exception_message(e));
         --  Force shutdown whenever possible:
         Trace.Log(Version & " aborting...", Trace.Always);
         --  Give some time to dump logs:
         delay 2.0;
         Trace.Log(Version & " aborted", Trace.Informative);
         OS.Kill_me;
   end Main_task;

begin
   Trace.Log ("Elaboration finished.", Trace.Informative);
end Aenea_Main;
