------------------------------------------------------------------------------
--                                  ADALID                                  --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adalid@mosteo.com)                                 --
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
--  As a special exception, if other files instantiate generics from a      --
--  unit whose name doesn't begin with the project name (adalid), or you    --
--  link that unit with other files to produce an executable, that unit     --
--  does not by itself cause the resulting executable to be covered by      --
--  the GNU General Public License. This exception does not however         --
--  invalidate any other reasons why the executable file might be           --
--  covered by the GNU Public License.                                      --
--                                                                          --
--  You are not allowed to use any part of Adalid's code to develop a tool  --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adalid connects with. All data collected with Adalid or a tool --
--  containing Adalid code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users. 
------------------------------------------------------------------------------
--  $Id: main.adb,v 1.2 2003/12/17 18:25:05 Jano Exp $

with Adalid.Chronos;
with Adalid.Globals;
with Adalid.Os;
with Adalid.Trace;
use  Adalid;

with Ada.Exceptions;

procedure Main is

   task Main_task is
      pragma Storage_size (1024 * 1024);
   end Main_task;

   task body Main_task is
   begin
      declare
         C : Chronos.Object;
      begin
         Trace.Log (Adalid.User_agent & " is starting...", Trace.Informative);
--         Adalid.Startup.Init;
         Trace.Log (Adalid.User_agent & " started in " & Chronos.Image (C),
            Trace.Informative);
      end;

      while not Adalid.Globals.Requested_exit loop
         delay 1.0;
      end loop;

--      Adalid.Startup.Shutdown;
      Trace.Log(Adalid.User_agent & " exited", Trace.Informative);

   exception
      when e: others =>
         OS.Message_box
           ("Adalid",
            "Error: " & Ada.Exceptions.Exception_name(e) & ": " &
                        Ada.Exceptions.Exception_message(e));
         -- Force shutdown whenever possible:
         Trace.Log(Adalid.User_agent & " aborting...", Trace.Always);
         -- Give some time to dump logs:
         delay 2.0;
         Trace.Log(Adalid.User_agent & " aborted", Trace.Informative);
         OS.Kill_me;
   end Main_task;

begin
   null;
end Main;
