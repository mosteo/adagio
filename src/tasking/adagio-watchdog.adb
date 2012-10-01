-- We'll use this task in the unexpected case that,
-- after some preconfigured time of shutdown requesting,
-- some task be still active.
with Ada.Real_time;

with Adagio.Globals.Options;
with Adagio.Library.Tasks;
with Adagio.Os;
with Adagio.Server.Tasks;
with Adagio.Trace;

with System;
with Text_io; use Text_io;

use Ada.Real_time;

package body Adagio.Watchdog is

   task body Object is
      Remaining: Natural;

      function Alive_tasks return boolean is
      begin
         return 
            Library.Tasks.File_maintenance'Callable or else
            Library.Tasks.Folder_maintenance'Callable or else
            Server.Tasks.Maintenance'Callable;
      end Alive_tasks;

      procedure Kill_tasks is
      begin
         begin
            abort Library.Tasks.File_maintenance;
         exception
            when others =>
               null;
         end;
         begin
            abort Library.Tasks.Folder_maintenance;
         exception
            when others =>
               null;
         end;
         begin
            abort Server.Tasks.Maintenance;
         exception
            when others =>
               null;
         end;
      end Kill_tasks;
   begin
      Remaining := Natural (Globals.Options.globals_watchdog_deadline);
      -- Loop until exit solicited
      while not Globals.Requested_exit loop
         delay 1.0;
      end loop;
      -- Start countdown!
      loop
         exit when Remaining = 0 or not Alive_tasks;
         if Remaining <= 10 then
            Trace.Log("Watchdog countdown:" & Natural'image(Remaining), 
                      Trace.Warning);
         end if;
         Delay 1.0;
         Remaining:= Remaining - 1;
      end loop;

      -- Kill! Kill!
      if Alive_tasks then
         Kill_tasks;
         Trace.Log("Watchdog: All tasks killed.", Trace.Warning);
         -- Brutal finish:
         Os.Kill_me;
      end if;

   end;

end Adagio.Watchdog;
