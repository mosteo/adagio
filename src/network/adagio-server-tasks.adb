With
Adagio.Chronos,
Adagio.Globals.Options,
Adagio.Trace,
Ada.Calendar;

Use
Ada.Calendar;

package body Adagio.Server.Tasks is

   -- Will purge the library every X seconds
   -- And save every 30 min.
   task body Maintenance is
      Startup        : Time := Clock;
      Exit_requested : Boolean:= false;
      Purge_P        : Duration
         renames Globals.Options.Hostcache_purge_period;
      Save_P         : Duration renames Globals.Options.Hostcache_save_period;
      Purge_B        : Boolean renames Globals.Options.Hostcache_purge;
      Save_B         : Boolean renames Globals.Options.Hostcache_save;
      Purge_cron,
      Save_cron      : Chronos.Object;

	Pragma Unreferenced( Startup );
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      loop
         begin
            -- PURGE
            if Purge_B and then Chronos.Elapsed (Purge_cron) > Purge_P then
               Chronos.Reset (Purge_cron);
               Server.List.Purge;
               -- Ad hoc, purge networks:
               Server.List.Purge("Gnutella2",
                  Globals.Options.G2_CachedServers);
               Server.List.Purge("GWebCache2",
                  Globals.Options.GWC2_CachedServers);
            end if;
            -- SAVE
            if Save_B and then Chronos.Elapsed (Save_cron) > Save_P then
               Chronos.Reset (Save_cron);
               Server.List.Save;
            end if;
            select
               accept Shutdown do
                  Exit_requested:= true;
               end Shutdown;
            or
               delay 5.0;
            end select;
            exit when Exit_requested;
         exception
            when E: others =>
               Trace.Log("Server.Tasks.Maintenance: " & Trace.Report(E),
                  Trace.Error);
         end;
      end loop;

      Trace.Log("Server.Tasks.Maintenance exited");

   end Maintenance;

end Adagio.Server.Tasks;
