-- Protected type to constraint to a desired CPU usage

package body Adagio.Throttler is

   protected body Object is
      -- Set a new target usage:
      procedure Set_target_usage(Usage: Percent:= 100) is
      begin
         Wanted_usage:= Usage;
      end Set_target_usage;
      -- Signal work start.
      -- Will be delayed as necessary to achieve expected use:
      procedure Start_work is
      begin
         if Clock < Next_run then
            -- We are too fast!
            delay until Next_run;
            Start:= Next_run;
         else
            -- Too slow...
            Start:= Clock;
         end if;
      end Start_work;
      -- Signal cycle work:
      procedure Cycle_work is
      begin
         End_work;
         Start_work;
      end Cycle_work;
      -- Signal work end:
      procedure End_work is
         Elapsed: Duration := Clock - Start;
         Lap    : Duration;
      begin
         -- Ensure some delay but not too much:
         if Elapsed = 0.0 then
            Elapsed:= Duration'Small;
         end if;
         Lap := Duration (10000.0 / float (Wanted_usage) * 
                float (Elapsed) / 100.0);
         -- Maximum delay limit
         if Lap > Elapsed and then Lap - Elapsed > Max_delay then
            Lap := Elapsed + Max_delay;
         end if;
         Next_run:= Start + Lap;
      end End_work;
   end Object;

   procedure Initialize(this: in out Controller) is
   begin
      this.Throttle.Start_work;
   end Initialize;

   procedure Finalize(this: in out Controller) is
   begin
      this.Throttle.End_work;
   end Finalize;

end Adagio.Throttler;
