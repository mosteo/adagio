-- Protected type to constraint to a desired CPU usage
with Ada.Finalization;
with Ada.Calendar; use Ada.Calendar;
with System;

use Ada;

package Adagio.Throttler is 

   subtype Percent is Natural range 0..100;

   -- Maximum delay allowed in any case.
   Max_delay : Duration := 1.0;

   -- Will raise constraint error if Target usage is 0
   protected type Object(Target_usage: Percent:= 100) is
      pragma Priority (System.Priority'Last);

      -- Set a new target usage:
      procedure Set_target_usage(Usage: Percent:= 100);
      -- Signal work start. 
      -- Will be delayed as necessary to achieve expected use:
      procedure Start_work;
      -- Signal cycle completed:
      procedure Cycle_work;
      -- Signal work end:
      procedure End_work;
   private
      Wanted_usage:  Percent  := Target_usage;
      Start:         Time     :=  Clock;
      Next_run:      Time     :=  Clock;
   end Object;

   -- Auxiliary type to help in controlling throttle.
   type Controller (Throttle: access Object) is tagged limited private;

private

   type Controller (Throttle: access Object) is new 
      Finalization.Limited_controlled with null record;

   procedure Initialize(This: in out Controller);
   procedure Finalize(This: in out Controller);
   
end Adagio.Throttler;
