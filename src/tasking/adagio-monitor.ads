-- Mutex with counter. A task may safely request it multiple times,
--  as long as it releases it the same times

with Ada.Finalization;        use Ada;
with Ada.Task_identification;
with System;

package Adagio.Monitor is

   pragma Elaborate_Body;

   use Ada.Task_identification;

   Use_error : Exception;

   protected type Semaphore (Priority: System.Priority:= System.Priority'Last)
   is
      pragma Priority (Priority);

      entry P;
      entry V;

   private

      entry Safe_P;

      Caller : Task_id := Null_task_id;           -- Requester
      In_use : Natural := 0;                      -- Times requested

   end Semaphore;

   type Semaphore_access is access all Semaphore;

   -- Use:
   -- S : aliased Semaphore;
   -- declare
   --   M : Object(S'access);
   -- begin
   --   Exclusive_work;
   -- end;
   type Object (S : access Semaphore) is new
      Finalization.Limited_Controlled with null record;

   procedure Initialize (this : in out Object);
   procedure Finalize   (this : in out Object);

end Adagio.Monitor;
