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
--  $Id: adagio-workers.adb,v 1.6 2004/01/21 21:05:48 Jano Exp $

--  Workers: tasks which carry a work that may be aborted by external means.
--  Intended for use with blocking calls.
--  Worker pools allow to have families of related tasks.
--  These tasks wait for a new work to be inserted.

--  We choose to make this package generic so all relevant tasks/protected
--  object can be hidden.
--  Unfortunately, this also means that no multi-purpose tasks can be used.

with Adagio.Globals;
with Adagio.Trace;

with System;

with Ada.Real_time;              use Ada.Real_time;
with Ada.Unchecked_deallocation; use Ada;

package body Adagio.Workers is

   use Timeout_queue;

   Timeouts : Timeout_queue.Object;

   Work_queue : Queue.Handle (Max_pending_works, System.Priority'Last);

   procedure Free is new 
      Unchecked_deallocation (Context_type, Context_access);

   -- We'll used this protected object to ensure that we never get blocked
   -- in the queue of works:
   protected Entrance is
      procedure Put (Work : Work_type; Success : out Boolean);
   end Entrance;

   protected body Entrance is

      procedure Put (Work : Work_type; Success : out Boolean) is
      begin
         if Work_queue.Full then
            Success := false;
         else
            Success := true;
            Work_queue.Put (Work);
         end if;
      end Put;

   end Entrance;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Will enqueue a task with given deadline. 
   -- A standard ATC will be used.
   -- Aditionally, on timeout the Abort_procedure will be called.
   -- This is useful to abort blocking calls to sockets, for example.
   procedure Start (
      Data     : in  Context_type;
      Deadline : in  Duration;
      Started  : out Boolean) is

      Work : Work_type;
   begin
      Work.Context  := new Context_type'(Data);
      Work.Deadline := Deadline;

      -- Create a timeout
      Create (
         Timeouts, 
         Work.Timeout, 
         Clock + To_time_span (Deadline),
         Data);

      Entrance.Put (Work, Started);
      if not Started then
         -- Cancel everything: 
         Free (Work.Context);
         Cancel (Timeouts, Work.Timeout);
      end if;

   end Start;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Timeout_queue.Shutdown (Timeouts);
   end Shutdown;

   -----------------------------------------------------------------------
   -- Workers: these tiny, hard labourers --------------------------------
   -----------------------------------------------------------------------

   -- They are always waiting to get a job. 
   task body Worker is
      Work    : Work_type;

      -------------
      -- Do_work --
      -------------
      procedure Do_work is
      begin
         -- Concurrently is running an abort timeout, so we can't be sure
         -- what thing will occur first.
         select
            delay Work.Deadline;
            Trace.Log ("Workers: Job aborted because of deadline failed.");
         then abort -- This won't work on NT because of the lack of polling
            Work_procedure (Work.Context.all);
            Cancel (Timeouts, Work.Timeout);
         end select;
      end Do_work;

   begin
      loop
         begin
            exit when Globals.Requested_exit;
            select
               Work_queue.Get (Work);
               Do_work;
               Free (Work.Context);
            or 
               delay 5.0;
            end select;
         exception
            when E : Others =>
               Trace.Log ("Workers.Worker [main loop]: " & Trace.Report (E),
                  Trace.Error);
               Free (Work.Context);
         end;
      end loop;
   end Worker;

   -- They're runnging!
   Workers : Worker_pool (1 .. Num_workers);
   pragma Unreferenced (Workers);

end Adagio.Workers;
