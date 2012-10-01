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
--  $Id: generic_event_queue.adb,v 1.4 2004/01/21 21:05:48 Jano Exp $

-- Efficient event queue. Useful for timeouts, as an example.

with Ada.Unchecked_deallocation; use Ada;

with Adagio;
with Adagio.Trace; use Adagio;

package body Generic_event_queue is

   procedure Free is new
      Unchecked_deallocation (Context_type, Context_access);

   -- Priority
   function Less (L, R : in Event_type) return Boolean is
   begin
      return L.Deadline < R.Deadline or else
         (L.Deadline = R.Deadline and L.Id < R.Id);
   end Less;

   -- Equal (by id)
   function Equal (L, R : in Event_type) return Boolean is
   begin
      return L.Id = R.Id;
   end Equal;

   -- Create an avent
   procedure Create (
      This     : in out Object;
      Event    : out    Event_type;
      Deadline : in     Time;
      Context  : in     Context_type) is
   begin
      This.Seq.Get_next (Event.Id);
      Event.Deadline  := Deadline;
      Event.Context   := new Context_type'(Context);
      This.List.Insert (Event);
      This.Waiter.Reschedule (New_event);
   end Create;

   procedure Cancel (
      This     : in out Object;
      Event    : in out Event_type) is
      Found    : Boolean;
   begin
      This.List.Get_remove (Event, Found);
      if Found then
         Free (Event.Context);
      end if;
   end Cancel;

   -- Pending events?
   function Is_empty (This : in Object) return Boolean is
   begin
      return This.List.Is_empty;
   end Is_empty;

   procedure Shutdown (This : in out Object) is
   begin
      This.Waiter.Shutdown;
   end Shutdown;

   task body Active_object is
      Next         : Event_type;
      Deadline     : Time;
      Found        : Boolean;
      Worker_ready : Boolean := true;
   begin
      loop
         -- Deadline triggered or rescheduling (new event)
         Parent.List.Get_first_remove (Next, Found);
         if not Found then
            Deadline := Clock + To_time_span (60.0);
         else
            if Next.Deadline <= Clock then
               -- Run it if possible
               if Worker_ready then
                  Parent.Doer.Execute (Next.Context);
                  Worker_ready := false;
                  Deadline := Clock + To_time_span (60.0);
               else
                  -- Busy. Delay until him signals us:
                  Deadline := Clock + To_time_span (60.0);
                  Parent.List.Insert (Next);
               end if;
            else
               -- Reinsert it
               Deadline := Next.Deadline;
               Parent.List.Insert (Next);
            end if;
         end if;
         -- Wait for news
         select
            accept Reschedule (Action : in Action_type) do
               if Action = Job_finished then
                  Worker_ready := true;
               end if;
            end Reschedule;
         or
            accept Shutdown;
            exit;
         or
            delay until Deadline;
         end select;
      end loop;
   end Active_object;

   task body Worker is
      Context : Context_access;
   begin
      loop
         select
            accept Execute (Context : in Context_access) do
               Worker.Context := Context;
            end Execute;
            begin
               Action_on_timeout (Context.all);
            exception
               when E : others =>
                  Trace.Log ("Generic_event_queue: " & Trace.Report (E),
                     Trace.Error);
            end;
            Free (Context);
            Parent.Waiter.Reschedule (Job_finished);
         or
            terminate;
         end select;
      end loop;
   end Worker;


end Generic_event_queue;
