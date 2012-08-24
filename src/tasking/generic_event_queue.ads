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
--  $Id: generic_event_queue.ads,v 1.3 2004/01/21 21:05:49 Jano Exp $

-- Efficient event queue. Useful for timeouts, for example.
-- Implemented via generics. Only a kind of actions can be applied to this 
--   queue.
-- However, the Action_on_timeout can be as complicated as we could need.
-- Better still, the context_type can have constraints (i.e. to be a variant
--   record).

-- DON'T USE IT ANY MORE. USE INSTEAD AGPL.EVENT_QUEUES.*

with Adagio;
with Protected_sorted_index;
with Sequence;

with Ada.Real_time;
use  Ada.Real_time;

generic
   type Context_type (<>) is private;
   with procedure Action_on_timeout (Context : in Context_type);
   Stack_size : Natural := 64 * 1024;
package Generic_event_queue is

   -- Handle for an event. Can be used to cancel it:
   type Event_type is private;

   type Object is limited private;
   type Object_access is access all Object;

   -- Create an event
   procedure Create (
      This     : in out Object; 
      Event    : out    Event_type;
      Deadline : in     Time;
      Context  : in     Context_type);

   procedure Cancel (
      This     : in out Object;
      Event    : in out Event_type);

   -- Pending events?
   function Is_empty (This : in Object) return Boolean;

   procedure Shutdown (This : in out Object);

private

   -- Uses timestamp
   function Less  (L, R : in Event_type) return Boolean;
   -- Uses Id.
   function Equal (L, R : in Event_type) return Boolean;
   pragma Inline (Less, Equal);

   -- Maximum simultaneous pending events:
   type Id_type is mod 2 ** 32;

   package Id_sequence is new Sequence (Id_type);

   type Context_access is access all Context_type;
--   for Context_access'Storage_pool use Adagio.Debug_pool;

   type Event_type is record
      Deadline : Time;
      Id       : Id_type;
      Context  : Context_access;
   end record;

   package Event_list is new
      Protected_sorted_index (Event_type, Less, Equal);

   type Action_type is (New_event, Job_finished);

   task type Active_object (Parent : access Object) is 
      entry Reschedule (Action : in Action_type);
      entry Shutdown;
   end Active_object;

   task type Worker (Parent : access Object) is
      pragma Storage_size (Stack_size);
      entry Execute (Context : in Context_access);
   end Worker;

   type Object is record
      List   : Event_list.Sorted_index;
      Seq    : Id_sequence.Object;
      Waiter : Active_object (Object'Access);
      Doer   : Worker (Object'Access);
   end record;

end Generic_event_queue;
