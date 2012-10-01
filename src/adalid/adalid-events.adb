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
--  $Id: adalid-events.adb,v 1.1 2004/03/22 07:14:53 Jano Exp $

--  All processing will be done through this intermediary event system, 
--    allowing for the CPU throttling to be centralized here.

with Adagio.Globals;

with Ada.Real_time;

package body Adalid.Events is

   ------------------------------------------------------------------------
   -- Internal types                                                     --
   ------------------------------------------------------------------------
   The_events : Event_queue.Object;

   ------------------------------------------------------------------------
   -- Action                                                             --
   ------------------------------------------------------------------------
   -- Action procedure with throttling
   procedure Action (Context : in Event_queue.Context_access) is
   begin
      Adagio.Globals.Main_throttle.Start_work;

      -- Dispatch to true action:
      Context_type (Context.all).Action (
         Context_type'Class (Context.all)'access);

      Adagio.Globals.Main_throttle.End_work;
   end Action;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      Deadline : in Calendar.Time;
      Action   : in Action_procedure;
      Context  : in Context_type'Class)
   is
      use Ada.Real_time;
      use type Calendar.Time;
      Event : Event_queue.Event_type;
      Ctx   : Context_type'Class := Context;
   begin
      Ctx.Action := Action;

      Event_queue.Create (
         The_events,
         Event,
         Real_time.Clock + To_time_span (Deadline - Calendar.Clock),
         Adalid.Events.Action'access,
         Ctx);
   end Create;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Event_queue.Shutdown (The_events);
   end Shutdown;

end Adalid.Events;
