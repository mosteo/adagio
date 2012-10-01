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
--  $Id: aenea-hub-schedule.adb,v 1.8 2004/03/14 21:09:48 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Net;
with Aenea.Walker;

with Ada.Real_time; use Ada.Real_time;

package body Aenea.Hub.Schedule is

   Span : Time_span := To_time_span (Globals.Options.Walk_RefreshUnit);

   ------------------------------------------------------------------------
   -- Create_timeout                                                     --
   ------------------------------------------------------------------------
   procedure Create_timeout (
      This     : in Hub.Object; 
      Deadline : in Duration := Globals.Options.Walk_Timeout)
   is
      E : Event_queue.Event_type;
      C : Hub_context;
      use Ada.Real_time;
   begin
      C.Hub_id := U (Id (This));
      Event_queue.Create (
         Net.Events, 
         E, 
         Clock + To_time_span (Deadline),
         Walker.Check_timeout'Access,
         C);
   end Create_timeout;

   ------------------------------------------------------------------------
   -- Reschedule                                                         --
   ------------------------------------------------------------------------
   procedure Reschedule (This : in out Hub.Object) is
      C : Hub_context;
      E : Event_queue.Event_type;
   begin
      if This.Scheduled then
         return;
      else
         This.Scheduled := true;
      end if;

      if This.Pings >= Globals.Options.Walk_Pings then
         This.Pings := 0;
         C.Msg_type := Query;
      else
         C.Msg_type := Ping;
      end if;

      C.Hub_id := U (Id (This));

      -- New events:
      Event_queue.Create (
         Net.Events, 
         E, 
         Clock + Span + 
            To_time_span (
               Duration (Event_queue.Length (Net.Events)) * 
               Globals.Options.Walk_delay),
         Walker.Check_hub'Access,
         C);
   end Reschedule;

end Aenea.Hub.Schedule;
