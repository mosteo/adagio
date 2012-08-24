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
--  $Id: aenea-hub-schedule.ads,v 1.4 2004/03/03 00:06:05 Jano Exp $

with Aenea.Globals.Options;
with Agpl.Event_queues;
with Agpl.Event_queues.Real_Time;

package Aenea.Hub.Schedule is

   package Event_Queue renames Agpl.Event_Queues.Real_Time;

   ------------------------------------------------------------------------
   -- Event types                                                        --
   ------------------------------------------------------------------------
   type Hub_messages is (Ping, Query);
   type Hub_context is new Agpl.Event_queues.Context_type with record
      Msg_type : Hub_messages;
      Hub_id   : Ustring;
   end record;

   ------------------------------------------------------------------------
   -- Create_timeout                                                     --
   ------------------------------------------------------------------------
   -- No delays involved.
   procedure Create_timeout (
      This     : in Hub.Object; 
      Deadline : in Duration := Globals.Options.Walk_Timeout);

   ------------------------------------------------------------------------
   -- Reschedule                                                         --
   ------------------------------------------------------------------------
   -- Has into account delay until the message will be sent.
   procedure Reschedule (This : in out Hub.Object);

end Aenea.Hub.Schedule;
