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
--  $Id: adagio-workers.ads,v 1.3 2004/01/21 21:05:48 Jano Exp $

--  Workers: tasks which carry a work that may be aborted by external means.
--  Intended for use with blocking calls.
--  Worker pools allow to have families of related tasks.
--  These tasks wait for a new work to be inserted.

--  We choose to make this package generic so all relevant tasks/protected
--  object can be hidden.
--  Unfortunately, this also means that no multi-purpose tasks can be used.

with Generic_event_queue;

with Pragmarc.Assignment;
with Pragmarc.Queue_bounded_blocking;

generic
   type Context_type (<>) is private;
   with procedure Work_procedure  (this : Context_type);
   with procedure Abort_procedure (this : Context_type);
   Num_workers       : Positive;
   Max_pending_works : Positive;
package Adagio.Workers is

   pragma Elaborate_Body;

   type Work_type is private;

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
      Started  : out Boolean);

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown;

private

   package Timeout_queue is new
      Generic_event_queue (Context_type, Abort_procedure);

   type Context_access is access all Context_type;

   type Work_type is record
      Context  : Context_access;
      Deadline : Duration;
      Timeout  : Timeout_queue.Event_type;
   end record;

   procedure Assign is new Pragmarc.Assignment (Work_type);

   package Queue is new
      Pragmarc.Queue_bounded_blocking (Work_type, Assign);

   task type Worker;

   type Worker_pool is array (Integer range <>) of Worker;

end Adagio.Workers;
