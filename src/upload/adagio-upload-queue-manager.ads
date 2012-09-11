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
--  $Id: adagio-upload-queue-manager.ads,v 1.7 2004/02/03 22:52:16 Jano Exp $

With
Adagio.Chronos,
Adagio.Globals,
Adagio.Globals.Options,
Adagio.Types,
Adagio.Upload.Client.Pool,
Agpl.Http.Server.Sort_handler,
Dynamic_vector,
Agpl.Types.Ustrings,
Charles.Hash_string,
Charles.Maps.Hashed.Strings.Unbounded;


Use
Agpl.Types.Ustrings;

package Adagio.Upload.Queue.Manager is

   pragma Elaborate_body;

   -- Stats
   Stat_session_upload    : constant String := "Uploads - Data sent";
   Stat_session_speed     : constant String := "Uploads - Mean speed";
   Stat_session_completed : constant String := "Uploads - Completed";

   Max_unknown : Natural renames Globals.Options.Uploads_MaxUnknown;

   package Queue_list is new Charles.Maps.Hashed.Strings.Unbounded (
      Queue.Object_access, Charles.Hash_string, "=", "=");
   package Queue_vector is new Dynamic_vector (Queue.Object_access, 8);

   -- For average speed checking:
   type Minimum_speed is record
      Next_deadline  : Calendar.Time;
      Target_amount  : File_size;
      Current_amount : File_size := 0;
   end record;

   type Client_slot is record
      Client        : Upload.Client.Object_access;
      Queue_id      : Ustring;

      Is_uploading  : Boolean := false;        -- Is uploading or allowed?
      Start_ack     : Boolean := false;        -- Client has started upload
      Position      : Natural;                 -- Best queue position.
      BW_boost      : Float;                   -- Bandwidth modifier.

      Active_queue  : Queue.Object_access;     -- Queue in which uploads
      Queues        : Queue_vector.Object (1); -- All enqueued queues

      Last_run      : Calendar.Time;

      Speed         : Minimum_speed;
      Session_sent  : File_size;
      Session_start : Calendar.Time;
   end record;
   type Client_slot_access is access all Client_slot;

   package Client_list is new Charles.Maps.Hashed.Strings.Unbounded (
      Client_slot_access, Charles.Hash_string, "=", "=");

   protected Object is

   ------------------------------------------------------------------------
   -- Enqueue                                                            --
   ------------------------------------------------------------------------
   procedure Enqueue (this : access Client.Object'Class);

   ------------------------------------------------------------------------
   -- Init                                                              --
   ------------------------------------------------------------------------
   -- Initialization plus recovering from folder ("" for no recovery)
   -- Each queue is stored in a separate file, but the base forder is
   -- specified here.
   procedure Init (File : in String);

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Processes pending unknowns.
   procedure Process;

   ------------------------------------------------------------------------
   -- Process_event                                                      --
   ------------------------------------------------------------------------
   -- Process events for already identified and queued clients
   procedure Process_event (Queue_id : in String);

   ------------------------------------------------------------------------
   -- Save_queues                                                        --
   ------------------------------------------------------------------------
   -- Dump queues to disk
   procedure Save_queues;

   ------------------------------------------------------------------------
   -- Total_length                                                       --
   ------------------------------------------------------------------------
   -- Sumed length of all queues
   function Total_length return Natural;

   ------------------------------------------------------------------------
   -- Max_length                                                         --
   ------------------------------------------------------------------------
   -- Max length of any queue
   function Max_length return Natural;

   ------------------------------------------------------------------------
   -- Max_active_length                                                  --
   ------------------------------------------------------------------------
   -- Sumed length of allowed active uploads for every queue
   function Max_active_length return Natural;

   ------------------------------------------------------------------------
   -- Num_active_uploads                                                 --
   ------------------------------------------------------------------------
   -- Total numbr of clients uploading
   function Num_active_uploads return Natural;

   ------------------------------------------------------------------------
   -- Num_waiting                                                        --
   ------------------------------------------------------------------------
   -- Total number of clients waiting
   function Num_waiting return Natural;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Return queues names:
   function Report return Ustring_array;

   ------------------------------------------------------------------------
   -- Report_queue                                                       --
   ------------------------------------------------------------------------
   function Report_queue (
      Name : in String;
      From : in Natural;
      Qty  : in Natural;
      Lost : in Boolean) return Queue.Report_array;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (
      Name : in  String;  -- Queue name
      Lost : in  Boolean; -- Show lost ones
      Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown;

   ------------------------------------------------------------------------
   -- Members access                                                     --
   ------------------------------------------------------------------------
   function Get_session_sent return Long_long_integer;
   function Get_mean_speed return Speed;

   private

      Path    : Ustring;            -- File for persistence.

      Pending : Client.Pool.Object; -- Pending of being enqueued.

      Queues  : Queue_list.Container_type; -- Defined queues.

      Clients : Client_list.Container_type;

      Session_sent : Long_long_integer := 0; -- Total uploaded data.
      Sent_cron    : Chronos.Object;

      Mean_speed   : Speed := 0; -- Last known mean speed

   end Object;

private

end Adagio.Upload.Queue.Manager;
