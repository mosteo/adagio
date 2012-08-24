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
--  $Id: adagio-upload-queue.ads,v 1.7 2004/02/29 20:36:46 Jano Exp $

--  In all queue packages, Queue_id identifies client + requested file
--  It can appear thus in more than a queue.
--  Client_id identifies uniquely the client, and several queue slots
--  can have the same client_id. The same queue_id can't appear more than
--  once in the same queue.

with Adagio.File;
with Adagio.Misc;
with Adagio.Types; use Adagio.Types;
with Adagio.Upload.Client;
with Adagio.Upload.Ratings;
with Adagio.Upload.Resource;
with Adagio.Xml;
with Adagio.Xml.Utils;
with Agpl.Http.Server.Sort_handler;
with Dynamic_vector;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Maps.Sorted.Strings.Unbounded;

with Ada.Calendar; use Ada;
with System;

package Adagio.Upload.Queue is

   pragma Elaborate_Body;

   use type Types.File_Size;

   type Object;
   type Object_access is access all Object;

   type Rating is new float;

   -- Type for keeping track of queued clients:
   -- It stores the current position, rating if applicable,
   --    permission to start upload in this queue
   --    and alive status.
   type Queue_slot is record
      Queue_id   : Ustring;
      Client_id  : Ustring;
      Resource   : Upload.Resource.Handle := Upload.Resource.Null_handle;
      Client     : Upload.Client.Object_access; -- Only valid if alive

      Position   : Natural;
      Max_slots  : Natural;
      Used_slots : Natural;
      Rating     : Queue.Rating;

      Arrival    : Calendar.Time := Calendar.Clock; -- For fifo, etc.
      Last_seen  : Calendar.Time := Calendar.Clock; -- For information.
      Expiration : Calendar.Time;

      Can_start  : Boolean;
      Alive      : Boolean;

      Client_name : Ustring; -- To remember it when lost
      Client_file : Ustring; -- Same as above
      Client_ip   : Ustring;
   end record;
   type Queue_slot_access is access all Queue_slot;
   --for Queue_slot_access'Storage_pool use Debug.Debug_pool;

   type Preemption_types is (None, Time, Size, Both);

   type Preemptions is record
      Kind : Preemption_types := None;
      Time : Duration         := 15.0 * 60.0;
      Size : File_size        := 1024 * 1024 * 20;
   end record;

   Default_preemption : Preemptions;

   type Order_types is (Fifo, Rated);

   type Orderings is record
      Kind       : Order_types := Fifo;
      Expression : Ustring     := U ("Position");
   end record;

   Default_ordering : Orderings;

   package Slot_vector is new Dynamic_vector (Queue_slot_access, 100);
   package Id_list is new Charles.Maps.Sorted.Strings.Unbounded (
      Queue_slot_access, "<", "=");

   type Queue_report_type is record
      Client_id : Ustring;  -- IP
      Position  : Positive;
      Status    : Ustring;
      Speed     : Float;
      Client    : Ustring;  -- User_agent
      File      : Ustring;
   end record;

   type Report_array is array (Positive range <>) of Queue_report_type;

   ------------------------------------------------------------------------
   -- Rate                                                                  --
   ------------------------------------------------------------------------
   function Rate (This : in Queue_slot; Expr : in String) return Rating;

   ------------------------------------------------------------------------
   -- OBJECT                                                             --
   ------------------------------------------------------------------------
   protected type Object (
      Priority : System.Priority := System.Priority'Last) is
      pragma Priority (Priority);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation also includes an attempt of loading its persistent part.
   --    For now only clients.
   procedure Create (
      This           : in  Object_access;  -- Self reference
      Name           : in  String;
      Length         : in  Natural;
      Uploads        : in  Natural;         -- Active uploads.
      Bandwidth      : in  File_size;       -- Base bandwidth.
      Minimum_speed  : in  Speed := 1024; -- That active clients must hold.
      Average_period : in  Duration := 30.0 * 60.0;
      Criteria       : in  String   := "true";
      Preemptive     : in  Preemptions := Default_preemption;
      Ordering       : in  Orderings   := Default_ordering;
      Base_folder    : in  String      := "");

   ------------------------------------------------------------------------
   -- Enqueue                                                            --
   ------------------------------------------------------------------------
   -- Try to add a new client.
   -- Can fail if queue is full or criteria aren't meet.
   procedure Enqueue (
      Client  : access Upload.Client.Object'Class;
      Success : out    Boolean);

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Remove an upload completely
   procedure Remove (Queue_id : in String);

   ------------------------------------------------------------------------
   -- Lost                                                               --
   ------------------------------------------------------------------------
   -- Mark a slot as lost (dead)
   procedure Lost (Queue_id : in String);

   ------------------------------------------------------------------------
   -- Schedule                                                           --
   ------------------------------------------------------------------------
   -- Re-sorts the queue and marks the slots which should start.
   procedure Schedule;

   ------------------------------------------------------------------------
   -- Serialize                                                          --
   ------------------------------------------------------------------------
   -- Base folder for writing is passed.
   -- Will save critical info for every actual slot.
   procedure Serialize (Base : String := "");

   ------------------------------------------------------------------------
   -- Check_client                                                       --
   ------------------------------------------------------------------------
   -- Obtains awareness about a queued client
   procedure Check_client (Queue_id : in String; Data : out Queue_slot);

   ------------------------------------------------------------------------
   -- Busy                                                               --
   ------------------------------------------------------------------------
   -- Maximum uploads reached
   function Busy return Boolean;

   ------------------------------------------------------------------------
   -- Is_candidate                                                       --
   ------------------------------------------------------------------------
   -- Says if a file satisfy queue entry condition
   function Is_candidate (F : in File.Object) return Boolean;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Status of the queue:
   function Report (
      From : in Natural;
      Qty  : in Natural;
      Lost : in Boolean) return Report_array;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (
      Lost : in  Boolean; -- Show lost ones
      Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- Members_access                                                     --
   ------------------------------------------------------------------------
   function Get_name return String;
   function Get_slot_bandwidth return Speed;
   function Get_max_length return Natural;
   function Get_current_length return Natural;
   function Get_alive_length return Natural;
   function Get_avg_period return Duration;
   function Get_min_speed return Speed;
   function Get_preemptions return Preemptions;
   function Get_uploads return Natural;

   function Get_current_uploads return Natural; -- uploading now
   function Get_current_waiting return Natural; -- alive waiting now

   private

      Name       : Ustring;
      Length     : Natural;
      Uploads    : Natural;
      Bandwidth  : Speed;
      Min_speed  : Speed;
      Avg_period : Duration;
      Criteria   : Ustring;
      Preemptive : Preemptions;
      Order      : Orderings;
      Expression : Ustring;

      Max_per_client : Natural; -- Globally for all queues max per client

      -- All queued/active clients:
      -- The 0 position is used only for orderings, real ones are after 1.
      Clients    : Slot_vector.Object (0);

      -- Ids to verify presence quickly. Must be synchronized with Clients.
      Ids        : Id_list.Container_type;

      -- Currently active uploads:
      Current_uploads : Natural := 0;

      -- Saving location
      File_base  : Ustring;

      -- Self-reference
      Self       : Object_access;

   end Object;

end Adagio.Upload.Queue;
