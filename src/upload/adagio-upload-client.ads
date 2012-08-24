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
--  $Id: adagio-upload-client.ads,v 1.4 2004/02/03 22:52:15 Jano Exp $

--  Abstract base class for upload clients

with Adagio.File;
with Adagio.Safe_access;
with Adagio.Types; use Adagio.Types;
with Adagio.Upload.Resource;

with Ada.Finalization;
with Ada.Real_time; use Ada;
with Ada.Streams;   use Ada.Streams;
with Ada.Unchecked_deallocation;

package Adagio.Upload.Client is

   pragma Elaborate_body;

   -- Some exceptions that clients could raise:
   Connection_lost             : exception;
   Unknown_request             : exception;
   Client_polled_too_soon      : exception;
   Client_missed_poll_deadline : exception;
   User_agent_is_banned        : exception;

   type Object is abstract new 
      Finalization.Limited_controlled with null record;
   type Object_access is access all Object'Class;

   subtype Ratings is Float range 0.0 .. Float'Last;

   type Queue_context is record
      Position      : Positive;  -- Our position in the queue
      Max_slots     : Natural;   -- Max queue length
      Current_slots : Natural;   -- Current queued clients

      Must_start    : Boolean;   -- Upload must start or is running.
         -- This is for the extended client type to know it must start.

      Allowed_up    : File_size;   -- Bytes allowed for own upload
      Allowed_down  : File_size;   -- Bytes allowed for own download
   end record;

   type Client_results is record
      Is_done       : Boolean;     -- Client has finished (ok).

      Is_uploading  : Boolean := false;
                                   -- Must be set by the client when it is
                                   --   actively uploading.

      Sent          : File_size := 0;   -- Bytes sent in this iteration.
      Received      : File_size := 0;   -- Bytes received in this iteration.

      Awakening     : Real_time.Time; 
                                 -- Next time this client wants to we run.
   end record;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Do whatever processing the queued client needs.
   -- This function is invoked periodically.
   -- MUST NEVER BLOCK!!
   -- When connection finish correctly, Result.Is_done must be set.
   --    Be it download complete, unsatisfiable request, or whatever.
   -- When some error happens, a exception must be raised (client will be
   --    marked as lost and will be capable of requeuing).
   -- However, exceptions while uploading will result in place lost.
   procedure Process (
      This    : in out Object;
      Context : in     Queue_context;  -- Info for the client
      Result  : out    Client_results  -- Info for the queue manager
      ) is abstract;

   ------------------------------------------------------------------------
   -- Resource                                                       --
   ------------------------------------------------------------------------
   -- Get the requested resource
   -- Should return Null_handle until it's known
   function Requested_resource (This : in Object) 
      return Upload.Resource.Handle is abstract;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Get an unique id for the client. Ideally should be IP independent and
   --    portable across networks.
   function Id (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Name                                                               --
   ------------------------------------------------------------------------
   -- Name of the client software (if known)
   function Name (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Address                                                            --
   ------------------------------------------------------------------------
   -- Ip of the client software
   function Address (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Reject                                                             --
   ------------------------------------------------------------------------
   -- Instructs to send a reject signal if possible to the client
   -- Must not block, and must activate Done when finished.

   type Reject_reason is (Busy, Unavailable);

   procedure Reject (
      This   : in Object; 
      Reason : in Reject_reason; 
      Done   : out Boolean) is abstract;
      
   ------------------------------------------------------------------------
   -- Cancel                                                             --
   ------------------------------------------------------------------------
   -- Should close connection and free all resources.
   procedure Cancel (This : in out Object) is abstract;

   ------------------------------------------------------------------------
   -- Queue_id                                                           --
   ------------------------------------------------------------------------
   -- Function for quickly unique id an upload:
   -- Must provide a client id + requested resource id
   function Queue_id (This : in Object'Class) return String;

   ------------------------------------------------------------------------
   -- Speed                                                              --
   ------------------------------------------------------------------------
   -- Says the average speed
   function Speed (This : in Object) return Float is abstract;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Object);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize   (This : in out Object);

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new 
      Unchecked_deallocation (Object'Class, Object_access);

end Adagio.Upload.Client;
