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
--  $Id: adagio-socket-throttled.ads,v 1.3 2004/04/01 22:11:25 Jano Exp $

--  The stream defined here allows for writing and reading without taking
--  care of the actual throttling. See inner details for inner workings.

with Adagio.Bandwidth_manager;

with Agpl.Streams;
with Agpl.Streams.Circular;
with Agpl.Sequence;

with Ada.Exceptions;
use  Ada;
with Interfaces;

package Adagio.Socket.Throttled is

   pragma Elaborate_Body;

   Refresh_period : Duration := 0.5;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type (<>) is new Ada.Streams.Root_stream_type with private;
   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- You MUST close any stream hereby created.
   -- The closing must precede closing its socket.
   procedure Close (Stream : in out Stream_type);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Any stream hereby obtained must later be closed.
   -- The socket of this stream must be non-blocking if no unexpected
   -- waitings are desired.
   -- The BW managers can be the same if common totals are desired.
   function Create (
      From        : access Socket.Stream_type; 
      Manager_in  : access Bandwidth_manager.Object;
      Manager_out : access Bandwidth_manager.Object;
      Buffer      : in     Positive := 4096)
      return Stream_access;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   -- Says how many data is ready to be read
   function Available_read (This : in Stream_access) return Natural;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   -- Says how many data can be written now
   function Available_write (This : in Stream_access) return Natural;

   ------------------------------------------------------------------------
   -- Pending_read                                                       --
   ------------------------------------------------------------------------
   -- Says if there is data pending, including unreadable because throttling
   function Pending_read (This : in Stream_access) return Natural;

   ------------------------------------------------------------------------
   -- Pending_write                                                      --
   ------------------------------------------------------------------------
   -- Says how many data is pending to be sent because of throttle
   function Pending_write (This : in Stream_access) return Natural;

   ------------------------------------------------------------------------
   -- Check_exceptions                                                   --
   ------------------------------------------------------------------------
   -- Will raise any stored exception
   procedure Check_exceptions (Stream : in out Stream_type);


private

   type Stream_id is new Interfaces.Unsigned_32;

   package Unsigned_sequence is new Agpl.Sequence (Stream_id);

   type Stream_type (
      Stream : access Adagio.Socket.Stream_type;
      BW_in  : access Adagio.Bandwidth_manager.Object;
      BW_out : access Adagio.Bandwidth_manager.Object) 
   is new Ada.Streams.Root_stream_type with record
      Buf_in  : Agpl.Streams.Circular.Stream_access;
      Buf_out : Agpl.Streams.Circular.Stream_access;
      Self    : Stream_access;
      Id      : Stream_id;

      -- Buffer for data read from the circular out but not yet sent to sock.
      Buf_wrt : Agpl.Streams.Stream_element_array_access;

      -- Exceptions
      Exception_pending : Boolean := false;
      Exception_type    : Exceptions.Exception_occurrence;
   end record;

   -- This tasks periodically checks a private list of sockets with
   -- data pending to be sent, and tries to dispatch as much data as
   -- possible
   task Sender;

   ------------------------------------------------------------------------
   -- Attempt_read                                                       --
   ------------------------------------------------------------------------
   -- Read as many data as possible restricted by BW and socket
   -- Extra indicates if the bandwidth should be taken from the extra pool
   procedure Attempt_read (
      Stream : in Throttled.Stream_access; Extra : in Boolean);
   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   -- Write as many data as possible restricted by BW and socket
   procedure Attempt_write (
      Stream : in Throttled.Stream_access; Extra : in Boolean);

end Adagio.Socket.Throttled;
