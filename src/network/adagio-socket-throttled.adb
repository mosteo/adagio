
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
--  $Id: adagio-socket-throttled.adb,v 1.3 2004/04/01 22:11:25 Jano Exp $

--  The stream defined here allows for writing and reading without taking
--  care of the actual throttling. See inner details for inner workings.

with Adagio.Globals;
with Adagio.Trace;

with Charles.Maps.Sorted.Unbounded;

with Ada.Streams;

with Ada.Unchecked_deallocation;
use  Ada;

package body Adagio.Socket.Throttled is

   use type Ada.Streams.Stream_element_offset;
   use type Agpl.Streams.Stream_element_array_access;

   package Circular renames Agpl.Streams.Circular;

   package Stream_map is new Charles.Maps.Sorted.Unbounded (
      Stream_id, Stream_access, "<", "=");

   Seq : Unsigned_sequence.Object;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new Unchecked_deallocation (
      Streams.Stream_element_array, Agpl.Streams.Stream_element_array_access);

   ------------------------------------------------------------------------
   -- Pending                                                            --
   ------------------------------------------------------------------------
   protected Pending is
      procedure Delete_in  (Stream : in Stream_type);
      procedure Delete_out (Stream : in Stream_type);

      procedure Insert_in  (Stream : in Stream_access);
      procedure Insert_out (Stream : in Stream_access);

      procedure Check_all;

      function Contains_in  (Stream : in Stream_type) return Boolean;
      function Contains_out (Stream : in Stream_type) return Boolean;
   private
      List_in    : Stream_map.Container_type; -- Pending for reading
      List_out   : Stream_map.Container_type; -- Pending for writing
      Read_first : Boolean := true; -- To switch between read/write first

      Start_in,
      Start_out  : Stream_id := Stream_id'First;
   end Pending;

   protected body Pending is
      procedure Delete_in (Stream : in Stream_type) is
      begin
         Stream_map.Delete (List_in, Stream.Id);
      end Delete_in;
      procedure Delete_out (Stream : in Stream_type) is
      begin
         Stream_map.Delete (List_out, Stream.Id);
      end Delete_out;
      procedure Insert_in  (Stream : in Stream_access) is
      begin
         Stream_map.Insert (List_in, Stream.Id, Stream);
      end Insert_in;
      procedure Insert_out (Stream : in Stream_access) is
      begin
         Stream_map.Insert (List_out, Stream.Id, Stream);
      end Insert_out;
      function Contains_in  (Stream : in Stream_type) return Boolean is
      begin
         return Stream_map.Find (List_in, Stream.Id);
      end Contains_in;
      function Contains_out (Stream : in Stream_type) return Boolean is
      begin
         return Stream_map.Find (List_out, Stream.Id);
      end Contains_out;
      procedure Check_all is
         use Stream_map;

         procedure Do_read (Extra : in Boolean) is
            I, J    : Stream_map.Iterator_type;
            Len     : Natural;
         begin
            I := Upper_bound (List_in, Start_in);
            if I = Back (List_in) then
               I := First (List_in);
            end if;
            Len := Length (List_in);
            for N in 1 .. Len loop
               J := Succ (I);-- We do it first because the attempts may result
                             -- in a removed I
               Attempt_read (Element (I), Extra);
               I := J;
               exit when Is_empty (List_in);
               if I = Back (List_in) then
                  I := First (List_in);
               end if;
            end loop;
         end Do_read;
         procedure Do_write (Extra : in Boolean) is
            I, J    : Stream_map.Iterator_type;
            Len     : Natural;
         begin
            I   := Upper_bound (List_out, Start_out);
            if I = Back (List_out) then
               I := First (List_out);
            end if;
            Len := Length (List_out);
            for N in 1 .. Len loop
               J := Succ (I);
               Attempt_write (Element (I),  Extra);
               I := J;
               exit when Is_empty (List_out);
                  if I = Back (List_out) then
                     I := First (List_out);
                  end if;
            end loop;
         end Do_write;
      begin
         -- First pass, non-extra BW
         if Read_first then
            Do_read (Extra => false);
         end if;

         Do_write (Extra => false);

         if not Read_first then
            Do_read (Extra => false);
         end if;

         -- Second pass, extra BW
         if Read_first then
            Do_read (Extra => true);
         end if;

         Do_write (Extra => true);

         if not Read_first then
            Do_read (Extra => true);
         end if;

         if Read_first then
            Start_in  := Start_in + 1;
         else
            Start_out := Start_out + 1;
         end if;

         Read_first := not Read_first;
      end Check_all;
   end Pending;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- You MUST close any stream hereby created.
   -- Closing this stream also closes its socket!
   procedure Close (Stream : in out Stream_type) is
      procedure Free is new Unchecked_deallocation (
         Agpl.Streams.Circular.Stream_type, 
         Agpl.Streams.Circular.Stream_access);
   begin
      Pending.Delete_in  (Stream);
      Pending.Delete_out (Stream);
      Free (Stream.Buf_in);
      Free (Stream.Buf_out);
      Free (Stream.Buf_wrt);
   end Close;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Any stream hereby obtained must later be closed.
   function Create (
      From        : access Socket.Stream_type; 
      Manager_in  : access Bandwidth_manager.Object;
      Manager_out : access Bandwidth_manager.Object;
      Buffer      : in     Positive := 4096) return Stream_access
   is
      Stream : Stream_access := new Stream_type (
         Stream => From,
         BW_in  => Manager_in,
         BW_out => Manager_out);        
   begin
      Seq.Get_next (Stream.Id);
      Stream.Self    := Stream;

      Stream.Buf_in  := new Circular.Stream_type (
         Streams.Stream_element_count (Buffer));
      Stream.Buf_out := new Circular.Stream_type (
         Streams.Stream_element_count (Buffer));

      return Stream;
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if not Pending.Contains_in (Stream) then
         Attempt_read (Stream.Self, Extra => false);
      end if;
      Circular.Read (Stream.Buf_in.all, Item, Last);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
	procedure Write(
      Stream : in out Stream_type;
      Item   : in Ada.Streams.Stream_Element_Array) is
   begin
      Circular.Write (Stream.Buf_out.all, Item);
      if not Pending.Contains_out (Stream) then
         Attempt_write (Stream.Self, Extra => true);
      end if;
   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   function Available_read (This : in Stream_access) return Natural is
   begin
      if not Pending.Contains_in (This.all) then
         Attempt_read (This, Extra => true);
      end if;
      return Circular.Available_read (This.Buf_in.all);
   end Available_read;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   function Available_write (This : in Stream_access) return Natural is
   begin
      if not Pending.Contains_out (This.all) then
         Attempt_write (This, Extra => true);
      end if;
      return Circular.Available_write (This.Buf_out.all);
   end Available_write;

   -- This tasks periodically checks a private list of sockets with
   -- data pending to be sent, and tries to dispatch as much data as
   -- possible
   task body Sender is 
   begin
      loop
         exit when Globals.Requested_exit;
         delay Refresh_period;
         begin
            Pending.Check_all;
         exception
            when E : others =>
               Trace.Log ("Socket.Throttled: " & Trace.Report (E), 
                  Trace.Error);
         end;
      end loop;
   end Sender;

   ------------------------------------------------------------------------
   -- Pending_read                                                       --
   ------------------------------------------------------------------------
   -- Says if there is data pending, including unreadable because throttling
   function Pending_read (This : in Stream_access) return Natural is
   begin
      return Available_read (This) + Available (This.Stream.all);
   end Pending_read;

   ------------------------------------------------------------------------
   -- Pending_write                                                      --
   ------------------------------------------------------------------------
   -- Says how many data is pending to be sent because of throttle
   function Pending_write (This : in Stream_access) return Natural is
   begin
      if not Pending.Contains_out (This.all) then
         Attempt_write (This, Extra => true);
      end if;
      if This.Buf_wrt /= null then
         return This.Buf_wrt'Length + 
                Circular.Available_read (This.Buf_out.all);
      else
         return Circular.Available_read (This.Buf_out.all);
      end if;
   end Pending_write;

   ------------------------------------------------------------------------
   -- Attempt_read                                                       --
   ------------------------------------------------------------------------
   procedure Attempt_read (
      Stream : in Throttled.Stream_access; Extra : in Boolean)
   is
      Available : Natural := Natural'Min (
         Socket.Available (Stream.Stream.all), 
         Circular.Available_write (Stream.Buf_in.all));
      Awarded   : Natural;
   begin
      if Available > 0 then
         Stream.BW_in.Commit (Available, Awarded, Extra => Extra);
         declare
            Buf  : Streams.Stream_element_array (
               1 .. Streams.Stream_element_offset (Awarded));
            Last : Streams.Stream_element_offset;
         begin
            Read (Stream.Stream.all, Buf, Last);
            Circular.Write (Stream.Buf_in.all, Buf (1 .. Last));
         exception
            when E : Socket.Socket_error =>
               Exceptions.Save_occurrence (Stream.Exception_type, E);
               Stream.Exception_pending := true;
               Pending.Delete_in  (Stream.all);
               Pending.Delete_out (Stream.all);
         end;
      end if;

      if Socket.Available (Stream.Stream.all) > 0 then
         Pending.Insert_in (Stream);
      else
         Pending.Delete_in (Stream.all);
      end if;
   end Attempt_read;

   ------------------------------------------------------------------------
   -- Attempt_write                                                      --
   ------------------------------------------------------------------------
   procedure Attempt_write (
      Stream : in Throttled.Stream_access; Extra : in Boolean) 
   is
      Available : Natural := Circular.Available_read (Stream.Buf_out.all);
      Awarded   : Natural;
      Last      : Streams.Stream_element_offset;
      Aux       : Agpl.Streams.Stream_element_array_access;
   begin
      -- Read new data only if wrt buffer is empty
      if Stream.Buf_wrt = null and then Available > 0 then
         Stream.BW_out.Commit (Available, Awarded, Extra => Extra);

         Stream.Buf_wrt := new Streams.Stream_element_array (
            1 .. Streams.Stream_element_offset (Awarded));

         Circular.Read (Stream.Buf_out.all, Stream.Buf_wrt.all, Last);

         if Last /= Stream.Buf_wrt'Last then
            -- Reallocate 
            Aux := Stream.Buf_wrt;
            Stream.Buf_wrt := new Streams.Stream_element_array'(
               Stream.Buf_wrt (Last + 1 .. Stream.Buf_wrt'Last));
            Free (Aux);
         end if;

      end if;

      -- Attempt to write buffer
      if Stream.Buf_wrt /= null then
         begin
            Write (Stream.Stream.all, Stream.Buf_wrt.all);
            -- Write successful:
            -- Free the buffer
            Free (Stream.Buf_wrt);
         exception
            when E : Socket_error =>
               if Get_error (E) /= Operation_would_block then
                  Exceptions.Save_occurrence (Stream.Exception_type, E);
                  Stream.Exception_pending := true;
                  Pending.Delete_in  (Stream.all);
                  Pending.Delete_out (Stream.all);
               end if;
            when others =>
               raise;
         end;
      end if;

      -- Insert if pending
      if Stream.Buf_wrt /= null or else 
         Circular.Available_read (Stream.Buf_out.all) > Natural'(0)
      then
         Pending.Insert_out (Stream);
      else
         Pending.Delete_out (Stream.all);
      end if;
   end Attempt_write;
    
   ------------------------------------------------------------------------
   -- Check_exceptions                                                   --
   ------------------------------------------------------------------------
   procedure Check_exceptions (Stream : in out Stream_type) is
   begin
      if Stream.Exception_pending then
         Stream.Exception_pending := false;
         Exceptions.Reraise_occurrence (Stream.Exception_type);
      end if;
   end Check_exceptions;

end Adagio.Socket.Throttled;
