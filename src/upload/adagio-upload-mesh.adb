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

with Adagio.Chronos;
with Adagio.Convert;
with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Statistics.Strings;
with Adagio.Trace;

with Gnat.Os_lib; 
use  Gnat;

with Ada.Streams.Stream_io;

package body Adagio.Upload.Mesh is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- A mesh with given Id (for example network).
   protected body Object is

   procedure Update_stats is
      use Element_list;
   begin
      Statistics.Object.Set ("Uploads - " & Id.all & " mesh entries",
         Statistics.Integers.Create (Length (Elements)));
      Statistics.Object.Set ("Uploads - " & Id.all & " mesh memory",
         Statistics.Strings.Create (
            Convert.To_size (Length (Elements) * 
               (Element_type'Size / 8))));
   end Update_stats;

   ------------------------------------------------------------------------
   -- Configure                                                          --
   ------------------------------------------------------------------------
   -- Set the number of alt-sources to keep for an item and
   -- the time they will last.
   procedure Configure (
      Sources : in Positive := Default_cached_elements;
      TTL     : in Duration := 24.0 * 60.0 * 60.0) is
   begin
      Max_sources  := Sources;
      Time_to_live := TTL;
   end Configure;

   ------------------------------------------------------------------------
   -- Restore                                                               --
   ------------------------------------------------------------------------
   -- Restore a mesh from hard disk or stream
   procedure Restore (Path   : in     String) is
      use Stream_io;
      F : File_type;
      C : Chronos.Object;
   begin
      if not Os_lib.Is_regular_file (Path) then
         Trace.Log ("Upload.Mesh.Restore: File " & Path & " does not exist");
         return;
      end if;
      Open (F, Name => Path, Mode => In_file);
      Restore (Stream (F));
      Close (F);
      Trace.Log ("Mesh for " & Id.all & " restored (" &
         Chronos.Image (C) & ").");
      Update_stats;
   exception
      when E : others =>
         if Is_open (F) then
            Close (F);
         end if;
         Trace.Log ("Upload.Mesh.Restore: " & Trace.Report (E), Trace.Error);
   end Restore;

   procedure Restore (Stream : access Root_stream_type'Class) is
      Num   : Natural;
      Slot  : Slot_type;
      use Element_list;
   begin
      -- Num of slots
      Natural'Read (Stream, Num);
      -- Slots
      for N in 1 .. Num loop
         Slot := Slot_type'Input (Stream);
         Insert (Elements, Key (Slot.Element), Slot);
      end loop;
   end Restore;

   ------------------------------------------------------------------------
   -- Save                                                               --
   ------------------------------------------------------------------------
   -- Save to a file or stream
   procedure Save (Path   : in     String) is
      use Stream_io;
      F : File_type;
      C : Chronos.Object;
   begin
      Purge;
      Trace.Log ("Mesh for " & Id.all & " purged (" &
         Chronos.Image (C) & ").");

      Chronos.Reset (C);
      Create (F, Name => Path, Mode => Out_file);
      Save (Stream (F));
      Close (F);
      Trace.Log ("Mesh for " & Id.all & " saved correctly (" &
         Chronos.Image (C) & ").");
   exception
      when E : others =>
         if Is_open (F) then
            Close (F);
         end if;
         Trace.Log ("Upload.Mesh.Save: " & Trace.Report (E), Trace.Error);
   end Save;
   procedure Save (Stream : access Root_stream_type'Class) is
      use Element_list;
      I : Iterator_type := First (Elements);
   begin
      -- Num of slots
      Natural'Write (Stream, Length (Elements));
      -- Slots
      while I /= Back (Elements) loop
         Slot_type'Output (Stream, Element (I));
         I := Succ (I);
      end loop;
   end Save;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   -- Add an element to the mesh with a given age under a given key.
   procedure Add (
      Element : in Element_type; 
      Born    : in Calendar.Time := Calendar.Clock) is
   begin
      if Contains_better (Element) then
         return;
      else
         Remove (Key (Element), Location (Element));
      end if;
      Element_list.Insert (Elements, Key (Element),
         (Element => Element, Born => Born));
      Update_stats;
   end Add;

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Removes any elements for the give Key/Location.
   procedure Remove (Key : in String; Loc : in String) is
      use Element_list;
      From : Element_list.Iterator_type := Lower_bound (Elements, Key);
      To   : Element_list.Iterator_type := Upper_bound (Elements, Key);
      I    : Element_list.Iterator_type := From;
   begin
      while I /= To loop
         if Location (Element (I).Element) = Loc then
            Delete (Elements, I);
            return;
         else
            I := Succ (I);
         end if;
      end loop;
   end Remove;

   ------------------------------------------------------------------------
   -- Get an array of objects (the N youngest)                           --
   ------------------------------------------------------------------------
   -- We'll create a sorted list of the elements for the key and return 
   --    the first and best ones.
   function Get (
      Key    : in String;
      Amount : in Positive := Default_cached_elements) return Element_array
   is
      use Element_set;
      use Element_list;
      From : Element_list.Iterator_type := Lower_bound (Elements, Key);
      To   : Element_list.Iterator_type := Upper_bound (Elements, Key);
      I    : Element_list.Iterator_type := From;
      R    : Element_set.Container_type;
   begin
      while I /= To loop
         Insert (R, Element (I).Element);
         I := Succ (I);
      end loop;
      declare
         I      : Element_set.Iterator_type := First (R);
         Result : Element_array (1 .. Amount);
         Pos    : Natural := 1;
      begin
         while I /= Back (R) and then Pos <= Result'Last loop
            Result (Pos) := Element (I);
            I   := Succ (I);
            Pos := Pos + 1;
         end loop;
         return Result (1 .. Pos - 1);
      end;
   end Get;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a Key/Location are already present:
   function Contains (Key : in String; Loc : in String) return Boolean is
      use Element_list;
      From : Element_list.Iterator_type := Lower_bound (Elements, Key);
      To   : Element_list.Iterator_type := Upper_bound (Elements, Key);
      I    : Element_list.Iterator_type := From;
   begin
      while I /= To loop
         if Location (Element (I).Element) = Loc then
            return true;
         end if;
         I := Succ (I);
      end loop;
      return false;
   end Contains;

   ------------------------------------------------------------------------
   -- Contains_better                                                  --
   ------------------------------------------------------------------------
   -- Says if a Key/Location are already present and are better:
   function Contains_better (E : in Element_type) return Boolean is
      use Element_list;
      From : Element_list.Iterator_type := 
         Lower_bound (Elements, Key (E));
      To   : Element_list.Iterator_type := 
         Upper_bound (Elements, Key (E));
      I    : Element_list.Iterator_type := From;
   begin
      while I /= To loop
         if Location (Element (I).Element) = Location (E) then
            return Better (Element (I).Element, E);
         end if;
         I := Succ (I);
      end loop;
      return false;
   end Contains_better;

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   -- Says the number of elements stored under a given key:
   function Count (Key : in String) return Natural is
   begin
      return Element_list.Count (Elements, Key);
   end Count;

   ------------------------------------------------------------------------
   -- Purge                                                              --
   ------------------------------------------------------------------------
   -- Deletes elements too old
   procedure Purge is
      use Calendar;
      use Element_list;
      I : Iterator_type := First (Elements);
   begin
      while I /= Back (Elements) loop
         if Clock - Element (I).Born > Time_to_live then
            Delete (Elements, I);
         else
            I := Succ (I);
         end if;
      end loop;
      Update_stats;
   end;

   end Object;

end Adagio.Upload.Mesh;
