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

--  Facilities to pack separate entities as a single file.

with Charles.Maps.Sorted.Strings.Unbounded;

with Ada.Streams;             use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package Filepack is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   File_already_exists : exception;
   File_not_found      : exception; -- Refers to internal files
   File_not_open       : exception;
   Index_out_of_bounds : exception; -- For seeks
   Filepack_not_bound  : exception; 
   Access_mode_error   : exception; -- For stream read/writes
   Unknown_error       : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is limited private;

   ------------------------------------------------------------------------
   -- Bind                                                               --
   ------------------------------------------------------------------------
   -- Opens or creates a filepack. It's opened always in Append mode.
   procedure Bind (This : out Object; Name : in String);

   ------------------------------------------------------------------------
   -- Unbind                                                             --
   ------------------------------------------------------------------------
   -- Cuts the link with a filepack.
   procedure Unbind (This : in out Object);

   ------------------------------------------------------------------------
   -- Open                                                               --
   ------------------------------------------------------------------------
   -- Opens a file inside the filepack. May raise File_not_found
   -- Opening a new file don't need to close the previous.
   procedure Open (This : in out Object; Name : in String);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Create a new file inside the filepack.
   -- May raise File_already_exists.
   -- The created file remains opened.
   procedure Create (This : in out Object; Name : in String);

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   -- Marks the opened file as deleted.
   procedure Delete (This : in out Object);

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   -- Marks the named file as deleted.
   procedure Delete (This : in out Object; Name : in String);

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Commits changes in a file opened for writing. 
   procedure Close (This : in out Object);

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a certain file is in the filepack
   function Contains (This : in Object; Name : in String) return Boolean;
      
   ------------------------------------------------------------------------
   -- Is_open                                                            --
   ------------------------------------------------------------------------
   -- Says if some file is open
   function Is_open (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   -- Sets the starting position. A file must have been opened.
   -- May raise Index_out_of_bounds or File_not_open
   -- Index starts at 1
   procedure Set_index (This : in out Object; Index : in Positive);

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Gets the size of the currently opened file (not the filepack)
   -- May raise File_not_open
   function Size (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- End_of_file                                                        --
   ------------------------------------------------------------------------
   -- Says if the current opened underlying file has reached EOF
   function End_of_file (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Gets an stream for the current opened file.
   -- If the file was opened, only read is allowed.
   -- If the file was created, only writing is allowed.
   function Stream (This : in Object) return 
      Ada.Streams.Stream_IO.Stream_access;

   ------------------------------------------------------------------------
   -- Wasted                                                             --
   ------------------------------------------------------------------------
   -- Returns the per one wasted proportion due to deleted files
   subtype Wasted_percent is Float range 0.0 .. 1.0;
   function Wasted (This : in Object) return Wasted_percent;

   ------------------------------------------------------------------------
   -- Purge                                                              --
   ------------------------------------------------------------------------
   -- Recreates the filepack to achieve a 0.0 of wasted space.
   -- The current file will be closed.
   -- The filepack must be bound, and so will remain.
   procedure Purge (This : in out Object);

   ------------------------------------------------------------------------
   -- Mark_deletable                                                     --
   ------------------------------------------------------------------------
   -- To do a selective purge, this procedure marks files as deletables.
   -- No name implies all files.
   -- See Mark_not_deletable
   procedure Mark_deletable (This : in out Object; Name : in String := "");

   ------------------------------------------------------------------------
   -- Mark_not_deletable                                                 --
   ------------------------------------------------------------------------
   -- Marks a file (or all) as not candidate for deleting
   procedure Mark_not_deletable (
      This : in out Object; Name : in String := "");

   ------------------------------------------------------------------------
   -- Delete_marked                                                      --
   ------------------------------------------------------------------------
   -- This effectively deletes all marked files.
   procedure Delete_marked (This : in out Object);

   ------------------------------------------------------------------------
   -- Export                                                             --
   ------------------------------------------------------------------------
   -- Exports a file from the filepack, giving an open for read File_type.
   -- If no name is supplied, the file will be temporary.
   procedure Export (
      This    : in out Object; 
      Name    : in     String; 
      To      : in out Stream_io.File_type;
      To_name : in String := "");

private

   -- The filepack is a sequence of File_entries + data. The format is as
   -- follows:
   --   <Name><Len><Deleted><Data>
   -- where Name is Unbounded_String, Len is Natural and Deleted is Boolean.
   -- Upon binding, a sorted index is created. Files deleted aren't indexed.
   -- When a file is deleted, it's marked as so and their entry removed from
   -- the map.


   package ASU renames Ada.Strings.Unbounded;

   -- For the open file:
   type Open_modes is (Read, Write, Both);

   -- Records indexing the filepack:
   type File_entry is record
      Name          : ASU.Unbounded_string;
      Size          : Natural;
      Deleted       : Boolean;
      Deletable     : Boolean := false; -- For batch deletes.
      Offset        : Positive; -- Starts at 1, refers to true data
      Header_offset : Positive; -- refers to start of header
   end record;

   package Entry_map is new
      Charles.Maps.Sorted.Strings.Unbounded (File_entry, "<", "=");

   type Stream_type (Parent : access Object) is new
      Ada.Streams.Root_stream_type with null record;

   -- Overriden primitives:
   procedure Read(
      Stream : in out Stream_type;
      Item   : out    Stream_Element_Array;
      Last   : out    Stream_Element_Offset);

	procedure Write(
      Stream : in out Stream_type;
      Item   : in     Stream_Element_Array);

   type Object is limited record
      Name        : ASU.Unbounded_string; -- Filepack filesystem path

      Bound       : Boolean; -- For the filepack
      Found       : Boolean; -- For the inside file

      F           : Stream_IO.File_type;  -- Underlying filepack
      FS          : Stream_IO.Stream_access;
      Stream      : aliased Stream_type (Object'Access);
      Stream_access : Stream_IO.Stream_access;

      Mode        : Open_modes; -- Of the file inside the filepack

      Total_data  : Natural := 0; -- Doesn't account for headers.
      Wasted_data : Natural := 0;

      Index       : Entry_map.Container_type;
      Current     : File_entry;
      Remaining   : Natural; -- Of the current inside file
   end record;

end Filepack;
