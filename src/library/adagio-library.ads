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
--  $Id: adagio-library.ads,v 1.9 2004/01/28 15:33:32 Jano Exp $

with Ada.Calendar;
with Ada.Streams;  use Ada;

with Adagio.Dictionary;
with Adagio.File;
with Adagio.Folder;
with Adagio.Globals.Options;
with Adagio.QRP;
with Adagio.Streams;
with Dynamic_vector;
with Filepack;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Maps.Sorted.Strings.Unbounded;
with Charles.Sets.Sorted.Unbounded;

pragma Elaborate_all (Adagio.Dictionary);

package Adagio.Library is

   QRP_Size : Positive renames Globals.Options.Library_QRTSize;

   package Folder_list is new Charles.Maps.Sorted.Strings.Unbounded (
      Folder.Object, "<", Folder."=");
   package File_list   is new Charles.Maps.Sorted.Strings.Unbounded (
      File.Object,   "<", File.Same_file);
   package Word_dictionary is new Dictionary (
      File.Object, File.Object_array, File.Same_file);

   package File_set    is new Charles.Sets.Sorted.Unbounded (
      File.Object, File.Less, File.Same_file);

   package String_vector is new Dynamic_vector (Ustring);

   -- Protected object which encapsulates the library:
   -- The list of files_by_name contains all files, verified or not
   -- Sha1 list contains only files whose sha1 is known
   -- The dictionary will be not saved, but recreated each time
   protected Object is
      -- Load from disk:
      -- If delayed, files/folders will be refreshed after complete loading.
      procedure Initialize (Delayed : Boolean := false);
         
      -- Dump to disk (if necessary):
      procedure Save;

      -- Is a path already in library?
      function  Contains (Path : in String) return boolean;

      -- Add a shared folder to library:
      procedure Add      (
         Path    : in String; 
         Success : out boolean; 
         Rescan  : Duration:= Duration'Last);

      -- Remove a folder from library:
      procedure Remove   (Path : in String; Success : out boolean);

      -- Mark a folder as shared:
      procedure Share    (Path : in String; Success : out boolean);

      -- Mark a folder as unshared:
      procedure Unshare  (Path : in String; Success : out boolean);

      -- Add trusted/untrusted folders:
      procedure Trust (Path : in String);
      procedure Untrust (Path : in String);

      -- Check a path agains trusted/untrusted paths
      -- Untrusted checks for a untrusted parent explicitely,
      -- Trusted checks for trusted parent explicitely and not_untrusted.
      -- Note that Untrusted /= not Trusted for not trusted nor untrusted dirs
      function Is_trusted (Path : in String) return Boolean;
      function Is_in_untrusted_list (Path : in String) return Boolean;
         
      -- Retrieve from disk:
      procedure Restore  (Stream : in out Ada.Streams.Root_stream_type'Class);

      -- Dump to disk:
      procedure Serialize(Stream : in out Ada.Streams.Root_stream_type'Class);

      -- Check folders for refresh:
      procedure Refresh_folders;

      -- Process a pending folder:
      procedure Process_pending_folder;

      -- Recover a pending (not hashed) file:
      -- Removes it from pending list
      -- Last is the last position valid in Path
      -- After processed, use Add_file.
      -- In none pending, returns File.Null_file
      procedure Get_pending_file (Target : out File.Object);

      -- Actions on adding a file:
      -- If it's complete, goes to corresponding lists.
      -- If it's incomplete, goes to pending list.
      -- If there are no more pendings, a check for dropped TTHs will be made.
      procedure Add_file    (this : in File.Object);

      -- Save tigertree data.
      -- The hash must be the base32 rep of the root TTH:
      procedure Add_tigertree (F : in File.Object);

      -- Actions on removing a file:
      procedure Remove_file (this : in File.Object);

      -- Export a file tigertree from library:
      -- Returns a pointer to a fresly allocated stream array with the data.
      -- It should be disposed by the requester.
      procedure Export_tth (
         this : in  File.Object; 
         Data : out Adagio.Streams.Element_array_access);
         
      -- Number of files in library:
      function Num_files return Natural;
      function Num_pending_files return Natural;
      -- Number of KBs in library:
      function Size_files return Natural;

      -- Num of pending folders
      function Count_pending_folders return Natural;

      -- QRP related:
      function Get_QRP return String;
      function Get_QRP_ratio return Qrp.Ratios;
      function Get_QRP_timestamp return Calendar.Time;

      -- Querying:
      -- Searches files with that keyword in database.
      -- Word should be a basic charset, number only word without any other
      -- characters (spaces, colons, whatever)
      procedure Query_word (
         Word : in String; Result : in out File_set.Container_type);

      -- These get the Hash in user-readable format.
      -- Base32
      function Query_sha1 (Hash : in String) return File.Object;
      -- Hex
      function Query_ed2k (Hash : in String) return File.Object;
      -- TTH (base 32)
      function Query_TTH  (Hash : in String) return File.Object;

      -- Mark that a resaving is needed:
      procedure Mark_dirty;

      -- Update_statistics
      procedure Update_statistics;

      -------------------------
      -- Iteration functions --
      -------------------------
      -- First file from library:
      function Get_first_file return File.Object;

      -- Next file. Returns a null_file when finished.
      function Get_next_file (This : in File.Object) return File.Object;

      -- Get all in a row
      procedure Get_all_files (This : in out File_list.Container_type);

      -- First folder
      function Get_First_Folder return Folder.Object;

   private
      -- All hashed folders:
      Folders        : Folder_list.Container_type;
      -- Trusted folders. No folders outside here can be added:
      Trusted        : String_vector.Object (First => 1);
      -- Untrusted folders. They are not shared:
      Untrusted      : String_vector.Object (First => 1);

      -- There are a couple of lists to access the files efficiently:
      Files_by_name  : File_list.Container_type;
      Files_by_sha1  : File_list.Container_type;
      Files_by_ed2k  : File_list.Container_type;
      Files_by_tth   : File_list.Container_type;

      -- And a dictionary to search by keyword efficiently:
      Files_by_word  : Word_Dictionary.Object;

      -- Filepack with TTH data:
      TTH_Pack       : Filepack.Object;

      -- Folders not yet processed:
      Pending_folders: Folder_list.Container_type;

      -- Files not yet hashed:
      Pending_files  : File_list.Container_type;

      -- The currently hashing file:
      -- This is used to prevent the addition of the file in a refresh
      --    while it's being hashed.
      Hashing_file   : File.Object := File.Null_File;

      -- QRP table:
      QRP_table      : Qrp.Table (QRP_size, 2 ** QRP_size - 1); 
      QRP_timestamp  : Calendar.Time := Calendar.Clock;

      -- Counter of sizes of files (in KB):
      Size_count     : Natural := 0;

      -- Dirty signal to dump to disk:
      Dirty          : Boolean := false;
      Dirty_refresh  : Boolean := false;

      -- Delayed startup. To avoid throttles:
      Delayed_startup : Boolean := false;

      -- To check tths only one time:
      Checked_TTH    : Boolean := false;
   end Object;

private

   -- Deep copy
   procedure Copy (
      To   : in out File_set.Container_type; 
      From : in     File_set.Container_type);

end Adagio.Library;
