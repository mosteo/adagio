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
--  $Id: adagio-folder.ads,v 1.3 2004/01/21 21:05:28 Jano Exp $

with Ada.Calendar;      use Ada.Calendar;
with Ada.Finalization;
with Ada.Streams;

with Gnat.Directory_operations;
with Gnat.Os_lib;

with Adagio.File;
with Adagio.Safe_access;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Maps.Sorted.Strings.Unbounded;

use Ada;
use Gnat;

package Adagio.Folder is

   -- Maximum path length (doesn't affect memory usage)
   MAX_LENGTH   : constant:= 1024;

   type Object is tagged private;
   type Object_array is array (integer range <>) of Object;

   Null_folder : constant Object;

   -- Full qualified name for folder:
   function Path    (this : in Object) return String;
   pragma Inline (Path);

   -- Creation:
   procedure Create (
      this   : out Object; 
      Path   : in  String; 
      Shared : in  Boolean := true;
      Rescan : in  Duration := Duration'Last);

   -- Real existence. Says if it really matchs a existing folder
   function Exists (this : in Object) return boolean;

   -- Sharing: 
   procedure Share (this : in out Object; Shared : Boolean := true);
   pragma Inline (Share);

   -- Files:
  
   -- Refresh all contents
   -- Files changed will lost its sha1
   procedure Refresh (this : in out Object);

   -- Has files this folder?
   function Has_files (this : in Object) return boolean;
   pragma Inline (Has_files);

   -- Return an array of files in folder.
   function Files (this : in Object) return File.Object_array;

   -- Return an array of folders under this one.
   function Folders (this : in Object) return Folder.Object_array;

   -- Iterate over files/folders. Return the corresponding null object
   --    when end reached.
   procedure Open_files   (This : in out Object);
   procedure Open_folders (This : in out Object);

   -- Close will be automatically called when end reached
   function  Next_file   (This : in Object) return File.Object;
   function  Next_folder (This : in Object) return Folder.Object;

   -- For closing if not full listing done:
   procedure Close_files   (This : in out Object);
   procedure Close_folders (This : in out Object);

   -- Add a file
   procedure Add_file (this : in out Object; New_file : in File.Object);

   -- Remove
   procedure Remove_file (
      this     : in out Object; 
      Old_file : File.Object);

   -- Check for membership (full qualified name search):
   function Contains (this : in Object; Name : in String) return boolean;

   -- Get a file (exception if not found):
   function Get_file (this : in Object; Name : in String) return File.Object;

   -- Get/Set refresh time:
   function Rescan_period (this : in Object) return Duration;
   procedure Set_rescan_period (
      this : in out Object; Rescan_period : in Duration);
   function Last_scan (this : in Object) return Time;
   procedure Mark_scan (this : in out Object);

   -- Serialize:
   procedure Serialize (
      this   : in Object; 
      Stream : in out Ada.Streams.Root_stream_type'Class);
   procedure Restore (
      this   : out Object; 
      Stream : in out Ada.Streams.Root_stream_type'Class);
      
private

   package File_list is new
      Charles.Maps.Sorted.Strings.Unbounded (
         File.Object, "<", File."=");

   type Folder_object is new Finalization.Limited_controlled with
      record
            -- Full path to file in local filesystem.
         Path      : UString;       
            -- Folder is shared?
         Shared    : Boolean := true;
            -- Files it holds
         Files     : File_list.Container_type;
            -- Last change
         Timestamp : Os_lib.Os_time;

         -- Refresh of folders. If not specified, infinite.
         Rescan_period : Duration := Duration'Last;
         Last_scan : Time := Clock;

         -- For folder/file listing:
         Folder_pos : Gnat.Directory_operations.Dir_type;
         File_pos   : File_list.Iterator_type;
      end record;

   type Folder_access is access all Folder_object;

   procedure Finalize(this : in out Folder_object);

   package Safe_folder is new Safe_access (Folder_object, Folder_access);

   type Object is new Safe_folder.Object with null record;

   function V (this : in Object) return Folder_access;
   pragma Inline (V);

   Null_folder : constant Object :=(Safe_folder.Null_access with null record);
      --Object (Safe_folder.Null_access);

end Adagio.Folder;
