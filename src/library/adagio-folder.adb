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
--  $Id: adagio-folder.adb,v 1.7 2004/01/21 21:05:28 Jano Exp $

with Adagio.Trace;
with Dynamic_vector;

with Gnat.Directory_operations;
with Gnat.Os_lib;

use Gnat;

package body Adagio.Folder is

   use type File_list.Iterator_type;

   package GDO renames Gnat.Directory_operations;

   package Folder_vector is new Dynamic_vector (Object);
   package File_vector is new Dynamic_vector (File.Object);

   -- Quick access
   function V (this : in Object) return Folder_access is
   begin
      return Safe_folder.Value (Safe_folder.Object (this));
   end V;

   -- Full qualified name for folder:
   function Path (this : in Object) return String is
   begin
      return S (V (this).Path);
   end Path;

   -- Creation:
   procedure Create (
      this   : out Object;
      Path   : in  String;
      Shared : in  Boolean := true;
      Rescan : in Duration:= Duration'Last) is

      F : Folder_access;
   begin
      if Path'Length > MAX_LENGTH then
         raise Constraint_error;
      end if;
      F := new Folder_object;
      Safe_folder.Bind (Safe_folder.Object (this), F);

      F.Path   := U (GDO.Format_pathname (Path, GDO.Unix));
      F.Shared := Shared;
      F.Rescan_period := Rescan;
      -- Timestamp:
      begin
         F.Timestamp := Os_lib.File_time_stamp (Path);
      exception
         when E: others =>
            Trace.Log("Folder.Create " & Path & " searching timestamp: " &
            Trace.Report(E));
      end;
   end Create;

   -- Real existence. Says if it really matchs a existing folder
   function Exists (this : in Object) return boolean is
   begin
      return Os_lib.Is_directory (Path (this));
   end Exists;

   -- Sharing:
   procedure Share (this: in out Object; Shared : in Boolean := true) is
   begin
      V (this).Shared := Shared;
   end Share;

   -- Refresh all contents
   -- Files changed will lost its sha1
   procedure Refresh (this : in out Object) is

      Dir  : GDO.Dir_type;
      Name : String (1 .. MAX_LENGTH);
      Last : Natural;
      New_file : File.Object;

   begin
      -- Check files already known:
      declare
         I : File_list.Iterator_type := File_list.First (V (this).Files);
         J : File_list.Iterator_type;
      begin
         while I /= File_list.Back (V (this).Files) loop
            if not Os_lib.Is_regular_file (File.Path (File_list.Element (I)))
            then
               -- Remove a non-existent file:
               J := File_list.Succ (I);
               File_list.Delete (V (this).Files, I);
               I := J;
            else
               I := File_list.Succ (I);
            end if;
         end loop;
      end;

      -- Check files in physical path:
      GDO.Open (Dir, Path (this));
      loop
         GDO.Read (Dir, Name, Last);
         exit when Last = 0;
         declare
            N : String := Name (Name'First .. Last);
         begin
            delay 0.001;
            if N /= "." and then N /= ".." then
               if Os_lib.Is_directory (Path (this) & N) then
                  null; -- For the moment, we do nothing with it
               else
                  -- Regular file, process it:
                  if Contains (this, Path (this) & N) then
                     -- Changed timestamp?
                     declare
                        Old_file : File.Object := Get_file (
                           this, Path (this) & N);
                        use type Os_lib.Os_time;
                     begin
                        if Os_lib.File_time_stamp (Path (this) & N) /=
                           File.Timestamp (Old_file) then
                           -- Reset sha1
                           File.Reset (Old_file);
                        end if;
                     exception
                        when E : others =>
                           -- If any error, remove the file:
                           Trace.Log ("Folder.Refresh: Removed " & N &
                              " because: " & Trace.Report (E));
                           Remove_file (this, Old_file);
                     end;
                  else
                     -- Simply add it:
                     New_file := File.Create (Path (this) & N);
                     File.Refresh (New_file);
                     Add_file (this, New_file);
                  end if;
               end if;
            end if;
         exception
            when E : others =>
               Trace.Log ("Folder.Refresh: " & Trace.Report (E));
         end;
      end loop;
      GDO.Close (Dir);
   end Refresh;

   -- Has files this folder?
   function Has_files (this : in Object) return boolean is
   begin
      return not File_list.Is_empty (V (this).Files);
   end Has_files;

   -- Files:
   function Files (this : in Object) return File.Object_array is
      Result : File_vector.Object (1);
      I      : File_list.Iterator_type := File_list.First (V (this).Files);
   begin
      while I /= File_list.Back (V (this).Files) loop
         File_vector.Append (Result, File_list.Element (I));
         I := File_list.Succ (I);
      end loop;

      return
         File.Object_array (Result.Vector (1 .. File_vector.Last (Result)));
   end Files;

   -- Folders
   function Folders (this : in Object) return Folder.Object_array is
      Result : Folder_vector.Object (1);
      Dir    : GDO.Dir_type;
      Name   : String (1 .. MAX_LENGTH);
      Last   : Integer;
      New_dir: Object;
   begin
      GDO.Open (Dir, Path (this));
      loop
         GDO.Read (Dir, Name, Last);
         exit when Last = 0;
         begin
            if Name (1 .. Last) /= "." and then
               Name (1 .. Last) /= ".." and then
               Os_lib.Is_directory (Path (this) & Name (1 .. Last))
            then
               Create (New_dir, Path (this) & Name (1 .. Last));
               Folder_vector.Append (Result, New_dir);
            end if;
         exception
            when E : others =>
               Trace.Log ("Folder.Folders: " & Trace.Report (E),
                  Trace.Warning);
         end;
      end loop;
      GDO.Close (Dir);
      return Object_array (Result.Vector (1 .. Folder_vector.Last (Result)));
   end Folders;

   -- Iterate over files/folders. Return the corresponding null object
   --    when end reached.
   procedure Open_files   (This : in out Object) is
      F : Folder_access renames V (This);
   begin
      F.File_pos := File_list.First (F.Files);
   end Open_files;

   procedure Open_folders (This : in out Object) is
   begin
      GDO.Open (V (This).Folder_pos, Path (this));
   end Open_folders;

   -- Close will be automatically called when end reached
   function  Next_file   (This : in Object) return File.Object is
      F : Folder_access renames V (This);
      use File_list;
      result : File.Object;
   begin
      if F.File_pos /= Back (F.Files) then
         Result     := Element (F.File_pos);
         F.File_pos := Succ (F.File_pos);
         return Result;
      else
         return File.Null_file;
      end if;
   end Next_file;

   function  Next_folder (This : in Object) return Folder.Object is
      Name   : String (1 .. MAX_LENGTH);
      Last   : Integer;
      F      : Folder_access renames V (This);
   begin
      loop -- Will break when ok file found.
         GDO.Read (F.Folder_pos, Name, Last);
         if Last = 0 then
            GDO.Close (V (This).Folder_pos);
            return Null_folder;
         else
            declare
               Result : Object;
            begin
               if Name (1 .. Last) /= "." and then
                  Name (1 .. Last) /= ".." and then
                  Os_lib.Is_directory (Path (this) & Name (1 .. Last))
               then
                  Create (Result, Path (this) & Name (1 .. Last));
                  return Result;
               else
                  -- Must check next entry since this isn't a folder.
                  null;
               end if;
            exception
               when E : others =>
                  Trace.Log ("Folder.Folders: " & Trace.Report (E),
                     Trace.Warning);
                  GDO.Close (V (This).Folder_pos);
                  raise;
            end;
         end if;
      end loop;
   end Next_folder;

   -- For closing if not full listing done:
   procedure Close_files   (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Close_files;

   procedure Close_folders (This : in out Object) is
   begin
      GDO.Close (V (This).Folder_pos);
   end Close_folders;

   -- Add a file
   procedure Add_file(this: in out Object; New_file : in File.Object) is
   begin
      File_list.Insert (V (this).Files, File.Path (New_file), New_file);
   end Add_file;

   -- Remove
   procedure Remove_file(this : in out Object; Old_file : File.Object) is
   begin
      File_list.Delete (V (this).Files, File.Path (Old_file));
   end Remove_file;

    -- Check for membership:
   function Contains(this : in Object; Name : in String) return boolean is
   begin
      return File_list.Is_in (Name, V (this).Files);
   end Contains;

   -- Get a file (exception if not found):
   function Get_file(this : in Object; Name : in String) return File.Object is
   begin
      return File_list.Element (File_list.Find (V (this).Files, Name));
   end Get_file;

   -- Serialize to stream:
   procedure Serialize
     (this   : in Object;
      Stream : in out Ada.Streams.Root_stream_type'Class) is

      I : File_list.Iterator_type := File_list.First (V (this).Files);

   begin
      String'Output         (Stream'access, Path (this));
      Boolean'Output        (Stream'access, V (this).Shared);
      Os_lib.Os_time'Output (Stream'access, V (this).Timestamp);
      -- Files
      Integer'Write (Stream'access, File_list.Length (V (this).Files));
      while I /= File_list.Back (V (this).Files) loop
         File.Serialize (File_list.Element (I), Stream);
         I := File_list.Succ (I);
      end loop;
   end Serialize;

   procedure Restore (
      this   : out Object;
      Stream : in out Ada.Streams.Root_stream_type'Class) is

      Num      : Integer;
      New_file : File.Object;
   begin
      declare
         S : String := String'Input          (Stream'Access);
      begin
         Create (this, S);
      end;
      V (this).Shared    := Boolean'Input        (Stream'Access);
      V (this).Timestamp := Os_lib.Os_time'Input (Stream'Access);
      -- Files
      File_list.Clear (V (this).Files);
      Integer'Read    (Stream'Access, Num);
      for I in 1 .. Num loop
         -- Read file
         File.Restore (New_file, Stream);
         -- Insert into list
         File_list.Insert (V (this).Files, File.Path (New_file), New_file);
      end loop;
   end Restore;

   -- Finalization
   -- Let's delete the file list
   procedure Finalize(this : in out Folder_object) is
   begin
      File_list.Clear (this.Files);
   end Finalize;

   -- Get refresh time:
   function Rescan_period (this : in Object) return Duration is
   begin
      return V (this).Rescan_period;
   end Rescan_period;

   procedure Set_rescan_period (
      this : in out Object; Rescan_period : Duration) is
   begin
      V (this).Rescan_period := Rescan_period;
   end Set_rescan_period;

   function Last_scan (this : in Object) return Time is
   begin
      return V (this).Last_scan;
   end Last_scan;

   procedure Mark_scan (this : in out Object) is
   begin
      V (this).Last_scan := Clock;
   end Mark_scan;

end Adagio.Folder;
