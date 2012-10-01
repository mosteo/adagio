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
--  $Id: adagio-library.adb,v 1.10 2004/01/28 15:33:32 Jano Exp $

With
Adagio.Chronos,
Adagio.Convert,
Adagio.Ed2k,
Adagio.Globals,
Adagio.Misc,
Adagio.Os,
Adagio.Statistics,
Adagio.Statistics.Integers,
Adagio.Statistics.Floats,
Adagio.Statistics.Strings,
Adagio.Trace,
Sha1,
Strings.Fields,
Strings.Utils,
TigerTree,
Acf.Types,
Ada.Exceptions,
Ada.Streams.Stream_IO,
Gnat.Directory_operations,
Gnat.Os_lib;

use
GNAT;

package body Adagio.Library is

   use type Folder_list.Iterator_type;
   use type File_list.Iterator_type;
   use type Folder.Object;
   use type File.Object;

   Stat_library_size : constant String := "Library - Size";

   function V (I : Folder_list.Iterator_type) return Folder.Object
      renames Folder_list.Element;
   function V (I : File_list.Iterator_type) return File.Object
      renames File_list.Element;

   -- Is complete a file?
   -- I.e: has Sha1? Tigertree? Whatever?
   function Is_complete (F : in File.Object) return boolean is
      use type Sha1.Digest;
      use type Ed2k.Hash_type;
      use type TigerTree.Hash_type;
   begin
      return File.Sha (F) /= Sha1.Null_digest and then
             File.Ed2k (F) /= Ed2k.Null_hash and then
             File.TTH (F) /= TigerTree.Null_hash;
   end Is_complete;

   ------------
   -- Object --
   ------------

   protected body Object is
      procedure Check_dropped_TTHs;

      ----------------
      -- Initialize --
      ----------------
      -- Initialization:
      procedure Initialize (Delayed : in Boolean := false) is
         use Ada.Streams.Stream_IO;
         Location: String := S (Globals.Data_folder) & "library.dat";
         F   : File_type;
         T   : Calendar.Time:= Calendar.Clock;
         Dur : Duration;
         Success : Boolean;
         use type Calendar.Time;
      begin
         Delayed_startup := Delayed;

         -- Bind to TTH data:
         Filepack.Bind (TTH_pack, S (Globals.Data_folder) & "tth.dat");

         -- Special keywords
         if Globals.Options.Debug_CrawlerAllowed then
            QRP.Add_keyword (QRP_table, "accioagio");
            if QRP.Is_dirty (QRP_table) then
               QRP_timestamp := Calendar.Clock;
            end if;
         end if;

         if Os_lib.Is_regular_file (Location & ".tmp") and not
            Os_lib.Is_regular_file (Location) then
            Os_lib.Rename_file (Location & ".tmp", Location, Success);
         end if;
         if Os_lib.Is_regular_file(Location) then
            Open(F, Name => Location, Mode => In_file);
            Restore(Stream(F).all);
            Close(F);
         else
            Trace.Log("Library.Initialize: " & Location & " doesn't exists.");
            Dirty:= false;
            Update_statistics;
            return;
         end if;
         Dur := Calendar.Clock - T;
         Trace.Log("Library.Initialize: " & Location & " loaded correctly (" &
            Duration'Image(Dur) & "s).");
         Statistics.Object.Set (
            "Library - Loading time",
            Statistics.Strings.Create (Misc.Image (Dur) & " ~ " &
               Duration'Image (Dur) & "s"));

         Dirty:= Dirty_refresh;

         if not Delayed then
            T := Calendar.Clock;
            loop
               exit when Folder_list.Is_empty (Pending_folders);
               Process_pending_folder;
            end loop;
            Dur := Calendar.Clock - T;
            Trace.Log("Library.Initialize: Refreshing done (" &
               Duration'Image(Dur) & "s).");
            Statistics.Object.Set (
               "Library startup refresh time",
               Statistics.Strings.Create (Misc.Image (Dur) & " ~ " &
                  Duration'Image (Dur) & "s"));
         end if;

         Update_statistics;
      exception
         when E: others =>
            if Is_open(F) then
               Close(F);
               Trace.Log
                 ("Library.Initialize (loading): " & Trace.Report(E),
                  Trace.Error);
            end if;
      end Initialize;

      ----------------
      -- Save       --
      ----------------
      -- Save to disk:
      procedure Save is
         use Ada.Streams.Stream_IO;
         F: File_type;
         T: Calendar.Time:= Calendar.Clock;
         Success: Boolean;
         Location: String:= S (Globals.Data_folder) & "library.dat";
         use type Calendar.Time;
      begin
         if not Dirty then
            return;
         end if;
         -- Try to delete a temp failed:
         if Os_lib.Is_regular_file(Location & ".tmp") then
            Os_lib.Delete_file(Location & ".tmp", Success);
            if not Success then
               Exceptions.Raise_exception
                 (Storage_error'identity, "Cannot delete temp file.");
            end if;
         end if;
         Create(F, Name => Location & ".tmp");
         Serialize(Stream(F).all);
         Close(F);
         -- Delete old library
         if Os_lib.Is_regular_file(Location) then
            Os_lib.Delete_file(Location, Success);
            if not Success then
               Exceptions.Raise_exception
                 (Storage_error'identity, "Cannot delete old library.");
            end if;
         end if;
         Os_lib.Rename_file(Location & ".tmp", Location, Success);
         if not Success then
            Exceptions.Raise_exception
              (Storage_error'identity, "Cannot rename saved temp file.");
         end if;
         Trace.Log("Library.Save: " & Location & " saved correctly (" &
            Duration'Image(Calendar.Clock - T) & "s)");
         Dirty:= false;
      exception
         when E: others =>
            if Is_open(F) then
               Close(F);
            end if;
            Trace.Log("Library.Save: " & Trace.Report(E), Trace.Error);
      end Save;

      ----------------
      -- Contains   --
      ----------------
      -- Is a path already in library?
      function Contains (Path : in String) return boolean is
      begin
         if Path (Path'Last) /= Os.Folder_separator then
            return Contains (Path & Os.Folder_separator);
         end if;
         return Folder_list.Is_in (Path, Folders);
      end Contains;

      ----------------
      -- Add        --
      ----------------
      -- Add a shared folder to library:
      -- Folders always end with path separator
      procedure Add      (
         Path    : in String;
         Success : out boolean;
         Rescan  : Duration := Duration'Last) is
         New_folder : Folder.Object;
         New_path   : String := Directory_operations.Format_pathname (Path,
            Directory_operations.UNIX);
      begin
         if New_Path (New_Path'last) /= Os.Folder_separator then
            Add (New_Path & Os.Folder_separator, Success, Rescan);
            return;
         end if;
         -- Check correctness:
         if not Os_lib.Is_directory (New_Path) then
            Trace.Log("Library.Add " & Path & ": Not regular directory.");
            Success:= false;
            return;
         end if;
         if not Is_Trusted (New_path) then
            Trace.Log("Library.Add " & Path & ": Directory not sharable.");
            Success:= false;
            return;
         end if;
         -- Let's go:
         if not Contains (New_Path) then
            -- Add to library and pending list (initially shared):
            Folder.Create(New_folder, New_Path, Shared => true,
               Rescan => Rescan);
            Folder_list.Insert(Folders, New_path, New_folder);
            Folder_list.Insert(Pending_folders, New_path, New_folder);
            Success:= true;
            Dirty:= true;
         else
            New_folder := Folder_list.Element (
               Folder_list.Find (Folders, New_path));
            Folder.Set_rescan_period (New_folder, Rescan);

            Success:= false;
         end if;

         Update_statistics;
      exception
         when E: others =>
            Success:= false;
            Trace.Log("Library.Add " & New_Path & ": " & Trace.Report(E),
               Trace.Warning);
      end Add;

      ----------------
      -- Remove     --
      ----------------
      -- Remove a folder from library:
      procedure Remove (Path: String; Success: out boolean) is
         New_path: String:= Directory_operations.Format_pathname(Path,
            Directory_operations.UNIX);
      begin
         if New_Path(New_Path'last) /= Os.Folder_separator then
            Remove(New_Path & Os.Folder_separator, Success);
            return;
         end if;
         if not Contains (New_Path) then
            Trace.Log ("Library.Remove " & Path & " not in library.");
            Success:= false;
            return;
         end if;
         declare
            Removed : Folder.Object;
            Result  : Folder_list.Iterator_type :=
               Folder_list.Find (Folders, Path);
            use type Sha1.Digest;
         begin
            -- Find it:
            if not Folder_list.Is_in (Path, Folders) then
               Trace.Log("Library.Remove " & New_Path &
                  " not found!", Trace.Error);
               Success:= false;
               return;
            else
               Removed := V (Result);
            end if;
            -- Remove its files:
            declare
               F : File.Object;
            begin
               Folder.Open_files (Removed);
               loop
                  F := Folder.Next_file (Removed);
                  exit when F = File.Null_file;
                  Remove_file (F);
               end loop;
            end;
            -- Remove from lists:
            Folder_list.Delete (Folders, Path);
            Folder_list.Delete (Pending_folders, Path);
            -- Remove subfolders :
            declare
               F     : Folder.Object;
               Dummy   : boolean;
            begin
               Folder.Open_folders (Removed);
               loop
                  F := Folder.Next_folder (Removed);
                  exit when F = Folder.Null_folder;
                  Remove (Folder.Path (F), Dummy);
               end loop;
            end;
         end;
         Trace.Log("Library.Remove " & New_Path & ": Done.");
         Success:= true;
         Dirty:= true;

         Update_statistics;
      end Remove;

      ----------------
      -- Share      --
      ----------------
      -- Mark a folder as shared:
      procedure Share(Path: String; Success: out boolean) is
         Target   : Folder.Object;
         New_path : String := Directory_operations.Format_pathname(Path,
            Directory_operations.UNIX);
      begin
         if New_Path(New_Path'last) /= Os.Folder_separator then
            Share(New_Path & Os.Folder_separator, Success);
            return;
         end if;
         if not Folder_list.Is_in (New_path, Folders) then
            Trace.Log("Library.Share " & New_Path & ": Not in library.");
            Success:= False;
         else
            Target := V (Folder_list.Find (Folders, New_path));
            Folder.Share (Target);
            Trace.Log    ("Library.Share " & New_Path & ": Done.");
            Success := True;
         end if;
         Dirty:= true;
      end Share;

      ----------------
      -- Unshare    --
      ----------------
      -- Mark a folder as unshared:
      procedure Unshare (Path : in String; Success : out boolean) is
         Target   : Folder.Object;
         New_path : String := Directory_operations.Format_pathname(Path,
            Directory_operations.UNIX);
      begin
         if New_Path (New_Path'last) /= Os.Folder_separator then
            Unshare  (New_Path & Os.Folder_separator, Success);
            return;
         end if;
         if not Folder_list.Is_in (New_path, Folders) then
            Trace.Log("Library.Unshare " & New_Path & ": Not in library.");
            Success:= False;
         else
            Target := V (Folder_list.Find (Folders, New_path));
            Folder.Share (Target, Shared => false);
            Trace.Log("Library.Unshare " & New_Path & ": Done.");
            Success:= True;
         end if;
         Dirty:= true;
      end Unshare;

      -----------
      -- Trust --
      -----------
      procedure Trust (Path : in String) is
      begin
         String_vector.Append (Trusted, U (Path));
      end Trust;

      -------------
      -- Untrust --
      -------------
      procedure Untrust (Path : in String) is
      begin
         String_vector.Append (Untrusted, U (Path));
      end Untrust;

      ----------------
      -- Is_trusted --
      ----------------
      function Is_trusted (Path : in String) return Boolean is
      begin
         for N in 1 .. String_vector.Last (Trusted) loop
            if Misc.Starts (Path, S (Trusted.Vector (N))) then
               -- Trusted parent, but also untrusted parent?
               return not Is_in_untrusted_list (Path);
            end if;
         end loop;
         return false;
      end Is_trusted;

      --------------------------
      -- Is_in_untrusted_list --
      --------------------------
      function Is_in_untrusted_list (Path : in String) return Boolean is
      begin
         for N in 1 .. String_vector.Last (Untrusted) loop
            if Misc.Starts (Path, S (Untrusted.Vector (N))) then
               return true;
            end if;
         end loop;
         return false;
      end Is_in_untrusted_list;

      ----------------
      -- Restore    --
      ----------------
      -- Retrieve from disk:
      procedure Restore(Stream: in out Ada.Streams.Root_stream_type'class) is
         Num: Integer;
         New_folder: Folder.Object;
      begin
         -- Retrieve folders (that includes files)
         Folder_list.Clear (Folders);
         Integer'Read (Stream'access, Num);
         for I in 1 .. Num loop
            Folder.Restore (New_folder, Stream);
            -- Check it exists:
            if Os_lib.Is_directory (Folder.Path (New_folder)) and then
               Is_Trusted (Folder.Path (New_folder))
            then
               -- Add to list of folders
               Folder_list.Insert (
                  Folders, Folder.Path (New_folder), New_folder);
               -- Add to pending folders (for review)
               Folder_list.Insert (
                  Pending_folders, Folder.Path (New_folder), New_folder);
               -- Its files will be added upon pending dir processing
            else
               -- Untrusted folder found in library, purge needed.
               Dirty_refresh := true;
            end if;
         end loop;
      end Restore;

      ----------------
      -- Serialize  --
      ----------------
      -- Dump to disk:
      procedure Serialize(Stream: in out Ada.Streams.Root_stream_type'class)
      is
         I : Folder_list.Iterator_type := Folder_list.First (Folders);
      begin
         -- Dump folders (that includes files)
         Integer'Write (Stream'Access, Folder_list.Length (Folders));
         while I /= Folder_list.Back (Folders) loop
            Folder.Serialize (V (I), Stream);
            I := Folder_list.Succ (I);
         end loop;
      end Serialize;

      ----------------------------
      -- Process_pending_folder --
      ----------------------------
      -- Process a pending folder:
      procedure Process_pending_folder is
         Current : Ustring := U ("No folder acquired");
      begin
         -- If no pending folders, done with it.
         if Folder_list.Is_empty (Pending_folders) then
            if (not Checked_TTH) and then
               File_list.Is_empty (Pending_files)
            then
               Check_dropped_TTHs;
            end if;
            return;
         end if;

         Globals.Main_throttle.Start_work;

         -- Get first pending folder:
         declare
            Item   : Folder.Object := V (Folder_list.First (Pending_folders));
            Dummy  : Boolean;
         begin
            Current := U (Folder.Path (Item));
--          if Current = U ("d:/música/Bandas sonoras - Cine, TV y videojuegos/Dibujos animados/")
--          then
--             Trace.Log ("CRITICAL REACHED", Trace.Error);
--          end if;
            -- Remove from pending:
            Folder_list.Delete(Pending_folders, Folder.Path (Item));
            -- Check valid:
            if not Folder.Exists (Item) then
               Remove (Folder.Path (Item), Dummy);
               Update_statistics;
               Globals.Main_throttle.End_work;
               return;
            end if;
            -- Add its files:
            Folder.Refresh (Item);
            declare
               F : File.Object;
            begin
               Folder.Open_files (Item);
               loop
                  F := Folder.Next_file (Item);
                  exit when F = File.Null_file;
                  Add_file (F);
               end loop;
            end;
            -- Add its subfolders:
            declare
               F       : Folder.Object;
               Success : Boolean;
            begin
               Folder.Open_folders (Item);
               loop
                  F := Folder.Next_folder (Item);
                  exit when F = Folder.Null_folder;
                  Add (Folder.Path (F), Success, Duration'Last);
               end loop;
            end;
         end;
         Update_statistics;
         Globals.Main_throttle.End_work;
      exception
         when E : others =>
            Trace.Log ("Library.Process_pending_folder: " & Trace.Report (E) &
               " (" & S (Current) & ")",
               Trace.Error);
      end Process_pending_folder;

      ----------------------
      -- Get_pending_file --
      ----------------------
      -- Recover a pending (not hashed) file:
      -- It will be removed from pending.
      procedure Get_pending_file(Target : out File.Object) is
      begin
         if File_list.Is_empty (Pending_files) then
            Target := File.Null_file;
         else
            Target := V (File_list.First (Pending_files));
            -- Remove from pending list:
            File_list.Delete (Pending_files, File.Path (Target));
         end if;
         Hashing_file := Target;
      end Get_pending_file;

   ------------------------------------------------------------------------
   -- Add_keywords                                                       --
   ------------------------------------------------------------------------
      -- Extraction of keywords:
      procedure Add_keywords (this : in File.Object) is
         use Gnat.Directory_operations;
         use Strings.Fields;
         Path : String := File.Path (this);
         Name : String := Strings.Utils.Simplify (
            Base_name (Path, File_extension (Path)));
      begin
         --Trace.Log ("Adding keywords for: " & Path);
         for N in 1 .. Count_fields (Name, ' ') loop
            declare
               Word : String := Select_field (Name, N, ' ');
            begin
               if Word'Length >= 3 then
                  for M in Natural'Max (3, Word'Length - 2) .. Word'Length loop
                     -- Trace.Log ("  + Keyword: " &
                       -- Word (Word'First .. Word'First + M - 1));
                     Word_dictionary.Add (
                        Files_by_word,
                        Word (Word'First .. Word'First + M - 1),
                        this);
                     QRP.Add_keyword (
                        QRP_table, Word (Word'First .. Word'First + M - 1));
                     if QRP.Is_dirty (QRP_table) then
                        QRP_timestamp := Calendar.Clock;
                     end if;
                  end loop;
               end if;
            end;
         end loop;
         -- Add hash to table
         QRP.Add_keyword (
            QRP_table, "urn:sha1:" & Sha1.To_base32 (File.Sha (this)));
         QRP.Add_keyword (
            QRP_table, "urn:ed2k:" & Ed2k.Hash_as_hex (File.Ed2k (this)));
      end Add_keywords;

      -- Saves only TTHs not dropped in changes of shared files.
      procedure Check_dropped_TTHs is
         use File_list;
         I : Iterator_type := First (Files_by_TTH);
      begin
         Checked_TTH := true;
         Filepack.Mark_deletable (TTH_pack);
         while I /= Back (Files_by_TTH) loop
            Filepack.Mark_not_deletable (TTH_pack, Key (I));
            I := Succ (I);
         end loop;
         Trace.Log ("Library.Check_dropped_TTHs: Deleting unused TTHs");
         Filepack.Delete_marked (TTH_pack);
         if Filepack.Wasted (TTH_pack) > 0.25 then
            Trace.Log ("Library.Check_dropped_TTHs: Purging TTH database");
            declare
               Cron : Chronos.Object;
            begin
               Filepack.Purge (TTH_pack);
               Trace.Log ("Library.Check_dropped_TTHs: Purged TTH database " &
                  "(Elapsed: " & Chronos.Image (Cron) & ")");
            end;
         end if;
      end Check_dropped_TTHs;

      -- Actions on adding a file:
      procedure Add_file (this : in File.Object) is
         use Gnat.Directory_operations;
         Added : Boolean := false;
      begin
         Globals.Main_throttle.Start_work;
         if Is_complete (this) then
            -- To regular lists:
            if not File_list.Is_in (File.Path (this), Files_by_name) then
               File_list.Insert (Files_by_name, File.Path (this), this);
               -- Keywords when new name:
               Add_keywords (this);
               Dirty := true;
               Added := true;
            end if;
            if not File_list.Is_in (
               Sha1.To_base32 (File.Sha (this)), Files_by_sha1)
            then
               File_list.Insert (Files_by_sha1,
                  Sha1.To_base32 (File.Sha  (this)), this);
               Added := true;
            end if;
            if not File_list.Is_in (
               Ed2k.Hash_as_hex (File.Ed2k (this)), Files_by_ed2k)
            then
               File_list.Insert (Files_by_ed2k,
                  Ed2k.Hash_as_hex (File.Ed2k  (this)), this);
            end if;
            if not File_list.Is_in (
               TigerTree.To_base32 (File.TTH (this)), Files_by_tth)
            then
               File_list.Insert (Files_by_tth,
                  TigerTree.To_base32 (File.TTH (this)), this);
               Add_tigertree (This);
            end if;
            -- Size if freshly added (by hash or name)
            if Added then
               Size_count := Size_count + Natural'(File.Size (this)) / 1024;
            end if;
         else
            File.Free_TTH_bytes (This); -- Necessary to prevent leaks for
                                        -- duplicated files.
            -- To pending!
            if Hashing_file /= File.Null_file and then
               File.Same_file (Hashing_file, This)
            then
               null; -- Nothing to do, is currently being hashed
               Trace.Log ("Library.Add_File: Skipping currently hashing " &
                  "file: " & File.Path (Hashing_file));
            elsif not File_list.Is_in (File.Path (this), Pending_files) then
               File_list.Insert (Pending_files, File.Path (this), this);
            end if;
         end if;

         Update_statistics;
         Globals.Main_throttle.End_work;

      end Add_file;

      -- Save tigertree data.
      -- The hash must be the base32 rep of the root TTH:
      procedure Add_tigertree (F : File.Object) is
         Hash : constant String := TigerTree.To_base32 (File.TTH (F));
      begin
         if File.TTH_Bytes (F)'Length = 0 then -- Happens when reloading files
            return;
         end if;
         if FilePack.Contains (TTH_pack, Hash) then
            FilePack.Delete (TTH_pack, Hash);
         end if;
         FilePack.Create (TTH_pack, Hash);
         Acf.Types.Byte_array'Write (
            FilePack.Stream (TTH_pack), File.TTH_Bytes (F));
         File.Free_TTH_bytes (F);
         FilePack.Close (TTH_pack);
      exception
         when E : others =>
            Trace.Log ("Library.Add_tigertree: " & Trace.Report (E),
               Trace.Error);
            if FilePack.Is_open (TTH_pack) then
               FilePack.Close (TTH_pack);
            end if;
      end Add_tigertree;

      -- Actions on removing a file:
      procedure Remove_file (this : in File.Object) is
      begin
         -- Cancel from lists:
         Word_dictionary.Remove (Files_by_word, this);
         File_list.Delete (Files_by_name, File.Path (this));
         File_list.Delete (Files_by_sha1, Sha1.To_base32 (File.Sha  (this)));
         File_list.Delete (Files_by_ed2k, Ed2k.Hash_as_hex(File.Ed2k (this)));
         File_list.Delete (Files_by_tth, TigerTree.To_base32(File.TTH(this)));
         if FilePack.Contains (TTH_pack, TigerTree.To_base32(File.TTH(this)))
         then
            FilePack.Delete (TTH_pack, TigerTree.To_base32(File.TTH(This)));
         end if;
         -- Discount size
         Size_count :=
            Natural'Min (0, Size_count - Natural'(File.Size (this)) / 1024);

         Update_statistics;
      end Remove_file;

      -- Number of files in library:
      function Num_files return Natural is
      begin
         return File_list.Length (Files_by_sha1);
      end Num_files;

      function Num_pending_files return Natural is
      begin
         return File_list.Length (Pending_files);
      end Num_pending_files;

      -- Number of KBs in library:
      function Size_files return Natural is
      begin
         return Size_count;
      end Size_files;

   ------------------------------------------------------------------------
   -- Get_QRP                                                            --
   ------------------------------------------------------------------------
      function Get_QRP return String is
      begin
         return QRP.To_string (QRP_table);
      end Get_QRP;

   ------------------------------------------------------------------------
   -- Get_QRP_timestamp                                                  --
   ------------------------------------------------------------------------
      function Get_QRP_timestamp return Calendar.Time is
      begin
         return QRP_timestamp;
      end Get_QRP_timestamp;

   ------------------------------------------------------------------------
   -- Get_QRP_ratio                                                      --
   ------------------------------------------------------------------------
      function Get_QRP_ratio return Qrp.Ratios is
      begin
         return QRP.Ratio (QRP_table);
      end Get_QRP_ratio;


   ------------------------------------------------------------------------
   -- Query_word                                                         --
   ------------------------------------------------------------------------
      -- Searches files with that keyword in database.
      -- Word should be a basic charset, number only word without any other
      -- characters (spaces, colons, whatever)
      procedure Query_word (
         Word : in String; Result : in out File_set.Container_type) is
         Results : File.Object_array_access :=
            File.Object_array_access (
               Word_Dictionary.Find (Files_by_word, Word));
      begin
         File_set.Clear (Result);
         for N in Results'Range loop
            File_set.Insert (Result, Results (N));
         end loop;
         File.Free (Results);
      end Query_word;

      function Query_sha1 (Hash : in String) return File.Object is
         H : String := Misc.To_upper (Hash);
      begin
         if File_list.Is_in (H, Files_by_sha1) then
            return File_list.Element (File_list.Find (Files_by_sha1, H));
         else
            return File.Null_file;
         end if;
      end Query_sha1;

      function Query_ed2k (Hash : in String) return File.Object is
         H : String := Misc.To_lower (Hash);
      begin
         if File_list.Is_in (H, Files_by_ed2k) then
            return File_list.Element (File_list.Find (Files_by_ed2k, H));
         else
            return File.Null_file;
         end if;
      end Query_ed2k;

      -- TTH (base 32)
      function Query_TTH  (Hash : in String) return File.Object is
         H : String := Misc.To_upper (Hash);
      begin
         if File_list.Is_in (H, Files_by_TTH) then
            return File_list.Element (File_list.Find (Files_by_TTH, H));
         else
            return File.Null_file;
         end if;
      end Query_TTH;

   ------------------------------------------------------------------------
   -- Refresh_folders                                                    --
   ------------------------------------------------------------------------
      -- Check folders for refresh:
      procedure Refresh_folders is
         use Folder_list;
         use type Calendar.Time;
         I : Iterator_type := First (Folders);
         F : Folder.Object;
      begin
         while I /= Back (Folders) loop
            F := Element (I);
            if Calendar.Clock - Folder.Last_scan (F) >
               Folder.Rescan_period (F)
            then
               if not Is_in (Folder.Path (F), Pending_folders) then
                  Insert (Pending_folders, Folder.Path (F), F);
               end if;
               Folder.Mark_scan (F);
               Trace.Log ("Library.Refresh_folders: Checking changes in " &
                  Folder.Path (F));
            end if;
            I := Succ (I);
         end loop;

         Update_statistics;
      end Refresh_folders;

      -- Mark that a resaving is needed:
      procedure Mark_dirty is
      begin
         Dirty := true;
      end Mark_dirty;

      -- Num of pending folders
      function Count_pending_folders return Natural is
      begin
         return Folder_list.Length (Pending_folders);
      end Count_pending_folders;

      -- Update_statistics
      procedure Update_statistics is
      begin
         Statistics.Object.Set (
            "Library - Files", Statistics.Integers.Create (Num_files));
         Statistics.Object.Set (
            Stat_library_size, Statistics.Strings.Create (
               Convert.To_size (Float (Size_count) * 1024.0)));
         Statistics.Object.Set (
            "Library - QRT %",
         Statistics.Floats.Create (Get_QRP_ratio * 100.0));

         Statistics.Object.Set ("Library - Pending folders",
            Statistics.Integers.Create (
               Folder_list.Length (Pending_folders)));

         Statistics.Object.Set ("Library - Pending files",
            Statistics.Integers.Create (
               File_list.Length (Pending_files)));
      end;

      -- Export a file tigertree from library:
      -- Gives a file opened for read:
      procedure Export_tth (
         this : in  File.Object;
         Data : out Adagio.Streams.Element_array_access) is
         Last   : Ada.Streams.Stream_element_offset;
      begin
         Filepack.Open (
            TTH_pack,
            TigerTree.To_base32 (File.TTH (This)));
         Data := new Ada.Streams.Stream_element_array (
            1 .. Streams.Stream_element_offset (Filepack.Size (TTH_pack)));
         Ada.Streams.Read (Filepack.Stream (TTH_pack).all, Data.all, Last);
         -- Shouldn't happen never if filepack is working right.
         if Natural (Last) /= Filepack.Size (TTH_pack) then
            raise Constraint_error;
         end if;
         Filepack.Close (TTH_pack);
      exception
         when others =>
            if Filepack.Is_open (TTH_pack) then
               Filepack.Close (TTH_pack);
            end if;
            raise;
      end Export_tth;

      -------------------------
      -- Iteration functions --
      -------------------------
      -- First file from library:
      function Get_first_file return File.Object is
         use File_list;
         I : Iterator_type := First (Files_by_name);
      begin
         if I /= Back (Files_by_name) then
            return Element (I);
         else
            return File.Null_file;
         end if;
      end Get_first_file;

      -- Next file. Returns a null_file when finished.
      function Get_next_file (This : in File.Object) return File.Object is
         use File_list;
         I : Iterator_type := Find (Files_by_name, File.Path (This));
      begin
         if I /= Back (Files_by_name) then
            I := Succ (I);
            if I = Back (Files_by_name) then
               return File.Null_file;
            else
               return Element (I);
            end if;
         else
            return File.Null_file;
         end if;
      end Get_next_file;

      -- Get all in a row
      procedure Get_all_files (This : in out File_list.Container_type) is
         use File_list;
         I : File_list.Iterator_type := First (Files_by_name);
      begin
         Clear (This);
         while I /= Back (Files_by_name) loop
            Insert (This, Key (I), Element (I));
            I := Succ (I);
         end loop;
      end Get_all_files;

      -- First folder
      function Get_First_Folder return Folder.Object is
         use Folder_list;
         I : Iterator_type := First (Folders);
      begin
         if I /= Back (Folders) then
            return Element (I);
         else
            return Folder.Null_folder;
         end if;
      end Get_First_Folder;

   end Object;

   -- Deep copy
   procedure Copy (
      To   : in out File_set.Container_type;
      From : in     File_set.Container_type) is
      use File_set;
      I : Iterator_type := First (From);
   begin
      Clear (To);
      while I /= Back (From) loop
         Insert (To, Element (I));
         I := Succ (I);
      end loop;
   end Copy;

end Adagio.Library;
