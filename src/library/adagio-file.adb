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
--  $Id: adagio-file.adb,v 1.9 2004/01/29 21:47:08 Jano Exp $

with Adagio.Chronos;
with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.Globals.Options;
with Adagio.Buffered_stream;
with Adagio.Convert;
with Adagio.Decoupled_file_stream;
with Adagio.Misc;
with Adagio.Statistics;
with Adagio.Statistics.Strings;
with Adagio.Throttle_stream;
with Adagio.Throttler;
with Adagio.TTree;
with Adagio.Trace;
with Sha1.Bytes;
with Sha1.Files;
with Sha1.Streams;

with Acf.Hash.Algorithms.Ed2k;
with Acf.Hash.Algorithms.MD4;

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams.Stream_IO;      use Ada.Streams;
with Ada.Unchecked_deallocation;


with Gnat.Directory_operations;

package body Adagio.File is

   TTH_levels : Natural renames Globals.Options.Library_TTHSize;

   Stat_hashing_bytes : constant String :=
      "Library - Currently hashing bytes";
   Stat_hashing_speed : constant String :=
      "Library - Currently hashing average speed";

   Open_form : constant String := ""; -- "shared=yes";
   -- It's evident that "shared" reduces throughput by 2,
   -- so let's leave it out.

   procedure Free is new Unchecked_deallocation (
      Acf.Types.Byte_array, Byte_array_access);

   -- Quick access:
   function V (this : in Object) return File_access is
   begin
      return Safe_file.Value (Safe_file.Object (this));
   end V;

   -- Creates a file with a given path. No checks about real existence.
   function Create (Path : in String) return Object is
      F      : File_access := new File_object;
      Result : Safe_file.Object;
      package GDO renames Gnat.Directory_operations;
   begin
      F.Path := U (GDO.Format_pathname (Path, GDO.UNIX));
      Safe_file.Bind (Result, F);
      return (Result with null record);
   end Create;

   -- Full qualified name for file:
   function Path (this : in Object) return String is
   begin
      return To_string (V (this).Path);
   end Path;

   -- Name without path:
   function Name (this : Object) return String is
   begin
      return Directory_operations.Base_name (Path (this));
   end Name;

   -- Path without name (trailing /):
   function Dir_name (this : Object) return String is
      package GDO renames Gnat.Directory_operations;
   begin
      return GDO.Dir_name (Path (This));
   end Dir_name;

   -- Sha1 for file in canonical form:
   function Sha (this : in Object) return Sha1.Digest is
      use type Sha1.Digest;
   begin
      return V (this).Sha;
   end Sha;

   -- Ed2k hash:
   function Ed2k (this : Object)
      return Acf.Hash.Message_digests.Message_digest is
   begin
      return V (this).ed2k;
   end Ed2k;

   procedure Sha (this: in out Object; Hash : in Sha1.Digest) is
   begin
      V (this).Sha := Hash;
   end Sha;

   function TTH (this : Object) return TigerTree.Hash_type is
   begin
      return V (this).TTH;
   end TTH;

   function TTH_bytes (This : Object) return Acf.Types.Byte_array is
      Dummy : Acf.Types.Byte_array (1 .. 0);
   begin
      if V (This).TTH_bytes = null then
         return Dummy;
      else
         return V (This).TTH_bytes.all;
      end if;
   end TTH_bytes;

   procedure Free_TTH_bytes (This : Object) is
   begin
      Free (V (This).TTH_Bytes);
   end Free_TTH_bytes;

   -- Refresh the timestamp
   -- Raises file_not_found otherwise.
   procedure Refresh (this : in out Object) is
      F : Stream_IO.File_type;
   begin
      -- Timestamp
      if Os_lib.Is_regular_file (Path (this)) then
         V (this).Timestamp:= Os_lib.File_time_stamp (Path(this));
      else
         Ada.Exceptions.Raise_exception (File_not_found'Identity, Path (this));
      end if;

      -- Size
      if Os_lib.Is_regular_file (Path (this)) then
         Stream_IO.Open (F, Stream_IO.In_file, Path (this), Open_form);
         V (this).Size := File_size (Stream_IO.Size(F));
         Stream_IO.Close (F);
      else
         Ada.Exceptions.Raise_exception (File_not_found'Identity, Path (this));
      end if;
   end Refresh;

   -- Get
   function Timestamp (this : in Object) return Os_lib.Os_time is
   begin
      return V (this).Timestamp;
   end Timestamp;

   -- Set
   procedure Timestamp (this : in out Object; Timestamp: Os_lib.Os_time) is
   begin
      V (this).Timestamp:= Timestamp;
   end Timestamp;

   -- Reset computable values (sha, tigertree, etc...)
   procedure Reset (this : in out Object) is
   begin
      Sha (this, Sha1.Null_digest);
      V (this).Ed2k := Adagio.Ed2k.Null_hash;
   end Reset;

   -- Serialize:
   procedure Serialize(
      this   : in Object;
      Stream : in out Ada.Streams.Root_stream_type'Class) is

      C : Throttler.Controller (Globals.Main_throttle'access);
      pragma Unreferenced (C);
      Has_ed2k : Boolean;
      Has_tth  : Boolean;
      use type Adagio.Ed2k.Hash_type;
      use type TigerTree.Hash_type;
   begin
      String'Output         (Stream'access, Path      (this));
      Sha1.Digest'Write     (Stream'access, Sha       (this));
      Has_ed2k := Ed2k (this) /= Adagio.Ed2k.Null_hash;
      if Has_ed2k then
         Acf.Types.Byte_array'Write (Stream'access,
            Acf.Hash.Message_digests.To_byte_array (Ed2k (this)));
      else
         Acf.Types.Byte_array'Write (Stream'access,
            Adagio.Ed2k.Null_hash_as_bytes);
      end if;
      Has_tth  := TTH  (this) /= TigerTree.Null_hash;
      if Has_TTH then
         Acf.Types.Byte_array'Write (Stream'access,
            TigerTree.To_byte_array (TTH (this)));
      else
         Acf.Types.Byte_array'Write (Stream'access,
            TigerTree.Null_hash_as_bytes);
      end if;
      Os_lib.Os_time'Output (Stream'access, Timestamp (this));
      Boolean'Output        (Stream'access, V (this).Shared);
      Boolean'Output        (Stream'access, V (this).Folder_shared);
      Long_long_integer'Output (Stream'access,
         Long_long_integer (Natural'(Size (this))));
      Natural'Output        (Stream'access, V (this).Uploads);
      Natural'Output        (Stream'access, V (this).Hits_total);
   end Serialize;

   -- Restore
   procedure Restore(
      this   : out Object;
      Stream : in out Ada.Streams.Root_stream_type'Class) is

      C: Throttler.Controller(Globals.Main_throttle'access);
      pragma Unreferenced (C);
   begin
      declare
         S : String := String'Input (Stream'Access);
      begin
         this := Create (S);  -- From path
      end;
      Sha1.Digest'Read (Stream'access, V (this).Sha);
      declare
         B : Acf.Types.Byte_array (1 .. 16);
         use type Acf.Types.Byte_array;
      begin
         Acf.Types.Byte_array'Read (Stream'access, B);
         if B /= Adagio.Ed2k.Null_hash_as_bytes then
            V (this).Ed2k := Acf.Hash.Message_digests.To_message_digest(B);
         end if;
      end;
      declare
         B : Acf.Types.Byte_array (1 .. 24);
         use type Acf.Types.Byte_array;
      begin
         Acf.Types.Byte_array'Read (Stream'access, B);
         if B /= TigerTree.Null_hash_as_bytes then
            V (this).TTH := TigerTree.To_Hash (B);
         end if;
      end;
      Timestamp (this, Os_lib.Os_time'Input (Stream'access));
      V (this).Shared        := Boolean'input (Stream'access);
      V (this).Folder_shared := Boolean'input (Stream'access);
      V (this).Size    := File_size (Long_long_integer'Input (Stream'access));
      V (this).Uploads       := Natural'Input (Stream'access);
      V (This).TTH_bytes     := null;
      V (this).Hits_total    := Natural'Input (Stream'access);
   end Restore;

   -- Same file, by path:
   function Same_file (L, R : Object) return Boolean is
   begin
      return Path (L) = Path (R);
   end Same_file;

   -- Less path
   function Less (L, R : Object) return Boolean is
   begin
      return Path (L) < Path (R);
   end Less;

   -- Is Null?
   function Is_null (this : in Object) return Boolean is
   begin
      return Safe_file.Is_null (Safe_file.Object (this));
   end Is_null;

   -- Size
   function Size (this : in Object) return File_size is
   begin
      return V (this).Size;
   end Size;

   function Size (this : in Object) return Natural is
   begin
      return Natural (V (this).Size);
   end Size;

   -- Shared
   function Shared (this : in Object) return Boolean is
   begin
      return  V (this).Shared;
   end Shared;

   function Folder_shared (this : in Object) return Boolean is
   begin
      return V (this).Folder_shared;
   end Folder_shared;

   -- Uploads
   function Uploads (this : in Object) return Natural is
   begin
      return V (this).Uploads;
   end Uploads;
   function Uploads_session (this : in Object) return Natural is
   begin
      return V (this).Uploads_session;
   end Uploads_session;

   -- Hits
   function Hits_total (This : in Object) return Natural is
   begin
      return V (this).Hits_total;
   end Hits_total;
   function Hits_session (This : in Object) return Natural is
   begin
      return V (this).Hits_session;
   end Hits_session;

   procedure Compute_sha (this : in out Object; Speed : Hash.Hash_speeds) is
      bs: aliased Buffered_stream.Buffered_stream (64 * 1024);
      ds: aliased Decoupled_file_stream.Decoupled_file_stream;
      ts: aliased Throttle_stream.Throttle_stream
                       (Globals.Hash_throttle'Access);
      use Hash;
   begin
      if Speed = Fast then
         V (this).Sha := Sha1.Files.Hash (File.Path (this));
      else
         Decoupled_file_stream.Get_decoupled_file_stream (
            ds, File.Path (this));
         if Speed = Normal then
            Buffered_stream.Get_buffered_stream(bs, ds'Unchecked_Access);
            V (this).Sha := Sha1.Streams.Hash (bs'Unrestricted_access,
               Sha1.Message_length (Decoupled_file_stream.Size (ds)));
         elsif Speed = Slow then
            Buffered_stream.Get_buffered_stream (bs, ds'Unchecked_Access);
            Throttle_stream.Get_throttle_stream (ts, bs'Unchecked_Access);
            V (this).Sha := Sha1.Streams.Hash
              (ts'Unrestricted_access,
               Sha1.Message_length (Decoupled_file_stream.Size (ds)));
         else
            raise Unimplemented;
         end if;
      end if;
   end Compute_sha;

   procedure Compute_ed2k (this : in out Object; Speed : Hash.Hash_speeds) is
      package MD4 renames Acf.Hash.Algorithms.MD4;
      bs: aliased Buffered_stream.Buffered_stream (64 * 1024);
      ds: aliased Decoupled_file_stream.Decoupled_file_stream;
      ts: aliased Throttle_stream.Throttle_stream
                       (Globals.Hash_throttle'Access);
      use Stream_IO;
      fs: File_type;

      Source : Stream_access;

      Main_context    : aliased MD4.MD4_Context;
      Partial_context : aliased MD4.MD4_context;
      Partial_hash    : Acf.Hash.Message_digests.Message_digest;

      File_size       : Stream_io.Count;
      File_pos        : Stream_io.Count;
      Chunk_size      : Stream_io.Count;
      -- Progress within current ed2k block:
      Block_done      : Stream_io.Count;

      MD4_Block_bytes : constant := 64;

      use type Hash.Hash_speeds;
   begin
      -- Get file size:
      Open (
         fs, Name => File.Path (this), Mode => In_file, Form => Open_form);
      File_size := Size (fs);

      -- Setup streams:
      if Speed = Hash.fast then
         Source := Stream_access (Stream (fs));
      else
         Close (fs);
         Decoupled_file_stream.Get_decoupled_file_stream (
            ds, File.Path (this));
         if Speed = Hash.Normal then
            Buffered_stream.Get_buffered_stream(bs, ds'Unchecked_Access);
            Source := bs'Unchecked_access;
         elsif Speed = Hash.Slow then
            Buffered_stream.Get_buffered_stream (bs, ds'Unchecked_Access);
            Throttle_stream.Get_throttle_stream (ts, bs'Unchecked_Access);
            Source := ts'Unchecked_access;
         else
            raise Unimplemented;
         end if;
      end if;

      MD4.Hash_start (Main_context'Access);
      File_pos := 1;

      MD4.Hash_start (Partial_context'Access);
      Block_done := 0;

      loop
         -- We'll iterate over the fragment, taking full MD4 blocks until
         --    we reach the end.
         Chunk_size := Count'Min (MD4_block_bytes, File_size - File_pos + 1);
         declare
            Chunk : Acf.Types.Byte_array (1 .. Integer (Chunk_size));
         begin
            Acf.Types.Byte_array'Read (Source, Chunk);
            MD4.Hash_update (Partial_context'Access, Chunk);
            File_pos   := File_pos + Chunk_size;
            Block_done := Block_done + Chunk_size;
         end;
         -- End of ed2k block?
         if Block_done = Adagio.Ed2k.Hash_block_size or else
            File_pos > File_size
         then
            Partial_hash := MD4.Hash_end (Partial_context'Access);
            MD4.Hash_update (Main_context'Access,
               Acf.Hash.Message_digests.To_byte_array (Partial_hash));
            Block_done := 0;
            MD4.Hash_start (Partial_context'Access);
            exit when File_pos > File_size;
         end if;
      end loop;

      -- Take final ed2k hash:
      if File_size <= Adagio.Ed2k.Hash_block_size then
         V (This).ed2k := Partial_hash;
      else
         V (This).ed2k := MD4.Hash_end (Main_context'Access);
      end if;

      -- Finish:
      if Is_open (fs) then
         Close (fs);
      end if;
   exception
      when E : others =>
         if Is_open (fs) then
            Close (fs);
         end if;
         Trace.Log ("File.Compute_ed2k: " & Trace.Report (E), Trace.Error);
   end Compute_ed2k;

   procedure Compute_TTH (this : in out Object; Speed : Hash.Hash_speeds) is
      bs: aliased Buffered_stream.Buffered_stream (64 * 1024);
      ds: aliased Decoupled_file_stream.Decoupled_file_stream;
      ts: aliased Throttle_stream.Throttle_stream
                       (Globals.Hash_throttle'Access);
      use Stream_IO;
      fs: File_type;

      Source          : Stream_access;
      File_size       : Stream_io.Count;
      Tree            : TTree.Object;

      use type Hash.Hash_speeds;
      Pos   : Count := 1;
      Last  : Integer;
      Bytes : Byte_array_access := new Acf.Types.Byte_array (1 .. 1024);
   begin
      -- Get file size:
      Open (
         fs, Name => File.Path (this), Mode => In_file, Form => Open_form);
      File_size := Size (fs);

      -- Setup streams:
      if Speed = Hash.fast then
         Source := Stream_access (Stream (fs));
      else
         Close (fs);
         Decoupled_file_stream.Get_decoupled_file_stream (
            ds, File.Path (this));
         if Speed = Hash.Normal then
            Buffered_stream.Get_buffered_stream(bs, ds'Unchecked_Access);
            Source := bs'Unchecked_access;
         elsif Speed = Hash.Slow then
            Buffered_stream.Get_buffered_stream (bs, ds'Unchecked_Access);
            Throttle_stream.Get_throttle_stream (ts, bs'Unchecked_Access);
            Source := ts'Unchecked_access;
         else
            raise Unimplemented;
         end if;
      end if;

      -- Init context
      TTree.Hash_start (
         Tree,
         Size      => Natural (File_size),
         Leaf_size => 1024,
         Keep      => TTH_levels);

      -- Read and feed
      while Pos <= File_size loop
         if Globals.Requested_exit then
            raise Interrupted;
         end if;

         Last := Integer'Min (1024, Integer (File_size - Pos + 1));
         Acf.Types.Byte_array'Read (Source, Bytes (1 .. Last));

         -- Feed the bytes to context:
         TTree.Hash_update (Tree, Bytes (1 .. Last));
         Pos := Pos + Count (Last);
      end loop;

      -- Get hash:
      TTree.Hash_end (Tree);
      V (This).TTH  := TTree.Root_hash (Tree);
      Free_TTH_bytes (This); -- Just in case
      V (This).TTH_bytes :=
         new Acf.Types.Byte_array'(TTree.Get_bytes (Tree, TTH_levels));

      -- Finish:
      if Is_open (fs) then
         Close (fs);
      end if;
      Free (Bytes);
   exception
      when E : others =>
         if Is_open (fs) then
            Close (fs);
         end if;
         Trace.Log ("File.Compute_TTH: " & Trace.Report (E), Trace.Error);
   end Compute_TTH;

   ------------------------------------------------------------------------
   -- Compute_hashes                                                     --
   ------------------------------------------------------------------------
   procedure Compute_hashes (This : in out Object; Speed : Hash.Hash_speeds)
   is
      bs: aliased Buffered_stream.Buffered_stream (64 * 1024);
      ds: aliased Decoupled_file_stream.Decoupled_file_stream;
      ts: aliased Throttle_stream.Throttle_stream
                       (Globals.Hash_throttle'Access);
      use Stream_IO;
      fs: File_type;

      Source          : Stream_access;
      File_size       : Stream_io.Count;

      use type Hash.Hash_speeds;

      Pos   : Count := 1;

      -- Hash contexts:
      Sha1_context : Sha1.Bytes.Hash_context;
      Ed2k_context : aliased Acf.Hash.Algorithms.Ed2k.Ed2k_context;
      Tree         : TTree.Object;

      package Ed2k renames Acf.Hash.Algorithms.Ed2k;

      Cron    : Chronos.Object;
      Cron2   : Chronos.Object;
      Spd     : Float;
      Seconds : Duration;
      Remain  : Count;
      Bytes   : Byte_array_access := new Acf.Types.Byte_array (1 .. 1024);
      Last    : Integer;
   begin
      -- Get file size:
      Open (
         fs, Name => File.Path (this), Mode => In_file, Form => Open_form);
      File_size := Size (fs);

      -- Setup streams:
      if Speed = Hash.fast then
         Source := Stream_access (Stream (fs));
      else
         Close (fs);
         Decoupled_file_stream.Get_decoupled_file_stream (
            ds, File.Path (this));
         if Speed = Hash.Normal then
            Buffered_stream.Get_buffered_stream(bs, ds'Unchecked_Access);
            Source := bs'Unchecked_access;
         elsif Speed = Hash.Slow then
            Buffered_stream.Get_buffered_stream (bs, ds'Unchecked_Access);
            Throttle_stream.Get_throttle_stream (ts, bs'Unchecked_Access);
            Source := ts'Unchecked_access;
         else
            raise Unimplemented;
         end if;
      end if;

      -- Init contexts
      Ed2k.Hash_start (Ed2k_context'Access);
      TTree.Hash_start (
         Tree,
         Size      => Natural (File_size),
         Leaf_size => 1024,
         Keep      => TTH_levels);

      -- Read and feed
      while Pos <= File_size loop
         if Globals.Requested_exit then
            raise Interrupted;
         end if;

         Last := Integer'Min (1024, Integer (File_size - Pos + 1));
         Acf.Types.Byte_array'Read (Source, Bytes (1 .. Last));

         -- Feed the bytes to contexts:
         Sha1.Bytes.Feed (Sha1_context, Sha1.Byte_array (Bytes (1 .. Last)));
         Ed2k.Hash_update (Ed2k_context'Access, Bytes (1 .. Last));
         TTree.Hash_update (Tree, Bytes (1 .. Last));
         Pos := Pos + Count (Last);
         -- Update stats:
         if Chronos.Elapsed (Cron2) > 0.8 then
            Spd     := Float (Pos) / Float (Chronos.Elapsed (Cron));
            Remain  := File_size - (Pos - 1);
            Seconds := Duration (Float (Remain) / Spd);
            Statistics.Object.Set (
               Stat_hashing_bytes,
               Statistics.Strings.Create (
                  Misc.To_string (Integer (Pos - 1)) & " of " &
                  Misc.To_string (Integer (File_size)) & " (" &
                  Misc.To_string (Float (Pos -1) / Float (File_size)*100.0,
                     1) & "%) (" &
                  Misc.Image (Seconds) & " estimated remaining)."));
            Statistics.Object.Set (
               Stat_hashing_speed,
               Statistics.Strings.Create (Convert.To_size (Spd) & "/s"));
            Chronos.Reset (Cron2);
         end if;
      end loop;
      Statistics.Object.Set (
         Stat_hashing_bytes,
         Statistics.Strings.Create ("Building TigerTree..."));
      Statistics.Object.Set (
         Stat_hashing_speed, Statistics.Strings.Create ("n/a"));

      -- Get hashes:
      V (This).Sha  := Sha1.Bytes.Hash (Sha1_context);
      V (This).Ed2k := Ed2k.Hash_end (Ed2k_context'Access);
      TTree.Hash_end (Tree);
      V (This).TTH  := TTree.Root_hash (Tree);
      Free_TTH_bytes (This); -- Just in case
      V (This).TTH_bytes :=
         new Acf.Types.Byte_array'(TTree.Get_bytes (Tree, TTH_levels));

      Statistics.Object.Set (
         Stat_hashing_bytes, Statistics.Strings.Create ("Idle"));
      Statistics.Object.Set (
         Stat_hashing_speed, Statistics.Strings.Create ("Idle"));

      -- Finish:
      if Is_open (fs) then
         Close (fs);
      end if;
      Free (Bytes);
   exception
      when E : others =>
         if Is_open (fs) then
            Close (fs);
         end if;
         Free (Bytes);
         Trace.Log ("File.Compute_hashes: " & Trace.Report (E), Trace.Error);
   end Compute_hashes;

   procedure Free (This : in out Object_array_access) is
      procedure Delete is new Unchecked_deallocation (
         Object_array, Object_array_access);
   begin
      Delete (This);
   end Free;

end Adagio.File;
