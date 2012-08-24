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
--  $Id: adagio-file.ads,v 1.5 2004/01/29 21:47:08 Jano Exp $

with Adagio.Ed2k;
with Adagio.Hash;
with Adagio.Safe_access;
with Adagio.Types; use Adagio.Types;
with Sha1;
with TigerTree;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Acf.Hash.Message_digests;
with Acf.Types;

with Gnat.Os_lib;
use  Gnat;

with Ada.Streams;
use  Ada;

package Adagio.File is

   File_not_found   : Exception;
   Not_computed_yet : Exception;

   type Object is tagged private;
   type Object_array is array(integer range <>) of Object;
   type Object_array_access is access all Object_array;

   procedure Free (This : in out Object_array_access);

   Null_file : Constant Object;

   -- Creates a file with a given path. No checks about real existence.
   function Create (Path : in String) return Object;

   -- Full qualified name for file:
   function Path  (this : Object) return String;

   -- Name without path:
   function Name (this : Object) return String;

   -- Path without name (trailing /):
   function Dir_name (this : Object) return String;

   -- Sha1 for file in base32:
   -- Returns Sha1.Null_digest if not yet computed
   function Sha  (this : Object) return Sha1.Digest;
   -- Compute
   procedure Compute_sha (this : in out Object; Speed : Hash.Hash_speeds);

   -- ed2k hashing:
   function Ed2k (this : Object) 
      return Acf.Hash.Message_digests.Message_digest;
   -- Compute
   procedure Compute_ed2k (this : in out Object; Speed : Hash.Hash_speeds);

   -- Tiger things
   function TTH (this : Object) return TigerTree.Hash_type;
   function TTH_bytes (This : Object) return Acf.Types.Byte_array;
   procedure Free_TTH_bytes (This : Object);
   procedure Compute_TTH (this : in out Object; Speed : Hash.Hash_speeds);

   -- Hash computation
   -- Computes all hashes in a single file pass.
   -- Thus it's the efficienter and desirable method to use.
   procedure Compute_hashes (This : in out Object; Speed : Hash.Hash_speeds);

   -- Reset computable values (sha, tigertree, etc...)
   procedure Reset (this : in out Object);

   -- Refresh the timestamp and the size
   -- Raises file_not_found otherwise.
   procedure Refresh (this : in out Object);

   -- Get
   function Timestamp  (this : Object) return Os_lib.Os_time;
   -- Set
   procedure Timestamp (this : in out Object; Timestamp : Os_lib.Os_time);

   -- Serialize:
   procedure Serialize
     (this : Object; Stream : in out Ada.Streams.Root_stream_type'Class);
   procedure Restore
     (this : out Object; Stream : in out Ada.Streams.Root_stream_type'Class);

   -- Same file, by path:
   function Same_file (L, R : Object) return Boolean;

   -- Less path
   function Less (L, R : Object) return Boolean;

   -- Is Null?
   function Is_null (this : in Object) return Boolean;

   -- Size
   function Size (this : in Object) return File_size;
   function Size (this : in Object) return Natural;

   -- Shared
   function Shared (this : in Object) return Boolean;
   function Folder_shared (this : in Object) return Boolean;

   -- Uploads
   function Uploads (this : in Object) return Natural;
   function Uploads_session (this : in Object) return Natural;

   -- Hits
   function Hits_total (This : in Object) return Natural;
   function Hits_session (This : in Object) return Natural;

private

   type Stream_access is access all Streams.Root_stream_type'Class;

   type Byte_array_access is access all Acf.Types.Byte_array;

   type File_object is 
      record
         -- Full path to file in local filesystem.
         Path:                      UString;       
         -- Sha1 hash canonical form.
         Sha:                       Sha1.Digest := Sha1.Null_digest;
         -- ed2k hash
         ed2k:                      Acf.Hash.Message_digests.Message_digest :=
            Adagio.Ed2k.Null_hash;
         -- TigerTree hash
         TTH:                       TigerTree.Hash_type := 
            TigerTree.Null_hash; 
         -- TTH bytes for X levels:
         TTH_bytes:                 Byte_array_access := null;
         -- Last changes in file. We'll check for changes to rehash.
         Timestamp:                 Os_lib.Os_Time;
         -- Shared?
         Shared:                    boolean := true;
         -- Its folder is shared?
         Folder_shared:             boolean := true;
         -- Size:
         Size :                     File_size := 0;
         -- Number of requests for this file till date:
         Uploads :                  Natural := 0;
         Uploads_session :          Natural := 0;
         -- Number of search hits for this file, session and total
         Hits_session :             Natural := 0;
         Hits_total   :             Natural := 0;
      end record;

   type File_access is access all File_object;

   package Safe_file is new Safe_access (File_object, File_access);

   type Object is new Safe_file.Object with null record;

   function V (this : in Object) return File_access;
   pragma Inline (V);

   Null_file : Constant Object := (Safe_file.Null_access with null record);

end Adagio.File;
