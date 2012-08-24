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
--  $Id: tigertree.ads,v 1.3 2004/01/21 21:05:48 Jano Exp $

-- Types and functions to instantiate a TigerTree using HashTree.ads

with Acf.Hash.Algorithms.Tiger;
with Acf.Hash.Message_digests;
with Acf.Types;
use  Acf.Types;

package TigerTree is 

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------
   type Hash_type is private;
   type Hash_context is limited private;

   Null_hash : constant Hash_type;

   Null_hash_as_bytes : constant Byte_array;

   ------------------------------------------------------------------------
   -- Begin_hash                                                         --
   ------------------------------------------------------------------------
   procedure Begin_hash  (Context : in out Hash_context);

   ------------------------------------------------------------------------
   -- End_hash                                                           --
   ------------------------------------------------------------------------
   function  End_hash    (Context : in     Hash_context) return Hash_type;

   ------------------------------------------------------------------------
   -- Update_hash                                                        --
   ------------------------------------------------------------------------
   procedure Update_hash (
      Context : in out Hash_context;
      Bytes   : in     Byte_array);

   ------------------------------------------------------------------------
   -- Start_leaf_hash                                                    --
   ------------------------------------------------------------------------
   procedure Start_leaf_hash          (Context : in out Hash_context);

   ------------------------------------------------------------------------
   -- Start_intermediate_hash                                            --
   ------------------------------------------------------------------------
   procedure Start_intermediate_hash  (Context : in out Hash_context);

   ------------------------------------------------------------------------
   -- Start_root_hash                                                    --
   ------------------------------------------------------------------------
   procedure Start_root_hash          (Context : in out Hash_context)
      renames Start_intermediate_hash;

   ------------------------------------------------------------------------
   -- To_hash                                                            --
   ------------------------------------------------------------------------
   -- Converts a byte array into a hash
   function To_hash (Bytes : in Byte_array) return Hash_type;

   ------------------------------------------------------------------------
   -- To_byte_array                                                      --
   ------------------------------------------------------------------------
   -- Returns a Hash as a Byte_array
   function To_byte_array (This : in Hash_type) return Byte_array;

   ------------------------------------------------------------------------
   -- To_char_array
   ------------------------------------------------------------------------
   -- Returns a Hash as a string of bytes
   function To_char_array (This : in Hash_type) return String; 

   ------------------------------------------------------------------------
   -- To_hex                                                             --
   ------------------------------------------------------------------------
   -- Hex representation of a hash:
   function To_hex (This : in Hash_type) return String;

   ------------------------------------------------------------------------
   -- To_base32                                                          --
   ------------------------------------------------------------------------
   -- Base32 representation of a hash:
   function To_base32 (This : in Hash_type) return String;

private

   type Hash_type is record
      Hash : aliased Acf.Hash.Message_digests.Message_digest;
   end record;

   Null_hash : constant Hash_type := (
      Hash => Acf.Hash.Message_digests.Null_message_digest);

   Null_hash_as_bytes : constant Acf.Types.Byte_array (1 .. 24) := 
      (others => 0);

   type Hash_context is limited record
      Context : aliased Acf.Hash.Algorithms.Tiger.Tiger_context;
   end record;

end TigerTree;
