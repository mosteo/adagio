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
--  $Id: tigertree.adb,v 1.3 2004/01/21 21:05:47 Jano Exp $

-- Types and functions to instantiate a TigerTree using HashTree.ads

With
Bit_arrays,
Bit_arrays.Numbers,
Bit_arrays.Modular;

package body TigerTree is

   package Tiger renames Acf.Hash.Algorithms.Tiger;

   package Byte_bit is new Bit_arrays.Modular (Acf.Types.Byte);

   ------------------------------------------------------------------------
   -- Begin_hash                                                         --
   ------------------------------------------------------------------------
   procedure Begin_hash  (Context : in out Hash_context) is
   begin
      Tiger.Hash_start (Context.Context'Access);
   end Begin_hash;

   ------------------------------------------------------------------------
   -- End_hash                                                           --
   ------------------------------------------------------------------------
   function  End_hash    (Context : in     Hash_context) return Hash_type is
      Result : Hash_type;
   begin
      Result.Hash := Tiger.Hash_end (Context.Context'Unrestricted_access);
      return Result;
   end;

   ------------------------------------------------------------------------
   -- Update_hash                                                        --
   ------------------------------------------------------------------------
   procedure Update_hash (
      Context : in out Hash_context;
      Bytes   : in     Byte_array) is
   begin
      Tiger.Hash_update (Context.Context'Access, Bytes);
   end Update_hash;

   ------------------------------------------------------------------------
   -- Start_leaf_hash                                                    --
   ------------------------------------------------------------------------
   procedure Start_leaf_hash          (Context : in out Hash_context) is
   begin
      Tiger.Hash_update (
         Context.Context'Access,
         Acf.Types.Byte_array'(1 => 0));
   end Start_leaf_hash;

   ------------------------------------------------------------------------
   -- Start_intermediate_hash                                            --
   ------------------------------------------------------------------------
   procedure Start_intermediate_hash  (Context : in out Hash_context) is
   begin
      Tiger.Hash_update (
         Context.Context'Access,
         Acf.Types.Byte_array'(1 => 1));
   end Start_intermediate_hash;

   ------------------------------------------------------------------------
   -- To_hash                                                            --
   ------------------------------------------------------------------------
   -- Converts a byte array into a hash
   function To_hash (Bytes : in Byte_array) return Hash_type is
      B     : Byte_array (Bytes'Range);
      Hash  : Hash_type;
   begin
     -- Reverse Bytes:
     for N in 0 .. 7 loop
         B (B'First + N) := Bytes (Bytes'First + 7 - N);
     end loop;
     for N in 8 .. 15 loop
        B (B'First + N) := Bytes (Bytes'First + 15 - N + 8);
     end loop;
     for N in 16 .. 23 loop
        B (B'First + N) := Bytes (Bytes'First + 23 - N + 16);
     end loop;

     Hash.Hash := Acf.Hash.Message_digests.To_message_digest (B);
     return Hash;
   end To_hash;

   ------------------------------------------------------------------------
   -- To_byte_array                                                      --
   ------------------------------------------------------------------------
   -- Returns a Hash as a Byte_array
   -- We must reverse the byte orders because that's the way it is done
   --    in gnutella (little endian instead of big endian)
   function To_byte_array (This : in Hash_type) return Byte_array is
      Bytes : Byte_array :=
         Acf.Hash.Message_digests.To_byte_array (This.Hash);
      B     : Byte_array (Bytes'Range);
   begin
        -- Reverse Bytes:
      for N in 0 .. 7 loop
         B (B'First + N) := Bytes (Bytes'First + 7 - N);
     end loop;
     for N in 8 .. 15 loop
        B (B'First + N) := Bytes (Bytes'First + 15 - N + 8);
     end loop;
     for N in 16 .. 23 loop
        B (B'First + N) := Bytes (Bytes'First + 23 - N + 16);
     end loop;

     return B;
   end To_byte_array;

   ------------------------------------------------------------------------
   -- To_hex                                                             --
   ------------------------------------------------------------------------
   -- Hex representation of a hash:
   function To_hex (This : in Hash_type) return String is
   begin
      return Acf.Hash.Message_digests.To_hex_string (This.Hash);
   end To_hex;

   ------------------------------------------------------------------------
   -- To_base32                                                          --
   ------------------------------------------------------------------------
   -- Base32 representation of a hash:
   function To_base32 (This : in Hash_type) return String is
      Bytes : Acf.Types.Byte_array := To_byte_array (This);
      use Bit_arrays;
      Base32 : array (0 .. 31) of Character :=
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
      Raw    : Bit_array (1 .. 195) := (others => false);
      Pos    : Natural := Raw'First;
      s      : String (1 .. Raw'Length / 5);
   begin
      if Bytes'Length /= 24 then
         raise Constraint_error;
      end if;
     for i in Bytes'range loop
        Raw (Pos .. Pos + 7) := Byte_bit.To_bit_array_BE (Bytes (i));
        Pos:= Pos + 8;
     end loop;
      -- Churn the base32 representation:
      Pos := Raw'First;
      for i in s'range loop
         s (i) :=
            Base32 (Bit_arrays.Numbers.To_number_BE (Raw (Pos .. Pos + 4)));
         Pos := Pos + 5;
      end loop;
      return s;
   end To_base32;

   ------------------------------------------------------------------------
   -- To_char_array
   ------------------------------------------------------------------------
   -- Returns a Hash as a string of bytes
   function To_char_array (This : in Hash_type) return String is
      B : Byte_array := To_byte_array (This);
      S : String (B'range);
   begin
      for N in S'range loop
         S (N) := Character'Val (Byte'Pos (B (N)));
      end loop;
      return S;
   end To_char_array;

end TigerTree;
