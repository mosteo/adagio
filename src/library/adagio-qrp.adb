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
--  $Id: adagio-qrp.adb,v 1.3 2004/01/21 21:05:28 Jano Exp $

-- QRP tables are bitmaps. An 1 element is a missing element.
-- QRP tables are 0-based and have a size which is the width
--    of the hashing function.
-- For example, a 20 bits table has a bitmap of 0 .. 2 ** 20 - 1 bits.

With
Interfaces,
Adagio.Misc,
Bit_arrays.Strings,
Agpl.Types.Ustrings;

Use
Agpl.Types.Ustrings;

package body Adagio.QRP is

   -- Necessary because bit ordering in packed array is contrary to
   -- that used in QRTs
   function Transform (Pos : in Natural) return Natural is
      use Interfaces;
      X     : Unsigned_32 := Unsigned_32 (Pos);
   begin
      return Natural ((X and 16#fffffff8#) or (2#111# - X and 2#111#));
   end Transform;
   pragma Inline (Transform);

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear (this : out Table) is
   begin
      this.Bitmap       := (others => not Marked);
      this.Used_entries := 0;
   end Clear;

   ------------------------------------------------------------------------
   -- Set                                                                --
   ------------------------------------------------------------------------
   procedure Set (this : in out Table; Pos : in Natural) is
   begin
      if this.Bitmap (Transform (Pos)) /= Marked then
         this.Used_entries := this.Used_entries + 1;
      end if;
      this.Bitmap (Transform (Pos)) := Marked;
   end Set;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (this : in out Table; Pos : in Natural) is
   begin
      if this.Bitmap (Transform (Pos)) = Marked then
         this.Used_entries := this.Used_entries - 1;
      end if;
      this.Bitmap (Transform (Pos)) := not Marked;
   end Reset;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Check for containment:
   function Contains (this : in Table; Pos : in Natural) return Boolean is
   begin
      return this.Bitmap (Transform (Pos));
   end Contains;

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   -- Returns the table in a convenient format to be transferred:
   -- Returns a string of 2 ** N / 8 characters.
   function To_string (this : in Table) return String is
   begin
      return Bit_arrays.Strings.To_string (this.Bitmap);
   end To_string;

   ------------------------------------------------------------------------
   -- Ratio                                                              --
   ------------------------------------------------------------------------
   -- Return the fullness of the table (0.0 .. 1.0)
   function Ratio (this : in Table) return Ratios is
   begin
      return Float (this.Used_entries) / Float (this.Last_entry + 1);
   end Ratio;

   ------------------------------------------------------------------------
   -- Is_dirty                                                           --
   ------------------------------------------------------------------------
   -- Says if table is dirty from last cleaning:
   function Is_dirty (this : in Table) return Boolean is
   begin
      return this.Used_entries /= this.Prev_used;
   end Is_dirty;

   ------------------------------------------------------------------------
   -- Mark_undirty                                                       --
   ------------------------------------------------------------------------
   -- Mark table as clean:
   procedure Mark_undirty (this : in out Table) is
   begin
      this.Prev_used := this.Used_entries;
   end Mark_undirty;

   ------------------------------------------------------------------------
   -- Hash_number                                                        --
   ------------------------------------------------------------------------
   function Hash_number (Number, nBits : in Integer) return Integer is
      use Interfaces;
      nNumber  : Unsigned_64 := Unsigned_64 (Number);
      nProduct : Unsigned_64 := nNumber * 16#4F1BBCDC#;
      nHash    : Unsigned_64 :=
         Shift_right (Shift_left (nProduct, 32), 32 + (32 - nBits));
   begin
      return Integer (nHash);
   end Hash_number;

   ------------------------------------------------------------------------
   -- Hash_word                                                          --
   ------------------------------------------------------------------------
   function Hash_word (Word : in String; nBits : Integer)
      return Integer is
      use Interfaces;
      type Byte_range is mod 4;
      uXor  : Unsigned_32 := 0;
      uByte : Byte_range  := 0;
      b     : Unsigned_32;
   begin
      for N in Word'Range loop
         b     := Unsigned_32 (Character'Pos (Word (N))) and 16#ff#;
         b     := Shift_left (b, Integer (uByte) * 8);
         uXor  := uXor xor b;
         uByte := uByte + 1;
      end loop;

      return Hash_number (Integer (uXor), nBits);
   end Hash_word;

   ------------------------------------------------------------------------
   -- Add_keyword                                                        --
   ------------------------------------------------------------------------
   -- Hash and add a keyword:
   procedure Add_keyword (this : in out Table; Word : in String) is
   begin
      Set (this, Hash_word (Misc.To_lower (Word), this.Bits));
   end Add_keyword;

   Test_case : UString_array := (
      U (""), U("n"), U("nd"), U("ndflaleme"));

    Pragma Unreferenced( Test_case );
begin
   -- Test the hash functions
--   for N in test_case'Range loop
--      Put_line (S (Test_case (N)) & ": " &
--      Hash_word(S (Test_case (N)), 16)'img);
--   end loop;
   null;
end Adagio.QRP;
