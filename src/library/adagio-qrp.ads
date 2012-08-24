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
--  $Id: adagio-qrp.ads,v 1.3 2004/01/21 21:05:28 Jano Exp $

-- QRP tables are bitmaps. An 1 element is a missing element.
-- QRP tables are 0-based and have a size which is the width
--    of the hashing function.
-- For example, a 20 bits table has a bitmap of 0 .. 2 ** 20 - 1 bits.

with Bit_arrays;

package Adagio.QRP is

   -- A patch table of 2 ** N bits:
   -- Last entry must be 2 ** N - 1;
   type Table (Bits : Positive; Last_entry : Positive) is private;

   subtype Ratios is Float range 0.0 .. 1.0;

   -- Reset a table:
   procedure Clear (this : out Table);

   -- Set an element (table contains it):
   procedure Set (this : in out Table; Pos : in Natural);

   -- Reset an element (table doesn't contain it):
   procedure Reset (this : in out Table; Pos : in Natural);

   -- Hash and add a keyword:
   procedure Add_keyword (this : in out Table; Word : in String);

   -- Check for containment:
   function Contains (this : in Table; Pos : in Natural) return Boolean;

   -- Returns the table in a convenient format to be transferred:
   -- Returns a string of 2 ** N / 8 characters.
   function To_string (this : in Table) return String;

   -- Return the fullness of the table (0.0 .. 1.0)
   function Ratio (this : in Table) return Ratios;

   -- Says if table is dirty from last cleaning:
   function Is_dirty (this : in Table) return Boolean;

   -- Mark table as clean:
   procedure Mark_undirty (this : in out Table);

private

   Marked : constant Boolean := true;

   type Table (Bits : Positive; Last_entry : Positive) is record
      Bitmap : Bit_arrays.Bit_array (0 .. Last_entry) := 
         (others => not Marked);
      Used_entries : Natural := 0;
      Prev_used    : Natural := 0; -- For dirtying
   end record;

end Adagio.QRP;
