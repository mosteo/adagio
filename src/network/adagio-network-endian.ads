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
--  $Id: adagio-network-endian.ads,v 1.4 2004/02/29 20:36:45 Jano Exp $

package Adagio.Network.Endian is

   pragma Elaborate_body(Adagio.Network.Endian);

   -- Auxiliary types for later functions:
   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   type Byte_array is array (integer range <>) of Byte;
   pragma Pack (Byte_array);

   -- Endingness of machine (determined at elaboration time):
   Little_endian: boolean;

   -- Convert an arbitrary long byte array in any endingness to integer
   -- May raise Constraint_error if array lengths exceedes integer capacity.
   function Convert (
      From        : Byte_array;
      Big_endian  : Boolean := true) return Integer;

   function Convert_L (
      From        : Byte_array;
      Big_endian  : Boolean := true) return Long_Long_Integer;

   -- Converts an integer to an array of bytes, in the desired endianness.
   -- Optimally returns the shortest possible array:
   -- I.e, 0 is returned as an empty array.
   function Convert (
      From        : Long_Long_Integer;
      Big_endian  : Boolean := false) return Byte_array;

   -- Converts an integer to an array of bytes, in the desired endianness.
   -- Size specified (in bytes):
   function Convert (
      From        : Long_Long_Integer;
      Size        : Natural;
      Big_endian  : Boolean := false) return Byte_array;

   -- Converts an integer to an array of bytes, in the desired endianness.
   -- Optimally returns the shortest possible array:
   -- I.e, 0 is returned as an empty array.
   function Convert (
      From        : Integer;
      Big_endian  : Boolean := false) return Byte_array;

   -- Converts an integer to an array of bytes, in the desired endianness.
   -- Size specified (in bytes):
   function Convert (
      From        : Integer;
      Size        : Natural;
      Big_endian  : Boolean := false) return Byte_array;

   -- Converts a byte array into a string of same bytes:
   function To_string (From : Byte_array) return String;

   -- Inverse of the previous:
   function To_byte_array (From : String) return Byte_array;

   -- Invert a byte_array order
   function Invert (From : Byte_array) return Byte_array;

private

    Pragma Inline( Convert );
   function Is_little_endian return boolean;

end Adagio.Network.Endian;
