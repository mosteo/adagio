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
--  $Id: adagio-ed2k.adb,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Acf.Types;

package body Adagio.Ed2k is

   ------------------------------------------------------------------------
   -- Hash_as_hex                                                        --
   ------------------------------------------------------------------------
   -- Convert a ed2k hash to its Hex representation:
   function Hash_as_hex (Hash : in Hash_type) return String is 
   begin
      return Acf.Hash.Message_digests.To_hex_string (
         Hash, Acf.Types.Lower_case);
   end Hash_as_hex;

   ------------------------------------------------------------------------
   -- Hash_as_char_array                                                 --
   ------------------------------------------------------------------------
   -- Convert a ed2k hash to a String containing its bytes.
   function Hash_as_char_array (Hash : in Hash_type) return String is
      B : Acf.Types.Byte_array := 
         Acf.Hash.Message_digests.To_byte_array (Hash);
      S : String (B'range);
   begin
      for N in S'range loop
         S (N) := Character'Val (Acf.Types.Byte'Pos (B (N)));
      end loop;
      return S;
   end;

   ------------------------------------------------------------------------
   -- Hash_from_char_array                                               --
   ------------------------------------------------------------------------
   -- Obtains a digest from a String containing bytes.
   function Hash_from_char_array (S : in String) return Hash_type is
      B : Acf.Types.Byte_array (S'range);
   begin
      if B'length /= 16 then
         raise Incorrect_hash_length;
      end if;

      for N in B'range loop
         B (N) := Acf.Types.Byte'Val (Character'Pos (S (N)));
      end loop;

      return Acf.Hash.Message_digests.To_message_digest (B);
   end Hash_from_char_array;

end Adagio.Ed2k;
