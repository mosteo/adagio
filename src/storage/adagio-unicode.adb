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
--  $Id: adagio-unicode.adb,v 1.4 2004/01/21 21:05:42 Jano Exp $

--  Helper functions to deal with unicode strings

With
Agpl.Types.Ustrings,
Unicode.CCS.Iso_8859_1,
Unicode.CES.Utf8,
Unicode.CES.Basic_8bit;


Use
Agpl.Types.Ustrings,
Unicode.CCS,
Unicode.CES;

package body Adagio.Unicode is

   -- Decode an utf8 string, ensuring its Latin1:
   function From_utf8 (this : in String) return String is
   begin
      return
         Basic_8bit.From_utf32 (
            utf8.To_Utf32 (
               utf8.To_unicode_LE (
                  this,
                  Cs => Iso_8859_1.Iso_8859_1_character_set)));
   exception
      when Invalid_code | Invalid_encoding =>
         raise Invalid_encoding;
   end From_utf8;

   -- Decode a raw 16-bit unicode string:
   -- As we support only latin1, a high byte /= 0 raises Constraint_error
   function From_unicode16 (this : in String; Big_endian : in Boolean)
      return String is
      Pos    : Integer := this'First;
      Result : Ustring;
   begin
      while Pos < this'Last loop
         if Big_endian then
            if Character'Pos (this (Pos)) /= 0 then
               raise Constraint_error;
            end if;
            Pos := Pos + 1;
         end if;

         ASU.Append (Result, this (Pos));
         Pos := Pos + 1;

         if not Big_endian then
            if Character'Pos (this (Pos)) /= 0 then
               raise Constraint_error;
            end if;
            Pos := Pos + 1;
         end if;
      end loop;

      return S (Result);
   end From_unicode16;

   -- Returns a Latin1 string from a G2 encoded string
   -- It can be a UTF8 encoded string or a
   -- 16 bit (endianness applies then) raw unicode character string.
   -- The 16#ff# selector must be the first character in that case.
   -- May raise exception if Latin1 can't hold the resulting string.
   function G2_to_string (this : in String; Big_endian : in Boolean)
      return String is
   begin
      if This'Length = 0 then
         return "";
      elsif this (this'First) = Character'Val (16#ff#) then
         return From_unicode16 (
            this (this'First + 1 .. this'Last), Big_endian);
      else
         return From_utf8 (this);
      end if;
   end G2_to_string;

   -- Returns a Utf8 encoded string from Latin1 (Ada default)
   function To_utf8 (this : in String) return String is
   begin
      return Utf8.From_Utf32 (
         Basic_8bit.To_Utf32 (
            Basic_8bit.To_Unicode_LE (
               this,
               Iso_8859_1.Iso_8859_1_character_set)));
   end To_utf8;

end Adagio.Unicode;
