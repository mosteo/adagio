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
--  $Id: adagio-unicode.ads,v 1.3 2004/01/21 21:05:42 Jano Exp $

--  Helper functions to deal with unicode strings

package Adagio.Unicode is

   Invalid_encoding : exception;
   
   -- Returns a Latin1 string from a G2 encoded string
   -- It can be a UTF8 encoded string or a
   -- 16 bit (endianness applies then) raw unicode character string.
   -- The 16#ff# selector must be the first character in that case.
   -- May raise exception if Latin1 can't hold the resulting string.
   function G2_to_string (this : in String; Big_endian : in Boolean) 
      return String;

   -- Returns a Latin1 string from a utf8 encoded one
   function From_utf8 (this : in String) return String;

   -- Returns a Utf8 encoded string from Latin1 (Ada default)
   function To_utf8 (this : in String) return String;

end Adagio.Unicode;
