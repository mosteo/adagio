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
--  $Id: strings-utils.ads,v 1.3 2004/01/21 21:05:36 Jano Exp $

-- Utility functions for strings


package Strings.Utils is

   Illegal_character : Exception;

   type Indexes is record
      First, Last : Positive;
   end record;

   type Index_array is array (Positive range <>) of Indexes;

   -- Makes everything outside 'a' .. 'z', '0' .. '9' a blank
   -- Converts the string to lowercase and basic character mapping
   -- Preserves "-" for negative queries.
   -- Resulting string has no more of a consecutive space
   function Simplify (S : in String) return String;

   -- Removes -" " members in a string (must have quotes).
   function Positivize (S : in String) return String;

   -- Returns Indexes for all words inside a string, with a minimum length.
   -- String must be simplified or wrong results can occur
   function Tokenize (S : in String; Min_len : Positive := 3)
      return Index_array;

   -- These return "" if nothing to be returned
   function Head (S : in String; Separator : in Character := ' ')
      return String;
   function Tail (S : in String; Separator : in Character := ' ')
      return String;

   -- Trim a string from spaces
   function Trim (S : in String) return String;

   -- Replace all occurrences of a string by another:
   function Replace (
      Source : in String; Pattern : in String; By : in String) return String;

   -- return the nth enclosed field:
   function Select_field (
      S     : in String;
      Open  : in Character;
      Close : in Character;
      Pos   : in Positive := 1) return String;

end Strings.Utils;
