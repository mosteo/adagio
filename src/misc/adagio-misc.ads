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
--  $Id: adagio-misc.ads,v 1.3 2004/01/21 21:05:34 Jano Exp $

with Adagio.Types; use Adagio.types;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Streams;             use Ada.Streams;

package Adagio.Misc is

   -- Returns a formatted timestamp:
   function Timestamp (h: Ada.Calendar.Time:= Ada.Calendar.Clock) 
      return String;

   -- Returns a beautified duration in hours, minutes, seconds
   function Image (D : Duration) return String;

   -- Returns the string without the leading sign space.
   function To_string (n: in Integer) return String;

   -- Returns the string image without E notation and with given decimals:
   function To_string (n : float; decimals : natural := 2) return String;

   -- Returns a character as its hex rep.
   function To_hex (C : Character) return String;
   pragma Inline (To_hex);

   -- Converts a bytes string in its hex rep.
   function To_hex (S : in String) return String;

   -- Inverses:
   function From_hex (C : String) return Character;
   function From_hex (S : String) return String;

   -- Returns a lowerized/upperized string:
   function To_lower (S : in String) return String
      renames Ada.Characters.Handling.To_lower;
   function To_lower (S : in Character) return Character
      renames Ada.Characters.Handling.To_lower;
   function To_upper (S : in String) return String
      renames Ada.Characters.Handling.To_upper;
   function To_upper (S : in Character) return Character
      renames Ada.Characters.Handling.To_upper;

   -- Read a line from a stream (end of line is LF, CR are discarded)
   function Get_line (Stream : access Root_stream_type'Class) return String;

   -- Returns a duration interpreted from a string.
   -- The string can specify units : "100s", "100ms".
   -- Valid units are h, m, s, ms
   -- Lack of units are interpreted as seconds.
   function Parse_duration (this : in String) return Duration;

   -- Returns a size in bytes interpreted from a string:
   -- b bits, B bytes, multipliers: k, m, g, t
   -- Default is bytes.
   function Parse_size (this : in String) return File_size;

   -- Convert a stream element array into string
   function To_string (this : in Stream_element_array) return String;

   -- Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean;

   -- Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean;

   -- Gets chars from a string until finding a \0
   function Get_C_String (From : in String) return String;

end Adagio.Misc;
