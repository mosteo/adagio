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
--  $Id: adagio-misc.adb,v 1.6 2004/02/03 22:52:15 Jano Exp $

With
Interfaces,
Adagio.Constants,
Ada.Strings,
Ada.Strings.Fixed,
Text_io;

Use
Adagio.Constants;

package body Adagio.Misc is

   package ACH renames Ada.Characters.Handling;

  function Timestamp(h: Ada.Calendar.Time:= Ada.Calendar.Clock)
     return String is
    use Ada.Calendar;
    package Int_io is new Text_io.Integer_io(integer);
    use Int_io;
    s: String(1..11):= (3 => ':', 6 => ':', 9 => '.', others => '0');
    d: Day_duration;
    seg: integer;
  begin
    d:= Seconds(h);
    seg:= integer(d * 100);
    put(s(1..2), seg / (60 * 60 * 100));
    put(s(4..5), (seg rem 360000) / 6000);
    put(s(7..8), (seg rem 6000) / 100);
    put(s(10..11), seg rem 100);
    for i in s'range loop
      if s(i) = ' ' then s(i):= '0'; end if;
    end loop;
    return s;
  exception
    when others =>
      return "??:??:??.??";
  end Timestamp;

   -----------------------------------
   -- Image                         --
   -----------------------------------
   -- Returns a beautified duration in hours, minutes, seconds
   function Image (D : Duration) return String is
      S  : String (1 .. 11) := (3 => 'h', 7 => 'm', 11 => 's', others => ' ');
      S2 : String (1 .. 10);
      package Int_io is new Text_io.Integer_io(integer);
      use Int_io;
      Seconds : Integer := Integer (D);
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if D < 0.0 then
         return Image (-D);
      end if;

      Put (S (5 .. 6), (Seconds rem 3600) / 60);
      Put (S (9 ..10), Seconds rem 60);
      if Seconds / 3600 > 99 then
         Put (S2, Seconds / 3600);
         return Trim (S2, Both) & "h " & S (5 .. S'Last);
      else
         Put (S (1 .. 2), Seconds / 3600);
         return S;
      end if;
   end Image;

   -- Returns the string without the leading sign space.
   function To_string(n: in Integer) return String is
      s: Constant String:= Integer'Image(n);
   begin
      if s (s'First) = ' ' then
         return s(s'First + 1 .. s'Last);
      else
         return s;
      end if;
   end To_string;

   -----------------------------------------------------------------------
   -- To_String                                                         --
   -----------------------------------------------------------------------
   -- Returns the string image without E notation and with given decimals:
   function To_string (n : float; decimals : natural := 2) return String is

      package f_io is new Text_io.Float_io (Float);
      S    : String (1 .. 100);

      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      f_io.Put (S, n, Aft => decimals, Exp => 0);
      return Trim (S, Both);
   end To_string;

   -- Returns a character as its hex rep.
   THex : constant array (0 .. 15) of Character := "0123456789ABCDEF";
   function To_hex (C : Character) return String is

      use Interfaces;
      Byte : Unsigned_8 := Character'Pos (C);

   begin
      return "" &
         THex (Integer (Shift_right (Byte, 4))) &
         THex (Integer (Byte and 16#0f#));
   end To_hex;

   ------------------------------------------------------------------------
   -- To_hex                                                             --
   ------------------------------------------------------------------------
   -- Converts a bytes string in its hex rep.
   function To_hex (S : in String) return String is
      R : String (1 .. S'Length * 2);
      P : Natural := R'First;
   begin
      for N in S'Range loop
         R (P .. P + 1) := To_hex (S (N));
         P := P + 2;
      end loop;

      return R;
   end To_hex;

   ------------------------------------------------------------------------
   -- From_hex                                                           --
   ------------------------------------------------------------------------
   FHex : constant array (Character) of Interfaces.Unsigned_8 := (
         '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4,
         '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
         'a' => 10, 'A' => 10, 'b' => 11, 'B' => 11, 'c' => 12, 'C' => 12,
         'd' => 13, 'D' => 13, 'e' => 14, 'E' => 14, 'f' => 15, 'F' => 15,
         others => 255);
   function From_hex (C : String) return Character is
      use Interfaces;
   begin
      return Character'Val (
         Shift_left (FHex (C (C'First)), 4) or FHex (C (C'Last)));
   end From_hex;

   function From_hex (S : String) return String is
      R : String (1 .. S'Length / 2);
      P : Natural := S'First;
   begin
      for N in R'Range loop
         R (N) := From_hex (S (P .. P + 1));
         P := P + 2;
      end loop;

      return R;
   end From_hex;

   ------------------------------------------------------------------------
   -- Get_line                                                           --
   ------------------------------------------------------------------------
   -- Read a line from a stream (end of line is LF, CR are discarded)
   function Get_line (Stream : access Root_stream_type'Class) return String is
      Result : String (1 .. 4096);
      Pos    : Natural := Result'First;
      C      : Character;
   begin
      loop
         Character'Read (Stream, C);
         if C = Character'Val (10) then
            return Result (Result'First .. Pos - 1);
         elsif C /= Character'Val (13) then
            Result (Pos) := C;
            Pos := Pos + 1;
         end if;
      end loop;
   end Get_line;

   ------------------------------------------------------------------------
   -- Parse_duration                                                     --
   ------------------------------------------------------------------------
   -- Returns a duration interpreted from a string.
   -- The string can specify units : "100s", "100ms".
   -- Valid units are h, m, s, ms
   -- Lack of units are interpreted as seconds.
   function Parse_duration (this : in String) return Duration is
      D : Duration;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      S : String := To_lower (Trim (this, Both));
      Last : Natural;
      package Dur_io is new Text_io.Fixed_io (Duration);
   begin
      Dur_io.Get (S, D, Last);

      if S (S'Last) = 'd' then
         D := D * Duration'(24.0 * 60.0 * 60.0);
      elsif S (S'Last) = 'h' then
         D := D * Duration'(60.0 * 60.0);
      elsif S (S'Last) = 'm' then
         D := D * 60.0;
      elsif S'Length > 2 and then S (S'Last - 1 .. S'Last) = "ms" then
         D := D / 1000.0;
      end if;

      return D;
   end Parse_duration;

   ------------------------------------------------------------------------
   -- Parse_size                                                         --
   ------------------------------------------------------------------------
   -- Returns a size in bytes interpreted from a string:
   -- b bits, B bytes, multipliers: k, m, g, t
   -- Default is bytes.
   package Num_io is new Text_io.Integer_io (File_size);
   function Parse_size (this : in String) return File_size is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      S : String := Trim (this, Both);
      Last : Natural;
      Size : File_size;
   begin
      Num_io.Get (S, Size, Last);

      if S (S'Last) = 'b' then
         Size := Size / 8;
      elsif Last < S'Last and then S'Length >= 3 then
         if ACH.To_lower (S (S'Last - 1)) = 'k' then
            Size := Size * 1024;
         elsif ACH.To_lower (S (S'Last - 1)) = 'm' then
            Size := Size * 1024 * 1024;
         elsif ACH.To_lower (S (S'Last - 1)) = 'g' then
            Size := Size * 1024 * 1024 * 1024;
         elsif ACH.To_lower (S (S'Last - 1)) = 't' then
            Size := Size * 1024 * 1024 * 1024 * 1024;
         end if;
      end if;

      return Size;
   exception
      when Constraint_error =>
         return File_size'Last;
   end Parse_size;

   ------------------------------------------------------------------------
   -- To_string (stream_element_array)                                   --
   ------------------------------------------------------------------------
   -- Convert a stream element array into string
   function To_string (this : in Stream_element_array) return String is
      S : String (Integer (this'First) .. Integer (this'Last));
   begin
      for N in this'Range loop
         S (Integer (N)) := Character'Val (this (N));
      end loop;
      return S;
   end To_string;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Search_in, Search_for) > 0;
   end Contains;

   ------------------------------------------------------------------------
   -- Starts                                                             --
   ------------------------------------------------------------------------
   -- Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean is
   begin
      if Prefix'Length > Search_in'Length then
         return false;
      end if;

      return
         Search_in (Search_in'First .. Search_in'First + Prefix'Length - 1) =
            Prefix;
   end Starts;

   ------------------------------------------------------------------------
   -- Get_C_String                                                       --
   ------------------------------------------------------------------------
   -- Gets chars from a string until finding a \0
   function Get_C_String (From : in String) return String is
      Last : Positive := From'First;
   begin
      while From (Last) /= Nul loop
         Last := Last + 1;
      end loop;

      return From (From'First .. Last - 1);
   end Get_C_String;

end Adagio.Misc;
