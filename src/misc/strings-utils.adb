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
--  $Id: strings-utils.adb,v 1.3 2004/01/21 21:05:36 Jano Exp $

with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

with Strings.Fields;

package body Strings.Utils is

   function U (S : String) return Ada.Strings.Unbounded.Unbounded_string
      renames Ada.Strings.Unbounded.To_unbounded_string;
   function S (U : Ada.Strings.Unbounded.Unbounded_string) return String
      renames Ada.Strings.Unbounded.To_string;

   package ASU renames Ada.Strings.Unbounded;

   ------------------------------------------------------------------------
   -- Crunch                                                             --
   ------------------------------------------------------------------------
   function Crunch (this : in String) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Strings.Fields;
      R   : Unbounded_string;
      Pos : Integer := 1;
   begin
      while Select_field (this, Pos) /= "" loop
         R := R & Select_field (this, Pos) & " ";
         Pos := Pos + 1;
      end loop;

      declare
         Result : String := S (R);
      begin
         return Result (Result'First .. Result'Last - 1);
      end;
   end Crunch;

   ------------------------------------------------------------------------
   -- Simplify                                                           --
   ------------------------------------------------------------------------
   -- Makes everything outside 'a' .. 'z' a blank
   -- Converts the string to lowercase and basic character mapping
   function Simplify (S : in String) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;
      R : String (S'range);
   begin
      for N in R'Range loop
         if Is_digit (S (N)) or else S (N) = '-' then
            R (N) := S (N);
         else
            R (N) := Value (Lower_case_map, S (N));
            R (N) := Value (Basic_map, R (N));
            if not Is_basic (R (N)) then
               R (N) := ' ';
            end if;
         end if;
      end loop;
      
      return Crunch (Trim (R, Ada.Strings.Both));
   end Simplify;

   ------------------------------------------------------------------------
   -- Positivize                                                         --
   ------------------------------------------------------------------------
   -- Removes -" " members in a string (must have quotes).
   function Positivize (S : in String) return String is
      R   : ASU.Unbounded_string;
      N   : Natural := S'First;
   begin
      while N <= S'Last loop
         if S (N) = '-' and then N < S'Last and then S (N + 1) = '"' then
            N := N + 2;
            Inner: 
               loop
                  N := N + 1;
                  exit Inner when N > S'Last or else S (N) = '"';
               end loop Inner;
            N := N + 1;
         else
            ASU.Append (R, S (N));
            N := N + 1;
         end if;
      end loop;

      return ASU.To_string (R);
   end Positivize;

   ------------------------------------------------------------------------
   -- Tokenize                                                           --
   ------------------------------------------------------------------------
   -- Returns Indexes for all words inside a string, with a minimum length.
   -- String must be simplified or wrong results can occur
   function Tokenize (S : in String; Min_len : Positive := 3) 
      return Index_array is
      use Strings.Fields;
      Result : Index_array (1 .. Count_fields (S, ' '));
      Pos    : Integer := 1;
   begin
      if S'Length < Min_len then 
         return Result (1 .. 0);
      end if;
      for N in S'Range loop
         if S (N) = ' ' then
            if Pos = 1 then
               Result (Pos).First := S'First;
            else
               Result (Pos).First := Result (Pos - 1).Last + 2;
            end if;
            Result (Pos).Last  := N - 1;
            if Result (Pos).Last - Result (Pos).First + 1 >= Min_len then
               Pos := Pos + 1;
            end if;
         end if;
      end loop;
      if Pos = 1 then
         Result (Pos).First := S'First;
      else
         Result (Pos).First := Result (Pos - 1).Last + 2;
      end if;
      Result (Pos).Last  := S'Last;
      if Result (Pos).Last - Result (Pos).First + 1 >= Min_len then
         Pos := Pos + 1;
      end if;

   return Result (1 .. Pos - 1);
   end Tokenize;

   ------------------------------------------------------------------------
   -- Head                                                               --
   ------------------------------------------------------------------------
   function Head (S : in String; Separator : in Character := ' ') 
   return String is
      Pos : Natural := S'First + 1;
   begin
      if S = "" then 
         return "";
      end if;
      while Pos <= S'Last and then S (Pos) /= Separator loop
         Pos := Pos + 1;
      end loop;
      return S (S'First .. Pos - 1);
   end Head;

   ------------------------------------------------------------------------
   -- Tail                                                               --
   ------------------------------------------------------------------------
   function Tail (S : in String; Separator : in Character := ' ')
   return String is
      Pos : Natural := S'First;
   begin
      if S = "" then 
         return "";
      end if;
      while Pos <= S'Last and then S (Pos) /= Separator loop
         Pos := Pos + 1;
      end loop;
      if Pos >= S'Last then
         return "";
      else
         return S (Pos + 1 .. S'Last);
      end if;
   end Tail;

   ------------------------------------------------------------------------
   -- Trim                                                               --
   ------------------------------------------------------------------------
   -- Trim a string from spaces
   function Trim (S : in String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end;

   ------------------------------------------------------------------------
   -- Replace                                                            --
   ------------------------------------------------------------------------
   -- Replace all occurrences of a string by another:
   function Replace (
      Source  : in String; 
      Pattern : in String;
      By      : in String) return String is
      use Ada.Strings.Fixed;
      Pos : Natural;
   begin
      Pos := Index (Source, Pattern);
      if Pos = 0 then
         return Source;
      else
         return Replace (
            Replace_slice (Source, Pos, Pos + Pattern'Length - 1, By), 
            Pattern,
            By);
      end if;
   end Replace;

   ------------------------------------------------------------------------
   -- Select_field                                                       --
   ------------------------------------------------------------------------
   -- return the nth enclosed field:
   function Select_field (
      S     : in String;
      Open  : in Character;
      Close : in Character;
      Pos   : in Positive := 1) return String is
      use Strings.Fields;
   begin
      return Select_field (Select_field (S, Pos + 1, Open), 1, Close);
   end Select_field;

end Strings.Utils;
