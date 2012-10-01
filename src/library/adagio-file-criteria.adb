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
--  $Id: adagio-file-criteria.adb,v 1.4 2004/01/21 21:05:27 Jano Exp $

--  Criteria to qualify files

with Adagio.Misc;

with Strings.Fields; use Strings.Fields;
with Strings.Utils;  use Strings.Utils;

with Gnat.Directory_operations; use Gnat;

with Ada.Exceptions; use Ada.Exceptions;

package body Adagio.File.Criteria is

   use type Types.File_Size;

   -- Defined criteria:
   -- Greater_than            Size greater OR EQUAL than
   -- Smaller_than            Size smaller than 
   -- Is_in                   File is in a certain subfolder
   -- Extension_is            Extension comparison (with dot)
   --    Additionally we have: and or not true false, 
   --    evaluated always left to righ

   -- Examples:
   -- Smaller_than 1024 or Is_in c:/test or Extension_is .mp3
   -- Greater_than 1024 and Smaller_than 10240

   function L (S : in String) return String
      renames Misc.To_lower;

   function Qualify (
      this      : in File.Object; 
      Criterion : in String;
      Initially : in Boolean := false) 
      return Boolean is

      function Greater_than (S : String) return Boolean is
      begin
         return File.Size (this) >= Misc.Parse_size (S);
      end Greater_than;

      function Smaller_than (S : String) return Boolean is
      begin
         return File.Size (this) < Misc.Parse_size (S);
      end Smaller_than;

      function Is_in (S : String) return Boolean is
         Path : String := File.Path (This);
      begin
         return 
            S'Length > 0 and then
            S = Path (Path'First .. Path'First + S'length - 1);
      end Is_in;

      function Extension_is (S : String) return Boolean is
         Ext : String renames L (Directory_operations.File_extension (
            File.Path (This)));
      begin
         return Ext = L (S);
      end Extension_is;

      -- Extract a path delimited with ||, normalizing slashes to forward.
      function Next_path (S : String) return String is
         use Gnat.Directory_operations;
         Path : String renames Select_field (S, '|', '|');
      begin
         return Format_pathname (path, UNIX);
      end Next_path;

      function Skip_path (S : String) return String is
      begin
         return Select_field (S, 3, '|');
      end Skip_path;
      
      H    : String  := L (Head (Criterion));
      T    : String  := Tail (Criterion);
      Next : String  := Tail (T);

   begin
      if H = "" then
         return Initially;
      elsif H = "true" then
         return Qualify (this, T, true);
      elsif H = "false" then
         return Qualify (this, T, false);
      elsif H = "or" then
         return Initially or Qualify (this, T);
      elsif H = "and" then
         return Initially and Qualify (this, T);
      elsif H = "greater_than" then
         return Qualify (this, Next, Greater_than (Head (T)));
      elsif H = "smaller_than" then
         return Qualify (this, Next, Smaller_than (Head (T)));
      elsif H = "is_in" then
         return Qualify (this, Skip_path (T), Is_in (Next_path (T)));
      elsif H = "extension_is" then
         return Qualify (this, Next, Extension_is (Head (T)));
      else
         Raise_exception (Syntax_error'Identity, Criterion);
         return false;
      end if;
   end Qualify;
   
end Adagio.File.Criteria;
