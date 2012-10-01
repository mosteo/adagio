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
--  $Id: adagio-convert.adb,v 1.3 2004/01/21 21:05:28 Jano Exp $

with Adagio.Misc;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Adagio.Convert is

   Units : constant Ustring_array := 
      (U ("B"), U ("kB"), U ("mB"), U ("gB"), U ("tB"));

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   -- Beautifies some quantity (bytes) appending the correct units
   function To_size (
      Qty : in Integer; Decimals : in Natural := 2) return String 
   is
      Pos   : Integer := Units'First;
      Q     : Float   := Float (Qty);
   begin
      if Qty < 0 then
         return "-" & To_size (-Qty);
      end if;

      loop
         exit when Q / 1024.0 < 1.0 or else Pos = Units'Last;
         Q   := Q / 1024.0;
         Pos := Pos + 1;
      end loop;

      return Misc.To_string (Q, Decimals) & " " & S (Units (Pos));
   end To_size;

   ------------------------------------------------------------------------
   -- To_size                                                            --
   ------------------------------------------------------------------------
   -- Beautifies some quantity (bytes) appending the correct units
   -- Long integer flavor
   function To_size (
      Qty : in Float; Decimals : in Natural := 2) return String 
   is
      Pos   : Integer := Units'First;
      Q     : Float   := Qty;
   begin
      if Qty < 0.0 then
         return "-" & To_size (-Qty);
      end if;

      loop
         exit when Q / 1024.0 < 1.0 or else Pos = Units'Last;
         Q   := Q / 1024.0;
         Pos := Pos + 1;
      end loop;

      return Misc.To_string (Q, Decimals) & " " & S (Units (Pos));
   end To_size;

end Adagio.Convert;
