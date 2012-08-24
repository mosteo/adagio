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
--  $Id: adalid.ads,v 1.4 2004/01/21 21:05:24 Jano Exp $

with Ada.Calendar;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded;

package Adalid is

   -- Unimplemented features:
   Unimplemented        : exception;

   -- Initialization failed:
   Initialization_error : exception;

   -- Some constants:
   User_agent : String := "Adalid 0.0.1 20031017";

   Nul        : constant Character := Character'Val (0);

   Past_aeons : constant Ada.Calendar.Time :=
      Ada.Calendar.Time_of (1976, 9, 6);

   -- Some types:
   subtype File_size is Integer;
   subtype Speed     is Integer;

   -- Commodity concession:
   package ASU renames Ada.Strings.Unbounded;
   subtype UString is ASU.Unbounded_string;
   function To_string (U: UString) return String
      renames ASU.To_string;
   function To_ustring (S: String) return UString
      renames ASU.To_unbounded_string;
   function S (U: UString) return String
      renames ASU.To_string;
   function U (S: String) return UString
      renames ASU.To_unbounded_string;
   use type ASU.Unbounded_string;

   Null_ustring : UString renames ASU.Null_unbounded_string;

   type Ustring_array is array (Integer range <>) of Ustring;

end Adalid;
