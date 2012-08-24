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
--  $Id: aenea.ads,v 1.13 2004/03/22 07:14:56 Jano Exp $

with Interfaces;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash; 

package Aenea.Types is

   -- Modular sequences:
   type Sequences is new Interfaces.Unsigned_32;

   -- To keep top uptimes
   type Uptime_Pair is record
      Nick    : Ustring;
      Uptime  : Duration;
      Version : Ustring;
   end record;
   type Uptimes_Array is array (Positive range <>) of Uptime_Pair;

   function Less (L, R : in Types.Uptime_Pair) return Boolean;
   pragma Inline (Less);

   -- Equality by nick
   function Equal (L, R : in Types.Uptime_Pair) return Boolean;
   pragma Inline (Equal);

   package Sorted_Hubs is new Ada.Containers.Ordered_Sets (
      Types.Uptime_Pair, Less, Equal);

   package Hashed_Hubs is new Ada.Containers.Indefinite_Hashed_Maps (
      String, Types.Uptime_Pair, Ada.Strings.Hash, "=", Equal);

end Aenea.Types;
