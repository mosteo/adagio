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
--  $Id: acf-types-base32.adb,v 1.3 2004/01/21 21:05:40 Jano Exp $

with Bit_arrays; 
with Bit_arrays.Modular;
with Bit_arrays.Numbers;

package body Acf.Types.Base32 is

   package Byte_bit is new Bit_arrays.Modular (Byte);

   function To_base32 (Bytes : in Byte_array) return String is
      use Bit_arrays;
      Base32 : array (0 .. 31) of Character := 
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
      Raw    : Bit_array (1 .. Bytes'Length * 8);
      Pos    : Natural := Raw'First;
      s      : String (1 .. Raw'Length / 5);
      B : Byte_array (Bytes'range);
   begin
      -- Reverse Bytes:
      for N in 0 .. 7 loop
         B (B'First + N) := Bytes (Bytes'First + 7 - N);
      end loop;
      for N in 8 .. 15 loop
         B (B'First + N) := Bytes (Bytes'First + 15 - N + 8);
      end loop;
      for N in 16 .. 23 loop
         B (B'First + N) := Bytes (Bytes'First + 23 - N + 16);
      end loop;
      -- Init the bit array:
      for i in Bytes'range loop
         Raw (Pos .. Pos + 7) := Byte_bit.To_bit_array_BE (B (i));
         Pos:= Pos + 8;
      end loop;
      -- Churn the base32 representation:
      Pos := Raw'First;
      for i in s'range loop
         s (i) := 
            Base32 (Bit_arrays.Numbers.To_number_BE (Raw (Pos .. Pos + 4)));
         Pos := Pos + 5;
      end loop;
      return s;
   end To_Base32;
   
end Acf.Types.Base32;
