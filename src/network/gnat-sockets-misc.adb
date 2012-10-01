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
--  $Id: gnat-sockets-misc.adb,v 1.3 2004/01/21 21:05:39 Jano Exp $

with Bit_arrays.Strings;

package body Gnat.Sockets.Aux is

   ------------------------------------------------------------------------
   -- To_bit_array                                                       --
   ------------------------------------------------------------------------
   function To_bit_array (I : in Inet_addr_type) return Bit_array is
      use Strings;
   begin
      case I.Family is
         when Inet_addr_v4_type =>
            declare
               Aux : String := 
                  (1 => Character'Val (I.Sin_v4 (1)),
                   2 => Character'Val (I.Sin_v4 (2)),
                   3 => Character'Val (I.Sin_v4 (3)),
                   4 => Character'Val (I.Sin_v4 (4)));
            begin
               return To_bit_array (Aux);
            end;
         when others =>
            raise Unimplemented;
      end case;
   end To_bit_array;

end Adagio.Ip_address;
