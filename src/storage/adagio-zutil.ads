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
--  $Id: adagio-zutil.ads,v 1.3 2004/01/21 21:05:43 Jano Exp $

with Ada.Streams; use Ada.Streams;

with Zlib;

package Adagio.Zutil is

   Cannot_deflate   : exception;

   Max_initial_size : Constant := 128 * 1024;
   Increment_size   : Constant := 128 * 1024;

   ------------------------------------------------------------------------
   -- Inflate                                                            --
   ------------------------------------------------------------------------
   function Inflate (
      this     : in Stream_element_array; 
      Max_size : in Stream_element_offset := Max_Initial_Size)
   return Stream_element_array;

   ------------------------------------------------------------------------
   -- Deflate                                                            --
   ------------------------------------------------------------------------
   -- Will raise Cannot_deflate if the result is larger.
   function Deflate (this : in Stream_element_array) 
      return Stream_element_array;
   -- Will raise Cannot deflate if the resulting array is not big enough
   procedure Deflate (
      From   : in     Stream_element_array;
      To     : in out Stream_element_array;
      Last   :    out Stream_element_offset);

end Adagio.Zutil;
