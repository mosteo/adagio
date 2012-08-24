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
--  $Id: adagio-memory_stream_constrained.ads,v 1.3 2004/01/21 21:05:41 Jano Exp $

--  Allows reading from memory viewed as an stream:

with Ada.Streams;                use Ada.Streams;

with System;

package Adagio.Memory_stream_constrained is

   -- New stream type:
   type Stream_type is new Ada.Streams.Root_Stream_Type with private;

   -- Data should be the address of the first element.
   -- Beware of alignment!!!!
   -- Only valid for contiguous data (or if you known what you are doing).
   procedure Create (
      Stream : out Stream_type;
      Data   : System.Address;
      Length : Natural);

   procedure Read (
      Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Can cause constraint_error if the supplied buffer is exhausted.
	procedure Write (
      Stream : in out Stream_type;
      Item   : in Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   -- Amount of data written / read.
   function Index (Stream : in Stream_type) return Stream_element_offset;

   -- EOF
   function End_of_stream (Stream : in Stream_type) return boolean;

private

   subtype Big_array is 
      Stream_element_array (1 .. Stream_element_offset'Last);
   type Array_access is access all Big_array;

   type Stream_type is new Ada.Streams.Root_Stream_Type with record
      Buffer : Array_access; 
      Pos    : Stream_element_offset := 1;
      Length : Stream_element_offset;
   end record;

end Adagio.Memory_stream_constrained;
