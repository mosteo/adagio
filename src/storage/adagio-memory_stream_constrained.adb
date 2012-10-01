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
--  $Id: adagio-memory_stream_constrained.adb,v 1.3 2004/01/21 21:05:41 Jano Exp $

with System.Address_to_access_conversions;

package body Adagio.Memory_stream_constrained is

   package Convert is new System.Address_to_access_conversions (Big_array);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      Stream : out Stream_type;
      Data   : System.Address;
      Length : Natural) is
   begin
      Stream.Buffer := Array_access (Convert.To_pointer (Data));
      Stream.Length := Stream_element_offset (Length);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      if Item'Length > Stream.Length - Stream.Pos + 1 then
         Item (Item'First .. Item'First + Stream.Length - Stream.Pos) :=
            Stream.Buffer (Stream.Pos .. Stream.Length);
         Last        := Item'First + Stream.Length - Stream.Pos;
         Stream.Pos  := Stream.Length + 1;
      else
         Item := Stream.Buffer (Stream.Pos .. Stream.Pos + Item'Length - 1);
         Last        := Item'Last;
         Stream.Pos  := Stream.Pos + Item'Length;
      end if;
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
	procedure Write (
      Stream : in out Stream_type;
      Item   : in Stream_Element_Array) is
      Pos    : Stream_element_offset renames Stream.Pos;
   begin
      if Pos + Item'Length - 1 > Stream.Length then
         raise Constraint_error;
      end if;
      Stream.Buffer (Pos .. Pos + Item'Length - 1) := Item;
      Pos := Pos + Item'Length;
   end Write;

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   function Index (Stream : in Stream_type) return Stream_element_offset is
   begin
      return Stream.Pos - 1;
   end Index;

   ------------------------------------------------------------------------
   -- End_of_stream                                                      --
   ------------------------------------------------------------------------
   function End_of_stream (Stream : in Stream_type) return boolean is
   begin
      return Stream.Pos > Stream.Length;
   end End_of_stream;
   
end Adagio.Memory_stream_constrained;
