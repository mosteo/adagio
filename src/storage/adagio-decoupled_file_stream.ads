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
--  $Id: adagio-decoupled_file_stream.ads,v 1.3 2004/01/21 21:05:41 Jano Exp $
-- Decoupled file stream
-- Only useful for files, and only for reading (at the moment)
-- The purpose is to use this to read chunks of files, without
-- keeping them opened.
-- Thus, the file can be moved/deleted during reading and an
-- exception will be reported, instead of making the file unusable.
-- This also implies that modifications to the file will go undetected,
-- and that is probably no good.

with Ada.Streams;                use Ada.Streams;
with Ada.Streams.Stream_IO; 

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Adagio.Decoupled_file_stream is

   -- New stream type:
   type Decoupled_file_stream is new 
      Ada.Streams.Root_Stream_Type with private;
   type Object_access is access all Decoupled_file_stream;

   -- Overriden primitives:
   procedure Read(
      Stream : in out Decoupled_file_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Decoupled_file_stream;
      Item   : in Stream_Element_Array);

   -- Creation. We need a path to file:
   procedure Get_decoupled_file_stream
     (Stream: out Decoupled_file_stream; 
      File:   String); 

   -- Seek
   procedure Set_position
     (Stream: in out Decoupled_file_stream;
      Pos: Stream_IO.Positive_count);

   -- Size
   function Size(Stream: Decoupled_file_stream) return Stream_IO.Count;

   -- EOF
   function End_of_stream(Stream: Decoupled_file_stream) return boolean;

private

   type Decoupled_file_stream is new Ada.Streams.Root_Stream_Type 
   with record
      File: UString:= To_UString("");
      Filepos: Stream_IO.Positive_count:= 1;
   end record;

end Adagio.Decoupled_file_stream;
