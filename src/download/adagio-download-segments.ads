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

-- Download slots refer to which is normally known as "donwloads". A slot in
-- Adagio is a regular download, with a lot of sources and stuff.
-- This package is private because is only accessed by means of the download
-- manager.

-- The slots aren't generic nor abstract. They have definite functions and
-- behavior. All genericity is encapsulated in the sources, which can be of
-- many classes, of course, and in the data consumers.

-- Intended for direct use from Download.Manager body. 

with Agpl.Bmp;
with Agpl.Segmented_Thing;

with Ada.Streams;

package Adagio.Download.Segments is

   package Segmented_Validities is new Agpl.Segmented_Thing (
      Chunk_Validity_Status, 
      Ada.Streams.Stream_Element_Offset);
   package Seg_Valid renames Segmented_Validities;

   package Segmented_Availabilities is new Agpl.Segmented_Thing (
      Natural,
      Ada.Streams.Stream_Element_Offset);
   package Seg_Avail renames Segmented_Availabilities;

   ------------------------------------------------------------------------
   -- Get_Progress_Bmp                                                   --
   ------------------------------------------------------------------------
   -- Builds a three-line BMP with the following info:
   -- Completed/Invalid/Missing percents.
   -- Valid/Invalid/Unknown/Missing chunks.
   -- Avalaibility info.
   function Get_Progress_Bmp (
      Valid : in Segmented_Validities.Object;
      Avail : in Segmented_Availabilities.Object;
      Width : in Natural := 100) return Agpl.Bmp.Object;

end Adagio.Download.Segments;
