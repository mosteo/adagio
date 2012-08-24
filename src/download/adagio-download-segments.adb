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

with Agpl.Bmp.Draw;
with Agpl.Constants;

-- with Text_Io;

package body Adagio.Download.Segments is

   use Agpl;
   use Seg_Avail;
   use Seg_Valid;

   package Color renames Agpl.Constants;

   Bar_Height : Positive := 3; -- In pixels.

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
      Width : in Natural := 100) return Agpl.Bmp.Object
   is
      use Ada.Streams;
      Status : Chunk_Validity_Status_Count_Array := (others => 0);
      Total  : Stream_Element_Count              := 0;

      Bitmap : Bmp.Object;
      W      : constant Stream_Element_Count := Stream_Element_Count (Width);
   begin
      pragma Assert (Count (Valid) = Count (Avail));

      -- Prepare bitmap:
      Bmp.Create (Bitmap, Width => Width, Height => Bar_Height * 3 + 2);
      Bmp.Set_Checking (Bitmap, false);
      Bmp.Draw.Delete (Bitmap, Color.White);

      -- Empty backgrounds
      Bmp.Draw.Box (Bitmap,
         1, 1,
         Bar_Height, Width,
         Color.Silver);
      Bmp.Draw.Box (Bitmap,
         Bar_Height + 1 + 1, 1,
         Bar_Height + 1 + 3, Width,
         Color.Silver);
      Bmp.Draw.Box (Bitmap,
         Bar_Height * 2 + 2 + 1, 1,
         Bar_Height * 2 + 2 + 3, Width,
         Color.Silver);

      -- Percents:
      -- Text_Io.Put_Line ("Chunks:" & Natural'Image (Count (Valid)));
      for I in 1 .. Count (Valid) loop
         declare
            Chunk : constant Seg_Valid.Chunk_Type := Get (Valid, I);
         begin
            Status (Chunk.Data) := Status (Chunk.Data) + Chunk.Last - Chunk.First + 1;
            Total               := Total               + Chunk.Last - Chunk.First + 1;
            -- Text_Io.Put_Line ("Chunk" & Chunk.First'Img & Chunk.Last'Img & " " & Chunk.Data'Img);
         end;
      end loop;
      declare
         Idx1 : Natural := Natural (Status (Download.Valid) * W / Total);
         Idx2,
         Idx3 : Natural;
      begin
         Bmp.Draw.Box (Bitmap,
            1, 1,
            Bar_Height, Idx1, CVS_Colors (Download.Valid));
         Idx2 := Natural ((Status (Download.Valid) + Status (Unknown)) * W / Total);
         Bmp.Draw.Box (Bitmap,
            1, Idx1 + 1,
            Bar_Height, Idx2, CVS_Colors (Unknown));
         Idx3 := Natural ((Status (Download.Valid) + Status (Unknown) + Status (Invalid)) * W / Total);
         Bmp.Draw.Box (Bitmap,
            1, Idx2 + 1,
            Bar_Height, Idx3, CVS_Colors (Invalid));
         Bmp.Draw.Box (Bitmap,
            1, Idx3 + 1,
            Bar_Height, Width, CVS_Colors (Missing));
      end;

      -- Validity
      for I in 1 .. W loop
         Bmp.Draw.Box (Bitmap,
            Bar_Height + 1 + 1, Integer (I),
            Bar_Height + 1 + 3, Integer (I),
            CVS_Colors (Seg_Valid.Get_At (Valid, I - 1, W - 1)));
      end loop;

      -- Avalaibility:
      Bmp.Draw.Box (Bitmap,
         Bar_Height * 2 + 2 + 1, 1,
         Bar_Height * 2 + 2 + 3, Width,
         Color.Navy);

      return Bitmap;
   end Get_Progress_Bmp;

end Adagio.Download.Segments;
