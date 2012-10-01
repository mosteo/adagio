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

--  Handlers for the Adagio graphs

with Aenea.Gui.Graphs;
with Aenea.Trace; use Aenea.Trace;

with Agpl.Bmp;
with Agpl.Bmp.Draw;
with Agpl.Constants;
with Agpl.Http.Server;
with Agpl.Http.Server.Simple_handler;

with Aws.Messages;
with Aws.Parameters;
with Aws.Response;
with Aws.Status;

package body Aenea.Gui.Bitmaps is

   type Images is
     (None,
      BW_usage_udp,
      Memory_usage);

   Null_bitmap : Agpl.Bmp.Object; -- Empty black 1x1 image

   ------------------------------------------------------------------------
   -- Bitmap_handler                                                     --
   ------------------------------------------------------------------------
   function Bitmap_handler (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      Bitmap : Agpl.Bmp.Object;
      Params : Aws.Parameters.List := Aws.Status.Parameters (Request);
      Image  : Images;
   begin
      begin
         Image := Images'Value (Aws.Parameters.Get (Params, "bmp"));
      exception
         when others =>
            Image := None;
      end;

      Log ("Image requested: " & Image'Img, Trace.Never);

      begin
         case Image is
         when BW_usage_udp | Memory_usage =>
            Bitmap := Graphs.Get_graph
              (Graphs.Available_graphs'Value
                 (Images'Image (Image)));
         when others =>
            Bitmap := Null_bitmap;
         end case;
      exception
         when E : others =>
            Log ("Bitmap_Handler: " & Report (E));
      end;

      return Aws.Response.Build
        (Agpl.Bmp.Mime_Type,
         Agpl.Bmp.Get_stream (Bitmap),
         Cache_control => Aws.Messages.No_cache);
   end Bitmap_handler;

   Bitmap_object : Agpl.Http.Server.Simple_handler.Object (
      Bitmap_handler'Access);

   ------------------------------------------------------------------------
   -- Register                                                           --
   ------------------------------------------------------------------------
   procedure Register is
   begin
      Agpl.Http.Server.Register_handler ("/cgi-bmp", Bitmap_object);
   end Register;

begin
   Agpl.Bmp.Create (Null_bitmap, Width => 8, Height => 8);
   Agpl.Bmp.Draw.Delete (Null_bitmap, Agpl.Constants.White);
end Aenea.Gui.Bitmaps;
