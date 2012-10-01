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
--  $Id: adagio-http-header-response.adb,v 1.3 2004/01/21 21:05:37 Jano Exp $

with Strings.Fields;
with Strings.Utils;

package body Adagio.Http.Header.Response is

   ------------------------------------------------------------------------
   -- Create_response                                                    --
   ------------------------------------------------------------------------
   -- Create the response for a file request, being it partial or not
   procedure Create_response (
      Result  : in out Header.Set;
      Request : in     Header.Set;
      RSize   : in     Natural) is

      use Strings.Fields;
      use Strings.Utils;

      Partial : Boolean := Get (Request, "Range") /= "";
      Size    : String  := Trim (Natural'Image (RSize));
      Start   : Natural;
      Final   : Natural;
   begin
      if Partial then
         declare
            Rang  : String := 
               Trim (Select_field (Get (Request, "Range"), 2, '='));
            Len   : Natural;
         begin
            Start := Natural'Value (Select_field (Rang, 1, '-'));

            if Rang (Rang'Last) = '-' then
               Final := RSize - 1;
            else
               Final := 
                  Natural'Value (Select_field (Rang, 2, '-'));
            end if;
            Len   := Final - Start + 1;

            Add (Result, "Content-Length", 
               Trim (Natural'Image (Len)));
         end;
      else
         Add (Result, "Content-Length", Size);
      end if;

      if Partial then
         Add (Result, "Content-Range", "bytes " &
            Trim (Natural'Image (Start)) & "-" &
            Trim (Natural'Image (Final)) & "/" & Size);
      end if;
   end Create_response;

end Adagio.Http.Header.Response;
