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
--  $Id: adagio-upload-resource-factory.adb,v 1.5 2004/01/29 21:47:10 Jano Exp $

-- Obtaining resources from a request string.

With
Agpl.Types.Ustrings,
Adagio.File,
Adagio.Library,
Adagio.Misc,
Adagio.Upload.Resource.File,
Adagio.Upload.Resource.TTH,
Strings.Utils;

Use
Agpl.Types,
Agpl.Types.Ustrings,
Adagio.Misc,
Strings.Utils;

package body Adagio.Upload.Resource.Factory is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Returns a resource from a request string.
   -- Can raise the exceptions in Adagio.Upload.Resource.
   function Create (Request : in String) return Resource.Object_access is
      Hash : Ustring;
      F    : Adagio.File.Object;
      use type Adagio.File.Object;
   begin
      if Contains (Request, "uri-res") then
         -- FILE RESOURCES
         if Contains (Request, "sha1") then
            -- SHA1
            Hash   := U (Select_field (Request, ':', ' ', 2));
            if S (Hash)'Length /= 32 then
               raise Malformed_request;
            end if;
            F := Library.Object.Query_sha1 (S (Hash));
         elsif Contains (Request, "ed2k") then
            -- ED2K
            -- Accept N2R?ed2k: and N2R?urn:ed2k:
            Hash   := U (Select_field (Request, ':', ' ', 1));
            if S (Hash)'Length /= 32 then
               Hash   := U (Select_field (Request, ':', ' ', 2));
               if S (Hash)'Length /= 32 then
                  raise Malformed_request;
               end if;
            end if;
            F := Library.Object.Query_ed2k (S (Hash));
         else
            raise Unknown;
         end if;
         if F = Adagio.File.Null_file then
            raise Unavailable;
         else
            return Resource.Object_access (Upload.Resource.File.Create (F));
         end if;
      elsif Contains (Request, "/gnutella/tigertree/v3?urn") then
         -- TIGERTREE RESOURCES
         declare
            Hash : String := Select_field (Request, ':', ' ', 3);
         begin
            if Hash'Length /= 39 then
               raise Malformed_request;
            end if;
            F := Library.Object.Query_TTH (Hash);
         end;
         if F = Adagio.File.Null_file then
            raise Unavailable;
         else
            return Resource.Object_access (Upload.Resource.TTH.Create (F));
         end if;
      else
         raise Unknown;
      end if;
   end Create;

end Adagio.Upload.Resource.Factory;
