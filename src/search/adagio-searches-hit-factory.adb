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
--  $Id: adagio-upload.ads,v 1.4 2004/01/21 21:05:51 Jano Exp $

--  Root package for all search packages

with Adagio.G2.Hit;
with Adagio.Trace;

with Ada.Tags;
use  Ada;

package body Adagio.Searches.Hit.Factory is

   ------------------------------------------------------------------------
   -- Create_From_Xml                                                    --
   ------------------------------------------------------------------------
   function Create_From_Xml (Node : in Xml.Node) return Hit.Object'Class is
      use type Tags.Tag;
   begin
      if Tags.Internal_Tag (Xml.Get_Attribute (Node, "tag", "")) = G2.Hit.Object'Tag then
         declare
            H : G2.Hit.Object := G2.Hit.Create_From_Xml (Node);
         begin
            Hit.Merge_From_Xml (Hit.Object (H), Node); -- Restore common data.
            return Hit.Object'Class (H);
         end;
      else
         -- Unknown hit?
         Trace.Log ("Unknown hit while restoring, tag: " & Xml.Get_Attribute (Node, "tag", ""), Trace.Warning);
         raise Tags.Tag_Error;
      end if;
   end Create_From_Xml;

end Adagio.Searches.Hit.Factory;
