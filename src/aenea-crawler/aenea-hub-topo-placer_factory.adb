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
--  $Id: aenea-hub-topo-placer_factory.adb,v 1.3 2004/03/22 07:14:55 Jano Exp $

with Aenea.Hub.Topo.Placer_dynamic;
with Aenea.Hub.Topo.Placer_radial;
with Aenea.Hub.Topo.Placer_topdown;
with Aenea.Hub.Topo.Placer_tree;

package body Aenea.Hub.Topo.Placer_factory is

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   function Get (
      This   : in Kinds; 
      Width  : in Positive;
      Height : in Positive) return Placer_type'Class 
   is
   begin
      case This is
         when Dynamic => return Placer_dynamic.Get_root (Width, Height);
         when Radial  => return Placer_radial.Get_root (Width, Height);
         when TopDown => return Placer_topdown.Get_root (Width, Height);
         when Tree    => return Placer_tree.Get_root (Width, Height);
      end case;
   end Get;

end Aenea.Hub.Topo.Placer_factory;