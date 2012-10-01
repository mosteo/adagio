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
--  $Id: aenea-hub-topo-placer_tree.adb,v 1.1 2004/03/22 07:14:56 Jano Exp $

with Aenea.Globals.Options;

with Agpl.Dynamic_vector;

package body Aenea.Hub.Topo.Placer_tree is

   HSpace        : Float renames Globals.Options.Topo_tree_HSpace;
   VSpace        : Float renames Globals.Options.Topo_tree_VSpace;
   Pile          : Boolean renames Globals.Options.Topo_tree_pile;

   package Float_vector is new Agpl.Dynamic_vector (Float);
   use Float_vector;

   Level_pos : Float_vector.Object (First => 1);

   -- Places a hub according to the parent placer.
   function Place (This : in Placer_type; Pos, Num : in Positive) 
      return Placer_type
   is
      Result : Placer_type := This;
   begin

      if This.Is_root then
         Result.Is_root := false;
      end if;

      -- Regular placement:
      if Last (Level_pos) < This.Level then
         Append (Level_pos, 0.0);
      end if;

      if Pile then
         Result.X := Level_pos.Vector (This.Level);
      else
         Result.X := Float'Max (Level_pos.Vector (This.Level), This.X);
      end if;

      Level_pos.Vector (This.Level) := Result.X + HSpace;
      Result.Y                      := Float (This.Level - 1) * VSpace;
      Result.Level                  := Result.Level + 1;

      return Result;
   end Place;

   -- Get an initial placer to be passed to Build_topogram
   function Get_root (Width, Height : in Positive)
      return Placer_type
   is
   begin
      return (
         Is_root => true,
         X       => 0.0, 
         Y       => 0.0,
         W       => Float (Width), 
         H       => Float (Height),
         Level   => 1);
   end Get_root;

   -- Preprocess/postprocess data globally:
   procedure Preprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Data);
   begin
      null;
   end Preprocess;

   procedure Postprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Data);
   begin
      Clear (Level_pos);
   end Postprocess;

end Aenea.Hub.Topo.Placer_tree;
