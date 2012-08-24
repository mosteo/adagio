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
--  $Id: aenea-hub-topo.ads,v 1.6 2004/03/22 07:14:55 Jano Exp $

with Agpl.Bmp;
with Agpl.Dynamic_vector;

with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;

with Ada.Numerics;
with Ada.Numerics.Generic_elementary_functions;
use  Ada;

package Aenea.Hub.Topo is

   type Kinds is (Radial, Topdown, Dynamic, Tree);

   ------------------------------------------------------------------------
   -- Topodata                                                           --
   ------------------------------------------------------------------------
   type Topodata_type is private;
   type Topodata_access is access all Topodata_type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creates a topodata for a given hub:
   function Create (Target : in Hub.Object_access) return Topodata_access;

   ------------------------------------------------------------------------
   -- Add_neighbor                                                       --
   ------------------------------------------------------------------------
   procedure Add_neighbor (This, Neigh : access Topodata_type);
   
   ------------------------------------------------------------------------
   -- Topo_maps                                                          --
   ------------------------------------------------------------------------
   package Topo_maps is new Charles.Maps.Hashed.Strings.Unbounded (
      Topodata_access,
      Charles.Hash_string,
      "=",
      "=");
   
   ------------------------------------------------------------------------
   -- Roots                                                              --
   ------------------------------------------------------------------------
   type Root_type is private;
   type Root_array is array (Positive range <>) of Root_type;
   function Get_roots (Data : in Topo_maps.Container_type) return Root_array;

   ------------------------------------------------------------------------
   -- Placers                                                            --
   ------------------------------------------------------------------------
   -- The type.
   type Placer_type is abstract tagged private;
   -- Places a hub according to the parent placer.
   function Place (This : in Placer_type; Pos, Num : in Positive) 
      return Placer_type is abstract;
   -- Get coordinates
   function Get_X (This : in Placer_type) return Integer;
   function Get_Y (This : in Placer_type) return Integer;
   function Get_W (This : in Placer_type) return Integer;
   function Get_H (This : in Placer_type) return Integer;
      pragma Inline (Get_X, Get_Y, Get_W, Get_H);
   -- Get an initial placer to be passed to Build_topogram
   function Get_root (Width, Height : in Positive)
      return Placer_type is abstract;
   -- Preprocess/postprocess data globally:
   procedure Preprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type);
   procedure Postprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type);

   ------------------------------------------------------------------------
   -- Prepare_topogram                                                   --
   ------------------------------------------------------------------------
   -- Prepares a topomap from a map of hubs. It can be later used with
   -- build_topogram.
   procedure Prepare_topogram (
      Hubs     : in  Hub_map.Container_type;
      Topodata : out Topo_maps.Container_type);

   ------------------------------------------------------------------------
   -- Build_topogram                                                     --
   ------------------------------------------------------------------------
   -- Requires a list with the hubs to paint
   -- Will free all memory used by the topodatas
   function Build_topogram (
      Topodata : in Topo_maps.Container_type;
      Placer   : in Placer_type'Class
      )
      return Agpl.Bmp.Object;

private

   type Force_type is record
      DX : Float;
      DY : Float;
      X  : Float;
      Y  : Float;
   end record;

   Null_force : constant Force_type := (0.0, 0.0, 0.0, 0.0);

   type Topodata_type is record
      Expanded: Boolean  := false;
      Placed  : Boolean  := false;
      X, Y    : Integer  := 0;
      F       : Force_type;
      Leaves  : Positive;
      Neighs  : Topo_maps.Container_type;     
      Hub     : Aenea.Hub.Object_access;
   end record;

   type Root_type is record
      Root    : Topodata_access; -- Entry point with most neighbors
      Hubs    : Natural;         -- Num of hubs in this connected component.
   end record;
   package Root_arrays is new Dynamic_vector (Root_type);

   type Placer_type is abstract tagged record
      Is_root : Boolean;
      X, Y    : Float;
      W, H    : Float;
   end record;
   
   ------------------------------------------------------------------------
   -- Unplaced_neighbors                                                 --
   ------------------------------------------------------------------------
   function Unplaced_neighbors (This : access Topodata_type) return Natural;

   procedure Unplace (This : in Topo_maps.Iterator_type);
   procedure Unplace_all is new Topo_maps.Generic_iteration (Unplace);

   ------------------------------------------------------------------------
   -- Get_best_connected                                                 --
   ------------------------------------------------------------------------
   -- Returns the hub with most neighbors in this connected component
   function Get_best_connected (
      Data : in Topo_maps.Container_type; This : access Topodata_type)
      return Topodata_access;

   ------------------------------------------------------------------------
   -- Fit_to_size                                                        --
   ------------------------------------------------------------------------
   procedure Fit_to_size (
      Topodata : in Topo_maps.Container_type; W, H : in Positive);

   ------------------------------------------------------------------------
   -- Math                                                               --
   ------------------------------------------------------------------------
   package Math is new Ada.Numerics.Generic_elementary_functions (Float);

   Pi : constant Float := Ada.Numerics.Pi;

end Aenea.Hub.Topo;
