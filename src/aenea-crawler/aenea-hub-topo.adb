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
--  $Id: aenea-hub-topo.adb,v 1.7 2004/03/22 07:14:55 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Misc;
with Aenea.Trace;

with Agpl.Bmp.Draw;
with Agpl.Constants;
with Agpl.Types;
use  Agpl;

with Ada.Numerics;
with Ada.Numerics.Generic_elementary_functions;
with Ada.Unchecked_deallocation;
use  Ada;

package body Aenea.Hub.Topo is

   Umbral_New : Duration renames Globals.Options.Topo_UmbralNew;

   Margin : constant := 10;

   procedure Free is new Unchecked_deallocation (
      Topodata_type, Topodata_access);

   ------------------------------------------------------------------------
   -- Get_best_connected                                                 --
   ------------------------------------------------------------------------
   -- Returns the hub with most neighbors in this connected component
   function Get_best_connected (
      Data : in Topo_maps.Container_type; This : access Topodata_type)
      return Topodata_access
   is
      use Topo_maps;
      Max  : Natural := 0;
      Best : Topodata_access;
      procedure Search (This : access Topodata_type) is
         I : Topo_maps.Iterator_type := First (This.Neighs);
      begin
         This.Placed := true;
         if Length (This.Neighs) > Max then 
            Max  := Length (This.Neighs);
            Best := Topodata_access (This);
         end if;
         while I /= Back (This.Neighs) loop
            if not Element (I).Placed then
               Search (Element (I));
            end if;
            I := Succ (I);
         end loop;
      end Search;
   begin
      Unplace_all (First (Data), Back (Data));
      Search (This);
      Unplace_all (First (Data), Back (Data));
      if Best = null then
         return Topodata_access (This);
      else
         return Best;
      end if;
   end Get_best_connected;

   ------------------------------------------------------------------------
   -- Prepare_topogram                                                   --
   ------------------------------------------------------------------------
   -- Prepares a topomap from a map of hubs. It can be later used with
   -- build_topogram.
   procedure Prepare_topogram (
      Hubs     : in  Hub_map.Container_type;
      Topodata : out Topo_maps.Container_type)
   is
      use Hub_map;
      use Topo_maps;
      I      : Hub.Hub_map.Iterator_type;
      H, NH  : Hub.Object_access;
      T, NT  : Hub.Topo.Topodata_access;
      J      : Hub.Links_map.Iterator_type;
      K      : Hub.Topo.Topo_maps.Iterator_type;
      Neighs : Hub.Links_map.Container_type;
      use type Hub.Links_map.Iterator_type;
   begin
      Trace.Log ("****************************** STARTING TOPODATA BUILDING",
         Trace.Debug);
      -- First round, consolidate two way neighborships
      I := First (Hubs);
      while I /= Back (Hubs) loop
         H := Element (I);
         if Hub.Alive (H.all) and then 
            Hub.Leaves (H.all) > 0 
         then
            Hub.Get_neighbors (H.all, Neighs);
            J := Hub.Links_map.First (Neighs);
            while J /= Hub.Links_map.Back (Neighs) loop
               NH := Hub.Links_map.Element (J).Peer;
               Hub.Make_neighbors (H, NH);
               J := Hub.Links_map.Succ (J);
            end loop;
         end if;
         I := Succ (I);
      end loop;

      -- Second round, create all candidate topodatas
      I := First (Hubs);
      while I /= Back (Hubs) loop
         H := Element (I);
         if Hub.Alive (H.all) and then 
            Hub.Leaves (H.all) > 0
         then
            Trace.Log ("[+] Topodata: " & Hub.Id (H.all), Trace.Debug);
            T := Hub.Topo.Create (H);
            Insert (Topodata, Hub.Id (H.all), T);
         end if;
         I := Succ (I);
      end loop;

      -- Third round, create topodata links
      K := First (topodata);
      while K /= Back (Topodata) loop
         H := Element (Find (Hubs, Key (K)));
         Hub.Get_neighbors (H.all, Neighs);
         J := Hub.Links_map.First (Neighs);
         while J /= Hub.Links_map.Back (Neighs) loop
            NH := Hub.Links_map.Element (J).Peer;
            if Hub.Alive (NH.all) and then 
               Hub.Leaves (NH.all) > 0
            then
               Trace.Log ("[+] Link " & Key (K) & " - " & Hub.Id (NH.all),
                  Trace.Debug);
               NT := Element (Find (Topodata, Hub.Id (NH.all)));
               Hub.Topo.Add_neighbor (Element (K), NT);
            end if;
            J := Hub.Links_map.Succ (J);
         end loop;
         K := Succ (K);
      end loop;
      Trace.Log ("****************************** FINISHED TOPODATA BUILDING",
         Trace.Debug);
   end Prepare_topogram;

   ------------------------------------------------------------------------
   -- Roots                                                              --
   ------------------------------------------------------------------------
   function Get_roots (Data : in Topo_maps.Container_type) return Root_array 
   is
      use Root_arrays;
      use Topo_maps;

      Roots : Root_arrays.Object (First => 1);
      I     : Topo_maps.Iterator_type := First (Data);
      Num   : Natural;

      procedure Place (This : access Topodata_type; Total : in out Natural) is
         J : Topo_maps.Iterator_type := First (This.Neighs);
      begin
         This.Placed := true;
         Total       := Total + 1;
         while J /= Back (This.Neighs) loop
            if not Element (J).Placed then
               Place (Element (J), Total);
            end if;
            J := Succ (J);
         end loop;
      end Place;
   begin
      Unplace_all (First (Data), Back (Data));

      while I /= Back (Data) loop
         if not Element (I).Placed then
            Num := 0;
            Place (Element (I), Num);
            if Num > Globals.Options.Topo_MinimumCount then
               Append (Roots, (Element (I), Num));
            end if;
         end if;
         I := Succ (I);
      end loop;

      Unplace_all (First (Data), Back (Data));

      return Root_array (Roots.Vector (1 .. Last (Roots)));
   end Get_roots;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creates a topodata for a given hub:
   function Create (Target : in Hub.Object_access) return Topodata_access is
      TD : Topodata_access := new Topodata_type;
   begin
      TD.Leaves := Leaves (Target.all);
      TD.Hub    := Target;
      return TD;
   end Create;

   ------------------------------------------------------------------------
   -- Add_neighbor                                                       --
   ------------------------------------------------------------------------
   procedure Add_neighbor (This, Neigh : access Topodata_type) is
   begin
      Topo_maps.Insert (
         This.Neighs, Id (Neigh.Hub.all), Topodata_access (Neigh));
   end Add_neighbor;

   ------------------------------------------------------------------------
   -- Unplaced_neighbors                                                 --
   ------------------------------------------------------------------------
   function Unplaced_neighbors (This : access Topodata_type) return Natural is
      use Topo_maps;
      I   : Topo_maps.Iterator_type := First (This.Neighs);
      Num : Natural := 0;
   begin
      while I /= Back (This.Neighs) loop
         if not Element (I).Placed then
            Num := Num + 1;
         end if;
         I := Succ (I);
      end loop;
      return Num;
   end Unplaced_neighbors;

   ------------------------------------------------------------------------
   -- Build_topogram                                                     --
   ------------------------------------------------------------------------
   -- Requires a list with the hubs to paint
   function Build_topogram (
      Topodata : in Topo_maps.Container_type;
      Placer   : in Placer_type'Class) return Agpl.Bmp.Object 
   is
      B      : Bmp.Object;
      use Topo_maps;

      ---------------------
      -- Get_mean_uptime --
      ---------------------
      function Get_mean_uptime return Duration is
         Total : Duration := 0.0;
         I     : Topo_maps.Iterator_type := First (Topodata);
         Now   : Calendar.Time := Calendar.Clock;
      begin
         while I /= Back (Topodata) loop
            Total := Total + Now - Element (I).Hub.Stat_first_seen;
            I := Succ (I);
         end loop;
         return Total / Duration (Length (Topodata));
      end Get_mean_uptime;

      --------------------------
      -- Get_mean_link_uptime --
      --------------------------
      function Get_mean_link_uptime return Duration is
         Total : Duration := 0.0;
         I     : Topo_maps.Iterator_type := First (Topodata);
         J     : Topo_maps.Iterator_type;
         Num   : Natural := 0;
      begin
         while I /= Back (Topodata) loop
            J := First (Element (I).Neighs);
            while J /= Back (Element (I).Neighs) loop
               Total := Total +  
                  Hub.Get_link_duration (Element (I).Hub.all, Key (J));
               Num   := Num + 1;
               J := Succ (J);
            end loop;
            I := Succ (I);
         end loop;
         return Total / Duration (Num);
      end Get_mean_link_uptime;

      ---------------
      -- Place_hub --
      ---------------
      procedure Place_hub (
         T      : access Topodata_type;
         P      : Placer_type'Class)
      is
      begin
         pragma Assert (T.Placed = T.Expanded);
         if T.Placed then 
            return;
         end if;

         T.Placed   := true;
         T.X        := Get_X (P);
         T.Y        := Get_Y (P);
      end Place_hub;

      ----------------
      -- Expand_hub --
      ----------------
      procedure Expand_hub (
         T     : access Topodata_type;
         P     : in Placer_type'Class)
      is
         I   : Topo_maps.Iterator_type := First (T.Neighs);
         NT  : Topodata_access;
         Num : Natural                 := Unplaced_neighbors (T);
         Pos : Positive                := 1; -- Pos in unplaced
         PCa : array (1 .. Length (T.Neighs)) of Natural := (others => 0); 
         PIx : Positive                := 1; -- Cache of pos
      begin
         pragma Assert (T.Placed);

         -- Place neighbors:
         while I /= Back (T.Neighs) loop
            NT := Element (I);
            if not NT.Placed then
               declare
                  NP : Placer_type'Class := Place (P, Pos, Num);
               begin
                  Place_hub (NT, NP);
               end;  
               PCa (PIx) := Pos;
               Pos := Pos + 1;
            end if;
            PIx := PIx + 1;
            I  := Succ (I);
         end loop;

         -- Expand unexpanded:
         I   := First (T.Neighs);
         PIx := 1;
         while I /= Back (T.Neighs) loop
            NT := Element (I);
            if PCa (PIx) > 0 then
               declare
                  NP : Placer_type'Class := Place (P, PCa (PIx), Num);
               begin
                  Expand_hub (NT, NP);
               end;  
            end if;
            PIx := PIx + 1;
            I  := Succ (I);
         end loop;
      end Expand_hub;

      ----------------
      -- Draw_links --
      ----------------
      procedure Draw_links is
         I  : Topo_maps.Iterator_type := First (Topodata);
         J  : Topo_maps.Iterator_type;
         T1 : Topodata_access;
         T2 : Topodata_access;
         MeanL : Duration      := Get_mean_link_uptime;
         Color : Types.RGB_Triplet;
      begin
         while I /= Back (Topodata) loop
            T1 := Element (I);
            if T1.Placed then
               J  := First (T1.Neighs);
               while J /= Back (T1.Neighs) loop
                  T2 := Element (J);
                  if T2.Placed then
                     if Hub.Get_link_duration (T1.Hub.all, Key (J)) > MeanL 
                     then
                        Color := Constants.Crimson;
                     else
                        Color := Constants.Red;
                     end if;
                     Bmp.Draw.Line (B,
                        R1 => T1.Y,
                        C1 => T1.X,
                        R2 => T2.Y,
                        C2 => T2.X,
                        Color => Color);
                  else
                     Trace.Log ("Topo.Draw_links: UNPLACED NEIGHBOR", 
                        Trace.Error);
                  end if;
                  J := Succ (J);
               end loop;
            end if;
            I := Succ (I);
         end loop;
         Trace.Log ("Mean link uptime: " & Misc.Image (MeanL), 
            Trace.Informative);
      end Draw_links;

      ----------------
      -- Draw_cores --
      ----------------
      procedure Draw_cores is
         I  : Topo_maps.Iterator_type := First (Topodata);
         T  : Topodata_access;
         Color : Types.RGB_Triplet;
         Now   : Calendar.Time := Calendar.Clock;
         Mean  : Duration      := Get_mean_uptime;
         Upt   : Duration;
      begin
         while I /= Back (Topodata) loop
            T := Element (I);
            if T.Placed then
               Upt := Now - T.Hub.Stat_first_seen;
               if Upt < Umbral_New then
                  Color := Constants.Green;
               elsif T.Hub.Failures > 0 then
                  Color := Constants.Yellow;
               elsif Upt < Mean then
                  Color := Constants.Blue;
               else
                  Color := Constants.Navy;
               end if;
               Bmp.Draw.Circle (B,
                  Row    => T.Y,
                  Col    => T.X,
                  Rad    => T.Leaves / 100 + 2,
                  Color  => Constants.Navy,
                  Fill   => Color);
            end if;
            I := Succ (I);
         end loop;
         Trace.Log ("Mean uptime     : " & Misc.Image (Mean), 
            Trace.Informative);
      end Draw_cores;

      -------------------
      -- Free_topodata --
      -------------------
      procedure Free_topodata is
         I  : Topo_maps.Iterator_type := First (Topodata);
         T  : Topodata_access;
         Visited : Natural := 0;
      begin
         while I /= Back (Topodata) loop
            T := Element (I);
            if T.Placed then
               Visited := Visited + 1;
               Trace.Log (
                  Id (T.Hub.all) & 
                  T.Hub.Leaves'Img &
                  T.X'Img &
                  T.Y'Img, Trace.Debug);
            end if;
            Free (T);
            I := Succ (I);
         end loop;
         Trace.Log ("Visited" & Visited'Img & " hubs of" &
            Natural'Image (Topo_maps.Length (Topodata)), Trace.Debug);
      end Free_topodata;

      -----------------
      -- Report_hubs --
      -----------------
      procedure Report_hubs is
         I  : Topo_maps.Iterator_type := First (Topodata);
         J  : Topo_maps.Iterator_type;
         T  : Topodata_access;
         NT : Topodata_access;
      begin
         Trace.Log ("************* HUBS REPORT **************", Trace.Debug);
         while I /= Back (Topodata) loop
            T := Element (I);
            Trace.Log (Key (I) & " " & T.Leaves'Img, Trace.Debug);
            J := First (T.Neighs);
            while J /= Back (T.Neighs) loop
               NT := Element (J);
               Trace.Log ("   " & Key (J) & " " & NT.Leaves'Img, Trace.Debug);
               J := Succ (J);
            end loop;
            I := Succ (I);
         end loop;
         Trace.Log ("****************************************", Trace.Debug);
      end Report_hubs;

   begin
      Bmp.Create (B, Width => Get_W (Placer), Height => Get_H (Placer));
      Bmp.Draw.Delete (B, Constants.White);
      Bmp.Set_checking (B, false);
      
      if Topo_maps.Is_empty (Topodata) then
         return B;
      end if;

      Preprocess (Placer, Topodata);

      declare
         Roots : Root_array := Get_roots (Topodata);
      begin
         for N in Roots'range loop
            -- The hub with most connections goes first:
            Roots (N).Root := Get_best_connected (Topodata, Roots (N).Root);
         end loop;
         for N in Roots'range loop
            declare
               P : Placer_type'Class := Place (Placer, N, Roots'Length);
            begin
               Place_hub  (Roots (N).Root, P);
               Expand_hub (Roots (N).Root, P);
            end;
         end loop;
      end;

      Postprocess (Placer, Topodata);

      Fit_to_size (Topodata, Get_W (Placer), Get_H (Placer));
      Report_hubs;
      Draw_links;
      Draw_cores;
      Free_topodata;


      return B;
   end Build_topogram;

   ------------------------------------------------------------------------
   -- Fit_to_size                                                        --
   ------------------------------------------------------------------------
   procedure Fit_to_size (
      Topodata : in Topo_maps.Container_type; W, H : in Positive)
   is
      use Topo_maps;

      Max_X, Max_Y : Integer := Integer'First;
      Min_X, Min_Y : Integer := Integer'Last;

      -- Get_max_min
      procedure Get_max_min is
         I : Topo_maps.Iterator_type := First (Topodata);
      begin
         while I /= Back (Topodata) loop
            if Element (I).Placed then
               Max_X := Integer'Max (Max_X, Element (I).X);
               Min_X := Integer'Min (Min_X, Element (I).X);
               Max_Y := Integer'Max (Max_Y, Element (I).Y);
               Min_Y := Integer'Min (Min_Y, Element (I).Y);
            end if;
            I := Succ (I);
         end loop;
      end Get_max_min;

      -- Apply_scaling
      procedure Apply_scaling is
         NW, NH : Positive;
         I : Topo_maps.Iterator_type := First (Topodata);
         T : Topodata_access;
      begin
         if Max_X < Min_X or else Max_Y < Min_Y then 
            return;
         end if;
         NW := Max_X - Min_X;
         NH := Max_Y - Min_Y;
         while I /= Back (Topodata) loop
            T := Element (I);
            if T.Placed then
               T.X := (W - Margin * 2) * (T.X - Min_X) / NW + Margin;
               T.Y := (H - Margin * 2) * (T.Y - Min_Y) / NH + Margin;
            end if;
            I := Succ (I);
         end loop;
      end Apply_scaling;
   begin
      Get_max_min;
      Apply_scaling;
   end Fit_to_size;

   -------------
   -- Unplace --
   -------------
   procedure Unplace (This : in Topo_maps.Iterator_type) is
      use Topo_maps;
   begin
      Element (This).Placed := false;
      Element (This).X      := 0;
      Element (This).Y      := 0;
   end Unplace;

   ------------------------------------------------------------------------
   -- Placers                                                            --
   ------------------------------------------------------------------------
   -- Get coordinates
   function Get_X (This : in Placer_type) return Integer is
   begin
      return Integer (This.X);
   end Get_X;
   function Get_Y (This : in Placer_type) return Integer is
   begin
      return Integer (This.Y);
   end Get_Y;
   function Get_W (This : in Placer_type) return Integer is
   begin
      return Integer (This.W);
   end Get_W;
   function Get_H (This : in Placer_type) return Integer is
   begin
      return Integer (This.H);
   end Get_H;
   -- Preprocess/postprocess data globally:
   procedure Preprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type)
   is
      pragma Unreferenced (This, Data);
   begin
      null;
   end Preprocess;
   procedure Postprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type) 
   is
      pragma Unreferenced (This, Data);
   begin
      null;
   end Postprocess;

end Aenea.Hub.Topo;
