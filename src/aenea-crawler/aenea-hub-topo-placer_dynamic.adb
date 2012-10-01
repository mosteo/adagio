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
--  $Id: aenea-hub-topo-placer_dynamic.adb,v 1.2 2004/03/14 21:09:48 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Misc;
with Aenea.Trace;

package body Aenea.Hub.Topo.Placer_dynamic is

   Mass_factor : Float renames Globals.Options.Topo_dynamic_MassFactor;
   Link_force  : Float renames Globals.Options.Topo_dynamic_LinkForce;
   Hub_force   : Float renames Globals.Options.Topo_dynamic_HubForce;
   Core_force  : Float renames Globals.Options.Topo_dynamic_CoreForce;
   Link_pow    : Float renames Globals.Options.Topo_dynamic_LinkPow;
   Hub_pow     : Float renames Globals.Options.Topo_dynamic_HubPow;
   Core_pow    : Float renames Globals.Options.Topo_dynamic_CorePow;
   Max_force   : Float renames Globals.Options.Topo_dynamic_MaxForce;

   -- Places a hub according to the parent placer.
   function Place (This : in Placer_type; Pos, Num : in Positive) 
      return Placer_type
   is
   begin
      return (Placer_topdown.Placer_type'(
         Placer_topdown.Place (Placer_topdown.Placer_type (This), Pos, Num))
         with null record);
   end Place;

   -- Get an initial placer to be passed to Build_topogram
   function Get_root (Width, Height : in Positive)
      return Placer_type
   is
   begin
      return (Placer_topdown.Placer_type'(
         Placer_topdown.Get_root (Width, Height))
         with null record);
   end Get_root;

   procedure Postprocess (
      This : in Placer_type; Data : in Topo_maps.Container_type) 
   is
      pragma Unreferenced (This);
      I         : Topo_maps.Iterator_type;
      Max_drift : Float;
      use Topo_maps;

      function Distance2 (X1, Y1, X2, Y2 : in Float) return Float is
      begin
         return (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2);
      end Distance2;
      function Distance (X1, Y1, X2, Y2 : in Float) return Float is
      begin
         return Math.Sqrt (Distance2 (X1, Y1, X2, Y2));
      end Distance;
      function E (I : Topo_maps.Iterator_type) return Topodata_access
         renames Topo_maps.Element;

      procedure Compute_forces is
         use Math; -- For Float **
      begin
         I   := First (Data);
         while I /= Back (Data) loop
            if Element (I).Placed then
               declare
                  F  : Force_type renames E (I).F;
                  T  : Topodata_access renames Element (I);
                  D2 : Float := Distance2 (F.X, F.Y, 0.0, 0.0);
                  D  : Float := Math.Sqrt (D2);
                  Si : Float;
                  Co : Float;
                  J  : Topo_maps.Iterator_type;
                  NFX, 
                  NFY: Float;
               begin
                  -- CORE force
                  -- Considers all masses as unitary
                  -- Rises with the square of the distance:
                  if D /= 0.0 then
                     Si := -F.Y / D;
                     Co := -F.X / D;
                     F.DX := F.DX + 
                        D**Core_Pow * Core_force * Mass_factor * Co;
                     F.DY := F.DY + 
                        D**Core_pow * Core_force * Mass_factor * Si;
                     Trace.Log (Id (T.Hub.all) & " CoreF: " &
                        Misc.To_string (F.DX) & " " & Misc.To_string (F.DY),
                        Trace.Debug);
                  end if;

                  -- HUB forces
                  -- It's a repulsive force 
                  -- Shrinks with the square of the distance:
                  -- No neighborship necessary
                  J := First (Data);
                  while J /= Back (Data) loop
                     if J /= I and then E (J).Placed then
                        D2 := Distance2 (T.F.X, T.F.Y, E (J).F.X, E (J).F.Y);
                        if D2 = 0.0 then
                           D2 := 0.00001;
                        end if;
                        D    := Math.Sqrt (D2);
                        Si   := (T.F.Y - E (J).F.Y) / D;
                        Co   := (T.F.X - E (J).F.X) / D;
                        NFX  := 
                           Hub_force * Mass_factor * Co * 
                           Float (E (J).Leaves / 200 + 1) / (D**Hub_pow);
                        NFY  := 
                           Hub_force * Mass_factor * Si * 
                           Float (E (J).Leaves / 200 + 1) / (D**Hub_pow);
                        F.DX := F.DX + NFX;
                        F.DY := F.DY + NFY;
                        Trace.Log (Id (T.Hub.all) & " HubF: " &
                           Misc.To_string (NFX) & " " & Misc.To_string (NFY),
                           Trace.Debug);
                     end if;

                     J  := Succ (J);
                  end loop;

                  -- LINK forces
                  -- It's an attractive force 
                  -- Grows with the square of the distance:
                  J := First (T.Neighs);
                  while J /= Back (T.Neighs) loop
                     D2 := Distance2 (T.F.X, T.F.Y, E (J).F.X, E (J).F.Y);
                     if D2 = 0.0 then
                        D2 := 0.00001;
                     end if;
                     D    := Math.Sqrt (D2);
                     Si   := -(T.F.Y - E (J).F.Y) / D;
                     Co   := -(T.F.X - E (J).F.X) / D;
                     NFX  := 
                        Link_force * Mass_factor * Co * 
                        Float (E (J).Leaves / 200 + 1) * (D**Link_Pow);
                     NFY  := 
                        Link_force * Mass_factor * Si * 
                        Float (E (J).Leaves / 200 + 1) * (D**Link_Pow);
                     F.DX := F.DX + NFX;
                     F.DY := F.DY + NFY;
                     Trace.Log (Id (T.Hub.all) & " LinkF: " &
                        Misc.To_string (NFX) & " " & Misc.To_string (NFY),
                        Trace.Debug);

                     J  := Succ (J);
                  end loop;

               end;
            end if;
            I   := Succ (I);
         end loop;

         -- ACUMULATE current forces:
         I := First (Data);
         while I /= Back (Data) loop
            if E (I).Placed then
               declare
                  F : Force_type renames E (I).F;
                  T : Topodata_access renames E (I);
               begin
                  if F.DX > Max_force then
                     F.DX := Max_force;
                  elsif F.DX < -Max_force then
                     F.DX := -Max_force;
                  end if;
                  if F.DY > Max_force then
                     F.DY := Max_force;
                  elsif F.DY < -Max_force then
                     F.DY := -Max_force;
                  end if;
                  F.X := F.X + F.DX;
                  F.Y := F.Y + F.DY;

                  ---------------------------------------------------
                  Trace.Log (Id (T.Hub.all) & " Pos & F: " &
                     Misc.To_string (F.X) & " " & Misc.To_string (F.Y) & " " &
                     Misc.To_string (F.DX) & " " & Misc.To_string (F.DY),
                     Trace.Debug);
               end;
            end if;
            I := Succ (I);
         end loop;
      end Compute_forces;

   begin
      -- Normalize initial positions:
      Fit_to_size (Data, 1000, 1000);

      -- Assign initial forces to every hub and re-center things:
      I   := First (Data);
      while I /= Back (Data) loop
         if Element (I).Placed then
            E (I).F.X := Float (E (I).X - 500);
            E (I).F.Y := Float (E (I).Y - 500);
         end if;
         I   := Succ (I);
      end loop;

      for N in 1 .. Globals.Options.Topo_dynamic_MaxIterations loop
         Trace.Log ("********* ITERATION" & N'Img & " **************",
            Trace.Debug);
         -- Zero current forces
         I   := First (Data);
         while I /= Back (Data) loop
            if Element (I).Placed then
               E (I).F.DX := 0.0;
               E (I).F.DY := 0.0;
            end if;
            I   := Succ (I);
         end loop;

         -- Compute forces for each hub
         Compute_forces;

         -- Verify convergence
         I         := First (Data);
         Max_drift := 0.0;
         while I /= Back (Data) loop
            if Element (I).Placed then
               Max_drift := Float'Max (
                  Max_drift, Distance (E (I).F.DX, E (I).F.DY, 0.0, 0.0));
            end if;
            I   := Succ (I);
         end loop;

         if Max_drift <= Globals.Options.Topo_dynamic_convergence then
            Trace.Log ("Convergence reached in" & N'Img & " iterations with" &
               " drift=" & Misc.To_string (Max_Drift), 
               Trace.Informative);
            exit;
         else
            Trace.Log ("Max. Drift: " & Misc.To_string (Max_drift), 
               Trace.Debug);
         end if;

      end loop;
      Trace.Log ("Max. Drift: " & Misc.To_string (Max_drift), 
         Trace.Informative);

      -- Round movements
      I   := First (Data);
      while I /= Back (Data) loop
         if E (I).Placed then
            E (I).X := Integer (E (I).F.X);
            E (I).Y := Integer (E (I).F.Y);
         end if;
         I   := Succ (I);
      end loop;
   exception
      when E : others =>
         Trace.Log ("Placer_dynamic.Post: " & Trace.Report (E), Trace.Error);
   end Postprocess;

end Aenea.Hub.Topo.Placer_dynamic;
