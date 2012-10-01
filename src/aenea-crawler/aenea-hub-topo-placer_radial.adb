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
--  $Id: aenea-hub-topo-placer_radial.adb,v 1.2 2004/03/10 23:49:58 Jano Exp $

with Aenea.Globals.Options;

with Ada.Numerics.Float_random;

package body Aenea.Hub.Topo.Placer_radial is

   Level_delta : Float renames Globals.Options.Topo_radial_LevelDelta;

   package Rand renames Ada.Numerics.Float_random;
   Seed : Rand.Generator;

   -- Places a hub according to the parent placer.
   function Place (This : in Placer_type; Pos, Num : in Positive) 
      return Placer_type
   is
      Result : Placer_type := This;
      Incr   : Float       := 2.0 * Pi / Float (Num);
      Base   : Float       := Incr / 2.0;
      Noise  : Float       := Rand.Random (Seed) * 0.1 - 0.05;
   begin

      if This.Is_root and then Num = 1 then
         Result.Is_root := false;
         return Result;
      end if;

      -- Regular placement:
      if Num = 1 then
         Base := 0.0;
      end if;

      Result.Angle := This.Angle + Base + Incr * (Float (Pos - 1)) + Noise;

      Result.X := 
         This.X + This.Length * Math.Sin (Result.Angle);
      Result.Y := 
         This.Y + This.Length * Math.Cos (Result.Angle);

      -- Special adjust for root positions
      if This.Is_root and then Num > 1 then
         Result.Angle := Result.Angle - Pi;
      end if;

      Result.Length := This.Length * Level_delta;

      Result.Is_root := false;
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
         Length  => 1000.0,
         Angle   => Pi);
   end Get_root;

begin
   Rand.Reset (Seed);
end Aenea.Hub.Topo.Placer_radial;
