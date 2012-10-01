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
--  $Id: aenea-hub-topo-placer_topdown.adb,v 1.3 2004/03/14 21:09:49 Jano Exp $

with Aenea.Globals.Options;

package body Aenea.Hub.Topo.Placer_topdown is

   Branch_length : Float renames Globals.Options.Topo_topdown_BranchLength;
   Wave          : Float renames Globals.Options.Topo_topdown_Increment;
   Level_delta   : Float renames Globals.Options.Topo_topdown_LevelDelta;

   -- Places a hub according to the parent placer.
   function Place (This : in Placer_type; Pos, Num : in Positive) 
      return Placer_type
   is
      Result : Placer_type := This;
      Incr   : Float       := Wave;
      Base   : Float       := -Incr * Float (Num - 1) / 2.0;
   begin

      if This.Is_root then
         Result.X := 
            Result.X + Float (This.W - 20.0) / Float (Num) * Float (Pos - 1);
         Result.Is_root := false;
         return Result;
      end if;

      -- Regular placement:
      Result.Angle := This.Angle + Base + Incr * (Float (Pos - 1));

      Result.X := This.X + Branch_length * This.Dilta * Math.Sin (Result.Angle);
      Result.Y := This.Y + Branch_length * This.Dilta * Math.Cos (Result.Angle);

      Result.Dilta := This.Dilta * Level_delta;

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
         Angle   => Pi * 2.0 / 3.0,
         Dilta   => 1.0);
   end Get_root;

end Aenea.Hub.Topo.Placer_topdown;
