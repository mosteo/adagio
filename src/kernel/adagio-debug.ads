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
--  $Id: adagio-debug.ads,v 1.5 2004/02/24 15:26:12 Jano Exp $

with Agpl.Protected_Values.Integers;

with Ada.Finalization;
use  Ada;

with Gnat.Debug_pools;

package Adagio.Debug is
   
   ------------------------------------------------------------------------
   -- Debug_statistics_enabled                                           --
   ------------------------------------------------------------------------
   function Debug_statistics_enabled return Boolean;

   ------------------------------------------------------------------------
   -- Reset_alive_tasks                                                  --
   ------------------------------------------------------------------------
   -- Reset statistics for alive tasks:
   procedure Reset_alive_tasks;

   ----------------------
   -- Tracing finished --
   ----------------------
   Tracing_finished : Boolean := false;
   pragma Atomic (Tracing_finished);

   ------------------------------------------------------------------------
   -- Tracker object                                                     --
   ------------------------------------------------------------------------
   type Tracker_type (Id : access String) is limited private;

   ------------------------------------------------------------------------
   -- Debug_pool                                                         --
   ------------------------------------------------------------------------
   Debug_pool : Gnat.Debug_pools.Debug_pool;

   ------------------------------------------------------------------------
   -- Functors for Agpl.Protected_Values.Integers                        --
   ------------------------------------------------------------------------
   Sum_1  : Agpl.Protected_Values.Integers.Adder ( 1);
   Sum_m1 : Agpl.Protected_Values.Integers.Adder (-1);

private

   type Tracker_type (Id : access String) is new
      Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out Tracker_type);
   procedure Finalize   (This : in out Tracker_type);
   
end Adagio.Debug;
