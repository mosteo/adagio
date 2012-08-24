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
--  $Id: adagio-bandwidth_manager.ads,v 1.7 2004/03/22 07:14:57 Jano Exp $

--  Task for bandwidth management

with Adagio.Types; use Adagio.Types;

with Ada.Calendar;
use  Ada;

package Adagio.Bandwidth_manager is

   pragma Elaborate_Body;

   -- Bandwidth is the bytes/second to manage
   -- Period is the period for feedback. Longer ones allow the use
   --    of unused bandwidth for more time. (In milliseconds)
   protected type Object (Bandwidth : Speed; Period : Natural) is

      -- Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Natural);

      -- Issue a bandwidth petition. Awarded can be less that solicited.
      -- Awarded will never be more than Natural'Last / 2 so you can
      --    add Awarded + Awarded_Extra.
      -- Extra flag requests bandwidth from unused past cycles.
      procedure Commit (
         Desired : in  Natural; 
         Awarded : out Natural;
         Extra   : in  Boolean := false);

   private

      Gap      : Duration      := Duration (Period) / 1000.0;

      Remanent : Natural       := 0;
      Unused   : Natural       := 0;
      Last_req : Calendar.Time := Calendar.Clock;

   end Object;

   type Object_access is access all Object;

end Adagio.Bandwidth_manager;
