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
--  $Id: adagio-bandwidth_manager.adb,v 1.8 2004/01/21 21:05:48 Jano Exp $

--  Task for bandwidth management

with Adagio.Trace;

package body Adagio.Bandwidth_manager is

   Safe_Maximum : constant := (Natural'Last - 1) / 2;

   -- Bandwidth is the bytes/second to manage
   -- Period is the period for feedback. Longer ones allow the use
   -- of unused bandwidth for more time.
   protected body Object is

      procedure Refresh;
      pragma Inline (Refresh);

      procedure Report;
      pragma Inline (Report);

      ---------------
      -- Available --
      ---------------
      -- Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Natural) is
      begin
         Refresh;
         begin
            Bandwidth := Remanent + Unused;
         exception
            when Constraint_error =>
               Bandwidth := Natural'Last;
         end;
      end Available;

      ------------
      -- Commit --
      ------------
      -- Issue a bandwidth petition. Awarded can be less that solicited.
      -- Extra flag requests bandwidth from unused past cycles.
      procedure Commit (
         Desired : in  Natural; 
         Awarded : out Natural;
         Extra   : in  Boolean := false) is
      begin
         Refresh;
         if Extra then 
            Awarded  := Natural'Min (Desired, Natural'Min (Unused, Safe_Maximum));
            Unused   := Unused - Awarded;
         else
            Awarded  := Natural'Min (Desired, Natural'Min (Remanent, Safe_Maximum));
            Remanent := Remanent - Awarded;
         end if;
      end Commit;
   
      -------------
      -- Refresh --
      -------------
      -- Recomputes if necessary the BW available
      procedure Refresh is
         use Ada.Calendar;
         Elapsed : Duration := Clock - Last_req;
      begin
         -- Check for too many time elapsed:
         if Elapsed >= Gap then
            -- Keep unused from past cycles:
            Unused := Remanent;
            -- Update remanent:
            begin
               Remanent := Natural (Float (Bandwidth) * Float (Gap));
            exception
               when Constraint_error =>
                  Remanent := Natural'Last;
            end;
            -- Update clock:
            declare
               Gaps : Natural := 
                  Natural (Float'Floor (Float (Elapsed) / Float (Gap)));
            begin
               Last_req := Last_req + Gap * Duration (Gaps);
            end;
         end if;
      end Refresh;

      ------------
      -- Report --
      ------------
      -- Debug reports
      procedure Report is
      begin
         Trace.Log ("Rem:" & Remanent'Img & "; Unu:" & Unused'Img, 
            Trace.Always);
      end Report;

   end Object;

end Adagio.Bandwidth_manager;
