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
--  $Id: adagio-file-safe.adb,v 1.4 2004/01/29 21:47:08 Jano Exp $

with Adagio.Library;

package body Adagio.File.Safe is

   -- Protected object for all operations
   protected Monitor is
      procedure Add_upload (F : in File.Object; Num : Positive);
      procedure Add_hit    (F : in File.Object; Num : Positive);
   end Monitor;

   protected body Monitor is
      ----------------
      -- Add_upload --
      ----------------
      procedure Add_upload (F : in File.Object; Num : Positive) is
	    pragma Unreferenced (Num);
      begin
         if V (F).Uploads < Natural'Last then
            V (F).Uploads := V (F).Uploads + 1;
         end if;
         if V (F).Uploads_session < Natural'Last then
            V (F).Uploads_session := V (F).Uploads_session + 1;
         end if;
      end Add_upload;
      -------------
      -- Add_hit --
      -------------
      procedure Add_hit (F : in File.Object; Num : Positive) is
	    pragma Unreferenced (Num);
      begin
         if V (F).Hits_total < Natural'Last then
            V (F).Hits_total := V (F).Hits_total + 1;
         end if;
         if V (F).Hits_session < Natural'Last then
            V (F).Hits_session := V (F).Hits_session + 1;
         end if;
      end Add_hit;
   end Monitor;

   -- Mark a new upload for a file:
   procedure Add_upload (F : in Object; Num : Positive := 1) is
   begin
      Monitor.Add_upload (F, Num);
      Library.Object.Mark_dirty;
   end Add_upload;

   -- Mark a new hit for a file:
   procedure Add_hit    (F : in Object; Num : Positive := 1) is
   begin
      Monitor.Add_hit (F, Num);
      Library.Object.Mark_dirty;
   end Add_hit;

end Adagio.File.Safe;
