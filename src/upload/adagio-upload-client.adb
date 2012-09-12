
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
--  $Id: adagio-upload-client.adb,v 1.5 2004/01/21 21:05:49 Jano Exp $


package body Adagio.Upload.Client is

    Stat_alive_uploads : constant String := "Network - Allocated uploads";
	Pragma Unreferenced(Stat_alive_uploads);
   ------------------------------------------------------------------------
   -- Queue_id                                                           --
   ------------------------------------------------------------------------
   function Queue_id (This : in Object'Class) return String is
   begin
      return Id (This) & ":" &
         Upload.Resource.Id (
            Upload.Resource.V (Requested_resource (This)).all);
   end Queue_id;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Object) is
   begin
      null;
--      Statistics.Object.Update (
--         Stat_alive_uploads,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (1));
   end Initialize;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize   (This : in out Object) is
   begin
--      Statistics.Object.Update (
--         Stat_alive_uploads,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (-1));
      -- Just in case.
      Cancel (Object'Class (This));
   end Finalize;

begin

   null;

--   Statistics.Object.Set (
--      Stat_alive_uploads,
--      Statistics.Integers.Create (0));

end Adagio.Upload.Client;
