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

with Adagio.Download.Manager;
with Adagio.Searches.Manager;
with Adagio.Hash_Dictionary_Utils;
with Adagio.Trace;

-- Does all necessary initializatio for a new download:
-- Creates the slot, adds known sources, necessary consumers, etc.

   procedure Adagio.Download.Add (
      Hash   : in Agpl.Magnet.Object;
      Secure : in Boolean)
   is
      Id : Slot_Id;
   begin
      -- Create the Slot (in paused status)
      Download.Manager.Create_Slot (Hash, Secure, Id);

      -- Add known sources
      Searches.Manager.Add_Sources_To_Download (
         Hash_Dictionary_Utils.From_Magnet (Hash), Id);

      -- Add sources from the magnet link
      pragma Unimplemented;

      -- Add necessary consumers
      pragma Unimplemented;
      
      -- Enable the slot now that everything is fine
      pragma Unimplemented;

      -- Save new status of downloads.
      Download.Manager.Save (Id);

   exception
      when E : others =>
         Trace.Log ("Download.Add: " & Trace.Report (E), Trace.Error);
   end Adagio.Download.Add;
