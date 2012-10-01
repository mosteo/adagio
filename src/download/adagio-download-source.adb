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


with Ada.Strings.Hash;

package body Adagio.Download.Source is

   ------------------------------------------------------------------------
   -- Get_Slot_Id                                                        --
   ------------------------------------------------------------------------
   -- To track which slot is it in.
   function Get_Slot_Id (This : access Object) return Slot_Id is
   begin
      return This.Slot;
   end Get_Slot_Id;

   ------------------------------------------------------------------------
   -- Hash                                                               --
   ------------------------------------------------------------------------
   function Hash (Id : in Source_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (S (Ustring (Id)));
   end Hash;

   ------------------------------------------------------------------------
   -- Set_Slot_Id                                                        --
   ------------------------------------------------------------------------
   procedure Set_Slot_Id (This : access Object; Id : in Slot_Id) is
   begin
      This.Slot := Id;
   end Set_Slot_Id;

end Adagio.Download.Source;
