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

--  Sources may be of many kinds, provide implementations for each one.
--  One for each protocol, that's it.

with Ada.Containers;
with Ada.Unchecked_Deallocation;

package Adagio.Download.Source is

   type Object is abstract tagged limited private;
   type Object_Access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   -- Deallocation
   procedure Free (This : in out Object_Access);

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Unique id for the source.
   function Get_Id (This : access Object) return Source_Id is abstract;

   ------------------------------------------------------------------------
   -- Get_Slot_Id                                                        --
   ------------------------------------------------------------------------
   -- To track which slot is it in.
   function Get_Slot_Id (This : access Object) return Slot_Id;

   ------------------------------------------------------------------------
   -- Is_Active                                                          --
   ------------------------------------------------------------------------
   -- Say if the source is active (downloading, whatever)
   function Is_Active (This : access Object) return Boolean is abstract;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- All processsing must occur here.
   -- Once source is disposable, set Finished to true
   -- Set Again_In to the delay this source request (just orientative).
   procedure Process (
      This     : access Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is abstract;

   ------------------------------------------------------------------------
   -- Set_Paused                                                         --
   ------------------------------------------------------------------------
   procedure Set_Paused (This : access Object; Paused : in Boolean := true)
   is abstract;

   ------------------------------------------------------------------------
   -- Set_Slot_Id                                                        --
   ------------------------------------------------------------------------
   procedure Set_Slot_Id (This : access Object; Id : in Slot_Id);

   ------------------------------------------------------------------------
   -- Hash                                                               --
   ------------------------------------------------------------------------
   -- Internal use
   function Hash (Id : in Source_Id) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);

private 

   type Object is abstract tagged limited record
      Slot : Slot_Id;
   end record;

   procedure Free_Internal is new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   procedure Free (This : in out Object_Access) renames Free_Internal;

end Adagio.Download.Source;
