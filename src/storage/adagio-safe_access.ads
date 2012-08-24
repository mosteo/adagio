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
--  $Id: adagio-safe_access.ads,v 1.3 2004/01/21 21:05:41 Jano Exp $

-- This package provides facilities to have safe access pointers.
-- It offers two flavors: a unconstrained type which can be ini-
-- tialized with an access as constraint, or a regular type.
-- Thread *safe*.

with Ada.Finalization;  use Ada;

generic
   type Item (<>)   is limited private;
   type Item_access is access all Item;  -- This is the access we want safe
   Item_id : String := "Anonymous";
package Adagio.Safe_access is

   pragma Elaborate_body;

   -- Unconstrained version
   type Unconstrained_object (Data : Item_access) is private;

   -- Is null?
   function Is_null (this : in Unconstrained_object) return Boolean;
   pragma Inline (Is_null);

   -- Constrained version:
   type Object is tagged private;

   -- Is null?
   -- True if Object is not binded OR if is binded to a null access.
   function Is_null (this : in Object) return Boolean;
   pragma Inline (Is_null);

   -- Null initial:
   Null_access : constant Object;

   -- Initialization:
   -- Data should be allocated with new. 
   -- If it comes from an stack object, error will happen when freeing it.
   procedure Bind (this : in out Object; Data : in Item_access);

   -- Get value
   -- Of course, if the access value is copied outside, error can occur.
   function Value (this : in Object) return Item;
   pragma Inline (Value);
   function Value (this : in Object) return Item_access;
   pragma Inline (Value);

   -- Unbinding:
   -- The value is no longer controlled
   -- Only valid if one reference (last)
   -- In other case, constraint error will be raised.
   procedure Unbind (this : in out Object);

private

   -- Auxiliary type to do the counting.
   type Tracker_type is record
      Data  : Item_access := null;
      Count : Natural := 0;
   end record;
   type Tracker_access is access all Tracker_type;
-- for Tracker_access'Storage_pool use Debug_pool;

   procedure Discount (this : in out Tracker_access);
   procedure Addcount (this : in out Tracker_access);
   pragma Inline (Discount, Addcount);

   type Unconstrained_object (Data : Item_access) is new
      Finalization.Controlled with record
      Tracker : Tracker_access := null;
   end record;

   procedure Initialize (this : in out Unconstrained_object);
   procedure Adjust     (this : in out Unconstrained_object);
   procedure Finalize   (this : in out Unconstrained_object);
   pragma Inline (Initialize, Adjust, Finalize);

   type Object is new Finalization.Controlled with record
      Tracker : Tracker_access := null;
   end record;

   procedure Adjust   (this : in out Object);
   procedure Finalize (this : in out Object);
   pragma Inline (Adjust, Finalize);

   Null_access : constant Object := 
      (Finalization.Controlled with Tracker => null);

end Adagio.Safe_access;
