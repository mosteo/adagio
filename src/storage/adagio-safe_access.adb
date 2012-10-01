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
--  $Id: adagio-safe_access.adb,v 1.3 2004/01/21 21:05:41 Jano Exp $

-- This package provides facilities to have safe access pointers.
-- It offers two flavors: a unconstrained type which can be ini-
-- tialized with an access as constraint, or a regular type.

with Adagio.Monitor;
with Adagio.Trace;

with Text_io; use text_io;

with Ada.Unchecked_deallocation;

package body Adagio.Safe_access is

   Debug : Boolean := Item_id /= "Anonymous";

   -- Global monitor for each operation.
   -- This means that there is only *one* mutex of every instantiation.
   Mutex : aliased Adagio.Monitor.Semaphore;

   -- Delete item:
   procedure Free is new
      Unchecked_deallocation (Item, Item_access);

   procedure Free is new
      Unchecked_deallocation (Tracker_type, Tracker_access);

   -- Helper decrementing function:
   procedure Discount (this : in out Tracker_access) is
   begin
      if this /= null then
         this.Count := this.Count - 1;
         if this.Count = 0 then
            Free (this.Data);
            Free (this);
         end if;
      end if;
   end Discount;

   -- Helper decrement function:
   procedure Addcount (this : in out Tracker_access) is
   begin
      if this /= null then
         this.Count := this.Count + 1;
      end if;
   end Addcount;

   -- Is null?
   function Is_null (this : in Unconstrained_object) return Boolean is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      return this.Tracker = null or else this.Tracker.Data = null;
   end Is_null;

   -- Is null?
   function Is_null (this : in Object) return Boolean is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      return this.Tracker = null or else this.Tracker.Data = null;
   end Is_null;

   -- Association:
   procedure Bind (this : in out Object; Data : in Item_access) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      Discount (this.Tracker);
      this.Tracker := new Tracker_type'(Data, 1);
   end Bind;

   -- Unbinding:
   -- The value is no longer controlled
   procedure Unbind (this : in out Object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      if this.Tracker /= null then
         if this.Tracker.Count = 1 then
            Free (this.Tracker);
         else
            raise Constraint_error;
         end if;
      end if;
   end;

   -- Get value
   -- Of course, if the access value is copied outside, error can occur.
--     function Value (this : in Object) return Item is
--        M : Adagio.Monitor.Object (Mutex'Access);
--     begin
--        return this.Tracker.Data.all;
--     end;

   function Value (this : in Object) return Item_access is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      return this.Tracker.Data;
   end;

   procedure Initialize (this : in out Unconstrained_object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      this.Tracker := new Tracker_type'(this.Data, 1);
   end Initialize;

   procedure Adjust   (this : in out Unconstrained_object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      Addcount (this.Tracker);
   end Adjust;

   procedure Finalize (this : in out Unconstrained_object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      Discount (this.Tracker);
   end Finalize;

   procedure Adjust   (this : in out Object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      Addcount (this.Tracker);
   end Adjust;

   procedure Finalize (this : in out Object) is
      M : Adagio.Monitor.Object (Mutex'Access);
   begin
      Discount (this.Tracker);
   end Finalize;

end Adagio.Safe_access;
