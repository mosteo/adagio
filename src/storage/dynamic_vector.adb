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
--  $Id: dynamic_vector.adb,v 1.2 2004/01/21 14:48:47 Jano Exp $

-- Package for unbounded vectors, integer-indexed

with Ada.Unchecked_deallocation;

package body Dynamic_vector is

   ---------------
   -- Utilities --
   ---------------

   procedure Free is new Unchecked_deallocation (
      Item_array, Item_array_access);

   ------------
   -- Object --
   ------------

   -- First "attribute"
   -- O (1)
   function First (this : in Object) return integer is
   begin
      return this.First;
   end First;

   -- Last "attribute"
   -- O (1)
   function Last (this : in Object) return integer is
   begin
      return this.Last;
   end Last;

   -- Length "attribute"
   -- O (1)
   function Length (this : in Object) return integer is
   begin
      return this.Last - this.Vector.all'First + 1;
   end Length;

   -- Grows the vector according to the Grow_factor. Should not be necessary
   --    to be used. It's used internally.
   -- O (n)
   procedure Grow (this : in out Object; Empty_side : Sides := Ending) is
   begin
      declare
         Increment  : Natural := Natural'Max (1, 
            Natural (Float (this.Vector.all'Length) * Grow_factor));
         New_vector : Item_array_access := 
            new Item_array (this.Vector.all'First ..
               this.Vector.all'Last + Increment);
      begin
         -- Assign values:
         if Empty_side = Ending then
            New_vector (New_vector'First .. this.Last) :=
               this.Vector (this.Vector.all'First .. this.Last);
         else
            New_vector (New_vector'First + 1 .. this.Last + 1) :=
               this.Vector (this.Vector.all'First .. this.Last);
         end if;
         -- Replace:
         Free (this.Vector);
         this.Vector := New_vector;
      end;
   end Grow;

   -- Adds an item to the end. Will grow the vector if necessary.
   -- O (1) or O (n) if growing occurs
   procedure Append (this : in out Object; Item : in Item_type) is
   begin
      if this.Last = this.Vector.all'Last then
         -- grow it!
         Grow (this);
         -- Assign value
         this.Last := this.Last + 1;
         this.Vector (this.Last) := Item;
      else
         this.Last := this.Last + 1;
         this.Vector (this.Last) := Item;
      end if;
   end Append;

   -- Adds an item before a certain position (that could not exist if we
   --    want insertion at Last + 1, i.e., the end. Will grow the vector
   --    if necessary.
   -- O (n)
   procedure Insert (
      this : in out Object; Item : in Item_type; Pos : in Integer) is
   begin
      if this.Last = this.Vector.all'Last then
         -- grow it!
         Grow (this);
      end if;
      -- Slide the values:
      if Pos <= this.Last then
         this.Vector (Pos + 1 .. this.Last + 1) :=
            this.Vector (Pos .. this.Last);
      end if;
      -- Assign value
      this.Last := this.Last + 1;
      this.Vector (Pos) := Item;
   end Insert;

   -- Deletes an item at given pos
   -- O (n)
   procedure Delete (this : in out Object; Pos : in Integer) is
   begin
      if Pos > this.Last or else Pos < this.First then
         raise Constraint_error;
      end if;
      -- Reassign:
      if Pos /= this.Last then
         this.Vector (Pos ..this.Last - 1) := 
            this.Vector (Pos + 1 .. this.Last);
      end if;
      -- Shrink:
      this.Last := this.Last - 1;
   end Delete;

   -- Delete all ocurrences of an item
   -- O (n^2)
   procedure Delete_item (this : in out Object; Item : in Item_type) is
   begin
      for N in reverse this.First .. this.Last loop
         if this.Vector (N) = Item then
            Delete (this, N);
         end if;
      end loop;
   end Delete_item;

   -- Clean the vector, starts afresh
   -- O (1)
   procedure Reset (this : in out Object) is
   begin
      Finalize   (Proto_object (this));
      Initialize (Proto_object (this));
      this.Last := this.First - 1;
   end Reset;

   -- Optimize memory usage, vector of only valid positions
   -- Right after optimize, 'Last is valid.
   -- O (n)
   procedure Optimize (this : in out Object) is
      New_vector : Item_array_access := 
         new Item_array (this.First .. this.Last);
   begin
      -- Copy:
      New_vector (this.First .. this.Last) := 
         this.Vector (this.First .. this.Last);
      -- Free old:
      Free (this.Vector);
      -- Replace:
      this.Vector := New_vector;
   end Optimize;

   -- Member functions, not very useful if you access the vector directly:
   function Value (this : in Object) return Item_array is
   begin
      return this.Vector.all;
   end Value;

   function Value (this : in Object; Pos : in Integer) return Item_type is
   begin
      return this.Vector (Pos);
   end Value;

   -- Basic searching:
   -- Raise Item_not_found
   -- O (n)
   function Pos (
      this : in Object; 
      Item : in Item_type;
      Pos  : in Integer := Integer'First) return Integer is
   begin
      for N in Integer'Max(Pos, this.First - 1) + 1 .. this.Last loop
         if this.Vector (N) = Item then
            return N;
         end if;
      end loop;
      raise Item_not_found;
      return 0;
   end Pos;

   procedure Initialize (this : in out Proto_object) is
   begin
      if this.Vector /= null then
         Free (this.Vector);
      end if;

      this.Vector := 
         new Item_array (this.First .. this.First + Initial_size - 1);
   end Initialize;

   procedure Adjust     (this : in out Proto_object) is
      New_vector : Item_array_access := new Item_array (this.Vector'Range);
   begin
      New_vector.all := this.Vector.all;
      this.Vector := New_vector;
   end Adjust;

   procedure Finalize   (this : in out Proto_object) is
   begin
      Free (this.Vector);
   end Finalize;

end Dynamic_vector;
