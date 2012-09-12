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
--  $Id: adagio-dictionary.adb,v 1.4 2004/01/21 21:05:27 Jano Exp $

-- A fast lookup dictionary data structure
-- Case insensitive
-- No duplicate items are inserted


with Ada.Unchecked_deallocation;
use  Ada;

package body Adagio.Dictionary is

   use Store;

   procedure Free is new Unchecked_deallocation (
      Item_array, Item_array_access);

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   -- Checks for duplicates
   procedure Add (this : in out Object; Word : in String; Item : in Items) is
      Already : Item_array_access := Find (this, word);
   begin
      for N in Already'Range loop
         if Already (N) = Item then
            Free (Already);
            return;
         end if;
      end loop;
      Free (Already);

      Insert (this.Data, Word, Item);
   end Add;

   ------------------------------------------------------------------------
   -- Find                                                               --
   ------------------------------------------------------------------------
   function Find (this : in Object; Word : in String)
      return Item_array_access
   is
      Pos, Back : Iterator_type;
      Result    : Item_array_access :=
         new Item_array (1 .. Count (this.Data, Word));
   begin
      Equal_range (this.Data, Word, Pos, Back);
      for N in Result'Range loop
         if Pos = Back then
            raise Constraint_error;
         end if;
         Result (N) := Element (Pos);
         Pos := Succ (Pos);
      end loop;

      return Result;
   end Find;

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Remove all occurrences of an item.
   procedure Remove (this : in out Object; Item : in Items) is
      Pos : Iterator_type := First (this.Data);
   begin
      while Pos /= Back (this.Data) loop
         if Element (Pos) = Item then
            Delete (this.Data, Pos);
         else
            Pos := succ (Pos);
         end if;
      end loop;
   end;

end Adagio.Dictionary;
