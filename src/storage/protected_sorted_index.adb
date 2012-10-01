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
--  $Id: protected_sorted_index.adb,v 1.5 2004/02/09 18:21:09 Jano Exp $

--  Implements a double indexed container, with sorted semantics and
--  search by id.
--  However, currently the search isn't efficient (takes O (n)).
--  It's ok for use with not huge lists.

package body Protected_sorted_index is

   use Implementation;

   protected body Sorted_index is 

      procedure Clear is
      begin
         Clear (List);
      end Clear;

      -- No duplicates (replacement)
      procedure Insert (Item : in Element_type) is
         Success : Boolean;
      begin
         Delete (Item, Success);       -- Local
         Insert (List, Item); -- Implementation
      end Insert;

      function Find (Item : in Element_type) return Iterator_type is
         I : Iterator_type := First (List);
      begin
         while I /= Back (List) loop
            if Item = Element (I) then
               return I;
            else
               I := Succ (I);
            end if;
         end loop;

         return Back (List);
      end Find;

      function Find (Item : in Element_type) return Boolean is
      begin
         return Find (Item) /= Back (List);
      end Find;

      procedure Delete (Item : in Element_type; Success : out Boolean) is
         I : Iterator_type := Find (Item); -- Local!
      begin
         Success := I /= Back (List);
         if Success then
            Delete (List, I);
         end if;
      end Delete;

      -- Blocking if empty
      -- Doesn't remove it
      entry Get_first (Item : out Element_type) when not Is_empty is
      begin
         Item := Element (First (List));
      end Get_first;

      -- Get first if possible:
      procedure Get_first (Item : out Element_type; Success : out Boolean) is
      begin
         Success := not Is_empty;
         if Success then 
            Item := Element (First (List));
         end if;
      end Get_first;

      -- Get and remove an element if found, nothing else.
      procedure Get_remove (
         Item    : in out Element_type; Found : out Boolean) is
         I       : Iterator_type;
         Success : Boolean;
      begin
         I := Find (Item); -- Local
         Found := I /= Back (List);
         if Found then
            Item := Element (I);
            Delete (Item, Success); -- Local
            if not Success then
               raise Constraint_error;
            end if;
         end if;
      end Get_remove;

      -- Get first if exists and remove it
      procedure Get_first_remove (
         Item : out Element_type; Found : out Boolean) 
      is
         Success : Boolean;
      begin
         Get_first (Item, Found);
         if Found then
            Delete (Item, Success); -- Local
            if not Success then
               raise Constraint_error;
            end if;
         end if;
      end Get_first_remove;

      function Is_empty return Boolean is
      begin
         return Is_empty (List);
      end Is_empty;

      function Length return Natural is
      begin
         return Length (List);
      end Length;

   end Sorted_index;

end Protected_sorted_index;
