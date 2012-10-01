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
--  $Id: protected_skip_list.adb,v 1.3 2004/01/21 21:05:47 Jano Exp $

package body Protected_skip_list is

   protected body Skip_list is 

      procedure Clear is
      begin
         Implementation.Clear (List);
      end Clear;

      procedure Insert (Item : in Element) is
      begin
         Implementation.Insert (List, Item);
      end Insert;

      procedure Delete (Item : in Element) is
      begin
         Implementation.Delete (List, Item);
      end Delete;

      entry Get_first (Item : out Element) when not Is_empty is
      begin
         Item := Implementation.Get_first (List);
      end Get_first;

      procedure Get_remove (Item : in out Element; Found : out Boolean) is
         Result : Implementation.Result := 
            Implementation.Search (List, Item);
      begin
         Found := Result.Found;
         if Found then
            Item  := Result.Item;
            Delete (Item);
         end if;
      end Get_remove;

      -- Get first if exists and remove it
      procedure Get_first_remove (Item : out Element; Found : out Boolean) is
      begin
         Found := not Is_empty;
         if Found then
            Get_first (Item);
            Delete (Item);
         end if;
      end Get_first_remove;

      function Is_empty return Boolean is
      begin
         return Implementation.Is_empty (List);
      end Is_empty;

      function Length return Natural is
      begin
         return Implementation.Length (List);
      end Length;

   end Skip_list;

end Protected_skip_list;
