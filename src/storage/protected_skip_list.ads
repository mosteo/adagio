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
--  $Id: protected_skip_list.ads,v 1.3 2004/01/21 21:05:47 Jano Exp $

with Pragmarc.Skip_list_unbounded;

with System;

generic
   type Element is private;
   with function "<" (Left : Element; Right : Element) return Boolean is <>;
   -- Orders Elements; usually will work on a part (the key) of an Element
   -- Elements will be ordered in ascending order according to "<"
   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Usually operates on part (the key) of an Element
package Protected_skip_list is

   pragma Preelaborate;

   package Implementation is new Pragmarc.Skip_list_unbounded (
      Element, "<", "=");

   protected type Skip_list (Priority : System.Priority := System.Priority'Last)
   is 
      pragma Priority (Priority);

      procedure Clear;

      -- No duplicates (replacement)
      procedure Insert (Item : in Element);

      procedure Delete (Item : in Element);

      -- Blocking if empty
      -- Doesn't remove it
      entry Get_first (Item : out Element);

      -- Get and remove an element if found, nothing else.
      procedure Get_remove (Item : in out Element; Found : out Boolean);

      -- Get first if exists and remove it
      procedure Get_first_remove (Item : out Element; Found : out Boolean);

      function Is_empty return Boolean;

      function Length return Natural;

   private

      List : Implementation.Skip_list;

   end Skip_list;

end Protected_skip_list;
