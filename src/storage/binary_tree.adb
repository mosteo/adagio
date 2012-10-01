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
--  $Id: binary_tree.adb,v 1.3 2004/01/21 21:05:43 Jano Exp $

with Ada.Unchecked_deallocation;
use  Ada;

package body Binary_tree is

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new Unchecked_deallocation (Node_type, Node_access);

   ------------------------------------------------------------------------
   -- Add_child                                                          --
   ------------------------------------------------------------------------
   -- Returns the inserted child.
   -- Node can be null to create a non-linked node (without parent).
   function Add_child (
      Node : in Node_access; 
      Side : in Sides;
      Data : in Node_data) return Node_access 
   is
      Child : Node_access := new Node_type;
   begin
      Child.Data := Data;
      if Node /= null then
         Node.Children (Side) := Child;
      end if;
      return Child;
   end Add_child;

   ------------------------------------------------------------------------
   -- Delete_child                                                       --
   ------------------------------------------------------------------------
   -- Frees a branch.
   -- Node is null after call.
   procedure Delete_child (Node : in out Node_access) is
   begin
      -- End of recursion:
      if Node = null then
         return;
      end if;
      -- Delete children
      Delete_child (Node.Children (Left));
      Delete_child (Node.Children (Right));
      -- Delete itself
      Free (Node);
   end Delete_child;
   
   ------------------------------------------------------------------------
   -- Get_child                                                          --
   ------------------------------------------------------------------------
   -- Can return null if no child in that side.
   function Get_child (Node : access Node_type; Side : in Sides) 
      return Node_access is
   begin
      return Node.Children (Side);
   end Get_child;

   ------------------------------------------------------------------------
   -- Get_data                                                           --
   ------------------------------------------------------------------------
   -- Returns the node associated data.
   function Get_data (Node : access Node_type) return Node_data is
   begin
      return Node.Data;
   end Get_data;

   ------------------------------------------------------------------------
   -- Set_data                                                           --
   ------------------------------------------------------------------------
   -- Set the node associated data.
   procedure Set_data (Node : access Node_type; Data : in Node_data) is
   begin
      Node.Data := Data;
   end Set_data;

end Binary_tree;
