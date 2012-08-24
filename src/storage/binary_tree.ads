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
--  $Id: binary_tree.ads,v 1.3 2004/01/21 21:05:43 Jano Exp $

generic
   type Node_data is private;
package Binary_tree is

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------
   type Node_type is private;
   type Node_access is access all Node_type;
   subtype Tree   is Node_access;
   subtype Object is Node_access;

   type Sides is (Left, Right);

   ------------------------------------------------------------------------
   -- Add_child                                                          --
   ------------------------------------------------------------------------
   -- Returns the inserted child.
   -- Node can be null to create a non-linked node (without parent).
   function Add_child (
      Node : in Node_access; 
      Side : in Sides;
      Data : in Node_data) return Node_access;

   ------------------------------------------------------------------------
   -- Delete_child                                                       --
   ------------------------------------------------------------------------
   -- Frees a branch.
   -- Node is null after call.
   procedure Delete_child (Node : in out Node_access);
   
   ------------------------------------------------------------------------
   -- Get_child                                                          --
   ------------------------------------------------------------------------
   -- Can return null if no child in that side.
   function Get_child (Node : access Node_type; Side : in Sides) 
      return Node_access;

   ------------------------------------------------------------------------
   -- Get_data                                                           --
   ------------------------------------------------------------------------
   -- Returns the node associated data.
   function Get_data (Node : access Node_type) return Node_data;

   ------------------------------------------------------------------------
   -- Set_data                                                           --
   ------------------------------------------------------------------------
   -- Set the node associated data.
   procedure Set_data (Node : access Node_type; Data : in Node_data);

private

   type Side_array is array (Sides) of Node_access;

   type Node_type is record
      Data     : Node_data;
      Children : Side_array;
   end record;

end Binary_tree;
