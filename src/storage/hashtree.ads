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
--  $Id: hashtree.ads,v 1.5 2004/01/21 21:05:44 Jano Exp $

-- Generic computation of trees of hashes.
-- Can be used to implement for example Tiger trees.

with Acf.Types;
use  Acf.Types;

with Ada.Finalization;
with Ada.Streams;
use  Ada.Streams;
use  Ada;

generic
   type Hash_type is private;               -- A final hash.
   type Hash_context is limited private;    -- A hash state holding type.
   -- Initialize a hashing operation:
   with procedure Begin_inner_hash (Context : in out Hash_context);
   -- Finalize a hashing operation:
   with function  End_inner_hash (Context : in Hash_context) return Hash_type;
   -- Add data to hash:
   with procedure Update_inner_hash (
      Context : in out Hash_context;
      Bytes   : in     Byte_array);
   -- Prefix actions for a leaf hash:
   with procedure Start_leaf_hash (Context : in out Hash_context);
   -- Prefix actions for an intermediate hash:
   with procedure Start_intermediate_hash  (Context : in out Hash_context);
   -- Prefix actions for a root hash:
   with procedure Start_root_hash (Context : in out Hash_context);
   -- Converter for hashes:
   with function  To_byte_array (Hash : in Hash_type) return Byte_array;
package HashTree is 

   Default_leaf_size : constant := 1024;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- A full HashTree:
   type Object is limited private;

   ------------------------------------------------------------------------
   -- Hash_start                                                         --
   ------------------------------------------------------------------------
   -- Use these procedures to iteratively build a tree.
   -- An object can be reused.
   procedure Hash_start (
      This      : in out Object; 
      Size      : in     Natural;  -- Of the data to hash
      Leaf_size : in     Natural  := Default_leaf_size;
      Keep      : in     Positive := 10);

   ------------------------------------------------------------------------
   -- Hash_update                                                        --
   ------------------------------------------------------------------------
   -- Feed some bytes to the tree under construction.
   procedure Hash_update (
      This : in out Object; Bytes : in Acf.Types.Byte_array);

   ------------------------------------------------------------------------
   -- Hash_end                                                           --
   ------------------------------------------------------------------------
   -- Completes the building of the tree.
   procedure Hash_end (This : in out Object);

   ------------------------------------------------------------------------
   -- Root_hash                                                          --
   ------------------------------------------------------------------------
   -- Get the root hash:
   function Root_hash (This : in Object) return Hash_type;

   ------------------------------------------------------------------------
   -- Hash_as_base32                                                     --
   ------------------------------------------------------------------------
   -- Convert a hash to Base32:
   function Hash_as_base32 (Hash : in Hash_type) return String;

   ------------------------------------------------------------------------
   -- Get_bytes                                                          --
   ------------------------------------------------------------------------
   -- Get the first N levels of tree bytes in breadth first order:
   -- If it has less levels, returns entire tree.
   -- If less levels have been kept, raises Constraint_error.
   function Get_bytes (This : in Object; Levels : in Positive) 
      return Byte_array;

private

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------
   type Coords_type is record
      Row : Positive;
      Col : Positive;
   end record;

   type Node_type;
   type Node_access is access all Node_type;
   type Node_type is record
      Coords      : Coords_type; -- Location in the tree
      Hash        : Hash_type;   -- This node hash
   end record;
   pragma Pack (Node_type);

   type Node_matrix is array (Positive range <>) of Node_access;
   type Node_matrix_access is access all Node_matrix;

   type Object is new Finalization.Limited_controlled with record
      Leaf_size       : Natural := Default_leaf_size;
      Size            : Natural := 0;       -- Of the full data.
      Nodes           : Node_matrix_access; -- Nodes.
      Keep            : Positive;           -- Levels to keep.
      Levels          : Positive;           -- Levels it has.

      -- Temporal things
      Context         : Hash_context;
      Block_remaining : Natural := 0;
      Actual          : Node_access;
   end record;

   ---------------
   -- Mix_nodes --
   ---------------
   -- Computes the combination of two nodes.
   procedure Mix_nodes (This : in out Object; Left, Right : in Coords_type);

   ------------------
   -- Promote_node --
   ------------------
   -- Promotes a node which has no sibling to combine with.
   procedure Promote_node (This : in out Object; Left : access Node_type);

   ------------------------------------------------------------------------
   -- Destroy                                                            --
   ------------------------------------------------------------------------
   -- Disposes all memory used by a tree.
   procedure Destroy (This : in out Object);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object);

   ------------------------------------------------------------------------
   -- Pow2                                                               --
   ------------------------------------------------------------------------
   -- Computes quickly a power of 2
   function Pow2 (N : in Natural) return Positive;
   pragma Inline (Pow2);

   ------------------------------------------------------------------------
   -- Is_even                                                            --
   ------------------------------------------------------------------------
   -- Says if a number is even:
   function Is_even (N : in Natural) return Boolean;

   ------------------------------------------------------------------------
   -- Navigation functions                                               --
   ------------------------------------------------------------------------
   -- Get the index to a node:
   function Node_at (Coords : in Coords_type)
      return Positive;
   pragma Inline (Node_at);
   -- Get the node:
   function Node_at (This : in Object; Coords : in Coords_type)
      return Node_access;
   pragma Inline (Node_at);
   -- Get parent coordinates:
   function Parent_of (Coords : in Coords_type) return Coords_type;
   pragma Inline (Parent_of);
   -- Get parent node:
   function Parent_of (This : in Object; Node : access Node_type) 
      return Node_access;
   pragma Inline (Parent_of);
   -- Get left child coordinates:
   function Left_child (Coords : in Coords_type) return Coords_type;
   pragma Inline (Left_child);
   -- Get left child node:
   function Left_child (This : in Object; Node : access Node_type)
      return Node_access;
   pragma Inline (Left_child);
   -- Right counterparts
   function Right_child (Coords : in Coords_type) return Coords_type;
   pragma Inline (Right_child);
   function Right_child (This : in Object; Node : access Node_type)
      return Node_access;
   pragma Inline (Right_child);

end HashTree;
