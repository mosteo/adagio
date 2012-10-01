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
--  $Id: hashtree.adb,v 1.5 2004/01/21 21:05:43 Jano Exp $

-- Generic computation of trees of hashes.
-- Can be used to implement for example Tiger trees.

with Adagio.Trace;
use  Adagio;

with Acf.Types.Base32;

with Interfaces;
with Ada.Numerics.Generic_elementary_functions;
with Ada.Unchecked_deallocation;

package body HashTree is 

   -- Set to false to avoid extra checks:
   Free_check : constant Boolean := false;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new Unchecked_deallocation (Node_type, Node_access);
   procedure Free is new Unchecked_deallocation (
      Node_matrix, Node_matrix_access);

   ------------------------------------------------------------------------
   -- Go_up                                                              --
   ------------------------------------------------------------------------
   -- Goes all the way possible up from a node computing hashes.
   -- Frees nodes not needed anymore.
   -- Stops on odd nodes or on root if data fininished.
   procedure Go_up (
      This   : in out Object; 
      Node   : access Node_type; 
      Finish : in     Boolean := false) 
   is
      N      : Node_access := Node_access (Node);
      Parent : Node_access;
   begin
      loop
         -- Stopping conditions?
         if N.Coords.Row = 1 then
            return;
         elsif (not Is_even (N.Coords.Col)) and not Finish then
            return;
         end if;

         -- Go ahead!
         if Is_even (N.Coords.Col) then
            -- Regular pair
            Mix_nodes (
               This, (N.Coords.Row, N.Coords.Col - 1), N.Coords);
         else -- Finishing, nodes without sibling
            Promote_node (This, N);
         end if;

         Parent := Parent_of (This, N);

         -- Free if not more needed:
         if N.Coords.Row > This.Keep then
            if Is_even (N.Coords.Col) then
               Free (This.Nodes (
                  Node_at ((N.Coords.Row, N.Coords.Col - 1))));
            end if;
            Free (This.Nodes (Node_at (N.Coords)));
         end if;

         -- Go up!
         N := Parent;
      end loop;
   end Go_up;

   ---------------
   -- Mix_nodes --
   ---------------
   -- Computes the combination of two nodes.
   procedure Mix_nodes (This : in out Object; Left, Right : in Coords_type) is
      Context : Hash_context;
      Up      : Node_access := new Node_type;
      L       : Node_access := Node_at (This, Left);
      R       : Node_access := Node_at (This, Right);
   begin
      Up.Coords   := Parent_of (Left);
      Begin_inner_hash (Context);
      -- Root or middle?
      if Up.Coords.Row = 1 then
         Start_root_hash (Context);
      else
         Start_intermediate_hash (Context);
      end if;
      Update_inner_hash (Context, To_byte_array (L.Hash));
      Update_inner_hash (Context, To_byte_array (R.Hash));
      Up.Hash := End_inner_hash (Context);

      This.Nodes (Node_at (Up.Coords)) := Up;
   end Mix_nodes;

   ------------------
   -- Promote_node --
   ------------------
   -- Promotes a node which has no sibling to combine with.
   procedure Promote_node (This : in out Object; Left : access Node_type) is
      Up : Node_access := new Node_type'(Parent_of (left.Coords), Left.Hash);
   begin
      This.Nodes (Node_at (Up.Coords)) := Up;
   end;

   ------------------------------------------------------------------------
   -- Hash_start                                                         --
   ------------------------------------------------------------------------
   -- Use these procedures to iteratively build a tree.
   procedure Hash_start (
      This      : in out Object; 
      Size      : in     Natural;  -- Of the data to hash
      Leaf_size : in     Natural  := Default_leaf_size;
      Keep      : in     Positive := 10) 
   is
      Blocks    : Float;
      package Funcs is new Ada.Numerics.Generic_elementary_functions (float);
   begin
      Destroy (This);
      This.Leaf_size       := Leaf_size;
      This.Size            := Size;
      This.Keep            := Keep;

      Blocks := Float'Ceiling (Float (Size) / Float (Leaf_size));
      Blocks := Float'Max (Blocks, 1.0); -- At least a block
      This.Levels := Positive (Float'Ceiling (Funcs.Log (Blocks, 2.0)) + 1.0);
      This.Nodes := new Node_matrix (1 .. Pow2 (This.Levels) - 1);

      This.Block_remaining := Leaf_size;
      This.Actual          := new Node_type;
      This.Actual.Coords   := (Row => This.Levels, Col => 1);
      This.Nodes (Node_at (This.Actual.Coords)) := This.Actual;
      Begin_inner_hash (This.Context);
      Start_leaf_hash  (This.Context);
   end Hash_start;

   ------------------------------------------------------------------------
   -- Hash_update                                                        --
   ------------------------------------------------------------------------
   -- Feed some bytes to the tree under construction.
   procedure Hash_update (
      This : in out Object; Bytes : in Acf.Types.Byte_array) 
   is
      Remaining : Natural     := This.Block_remaining;
      Current   : Coords_type;
   begin
      if Bytes'Length = 0 then
         return;
      end if;
      if Bytes'Length > This.Block_remaining then
         Hash_update(
            This, Bytes (Bytes'First .. Bytes'First + Remaining - 1));
         This.Actual.Hash := End_inner_hash (This.Context);
         Begin_inner_hash (This.Context);
         Start_leaf_hash (This.Context);
         This.Block_remaining      := This.Leaf_size;
         Current                   := This.Actual.Coords;
         -- Go up from the node leaved behind
         if Is_even (Current.Col) then
            Go_up (This, This.Actual);
            -- Actual may not exist after this point!
         end if;
         This.Actual               := new Node_type;
         This.Actual.Coords        := (Current.Row, Current.Col + 1);
         This.Nodes (Node_at (This.Actual.Coords)) := This.Actual;
         Hash_update (
            This, Bytes (Bytes'First + Remaining .. Bytes'Last));
      else
         This.Block_remaining := This.Block_remaining - Bytes'Length;
         Update_inner_hash (This.Context, Bytes);
      end if;
   end Hash_update;

   ------------------------------------------------------------------------
   -- Hash_end                                                           --
   ------------------------------------------------------------------------
   -- Completes the building of the tree.
   procedure Hash_end (This : in out Object) is
   begin
      -- Complete leaf hashing:
      This.Actual.Hash := End_inner_hash (This.Context);

      -- Build upper levels:
      Go_up (This, This.Actual, Finish => true);
   end Hash_end;

   ------------------------------------------------------------------------
   -- Root_hash                                                          --
   ------------------------------------------------------------------------
   -- Get the root hash:
   function Root_hash (This : in Object) return Hash_type is
   begin
      return This.Nodes (1).Hash;
   end Root_hash;

   ------------------------------------------------------------------------
   -- Hash_as_base32                                                     --
   ------------------------------------------------------------------------
   -- Convert a hash to Base32:
   function Hash_as_base32 (Hash : in Hash_type) return String is
   begin
      return Acf.Types.Base32.To_base32 (To_byte_array (Hash));
   end Hash_as_base32;

   ------------------------------------------------------------------------
   -- Get_bytes                                                          --
   ------------------------------------------------------------------------
   -- Get the first N levels of tree bytes in breadth first order:
   function Get_bytes (This : in Object; Levels : in Positive) 
      return Byte_array is
      Hash_len : Positive := To_byte_array (Root_hash (This))'Length;
      Result   : Byte_array (1 .. (2 ** Levels - 1) * Hash_len);
      Pos           : Natural := Result'First;
   begin
      if Levels > This.Keep then
         raise Constraint_error;
      end if;
      for N in 1 .. Natural'Min (Pow2 (Levels) - 1, This.Nodes'Last) loop
         -- May be null if the tree isn't fully balanced:
         if This.Nodes (N) /= null then
            Result (Pos .. Pos + Hash_len - 1) := 
               To_byte_array (This.Nodes (N).Hash);
            Pos := Pos + Hash_len;
         end if;
      end loop;
      return Result (1 .. Pos - 1);
   end Get_bytes;

   ------------------------------------------------------------------------
   -- Destroy                                                            --
   ------------------------------------------------------------------------
   -- Disposes all memory used by a tree.
   procedure Destroy (This : in out Object) is
   begin
      if This.Nodes = Null then 
         return; -- <-- EARLY EXIT, ALREADY DESTROYED
      end if;

      -- Free keeped levels
      for N in 1 .. Pow2 (Natural'Min (This.Keep, This.Levels)) - 1 loop
         Free (This.Nodes (N)); -- Someone may be already null, it's ok.
      end loop;
      
      -- Check already discarded nodes
      if Free_check then
         for N in This.Nodes'Range loop
            if This.Nodes (N) /= null then
               Trace.Log ("Unfreed node at" & N'Img, Trace.Error);
               raise Constraint_error;
            end if;
         end loop;
      end if;

      -- Free matrix
      Free (This.Nodes);
   end Destroy;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      Destroy (This);
   end Finalize;

   ------------------------------------------------------------------------
   -- Pow2                                                               --
   ------------------------------------------------------------------------
   -- Computes quickly a power of 2
   function Pow2 (N : in Natural) return Positive is
      use Interfaces;
   begin
      if N = 0 then
         return 1;
      else
         return Positive (Interfaces.Shift_left (Unsigned_32' (1), N));
      end if;
   end Pow2;

   ------------------------------------------------------------------------
   -- Is_even                                                            --
   ------------------------------------------------------------------------
   -- Says if a number is even:
   function Is_even (N : in Natural) return Boolean is
      use Interfaces;
   begin
      return (Unsigned_32 (N) and 1) = 0;
   end Is_even;

   ------------------------------------------------------------------------
   -- Navigation functions                                               --
   ------------------------------------------------------------------------
   -- Get the index to a node:
   function Node_at (Coords : in Coords_type)
      return Positive is
   begin
      return Pow2 (Coords.Row - 1) + Coords.Col - 1;
   end Node_at;
   -- Get the node:
   function Node_at (This : in Object; Coords : in Coords_type)
      return Node_access is
   begin
      return This.Nodes (Node_at (Coords));
   end Node_at;
   -- Get parent coordinates:
   function Parent_of (Coords : in Coords_type) return Coords_type is
   begin
      return (
         Row => Coords.Row - 1, 
         Col => (Coords.Col + 1) / 2);
   end Parent_of;
   -- Get parent node:
   function Parent_of (This : in Object; Node : access Node_type) 
      return Node_access is
   begin
      return This.Nodes (Node_at (Parent_of (Node.Coords)));
   end Parent_of;
   -- Get left child coordinates:
   function Left_child (Coords : in Coords_type) return Coords_type is
   begin
      return (
         Row => Coords.Row + 1,
         Col => Coords.Col * 2 - 1);
   end Left_child;
   -- Get left child node:
   function Left_child (This : in Object; Node : access Node_type)
      return Node_access is
   begin
      return This.Nodes (Node_at (Left_child (Node.Coords)));
   end Left_child;
   -- Right counterparts
   function Right_child (Coords : in Coords_type) return Coords_type is
   begin
      return (
         Row => Coords.Row + 1,
         Col => Coords.Col * 2);
   end Right_child;
   function Right_child (This : in Object; Node : access Node_type)
      return Node_access is
   begin
      return This.Nodes (Node_at (Right_child (Node.Coords)));
   end Right_child;

end HashTree;
