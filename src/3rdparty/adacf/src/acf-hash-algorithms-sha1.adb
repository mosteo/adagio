------------------------------------------------------------------------
--         (c) 2001, Antonio Duran. All rights reserved               --
--                       aduran@inicia.es                             --
------------------------------------------------------------------------
-- The Ada Cryptographic Framework (ACF) is free software; you can    --
-- redistribute it and/or modify it under terms of the GNU General    --
-- Public License as published by the Free Software Foundation;       --
-- either version 2, or (at your option) any later version.           --
--                                                                    --
-- The ACF is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of        --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   --
-- General Public License for  more details. You should have received --
-- a copy of the GNU General Public License distributed with the ACF; --
-- see file COPYING. If not, write to the Free Software Foundation,   --
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------
-- Identification
--    File name         : acf-hash-algorithms-sha1.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 26th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the SHA-1 (Secure Hash Algorithm) message digest
--    algorithm.
------------------------------------------------------------------------
-- Portability issues:
-- TBD.
------------------------------------------------------------------------
-- Performance issues:
-- TBD.
------------------------------------------------------------------------
-- Revision history:
--
-- Ver   Who   When     Why
-- 1.0   ADD   11262001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.SHA1 is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              SHA1_Context,
                              SHA1_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[SHA1_Block_Words]---------------------------------------------
   --|   Number of 4-byte words in a SHA-1 block.
   --+------------------------------------------------------------------

   SHA1_Block_Words        : constant Positive := SHA1_Block_Bytes / 4;

   --+---[Initial_State]------------------------------------------------
   --|   Initial values for state registers.
   --+------------------------------------------------------------------

   Initial_State           : constant State_Registers :=
      (
         16#6745_2301#,
         16#EFCD_AB89#,
         16#98BA_DCFE#,
         16#1032_5476#,
         16#C3D2_E1F0#
      );

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Packed_Block]-------------------------------------------------
   --|  Type for handling SHA-1 input blocks as an array of words.
   --+------------------------------------------------------------------

   subtype Packed_Block is Four_Bytes_Array(1 .. SHA1_Block_Words);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Transform]----------------------------------------------------
   --|   Purpose:
   --|   Transforms SHA-1 state based on input block.
   --|
   --|   Arguments:
   --|   Context              Access to SHA1_Context value that mantains
   --|                        the state to transform.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access SHA1_Context);

   --+---[Pack_Block]---------------------------------------------------
   --|   Purpose:
   --|   Packs an input block (Byte_Array) into a Four_Bytes_Array
   --|   suitable for transformation.
   --|
   --|   Arguments:
   --|   B              Block to pack.
   --|
   --|   Returned value:
   --|   Packed_Block corresponding to B.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Pack_Block(
                  B              : in     SHA1_Block)
      return   Packed_Block;
   pragma Inline(Pack_Block);

   --+---[Unpack_State]-------------------------------------------------
   --|   Purpose:
   --|   Unpacks the state registers rendering a byte array that is the
   --|   computed digest.
   --|
   --|   Arguments:
   --|   S              State registers.
   --|
   --|   Returned value:
   --|   Unpacked Byte_Array corresponding to S.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Unpack_State(
                  S              : in     State_Registers)
      return   Byte_Array;
   pragma Inline(Unpack_State);

   ---------------------------------------------------------------------
   -- Body subprogram bodies
   ---------------------------------------------------------------------

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access SHA1_Context)
   is
      T              : State_Registers := Context.all.State;
      X              : Packed_Block := Pack_Block(Context.all.Block);
      W              : Four_Bytes_Array(1 .. 80) := (others => 0);
      Y              : Four_Bytes;
      Z              : Four_Bytes;
   begin

      --|   Initialize local buffer W.

      W(1 .. X'Length) := X;

      for I in X'Length + 1 .. W'Last loop
         Z := (W(I - 3) xor W(I - 8) xor W(I - 14) xor W(I - 16));
         W(I) := Rotate_Left(Z, 1);
      end loop;

      --|   The four transformation subrounds.

      for I in 1 .. 20 loop
         Y := Rotate_Left(T(1), 5);
         Y := Y + ((T(2) and T(3)) or ((not T(2)) and T(4)));
         Y := Y + T(5) + W(I) + 16#5A82_7999#;
         T(5) := T(4);
         T(4) := T(3);
         T(3) := Rotate_Left(T(2), 30);
         T(2) := T(1);
         T(1) := Y;
      end loop;

      for I in 21 .. 40 loop
         Y := Rotate_Left(T(1), 5);
         Y := Y + (T(2) xor T(3) xor T(4));
         Y := Y + T(5) + W(I) + 16#6ED9_EBA1#;
         T(5) := T(4);
         T(4) := T(3);
         T(3) := Rotate_Left(T(2), 30);
         T(2) := T(1);
         T(1) := Y;
      end loop;

      for I in 41 .. 60 loop
         Y := Rotate_Left(T(1), 5);
         Y := Y + ((T(2) and T(3)) or
                   (T(2) and T(4)) or
                   (T(3) and T(4)));
         Y := Y + T(5) + W(I) + 16#8F1B_BCDC#;
         T(5) := T(4);
         T(4) := T(3);
         T(3) := Rotate_Left(T(2), 30);
         T(2) := T(1);
         T(1) := Y;
      end loop;

      for I in 61 .. 80 loop
         Y := Rotate_Left(T(1), 5);
         Y := Y + (T(2) xor T(3) xor T(4));
         Y := Y + T(5) + W(I) + 16#CA62_C1D6#;
         T(5) := T(4);
         T(4) := T(3);
         T(3) := Rotate_Left(T(2), 30);
         T(2) := T(1);
         T(1) := Y;
      end loop;

      --|   Update state.

      for I in Context.all.State'Range loop
         Context.all.State(I) := Context.all.State(I) + T(I);
      end loop;

      --|   Clear intermediate values.

      T := (others => 0);
      X := (others => 0);
      W := (others => 0);
   end Transform;

   --+---[Pack_Block]---------------------------------------------------

   function    Pack_Block(
                  B              : in     SHA1_Block)
      return   Packed_Block
   is
      R              : Packed_Block := (others => 0);
      J              : Positive := B'First;
   begin
      for I in R'Range loop
         R(I) := Make_Four_Bytes(B(J + 3), B(J + 2), B(J + 1), B(J));
         J := J + 4;
      end loop;

      return R;
   end Pack_Block;

   --+---[Unpack_State]-------------------------------------------------

   function    Unpack_State(
                  S              : in     State_Registers)
      return   Byte_Array
   is
      R              : Byte_Array(1 .. SHA1_Digest_Bytes) :=
                           (others => 0);
      J              : Positive := R'First;
   begin
      for I in S'Range loop
         R(J .. J + 3) := To_Byte_Array(S(I), Little_Endian);
         J := J + 4;
      end loop;

      return R;
   end Unpack_State;

   ---------------------------------------------------------------------
   -- Specification declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------

   function    Allocate_Context
      return   SHA1_Context_Ptr
   is
   begin
      return new SHA1_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out SHA1_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access SHA1_Context)
   is
   begin
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access SHA1_Context;
                  Bytes          : in     Byte_Array)
   is
      I              : Positive;
      L              : Natural := Bytes'Length;
      R              : Natural;
      J              : Positive;
   begin

      --|   If input is not empty process it.

      if L > 0 then

         --|   Compute start index of context block.

         I := 1 +
              Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                      Eight_Bytes(SHA1_Block_Bytes));

         --|   Increment bit count.

         Context.all.Bit_Count :=
            Context.all.Bit_Count + Shift_Left(Eight_Bytes(L), 3);

         --|   Compute the number of free slots in context block.

         R := 1 + SHA1_Block_Bytes - I;

         J := Bytes'First;

         --|   If the input length is greater than or equal to the
         --|   number of free slots perform the needed
         --|   transformations of input.

         if L >= R then

            --|   Fill context block and transform.

            Context.all.Block(I .. SHA1_Block_Bytes) :=
               Bytes(J .. J + R - 1);
            Transform(Context);

            --|   Update counters.

            J := J + R;
            L := L - R;

            --|   Transform remaining input bytes in SHA1_Block_Bytes
            --|   chunks.

            while L >= SHA1_Block_Bytes loop
               Context.all.Block :=
                  Bytes(J .. J + SHA1_Block_Bytes - 1);
               Transform(Context);
               J := J + SHA1_Block_Bytes;
               L := L - SHA1_Block_Bytes;
            end loop;

            I := 1;
         end if;

         --|   Fill context block with remaining bytes.

         while J <= Bytes'Last loop
            Context.all.Block(I) := Bytes(J);
            I := I + 1;
            J := J + 1;
         end loop;
      end if;
   end Hash_Update;

   --+---[Hash_End]-----------------------------------------------------

   function    Hash_End(
                  Context        : access SHA1_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      BC             : Byte_Array(1 .. 8);
      I              : Positive;
      BC_Offset      : Positive := 1 + SHA1_Block_Bytes - 8;
   begin

      --|   Save bit counter.

      BC := To_Byte_Array(Context.all.Bit_Count, Little_Endian);

     --|   Compute start index of context block.

      I := 1 +
           Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                   Eight_Bytes(SHA1_Block_Bytes));

      --|   Perform pad

      Context.all.Block(I) := 16#80#;
      I := I + 1;

      if I <= SHA1_Block_Bytes then
         Context.all.Block(I .. SHA1_Block_Bytes) := (others => 0);
      end if;

      if I > BC_Offset then
         Transform(Context);
         Context.all.Block := (others => 0);
      end if;

      Context.all.Block(BC_Offset .. SHA1_Block_Bytes) := BC;
      Transform(Context);

      --|   Get digest from state.

      R := To_Message_Digest(Unpack_State(Context.all.State));

      --|   Clear context.

      Context.all.Bit_Count   := 0;
      Context.all.State       := (others => 0);
      Context.all.Block       := (others => 0);

      --|   Return computed digest.

      return R;
   end Hash_End;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out SHA1_Context)
   is
   begin
      Object.Algo_Id    := ACF.Hash.SHA1;
      Object.Bit_Count  := 0;
      Object.State      := Initial_State;
      Object.Block      := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out SHA1_Context)
   is
   begin
      Object.Bit_Count  := 0;
      Object.State      := (others => 0);
      Object.Block      := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.SHA1;
