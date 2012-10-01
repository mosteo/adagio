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
--    File name         : acf-hash-algorithms-md5.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 25th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RSA-MD5 message digest algorithm.
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
-- 1.0   ADD   11252001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.MD5 is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              MD5_Context,
                              MD5_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[MD5_Block_Words]----------------------------------------------
   --|   Number of 4-byte words in a MD5 block.
   --+------------------------------------------------------------------

   MD5_Block_Words         : constant Positive := MD5_Block_Bytes / 4;

   --+---[Initial_State]------------------------------------------------
   --|   Initial values for state registers.
   --+------------------------------------------------------------------

   Initial_State                 : constant State_Registers :=
      (
         16#6745_2301#,
         16#EFCD_AB89#,
         16#98BA_DCFE#,
         16#1032_5476#
      );

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Packed_Block]-------------------------------------------------
   --|  Type for handling MD5 input blocks as an array of words.
   --+------------------------------------------------------------------

   subtype Packed_Block is Four_Bytes_Array(1 .. MD5_Block_Words);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Basic MD5 Functions]------------------------------------------

   function    F(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes;
   pragma Inline(F);

   function    G(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes;
   pragma Inline(G);

   function    H(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes;
   pragma Inline(H);

   function    I(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes;
   pragma Inline(I);

   --+---[MD5 transformation procedures]--------------------------------

   procedure   FF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes);
   pragma Inline(FF);

   procedure   GG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes);
   pragma Inline(GG);

   procedure   HH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes);
   pragma Inline(HH);

   procedure   II(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes);
   pragma Inline(II);

   --+---[Transform]----------------------------------------------------
   --|   Purpose:
   --|   Transforms MD5 state based on input block.
   --|
   --|   Arguments:
   --|   Context              Access to MD5_Context value that mantains
   --|                        the state to transform.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access MD5_Context);

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
                  B              : in     MD5_Block)
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

   --+---[F]------------------------------------------------------------

   function    F(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return ((X and Y) or ((not X) and Z));
   end F;

   --+---[G]------------------------------------------------------------

   function    G(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return ((X and Z) or (Y and (not Z)));
   end G;

   --+---[H]------------------------------------------------------------

   function    H(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return (X xor Y xor Z);
   end H;

   --+---[I]------------------------------------------------------------

   function    I(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return (Y xor (X or (not Z)));
   end I;

   --+---[FF]-----------------------------------------------------------

   procedure   FF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes)
   is
   begin
      A := A + F(B, C, D) + X + Ac;
      A := Rotate_Left(A, S);
      A := A + B;
   end FF;

   --+---[GG]-----------------------------------------------------------

   procedure   GG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes)
   is
   begin
      A := A + G(B, C, D) + X + Ac;
      A := Rotate_Left(A, S);
      A := A + B;
   end GG;

   --+---[HH]-----------------------------------------------------------

   procedure   HH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes)
   is
   begin
      A := A + H(B, C, D) + X + Ac;
      A := Rotate_Left(A, S);
      A := A + B;
   end HH;

   --+---[II]-----------------------------------------------------------

   procedure   II(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in     Four_Bytes;
                  D        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural;
                  Ac       : in     Four_Bytes)
   is
   begin
      A := A + I(B, C, D) + X + Ac;
      A := Rotate_Left(A, S);
      A := A + B;
   end II;

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access MD5_Context)
   is
      T              : State_Registers := Context.all.State;
      X              : Packed_Block := Pack_Block(Context.all.Block);
   begin

      --|   Round 1

      FF(T(1), T(2), T(3), T(4), X( 1),  7, 16#D76A_A478#);
      FF(T(4), T(1), T(2), T(3), X( 2), 12, 16#E8C7_B756#);
      FF(T(3), T(4), T(1), T(2), X( 3), 17, 16#2420_70DB#);
      FF(T(2), T(3), T(4), T(1), X( 4), 22, 16#C1BD_CEEE#);
      FF(T(1), T(2), T(3), T(4), X( 5),  7, 16#F57C_0FAF#);
      FF(T(4), T(1), T(2), T(3), X( 6), 12, 16#4787_C62A#);
      FF(T(3), T(4), T(1), T(2), X( 7), 17, 16#A830_4613#);
      FF(T(2), T(3), T(4), T(1), X( 8), 22, 16#FD46_9501#);
      FF(T(1), T(2), T(3), T(4), X( 9),  7, 16#6980_98D8#);
      FF(T(4), T(1), T(2), T(3), X(10), 12, 16#8B44_F7AF#);
      FF(T(3), T(4), T(1), T(2), X(11), 17, 16#FFFF_5BB1#);
      FF(T(2), T(3), T(4), T(1), X(12), 22, 16#895C_D7BE#);
      FF(T(1), T(2), T(3), T(4), X(13),  7, 16#6B90_1122#);
      FF(T(4), T(1), T(2), T(3), X(14), 12, 16#FD98_7193#);
      FF(T(3), T(4), T(1), T(2), X(15), 17, 16#A679_438E#);
      FF(T(2), T(3), T(4), T(1), X(16), 22, 16#49B4_0821#);

      --|   Round 2

      GG(T(1), T(2), T(3), T(4), X( 2),  5, 16#F61E_2562#);
      GG(T(4), T(1), T(2), T(3), X( 7),  9, 16#C040_B340#);
      GG(T(3), T(4), T(1), T(2), X(12), 14, 16#265E_5A51#);
      GG(T(2), T(3), T(4), T(1), X( 1), 20, 16#E9B6_C7AA#);
      GG(T(1), T(2), T(3), T(4), X( 6),  5, 16#D62F_105D#);
      GG(T(4), T(1), T(2), T(3), X(11),  9, 16#0244_1453#);
      GG(T(3), T(4), T(1), T(2), X(16), 14, 16#D8A1_E681#);
      GG(T(2), T(3), T(4), T(1), X( 5), 20, 16#E7D3_FBC8#);
      GG(T(1), T(2), T(3), T(4), X(10),  5, 16#21E1_CDE6#);
      GG(T(4), T(1), T(2), T(3), X(15),  9, 16#C337_07D6#);
      GG(T(3), T(4), T(1), T(2), X( 4), 14, 16#F4D5_0D87#);
      GG(T(2), T(3), T(4), T(1), X( 9), 20, 16#455A_14ED#);
      GG(T(1), T(2), T(3), T(4), X(14),  5, 16#A9E3_E905#);
      GG(T(4), T(1), T(2), T(3), X( 3),  9, 16#FCEF_A3F8#);
      GG(T(3), T(4), T(1), T(2), X( 8), 14, 16#676F_02D9#);
      GG(T(2), T(3), T(4), T(1), X(13), 20, 16#8D2A_4C8A#);

      --|   Round 3

      HH(T(1), T(2), T(3), T(4), X( 6),  4, 16#FFFA_3942#);
      HH(T(4), T(1), T(2), T(3), X( 9), 11, 16#8771_F681#);
      HH(T(3), T(4), T(1), T(2), X(12), 16, 16#6D9D_6122#);
      HH(T(2), T(3), T(4), T(1), X(15), 23, 16#FDE5_380C#);
      HH(T(1), T(2), T(3), T(4), X( 2),  4, 16#A4BE_EA44#);
      HH(T(4), T(1), T(2), T(3), X( 5), 11, 16#4BDE_CFA9#);
      HH(T(3), T(4), T(1), T(2), X( 8), 16, 16#F6BB_4B60#);
      HH(T(2), T(3), T(4), T(1), X(11), 23, 16#BEBF_BC70#);
      HH(T(1), T(2), T(3), T(4), X(14),  4, 16#289B_7EC6#);
      HH(T(4), T(1), T(2), T(3), X( 1), 11, 16#EAA1_27FA#);
      HH(T(3), T(4), T(1), T(2), X( 4), 16, 16#D4EF_3085#);
      HH(T(2), T(3), T(4), T(1), X( 7), 23, 16#0488_1D05#);
      HH(T(1), T(2), T(3), T(4), X(10),  4, 16#D9D4_D039#);
      HH(T(4), T(1), T(2), T(3), X(13), 11, 16#E6DB_99E5#);
      HH(T(3), T(4), T(1), T(2), X(16), 16, 16#1FA2_7CF8#);
      HH(T(2), T(3), T(4), T(1), X( 3), 23, 16#C4AC_5665#);

      --|   Round 4

      II(T(1), T(2), T(3), T(4), X( 1),  6, 16#F429_2244#);
      II(T(4), T(1), T(2), T(3), X( 8), 10, 16#432A_FF97#);
      II(T(3), T(4), T(1), T(2), X(15), 15, 16#AB94_23A7#);
      II(T(2), T(3), T(4), T(1), X( 6), 21, 16#FC93_A039#);
      II(T(1), T(2), T(3), T(4), X(13),  6, 16#655B_59C3#);
      II(T(4), T(1), T(2), T(3), X( 4), 10, 16#8F0C_CC92#);
      II(T(3), T(4), T(1), T(2), X(11), 15, 16#FFEF_F47D#);
      II(T(2), T(3), T(4), T(1), X( 2), 21, 16#8584_5DD1#);
      II(T(1), T(2), T(3), T(4), X( 9),  6, 16#6FA8_7E4F#);
      II(T(4), T(1), T(2), T(3), X(16), 10, 16#FE2C_E6E0#);
      II(T(3), T(4), T(1), T(2), X( 7), 15, 16#A301_4314#);
      II(T(2), T(3), T(4), T(1), X(14), 21, 16#4E08_11A1#);
      II(T(1), T(2), T(3), T(4), X( 5),  6, 16#F753_7E82#);
      II(T(4), T(1), T(2), T(3), X(12), 10, 16#BD3A_F235#);
      II(T(3), T(4), T(1), T(2), X( 3), 15, 16#2AD7_D2BB#);
      II(T(2), T(3), T(4), T(1), X(10), 21, 16#EB86_D391#);

      --|   Update state

      for I in Context.all.State'Range loop
         Context.all.State(I) := Context.all.State(I) + T(I);
      end loop;

      --|   Zeroize sensitive information.

      T := (others => 0);
      X := (others => 0);
   end Transform;

   --+---[Pack_Block]---------------------------------------------------

   function    Pack_Block(
                  B              : in     MD5_Block)
      return   Packed_Block
   is
      R              : Packed_Block := (others => 0);
      J              : Positive := B'First;
   begin
      for I in R'Range loop
         R(I) := Make_Four_Bytes(B(J), B(J + 1), B(J + 2), B(J + 3));
         J := J + 4;
      end loop;

      return R;
   end Pack_Block;

   --+---[Unpack_State]-------------------------------------------------

   function    Unpack_State(
                  S              : in     State_Registers)
      return   Byte_Array
   is
      R              : Byte_Array(1 .. MD5_Digest_Bytes) :=
                           (others => 0);
      J              : Positive := R'First;
   begin
      for I in S'Range loop
         R(J .. J + 3) := To_Byte_Array(S(I), Big_Endian);
         J := J + 4;
      end loop;

      return R;
   end Unpack_State;

   ---------------------------------------------------------------------
   -- Specification declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------

   function    Allocate_Context
      return   MD5_Context_Ptr
   is
   begin
      return new MD5_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out MD5_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access MD5_Context)
   is
   begin
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access MD5_Context;
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
                      Eight_Bytes(MD5_Block_Bytes));

         --|   Increment bit count.

         Context.all.Bit_Count :=
            Context.all.Bit_Count + Shift_Left(Eight_Bytes(L), 3);

         --|   Compute the number of free slots in context block.

         R := 1 + MD5_Block_Bytes - I;

         J := Bytes'First;

         --|   If the input length is greater than or equal to the
         --|   number of free slots perform the needed
         --|   transformations of input.

         if L >= R then

            --|   Fill context block and transform.

            Context.all.Block(I .. MD5_Block_Bytes) :=
               Bytes(J .. J + R - 1);
            Transform(Context);

            --|   Update counters.

            J := J + R;
            L := L - R;

            --|   Transform remaining input bytes in MD5_Block_Bytes
            --|   chunks.

            while L >= MD5_Block_Bytes loop
               Context.all.Block := Bytes(J .. J + MD5_Block_Bytes - 1);
               Transform(Context);
               J := J + MD5_Block_Bytes;
               L := L - MD5_Block_Bytes;
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
                  Context        : access MD5_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      BC             : Byte_Array(1 .. 8);
      I              : Positive;
      BC_Offset      : Positive := 1 + MD5_Block_Bytes - 8;
   begin

      --|   Save bit counter.

      BC := To_Byte_Array(Context.all.Bit_Count, Big_Endian);

     --|   Compute start index of context block.

      I := 1 +
           Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                   Eight_Bytes(MD5_Block_Bytes));

      --|   Perform pad

      Context.all.Block(I) := 16#80#;
      I := I + 1;

      if I <= MD5_Block_Bytes then
         Context.all.Block(I .. MD5_Block_Bytes) := (others => 0);
      end if;

      if I > BC_Offset then
         Transform(Context);
         Context.all.Block := (others => 0);
      end if;

      Context.all.Block(BC_Offset .. MD5_Block_Bytes) := BC;
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
                  Object         : in out MD5_Context)
   is
   begin
      Object.Algo_Id    := ACF.Hash.MD5;
      Object.Bit_Count  := 0;
      Object.State      := Initial_State;
      Object.Block      := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out MD5_Context)
   is
   begin
      Object.Bit_Count  := 0;
      Object.State      := (others => 0);
      Object.Block      := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.MD5;
