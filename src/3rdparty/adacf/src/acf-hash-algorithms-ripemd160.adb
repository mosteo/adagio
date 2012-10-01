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
--    File name         : acf-hash-algorithms-ripemd160.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 27th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RIPEMD-160 message digest algorithm.
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
-- 1.0   ADD   11272001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.RIPEMD160 is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              RIPEMD160_Context,
                              RIPEMD160_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD160_Block_Words]----------------------------------------
   --|   Number of 4-byte words in a RIPEMD-160 block.
   --+------------------------------------------------------------------

   RIPEMD160_Block_Words   : constant Positive :=
                                    RIPEMD160_Block_Bytes / 4;

   --+---[Initial_State]------------------------------------------------
   --|   Initial values for state registers.
   --+------------------------------------------------------------------

   Initial_State                 : constant State_Registers :=
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
   --|  Type for handling RIPEMD-160 input blocks as an array of words.
   --+------------------------------------------------------------------

   subtype Packed_Block is Four_Bytes_Array(1 .. RIPEMD160_Block_Words);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Basic RIPEMD-160 functions]-----------------------------------

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

   function    J(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes;
   pragma Inline(J);

   --+---[RIPEMD-160 operations]----------------------------------------

   procedure   FF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(FF);

   procedure   GG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(GG);

   procedure   HH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(HH);

   procedure   II(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(II);

   procedure   JJ(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(JJ);

   procedure   FFF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(FFF);

   procedure   GGG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(GGG);

   procedure   HHH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(HHH);

   procedure   III(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(III);

   procedure   JJJ(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural);
   pragma Inline(JJJ);

   --+---[Transform]----------------------------------------------------
   --|   Purpose:
   --|   Transforms RIPEMD-160 state based on input block.
   --|
   --|   Arguments:
   --|   Context              Access to RIPEMD160_Context value that
   --|                        mantains the state to transform.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access RIPEMD160_Context);

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
                  B              : in     RIPEMD160_Block)
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
      return (X xor Y xor Z);
   end F;

   --+---[G]------------------------------------------------------------

   function    G(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return ((X and Y) or ((not X) and Z));
   end G;

   --+---[H]------------------------------------------------------------

   function    H(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return ((X or (not Y)) xor Z);
   end H;

   --+---[I]------------------------------------------------------------

   function    I(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return ((X and Z) or (Y and (not Z)));
   end I;

   --+---[J]------------------------------------------------------------

   function    J(
                  X        : in     Four_Bytes;
                  Y        : in     Four_Bytes;
                  Z        : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return (X xor (Y or (not Z)));
   end J;

   --+---[FF]-----------------------------------------------------------

   procedure   FF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + F(B, C, D) + X;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end FF;

   --+---[GG]-----------------------------------------------------------

   procedure   GG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + G(B, C, D) + X + 16#5A82_7999#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end GG;

   --+---[HH]-----------------------------------------------------------

   procedure   HH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + H(B, C, D) + X + 16#6ED9_EBA1#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end HH;

   --+---[II]-----------------------------------------------------------

   procedure   II(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + I(B, C, D) + X + 16#8F1B_BCDC#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end II;

   --+---[JJ]-----------------------------------------------------------

   procedure   JJ(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + J(B, C, D) + X + 16#A953_FD4E#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end JJ;

   --+---[FFF]----------------------------------------------------------

   procedure   FFF(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + F(B, C, D) + X;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end FFF;

   --+---[GGG]----------------------------------------------------------

   procedure   GGG(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + G(B, C, D) + X + 16#7A6D_76E9#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end GGG;

   --+---[HHH]----------------------------------------------------------

   procedure   HHH(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + H(B, C, D) + X + 16#6D70_3EF3#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end HHH;

   --+---[III]----------------------------------------------------------

   procedure   III(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + I(B, C, D) + X + 16#5C4D_D124#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end III;

   --+---[JJJ]----------------------------------------------------------

   procedure   JJJ(
                  A        : in out Four_Bytes;
                  B        : in     Four_Bytes;
                  C        : in out Four_Bytes;
                  D        : in     Four_Bytes;
                  E        : in     Four_Bytes;
                  X        : in     Four_Bytes;
                  S        : in     Natural)
   is
   begin
      A := A + J(B, C, D) + X + 16#50A2_8BE6#;
      A := Rotate_Left(A, S) + E;
      C := Rotate_Left(C, 10);
   end JJJ;

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access RIPEMD160_Context)
   is
      T              : State_Registers := Context.all.State;
      TT             : State_Registers := Context.all.State;
      X              : Packed_Block := Pack_Block(Context.all.Block);
   begin

      --|   Round 1

      FF(T(1), T(2), T(3), T(4), T(5), X( 1), 11);
      FF(T(5), T(1), T(2), T(3), T(4), X( 2), 14);
      FF(T(4), T(5), T(1), T(2), T(3), X( 3), 15);
      FF(T(3), T(4), T(5), T(1), T(2), X( 4), 12);
      FF(T(2), T(3), T(4), T(5), T(1), X( 5),  5);
      FF(T(1), T(2), T(3), T(4), T(5), X( 6),  8);
      FF(T(5), T(1), T(2), T(3), T(4), X( 7),  7);
      FF(T(4), T(5), T(1), T(2), T(3), X( 8),  9);
      FF(T(3), T(4), T(5), T(1), T(2), X( 9), 11);
      FF(T(2), T(3), T(4), T(5), T(1), X(10), 13);
      FF(T(1), T(2), T(3), T(4), T(5), X(11), 14);
      FF(T(5), T(1), T(2), T(3), T(4), X(12), 15);
      FF(T(4), T(5), T(1), T(2), T(3), X(13),  6);
      FF(T(3), T(4), T(5), T(1), T(2), X(14),  7);
      FF(T(2), T(3), T(4), T(5), T(1), X(15),  9);
      FF(T(1), T(2), T(3), T(4), T(5), X(16),  8);

      --|   Round 2

      GG(T(5), T(1), T(2), T(3), T(4), X( 8),  7);
      GG(T(4), T(5), T(1), T(2), T(3), X( 5),  6);
      GG(T(3), T(4), T(5), T(1), T(2), X(14),  8);
      GG(T(2), T(3), T(4), T(5), T(1), X( 2), 13);
      GG(T(1), T(2), T(3), T(4), T(5), X(11), 11);
      GG(T(5), T(1), T(2), T(3), T(4), X( 7),  9);
      GG(T(4), T(5), T(1), T(2), T(3), X(16),  7);
      GG(T(3), T(4), T(5), T(1), T(2), X( 4), 15);
      GG(T(2), T(3), T(4), T(5), T(1), X(13),  7);
      GG(T(1), T(2), T(3), T(4), T(5), X( 1), 12);
      GG(T(5), T(1), T(2), T(3), T(4), X(10), 15);
      GG(T(4), T(5), T(1), T(2), T(3), X( 6),  9);
      GG(T(3), T(4), T(5), T(1), T(2), X( 3), 11);
      GG(T(2), T(3), T(4), T(5), T(1), X(15),  7);
      GG(T(1), T(2), T(3), T(4), T(5), X(12), 13);
      GG(T(5), T(1), T(2), T(3), T(4), X( 9), 12);

      --|   Round 3

      HH(T(4), T(5), T(1), T(2), T(3), X( 4), 11);
      HH(T(3), T(4), T(5), T(1), T(2), X(11), 13);
      HH(T(2), T(3), T(4), T(5), T(1), X(15),  6);
      HH(T(1), T(2), T(3), T(4), T(5), X( 5),  7);
      HH(T(5), T(1), T(2), T(3), T(4), X(10), 14);
      HH(T(4), T(5), T(1), T(2), T(3), X(16),  9);
      HH(T(3), T(4), T(5), T(1), T(2), X( 9), 13);
      HH(T(2), T(3), T(4), T(5), T(1), X( 2), 15);
      HH(T(1), T(2), T(3), T(4), T(5), X( 3), 14);
      HH(T(5), T(1), T(2), T(3), T(4), X( 8),  8);
      HH(T(4), T(5), T(1), T(2), T(3), X( 1), 13);
      HH(T(3), T(4), T(5), T(1), T(2), X( 7),  6);
      HH(T(2), T(3), T(4), T(5), T(1), X(14),  5);
      HH(T(1), T(2), T(3), T(4), T(5), X(12), 12);
      HH(T(5), T(1), T(2), T(3), T(4), X( 6),  7);
      HH(T(4), T(5), T(1), T(2), T(3), X(13),  5);

      --|   Round 4

      II(T(3), T(4), T(5), T(1), T(2), X( 2), 11);
      II(T(2), T(3), T(4), T(5), T(1), X(10), 12);
      II(T(1), T(2), T(3), T(4), T(5), X(12), 14);
      II(T(5), T(1), T(2), T(3), T(4), X(11), 15);
      II(T(4), T(5), T(1), T(2), T(3), X( 1), 14);
      II(T(3), T(4), T(5), T(1), T(2), X( 9), 15);
      II(T(2), T(3), T(4), T(5), T(1), X(13),  9);
      II(T(1), T(2), T(3), T(4), T(5), X( 5),  8);
      II(T(5), T(1), T(2), T(3), T(4), X(14),  9);
      II(T(4), T(5), T(1), T(2), T(3), X( 4), 14);
      II(T(3), T(4), T(5), T(1), T(2), X( 8),  5);
      II(T(2), T(3), T(4), T(5), T(1), X(16),  6);
      II(T(1), T(2), T(3), T(4), T(5), X(15),  8);
      II(T(5), T(1), T(2), T(3), T(4), X( 6),  6);
      II(T(4), T(5), T(1), T(2), T(3), X( 7),  5);
      II(T(3), T(4), T(5), T(1), T(2), X( 3), 12);

      --|   Round 5

      JJ(T(2), T(3), T(4), T(5), T(1), X( 5),  9);
      JJ(T(1), T(2), T(3), T(4), T(5), X( 1), 15);
      JJ(T(5), T(1), T(2), T(3), T(4), X( 6),  5);
      JJ(T(4), T(5), T(1), T(2), T(3), X(10), 11);
      JJ(T(3), T(4), T(5), T(1), T(2), X( 8),  6);
      JJ(T(2), T(3), T(4), T(5), T(1), X(13),  8);
      JJ(T(1), T(2), T(3), T(4), T(5), X( 3), 13);
      JJ(T(5), T(1), T(2), T(3), T(4), X(11), 12);
      JJ(T(4), T(5), T(1), T(2), T(3), X(15),  5);
      JJ(T(3), T(4), T(5), T(1), T(2), X( 2), 12);
      JJ(T(2), T(3), T(4), T(5), T(1), X( 4), 13);
      JJ(T(1), T(2), T(3), T(4), T(5), X( 9), 14);
      JJ(T(5), T(1), T(2), T(3), T(4), X(12), 11);
      JJ(T(4), T(5), T(1), T(2), T(3), X( 7),  8);
      JJ(T(3), T(4), T(5), T(1), T(2), X(16),  5);
      JJ(T(2), T(3), T(4), T(5), T(1), X(14),  6);

      --|   Parallel round 1

      JJJ(TT(1), TT(2), TT(3), TT(4), TT(5), X( 6),  8);
      JJJ(TT(5), TT(1), TT(2), TT(3), TT(4), X(15),  9);
      JJJ(TT(4), TT(5), TT(1), TT(2), TT(3), X( 8),  9);
      JJJ(TT(3), TT(4), TT(5), TT(1), TT(2), X( 1), 11);
      JJJ(TT(2), TT(3), TT(4), TT(5), TT(1), X(10), 13);
      JJJ(TT(1), TT(2), TT(3), TT(4), TT(5), X( 3), 15);
      JJJ(TT(5), TT(1), TT(2), TT(3), TT(4), X(12), 15);
      JJJ(TT(4), TT(5), TT(1), TT(2), TT(3), X( 5),  5);
      JJJ(TT(3), TT(4), TT(5), TT(1), TT(2), X(14),  7);
      JJJ(TT(2), TT(3), TT(4), TT(5), TT(1), X( 7),  7);
      JJJ(TT(1), TT(2), TT(3), TT(4), TT(5), X(16),  8);
      JJJ(TT(5), TT(1), TT(2), TT(3), TT(4), X( 9), 11);
      JJJ(TT(4), TT(5), TT(1), TT(2), TT(3), X( 2), 14);
      JJJ(TT(3), TT(4), TT(5), TT(1), TT(2), X(11), 14);
      JJJ(TT(2), TT(3), TT(4), TT(5), TT(1), X( 4), 12);
      JJJ(TT(1), TT(2), TT(3), TT(4), TT(5), X(13),  6);

      --|   Parallel round 2

      III(TT(5), TT(1), TT(2), TT(3), TT(4), X( 7),  9);
      III(TT(4), TT(5), TT(1), TT(2), TT(3), X(12), 13);
      III(TT(3), TT(4), TT(5), TT(1), TT(2), X( 4), 15);
      III(TT(2), TT(3), TT(4), TT(5), TT(1), X( 8),  7);
      III(TT(1), TT(2), TT(3), TT(4), TT(5), X( 1), 12);
      III(TT(5), TT(1), TT(2), TT(3), TT(4), X(14),  8);
      III(TT(4), TT(5), TT(1), TT(2), TT(3), X( 6),  9);
      III(TT(3), TT(4), TT(5), TT(1), TT(2), X(11), 11);
      III(TT(2), TT(3), TT(4), TT(5), TT(1), X(15),  7);
      III(TT(1), TT(2), TT(3), TT(4), TT(5), X(16),  7);
      III(TT(5), TT(1), TT(2), TT(3), TT(4), X( 9), 12);
      III(TT(4), TT(5), TT(1), TT(2), TT(3), X(13),  7);
      III(TT(3), TT(4), TT(5), TT(1), TT(2), X( 5),  6);
      III(TT(2), TT(3), TT(4), TT(5), TT(1), X(10), 15);
      III(TT(1), TT(2), TT(3), TT(4), TT(5), X( 2), 13);
      III(TT(5), TT(1), TT(2), TT(3), TT(4), X( 3), 11);

      --|   Parallel round 3

      HHH(TT(4), TT(5), TT(1), TT(2), TT(3), X(16),  9);
      HHH(TT(3), TT(4), TT(5), TT(1), TT(2), X( 6),  7);
      HHH(TT(2), TT(3), TT(4), TT(5), TT(1), X( 2), 15);
      HHH(TT(1), TT(2), TT(3), TT(4), TT(5), X( 4), 11);
      HHH(TT(5), TT(1), TT(2), TT(3), TT(4), X( 8),  8);
      HHH(TT(4), TT(5), TT(1), TT(2), TT(3), X(15),  6);
      HHH(TT(3), TT(4), TT(5), TT(1), TT(2), X( 7),  6);
      HHH(TT(2), TT(3), TT(4), TT(5), TT(1), X(10), 14);
      HHH(TT(1), TT(2), TT(3), TT(4), TT(5), X(12), 12);
      HHH(TT(5), TT(1), TT(2), TT(3), TT(4), X( 9), 13);
      HHH(TT(4), TT(5), TT(1), TT(2), TT(3), X(13),  5);
      HHH(TT(3), TT(4), TT(5), TT(1), TT(2), X( 3), 14);
      HHH(TT(2), TT(3), TT(4), TT(5), TT(1), X(11), 13);
      HHH(TT(1), TT(2), TT(3), TT(4), TT(5), X( 1), 13);
      HHH(TT(5), TT(1), TT(2), TT(3), TT(4), X( 5),  7);
      HHH(TT(4), TT(5), TT(1), TT(2), TT(3), X(14),  5);

      --|   Parallel round 4

      GGG(TT(3), TT(4), TT(5), TT(1), TT(2), X( 9), 15);
      GGG(TT(2), TT(3), TT(4), TT(5), TT(1), X( 7),  5);
      GGG(TT(1), TT(2), TT(3), TT(4), TT(5), X( 5),  8);
      GGG(TT(5), TT(1), TT(2), TT(3), TT(4), X( 2), 11);
      GGG(TT(4), TT(5), TT(1), TT(2), TT(3), X( 4), 14);
      GGG(TT(3), TT(4), TT(5), TT(1), TT(2), X(12), 14);
      GGG(TT(2), TT(3), TT(4), TT(5), TT(1), X(16),  6);
      GGG(TT(1), TT(2), TT(3), TT(4), TT(5), X( 1), 14);
      GGG(TT(5), TT(1), TT(2), TT(3), TT(4), X( 6),  6);
      GGG(TT(4), TT(5), TT(1), TT(2), TT(3), X(13),  9);
      GGG(TT(3), TT(4), TT(5), TT(1), TT(2), X( 3), 12);
      GGG(TT(2), TT(3), TT(4), TT(5), TT(1), X(14),  9);
      GGG(TT(1), TT(2), TT(3), TT(4), TT(5), X(10), 12);
      GGG(TT(5), TT(1), TT(2), TT(3), TT(4), X( 8),  5);
      GGG(TT(4), TT(5), TT(1), TT(2), TT(3), X(11), 15);
      GGG(TT(3), TT(4), TT(5), TT(1), TT(2), X(15),  8);

      --|   Parallel round 5.

      FFF(TT(2), TT(3), TT(4), TT(5), TT(1), X(13),  8);
      FFF(TT(1), TT(2), TT(3), TT(4), TT(5), X(16),  5);
      FFF(TT(5), TT(1), TT(2), TT(3), TT(4), X(11), 12);
      FFF(TT(4), TT(5), TT(1), TT(2), TT(3), X( 5),  9);
      FFF(TT(3), TT(4), TT(5), TT(1), TT(2), X( 2), 12);
      FFF(TT(2), TT(3), TT(4), TT(5), TT(1), X( 6),  5);
      FFF(TT(1), TT(2), TT(3), TT(4), TT(5), X( 9), 14);
      FFF(TT(5), TT(1), TT(2), TT(3), TT(4), X( 8),  6);
      FFF(TT(4), TT(5), TT(1), TT(2), TT(3), X( 7),  8);
      FFF(TT(3), TT(4), TT(5), TT(1), TT(2), X( 3), 13);
      FFF(TT(2), TT(3), TT(4), TT(5), TT(1), X(14),  6);
      FFF(TT(1), TT(2), TT(3), TT(4), TT(5), X(15),  5);
      FFF(TT(5), TT(1), TT(2), TT(3), TT(4), X( 1), 15);
      FFF(TT(4), TT(5), TT(1), TT(2), TT(3), X( 4), 13);
      FFF(TT(3), TT(4), TT(5), TT(1), TT(2), X(10), 11);
      FFF(TT(2), TT(3), TT(4), TT(5), TT(1), X(12), 11);

      --|   Combine results.

      TT(4)                := Context.all.State(2) + T(3) + TT(4);
      Context.all.State(2) := Context.all.State(3) + T(4) + TT(5);
      Context.all.State(3) := Context.all.State(4) + T(5) + TT(1);
      Context.all.State(4) := Context.all.State(5) + T(1) + TT(2);
      Context.all.State(5) := Context.all.State(1) + T(2) + TT(3);
      Context.all.State(1) := TT(4);

      --|   Zeroize sensitive information.

      T  := (others => 0);
      TT := (others => 0);
      X  := (others => 0);
   end Transform;

   --+---[Pack_Block]---------------------------------------------------

   function    Pack_Block(
                  B              : in     RIPEMD160_Block)
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
      R              : Byte_Array(1 .. RIPEMD160_Digest_Bytes)
                           := (others => 0);
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
      return   RIPEMD160_Context_Ptr
   is
   begin
      return new RIPEMD160_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out RIPEMD160_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access RIPEMD160_Context)
   is
   begin
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access RIPEMD160_Context;
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
                      Eight_Bytes(RIPEMD160_Block_Bytes));

         --|   Increment bit count.

         Context.all.Bit_Count :=
            Context.all.Bit_Count + Shift_Left(Eight_Bytes(L), 3);

         --|   Compute the number of free slots in context block.

         R := 1 + RIPEMD160_Block_Bytes - I;

         J := Bytes'First;

         --|   If the input length is greater than or equal to the
         --|   number of free slots perform the needed
         --|   transformations of input.

         if L >= R then

            --|   Fill context block and transform.

            Context.all.Block(I .. RIPEMD160_Block_Bytes) :=
               Bytes(J .. J + R - 1);
            Transform(Context);

            --|   Update counters.

            J := J + R;
            L := L - R;

            --|   Transform remaining input bytes in
            --|   RIPEMD160_Block_Bytes chunks.

            while L >= RIPEMD160_Block_Bytes loop
               Context.all.Block :=
                  Bytes(J .. J + RIPEMD160_Block_Bytes - 1);
               Transform(Context);
               J := J + RIPEMD160_Block_Bytes;
               L := L - RIPEMD160_Block_Bytes;
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
                  Context        : access RIPEMD160_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      BC             : Byte_Array(1 .. 8);
      I              : Positive;
      BC_Offset      : Positive := 1 + RIPEMD160_Block_Bytes - 8;
   begin

      --|   Save bit counter.

      BC := To_Byte_Array(Context.all.Bit_Count, Big_Endian);

     --|   Compute start index of context block.

      I := 1 +
           Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                   Eight_Bytes(RIPEMD160_Block_Bytes));

      --|   Perform pad

      Context.all.Block(I) := 16#80#;
      I := I + 1;

      if I <= RIPEMD160_Block_Bytes then
         Context.all.Block(I .. RIPEMD160_Block_Bytes) := (others => 0);
      end if;

      if I > BC_Offset then
         Transform(Context);
         Context.all.Block := (others => 0);
      end if;

      Context.all.Block(BC_Offset .. RIPEMD160_Block_Bytes) := BC;
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
                  Object         : in out RIPEMD160_Context)
   is
   begin
      Object.Algo_Id    := ACF.Hash.RIPEMD_160;
      Object.Bit_Count  := 0;
      Object.State      := Initial_State;
      Object.Block      := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out RIPEMD160_Context)
   is
   begin
      Object.Bit_Count  := 0;
      Object.State      := (others => 0);
      Object.Block      := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.RIPEMD160;
