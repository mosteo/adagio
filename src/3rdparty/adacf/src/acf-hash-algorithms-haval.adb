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
--    File name         : acf-hash-algorithms-haval.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : December 4th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the HAVAL message digest algorithm.
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
-- 1.0   ADD   12042001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.HAVAL is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              HAVAL_Context,
                              HAVAL_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[HAVAL_Block_Words]--------------------------------------------
   --|   Number of 4-byte words in a HAVAL block.
   --+------------------------------------------------------------------

   HAVAL_Block_Words             : constant Positive :=
                                       HAVAL_Block_Bytes / 4;

   --+---[Initial_State]------------------------------------------------
   --|   Initial values for state registers.
   --+------------------------------------------------------------------

   Initial_State                 : constant State_Registers :=
      (
         16#243F_6A88#,
         16#85A3_08D3#,
         16#1319_8A2E#,
         16#0370_7344#,
         16#A409_3822#,
         16#299F_31D0#,
         16#082E_FA98#,
         16#EC4E_6C89#
      );

   --+---[HAVAL_Sizes]--------------------------------------------------
   --+------------------------------------------------------------------

   HAVAL_Sizes                   : constant array(HAVAL_Digest_Size) of
                                       Two_Bytes :=
      (
         HAVAL_128   => 128,
         HAVAL_160   => 160,
         HAVAL_192   => 192,
         HAVAL_224   => 224,
         HAVAL_256   => 256
      );

   --+---[HAVAL_Version]------------------------------------------------
   --+------------------------------------------------------------------

   HAVAL_Version                 : constant Byte := 1;

   --+---[Max_HAVAL_Digest_Bytes]---------------------------------------
   --+------------------------------------------------------------------

   Max_HAVAL_Digest_Bytes        : constant Positive := 32;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Packed_Block]-------------------------------------------------
   --|  Type for handling HAVAL input blocks as an array of words.
   --+------------------------------------------------------------------

   subtype Packed_Block is Four_Bytes_Array(1 .. HAVAL_Block_Words);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[HAVAL non-linear F functions]---------------------------------

   function    F_1(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes;

   function    F_2(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes;

   function    F_3(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes;

   function    F_4(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes;

   function    F_5(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes;

   --+---[Transform]----------------------------------------------------
   --|   Purpose:
   --|   Transforms HAVAL state based on input block.
   --|
   --|   Arguments:
   --|   Context              Access to HAVAL_Context value that
   --|                        mantains the state to transform.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access HAVAL_Context);

   --+---[Transform specific procedures for passes]---------------------

   procedure   Transform_3(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block);

   procedure   Transform_4(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block);

   procedure   Transform_5(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block);

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
                  B              : in     HAVAL_Block)
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

   --+---[Tailor]-------------------------------------------------------
   --+------------------------------------------------------------------

   procedure   Tailor(
                  Context        : access HAVAL_Context);

   ---------------------------------------------------------------------
   -- Body subprogram bodies
   ---------------------------------------------------------------------

   --+---[F_1]----------------------------------------------------------

   function    F_1(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return
         (
            (X1 and (X0 xor X4))    xor
            (X2 and X5)             xor
            (X3 and X6)             xor
            X0
         );
   end F_1;

   --+---[F_2]----------------------------------------------------------

   function    F_2(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return
         (
            (X2 and ((X1 and (not X3)) xor (X4 and X5) xor X6 xor X0)) xor
            ((X4 and (X1 xor X5)) xor (X3 and X5) xor X0)
         );
   end F_2;

   --+---[F_3]----------------------------------------------------------

   function    F_3(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return
         (
            (X3 and ((X1 and X2) xor X6 xor X0)) xor
            (X1 and X4) xor (X2 and X5) xor X0
         );
   end F_3;

   --+---[F_4]----------------------------------------------------------

   function    F_4(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return
         (
            (X4 and ((X5 and (not X2)) xor (X3 and (not X6)) xor X1 xor X6 xor X0)) xor
            (X3 and ((X1 and X2) xor X5 xor X6)) xor
            (X2 and X6) xor X0
         );
   end F_4;

   --+---[F_5]----------------------------------------------------------

   function    F_5(
                  X6             : in     Four_Bytes;
                  X5             : in     Four_Bytes;
                  X4             : in     Four_Bytes;
                  X3             : in     Four_Bytes;
                  X2             : in     Four_Bytes;
                  X1             : in     Four_Bytes;
                  X0             : in     Four_Bytes)
      return   Four_Bytes
   is
   begin
      return
         (
         	(X1 and (X4 xor (X0 and X2 and X3))) xor
         	((X2 xor X0) and X5) xor (X3 and X6) xor X0
         );
   end F_5;

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access HAVAL_Context)
   is
   begin
      case Context.all.Passes is
         when 3 =>
            Transform_3(
               Context.all.State,
               Context.all.Block);
         when 4 =>
            Transform_4(
               Context.all.State,
               Context.all.Block);
         when 5 =>
            Transform_5(
               Context.all.State,
               Context.all.Block);
      end case;
   end Transform;

   --+---[Transform_3]--------------------------------------------------

   procedure   Transform_3(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block)
   is
      procedure   FF_1(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_1(X1, X0, X3, X5, X6, X2, X4);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W;
      end FF_1;

      procedure   FF_2(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_2(X4, X2, X1, X0, X5, X3, X6);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_2;

      procedure   FF_3(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_3(X6, X1, X2, X3, X4, X5, X0);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_3;

      T              : State_Registers := E;
      X              : Packed_Block := Pack_Block(B);
   begin

      --|   Pass 1

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 2));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 3));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 5));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 6));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 7));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 8));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 9));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(11));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(12));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(13));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(14));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(15));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(16));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(17));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(18));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(19));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(20));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(21));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(22));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(23));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(24));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(25));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(26));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(28));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(30));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(31));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(32));

      --|   Pass 2

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 6), 16#452821E6#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#38D01377#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27), 16#BE5466CF#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(19), 16#34E90C6C#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(12), 16#C0AC29B7#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(29), 16#C97C50DD#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 8), 16#3F84D5B5#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(17), 16#B5470917#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1), 16#9216D5D9#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(24), 16#8979FB1B#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(21), 16#D1310BA6#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(23), 16#98DFB5AC#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#2FFD72DB#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(11), 16#D01ADFB7#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 5), 16#B8E1AFED#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 9), 16#6A267E96#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(31), 16#BA7C9045#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 4), 16#F12C7F99#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#24A19947#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(10), 16#B3916CF7#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#0801F2E2#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(25), 16#858EFC16#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(30), 16#636920D8#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 7), 16#71574E69#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#A458FEA3#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(13), 16#F4933D7E#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(16), 16#0D95748F#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(14), 16#728EB658#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 3), 16#718BCD58#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(26), 16#82154AEE#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(32), 16#7B54A41D#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#C25A59B5#);

      --|   Pass 3

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#9C30D539#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10), 16#2AF26013#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 5), 16#C5D1B023#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(21), 16#286085F0#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29), 16#CA417918#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(18), 16#B8DB38EF#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 9), 16#8E79DCB0#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(23), 16#603A180E#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(30), 16#6C9E0E8B#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#B01E8A3E#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(26), 16#D71577C1#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(13), 16#BD314B27#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(25), 16#78AF2FDA#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(31), 16#55605C60#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(17), 16#E65525F3#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(27), 16#AA55AB94#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(32), 16#57489862#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(16), 16#63E81440#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 8), 16#55CA396A#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4), 16#2AAB10B6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#B4CC5C34#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 1), 16#1141E8CE#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(19), 16#A15486AF#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#7C72E993#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(14), 16#B3EE1411#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 7), 16#636FBC2A#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#2BA9C55D#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(11), 16#741831F6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(24), 16#CE5C3E16#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(12), 16#9B87931E#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 6), 16#AFD6BA33#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 3), 16#6C24CF5C#);

      --|   Update state registers.

      for I in E'Range loop
         E(I) := E(I) + T(I);
      end loop;

      --|   Zeroize sensitive information.

      T := (others => 0);
      X := (others => 0);
   end Transform_3;

   --+---[Transform_4]--------------------------------------------------

   procedure   Transform_4(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block)
   is
      procedure   FF_1(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_1(X2, X6, X1, X4, X5, X3, X0);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W;
      end FF_1;

      procedure   FF_2(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_2(X3, X5, X2, X0, X1, X6, X4);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_2;

      procedure   FF_3(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_3(X1, X4, X3, X6, X0, X2, X5);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_3;

      procedure   FF_4(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_4(X6, X4, X0, X5, X2, X1, X3);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_4;

      T              : State_Registers := E;
      X              : Packed_Block := Pack_Block(B);
   begin

      --|   Pass 1

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 2));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 3));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 5));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 6));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 7));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 8));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 9));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(11));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(12));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(13));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(14));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(15));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(16));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(17));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(18));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(19));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(20));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(21));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(22));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(23));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(24));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(25));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(26));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(28));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(30));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(31));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(32));

      --|   Pass 2

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 6), 16#452821E6#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#38D01377#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27), 16#BE5466CF#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(19), 16#34E90C6C#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(12), 16#C0AC29B7#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(29), 16#C97C50DD#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 8), 16#3F84D5B5#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(17), 16#B5470917#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1), 16#9216D5D9#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(24), 16#8979FB1B#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(21), 16#D1310BA6#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(23), 16#98DFB5AC#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#2FFD72DB#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(11), 16#D01ADFB7#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 5), 16#B8E1AFED#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 9), 16#6A267E96#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(31), 16#BA7C9045#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 4), 16#F12C7F99#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#24A19947#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(10), 16#B3916CF7#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#0801F2E2#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(25), 16#858EFC16#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(30), 16#636920D8#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 7), 16#71574E69#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#A458FEA3#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(13), 16#F4933D7E#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(16), 16#0D95748F#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(14), 16#728EB658#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 3), 16#718BCD58#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(26), 16#82154AEE#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(32), 16#7B54A41D#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#C25A59B5#);

      --|   Pass 3

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#9C30D539#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10), 16#2AF26013#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 5), 16#C5D1B023#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(21), 16#286085F0#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29), 16#CA417918#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(18), 16#B8DB38EF#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 9), 16#8E79DCB0#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(23), 16#603A180E#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(30), 16#6C9E0E8B#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#B01E8A3E#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(26), 16#D71577C1#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(13), 16#BD314B27#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(25), 16#78AF2FDA#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(31), 16#55605C60#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(17), 16#E65525F3#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(27), 16#AA55AB94#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(32), 16#57489862#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(16), 16#63E81440#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 8), 16#55CA396A#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4), 16#2AAB10B6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#B4CC5C34#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 1), 16#1141E8CE#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(19), 16#A15486AF#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#7C72E993#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(14), 16#B3EE1411#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 7), 16#636FBC2A#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#2BA9C55D#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(11), 16#741831F6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(24), 16#CE5C3E16#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(12), 16#9B87931E#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 6), 16#AFD6BA33#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 3), 16#6C24CF5C#);

      --|   Pass 4

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(25), 16#7A325381#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 5), 16#28958677#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 1), 16#3B8F4898#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(15), 16#6B4BB9AF#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 3), 16#C4BFE81B#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 8), 16#66282193#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(29), 16#61D809CC#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(24), 16#FB21A991#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(27), 16#487CAC60#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 7), 16#5DEC8032#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(31), 16#EF845D5D#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(21), 16#E98575B1#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(19), 16#DC262302#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(26), 16#EB651B88#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(20), 16#23893E81#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 4), 16#D396ACC5#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(23), 16#0F6D6FF3#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(12), 16#83F44239#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(32), 16#2E0B4482#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(22), 16#A4842004#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 9), 16#69C8F04A#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(28), 16#9E1F9B5E#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(13), 16#21C66842#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(10), 16#F6E96C9A#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 2), 16#670C9C61#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(30), 16#ABD388F0#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 6), 16#6A51A0D2#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(16), 16#D8542F68#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#960FA728#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(11), 16#AB5133A3#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(17), 16#6EEF0B6C#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(14), 16#137A3BE4#);

      --|   Update state registers.

      for I in E'Range loop
         E(I) := E(I) + T(I);
      end loop;

      --|   Zeroize sensitive information.

      T := (others => 0);
      X := (others => 0);
   end Transform_4;

   --+---[Transform_5]--------------------------------------------------

   procedure   Transform_5(
                  E              : in out State_Registers;
                  B              : in     HAVAL_Block)
   is
      procedure   FF_1(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_1(X3, X4, X1, X0, X5, X2, X6);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W;
      end FF_1;

      procedure   FF_2(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_2(X6, X2, X1, X0, X3, X4, X5);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_2;

      procedure   FF_3(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_3(X2, X6, X0, X4, X3, X1, X5);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_3;

      procedure   FF_4(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_4(X1, X5, X3, X2, X0, X4, X6);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_4;

      procedure   FF_5(
                     X7             : in out Four_Bytes;
                     X6             : in     Four_Bytes;
                     X5             : in     Four_Bytes;
                     X4             : in     Four_Bytes;
                     X3             : in     Four_Bytes;
                     X2             : in     Four_Bytes;
                     X1             : in     Four_Bytes;
                     X0             : in     Four_Bytes;
                     W              : in     Four_Bytes;
                     C              : in     Four_Bytes)
      is
         T              : Four_Bytes := F_5(X2, X5, X0, X6, X4, X3, X1);
      begin
         X7 := Rotate_Right(T, 7) + Rotate_Right(X7, 11) + W + C;
      end FF_5;

      T              : State_Registers := E;
      X              : Packed_Block := Pack_Block(B);
   begin

      --|   Pass 1

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 2));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 3));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 5));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 6));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 7));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 8));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 9));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(11));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(12));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(13));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(14));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(15));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(16));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(17));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(18));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(19));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(20));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(21));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(22));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(23));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(24));

      FF_1(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(25));
      FF_1(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(26));
      FF_1(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27));
      FF_1(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(28));
      FF_1(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29));
      FF_1(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(30));
      FF_1(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(31));
      FF_1(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(32));

      --|   Pass 2

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 6), 16#452821E6#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#38D01377#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(27), 16#BE5466CF#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(19), 16#34E90C6C#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(12), 16#C0AC29B7#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(29), 16#C97C50DD#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 8), 16#3F84D5B5#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(17), 16#B5470917#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 1), 16#9216D5D9#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(24), 16#8979FB1B#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(21), 16#D1310BA6#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(23), 16#98DFB5AC#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#2FFD72DB#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(11), 16#D01ADFB7#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 5), 16#B8E1AFED#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 9), 16#6A267E96#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(31), 16#BA7C9045#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 4), 16#F12C7F99#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#24A19947#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(10), 16#B3916CF7#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#0801F2E2#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(25), 16#858EFC16#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(30), 16#636920D8#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 7), 16#71574E69#);

      FF_2(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#A458FEA3#);
      FF_2(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(13), 16#F4933D7E#);
      FF_2(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(16), 16#0D95748F#);
      FF_2(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(14), 16#728EB658#);
      FF_2(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 3), 16#718BCD58#);
      FF_2(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(26), 16#82154AEE#);
      FF_2(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(32), 16#7B54A41D#);
      FF_2(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#C25A59B5#);

      --|   Pass 3

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#9C30D539#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10), 16#2AF26013#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 5), 16#C5D1B023#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(21), 16#286085F0#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(29), 16#CA417918#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(18), 16#B8DB38EF#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 9), 16#8E79DCB0#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(23), 16#603A180E#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(30), 16#6C9E0E8B#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(15), 16#B01E8A3E#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(26), 16#D71577C1#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(13), 16#BD314B27#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(25), 16#78AF2FDA#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(31), 16#55605C60#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(17), 16#E65525F3#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(27), 16#AA55AB94#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(32), 16#57489862#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(16), 16#63E81440#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 8), 16#55CA396A#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 4), 16#2AAB10B6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 2), 16#B4CC5C34#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 1), 16#1141E8CE#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(19), 16#A15486AF#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(28), 16#7C72E993#);

      FF_3(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(14), 16#B3EE1411#);
      FF_3(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 7), 16#636FBC2A#);
      FF_3(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#2BA9C55D#);
      FF_3(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(11), 16#741831F6#);
      FF_3(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(24), 16#CE5C3E16#);
      FF_3(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(12), 16#9B87931E#);
      FF_3(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X( 6), 16#AFD6BA33#);
      FF_3(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 3), 16#6C24CF5C#);

      --|   Pass 4

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(25), 16#7A325381#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 5), 16#28958677#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 1), 16#3B8F4898#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(15), 16#6B4BB9AF#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 3), 16#C4BFE81B#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 8), 16#66282193#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(29), 16#61D809CC#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(24), 16#FB21A991#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(27), 16#487CAC60#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 7), 16#5DEC8032#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(31), 16#EF845D5D#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(21), 16#E98575B1#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(19), 16#DC262302#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(26), 16#EB651B88#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(20), 16#23893E81#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X( 4), 16#D396ACC5#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(23), 16#0F6D6FF3#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(12), 16#83F44239#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(32), 16#2E0B4482#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(22), 16#A4842004#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 9), 16#69C8F04A#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(28), 16#9E1F9B5E#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(13), 16#21C66842#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(10), 16#F6E96C9A#);

      FF_4(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 2), 16#670C9C61#);
      FF_4(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(30), 16#ABD388F0#);
      FF_4(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X( 6), 16#6A51A0D2#);
      FF_4(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(16), 16#D8542F68#);
      FF_4(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#960FA728#);
      FF_4(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(11), 16#AB5133A3#);
      FF_4(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(17), 16#6EEF0B6C#);
      FF_4(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(14), 16#137A3BE4#);

      --|   Pass 5

      FF_5(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(28), 16#BA3BF050#);
      FF_5(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 4), 16#7EFB2A98#);
      FF_5(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(22), 16#A1F1651D#);
      FF_5(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(27), 16#39AF0176#);
      FF_5(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(18), 16#66CA593E#);
      FF_5(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X(12), 16#82430E88#);
      FF_5(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(21), 16#8CEE8619#);
      FF_5(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(30), 16#456F9FB4#);

      FF_5(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X(20), 16#7D84A5C3#);
      FF_5(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X( 1), 16#3B8B5EBE#);
      FF_5(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(13), 16#E06F75D8#);
      FF_5(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X( 8), 16#85C12073#);
      FF_5(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(14), 16#401A449F#);
      FF_5(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 9), 16#56C16AA6#);
      FF_5(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(32), 16#4ED3AA62#);
      FF_5(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(11), 16#363F7706#);

      FF_5(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 6), 16#1BFEDF72#);
      FF_5(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(10), 16#429B023D#);
      FF_5(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(15), 16#37D0D724#);
      FF_5(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(31), 16#D00A1248#);
      FF_5(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X(19), 16#DB0FEAD3#);
      FF_5(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 7), 16#49F1C09B#);
      FF_5(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(29), 16#075372C9#);
      FF_5(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(25), 16#80991B7B#);

      FF_5(T(8), T(7), T(6), T(5), T(4), T(3), T(2), T(1), X( 3), 16#25D479D8#);
      FF_5(T(7), T(6), T(5), T(4), T(3), T(2), T(1), T(8), X(24), 16#F6E8DEF7#);
      FF_5(T(6), T(5), T(4), T(3), T(2), T(1), T(8), T(7), X(17), 16#E3FE501A#);
      FF_5(T(5), T(4), T(3), T(2), T(1), T(8), T(7), T(6), X(23), 16#B6794C3B#);
      FF_5(T(4), T(3), T(2), T(1), T(8), T(7), T(6), T(5), X( 5), 16#976CE0BD#);
      FF_5(T(3), T(2), T(1), T(8), T(7), T(6), T(5), T(4), X( 2), 16#04C006BA#);
      FF_5(T(2), T(1), T(8), T(7), T(6), T(5), T(4), T(3), X(26), 16#C1A94FB6#);
      FF_5(T(1), T(8), T(7), T(6), T(5), T(4), T(3), T(2), X(16), 16#409F60C4#);

      --|   Update state registers.

      for I in E'Range loop
         E(I) := E(I) + T(I);
      end loop;

      --|   Zeroize sensitive information.

      T := (others => 0);
      X := (others => 0);
   end Transform_5;

   --+---[Pack_Block]---------------------------------------------------

   function    Pack_Block(
                  B              : in     HAVAL_Block)
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
      R              : Byte_Array(1 .. Max_HAVAL_Digest_Bytes) :=
                           (others => 0);
      J              : Positive := R'First;
   begin
      for I in S'Range loop
         R(J .. J + 3) := To_Byte_Array(S(I), Big_Endian);
         J := J + 4;
      end loop;

      return R;
   end Unpack_State;

   --+---[Tailor]-------------------------------------------------------

   procedure   Tailor(
                  Context        : access HAVAL_Context)
   is
      T              : Four_Bytes;
   begin
      case Context.all.Digest_Size is
         when HAVAL_128 =>
            T :=
               (Context.all.State(8) and 16#0000_00FF#)   or
               (Context.all.State(7) and 16#FF00_0000#)   or
               (Context.all.State(6) and 16#00FF_0000#)   or
               (Context.all.State(5) and 16#0000_FF00#);
            Context.all.State(1) :=
               Context.all.State(1) + Rotate_Right(T, 8);

            T :=
               (Context.all.State(8) and 16#0000_FF00#)   or
               (Context.all.State(7) and 16#0000_00FF#)   or
               (Context.all.State(6) and 16#FF00_0000#)   or
               (Context.all.State(5) and 16#00FF_0000#);
            Context.all.State(2) :=
               Context.all.State(2) + Rotate_Right(T, 16);


            T :=
               (Context.all.State(8) and 16#00FF_0000#)   or
               (Context.all.State(7) and 16#0000_FF00#)   or
               (Context.all.State(6) and 16#0000_00FF#)   or
               (Context.all.State(5) and 16#FF00_0000#);
            Context.all.State(3) :=
               Context.all.State(3) + Rotate_Right(T, 24);


            T :=
               (Context.all.State(8) and 16#FF00_0000#)   or
               (Context.all.State(7) and 16#00FF_0000#)   or
               (Context.all.State(6) and 16#0000_FF00#)   or
               (Context.all.State(5) and 16#0000_00FF#);
            Context.all.State(4) :=
               Context.all.State(4) + T;

         when HAVAL_160 =>
            T :=
               (Context.all.State(8) and Shift_Left(16#0000_003F#,  0))   or
               (Context.all.State(7) and Shift_Left(16#0000_007F#, 25))   or
               (Context.all.State(6) and Shift_Left(16#0000_003F#, 19));
            Context.all.State(1) :=
               Context.all.State(1) + Rotate_Right(T, 19);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_003F#,  6))   or
               (Context.all.State(7) and Shift_Left(16#0000_003F#,  0))   or
               (Context.all.State(6) and Shift_Left(16#0000_007F#, 25));
            Context.all.State(2) :=
               Context.all.State(2) + Rotate_Right(T, 25);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_007F#, 12))   or
               (Context.all.State(7) and Shift_Left(16#0000_003F#,  6))   or
               (Context.all.State(6) and Shift_Left(16#0000_003F#,  0));
            Context.all.State(3) :=
               Context.all.State(3) + T;

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_003F#, 19))   or
               (Context.all.State(7) and Shift_Left(16#0000_007F#, 12))   or
               (Context.all.State(6) and Shift_Left(16#0000_003F#,  6));
            Context.all.State(4) :=
               Context.all.State(4) + Shift_Right(T, 6);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_007F#, 25))   or
               (Context.all.State(7) and Shift_Left(16#0000_003F#, 19))   or
               (Context.all.State(6) and Shift_Left(16#0000_007F#, 12));
            Context.all.State(5) :=
               Context.all.State(5) + Shift_Right(T, 12);

         when HAVAL_192 =>
            T :=
               (Context.all.State(8) and Shift_Left(16#0000_001F#,  0))   or
               (Context.all.State(7) and Shift_Left(16#0000_003F#, 26));
            Context.all.State(1) :=
               Context.all.State(1) + Rotate_Right(T, 26);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_001F#,  5))   or
               (Context.all.State(7) and Shift_Left(16#0000_001F#,  0));
            Context.all.State(2) :=
               Context.all.State(2) + T;

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_003F#, 10))   or
               (Context.all.State(7) and Shift_Left(16#0000_001F#,  5));
            Context.all.State(3) :=
               Context.all.State(3) + Shift_Right(T, 5);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_001F#, 16))   or
               (Context.all.State(7) and Shift_Left(16#0000_003F#, 10));
            Context.all.State(4) :=
               Context.all.State(4) + Shift_Right(T, 10);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_001F#, 21))   or
               (Context.all.State(7) and Shift_Left(16#0000_001F#, 16));
            Context.all.State(5) :=
               Context.all.State(5) + Shift_Right(T, 16);

            T :=
               (Context.all.State(8) and Shift_Left(16#0000_003F#, 26))   or
               (Context.all.State(7) and Shift_Left(16#0000_001F#, 21));
            Context.all.State(6) :=
               Context.all.State(6) + Shift_Right(T, 21);

         when HAVAL_224 =>
            Context.all.State(1) :=
               Context.all.State(1) +
               (Shift_Right(Context.all.State(8), 27) and 16#0000_001F#);
            Context.all.State(2) :=
               Context.all.State(2) +
               (Shift_Right(Context.all.State(8), 22) and 16#0000_001F#);
            Context.all.State(3) :=
               Context.all.State(3) +
               (Shift_Right(Context.all.State(8), 18) and 16#0000_000F#);
            Context.all.State(4) :=
               Context.all.State(4) +
               (Shift_Right(Context.all.State(8), 13) and 16#0000_001F#);
            Context.all.State(5) :=
               Context.all.State(5) +
               (Shift_Right(Context.all.State(8),  9) and 16#0000_000F#);
            Context.all.State(6) :=
               Context.all.State(6) +
               (Shift_Right(Context.all.State(8),  4) and 16#0000_001F#);
            Context.all.State(7) :=
               Context.all.State(7) +
               (Context.all.State(8) and 16#0000_000F#);
         when others =>
            null;
      end case;
   end Tailor;

   ---------------------------------------------------------------------
   -- Specification declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------

   function    Allocate_Context
      return   HAVAL_Context_Ptr
   is
   begin
      return new HAVAL_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context        : in out HAVAL_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access HAVAL_Context)
   is
   begin
      Context.all.Passes      := 3;
      Context.all.Digest_Size := HAVAL_128;
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access HAVAL_Context;
                  Passes         : in     HAVAL_Passes;
                  Digest_Size    : in     HAVAL_Digest_Size)
   is
   begin
      Context.all.Passes      := Passes;
      Context.all.Digest_Size := Digest_Size;
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access HAVAL_Context;
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
                      Eight_Bytes(HAVAL_Block_Bytes));

         --|   Increment bit count.

         Context.all.Bit_Count :=
            Context.all.Bit_Count + Shift_Left(Eight_Bytes(L), 3);

         --|   Compute the number of free slots in context block.

         R := 1 + HAVAL_Block_Bytes - I;

         J := Bytes'First;

         --|   If the input length is greater than or equal to the
         --|   number of free slots perform the needed
         --|   transformations of input.

         if L >= R then

            --|   Fill context block and transform.

            Context.all.Block(I .. HAVAL_Block_Bytes) :=
               Bytes(J .. J + R - 1);
            Transform(Context);

            --|   Update counters.

            J := J + R;
            L := L - R;

            --|   Transform remaining input bytes in HAVAL_Block_Bytes
            --|   chunks.

            while L >= HAVAL_Block_Bytes loop
               Context.all.Block :=
                  Bytes(J .. J + HAVAL_Block_Bytes - 1);
               Transform(Context);
               J := J + HAVAL_Block_Bytes;
               L := L - HAVAL_Block_Bytes;
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
                  Context        : access HAVAL_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      Tail           : Byte_Array(1 .. 10) := (others => 0);
      I              : Positive;
      Tail_Offset    : Positive := 1 + HAVAL_Block_Bytes - 10;
      D_Bytes        : Positive := Positive(HAVAL_Sizes(Context.all.Digest_Size)) / 8;
   begin

      --|   Save the version number, the number of passes, the
      --|   message digest length and the bit counter.

      Tail(1) :=
         Shift_Left(
            Byte(HAVAL_Sizes(Context.all.Digest_Size) and 16#0003#), 6) or
         Shift_Left(Byte(Context.all.Passes) and 16#07#, 3) or
         (Byte(HAVAL_Version) and 16#07#);
      Tail(2) :=
         Lo_Byte(Shift_Right(HAVAL_Sizes(Context.all.Digest_Size), 2));
      Tail(3 .. 10) := To_Byte_Array(Context.all.Bit_Count, Big_Endian);

     --|   Compute start index of context block.

      I := 1 +
           Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                   Eight_Bytes(HAVAL_Block_Bytes));

      --|   Perform pad

      Context.all.Block(I) := 16#80#;
      I := I + 1;

      if I <= HAVAL_Block_Bytes then
         Context.all.Block(I .. HAVAL_Block_Bytes) := (others => 0);
      end if;

      if I > Tail_Offset then
         Transform(Context);
         Context.all.Block := (others => 0);
      end if;

      Context.all.Block(Tail_Offset .. HAVAL_Block_Bytes) := Tail;
      Transform(Context);

      --|   Tailor the last output

      Tailor(Context);

      --|   Get digest from state.

      R := To_Message_Digest(Unpack_State(Context.all.State)(1 .. D_Bytes));

      --|   Clear context.

      Context.all.Passes      := 3;
      Context.all.Digest_Size := HAVAL_128;
      Context.all.Bit_Count   := 0;
      Context.all.State       := (others => 0);
      Context.all.Block       := (others => 0);

      --|   Return computed digest.

      return R;
   end Hash_End;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out HAVAL_Context)
   is
   begin
      Object.Algo_Id       := ACF.Hash.HAVAL;
      Object.Passes        := 3;
      Object.Digest_Size   := HAVAL_128;
      Object.Bit_Count     := 0;
      Object.State         := Initial_State;
      Object.Block         := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out HAVAL_Context)
   is
   begin
      Object.Algo_Id       := ACF.Hash.HAVAL;
      Object.Passes        := 3;
      Object.Digest_Size   := HAVAL_128;
      Object.Bit_Count     := 0;
      Object.State         := Initial_State;
      Object.Block         := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.HAVAL;
