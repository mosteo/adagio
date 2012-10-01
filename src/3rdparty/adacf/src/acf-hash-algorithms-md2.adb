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
--    File name         : acf-hash-algorithms-md2.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RSA-MD2 message digest algorithm.
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
-- 1.0   ADD   11222001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.MD2 is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              MD2_Context,
                              MD2_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[Pi_Subst]-----------------------------------------------------
   --|   Provides a permutation of 0 .. 255 constructed from the digits
   --|   of PI. It gives a "random" nonlinear byte substitution
   --|   operation.
   --+------------------------------------------------------------------

   Pi_Subst                      : constant array(Byte) of Byte :=
      (
          41,   46,   67,  201,  162,  216,  124,    1,
          61,   54,   84,  161,  236,  240,    6,   19,

          98,  167,    5,  243,  192,  199,  115,  140,
         152,  147,   43,  217,  188,   76,  130,  202,

          30,  155,   87,   60,  253,  212,  224,   22,
         103,   66,  111,   24,  138,   23,  229,   18,

         190,   78,  196,  214,  218,  158,  222,   73,
         160,  251,  245,  142,  187,   47,  238,  122,

         169,  104,  121,  145,   21,  178,    7,   63,
         148,  194,   16,  137,   11,   34,   95,   33,

         128,  127,   93,  154,   90,  144,   50,   39,
          53,   62,  204,  231,  191,  247,  151,    3,

         255,   25,   48,  179,   72,  165,  181,  209,
         215,   94,  146,   42,  172,   86,  170,  198,

          79,  184,   56,  210,  150,  164,  125,  182,
         118,  252,  107,  226,  156,  116,    4,  241,

          69,  157,  112,   89,  100,  113,  135,   32,
         134,   91,  207,  101,  230,   45,  168,    2,

          27,   96,   37,  173,  174,  176,  185,  246,
          28,   70,   97,  105,   52,   64,  126,   15,

          85,   71,  163,   35,  221,   81,  175,   58,
         195,   92,  249,  206,  186,  197,  234,   38,

          44,   83,   13,  110,  133,   40,  132,    9,
         211,  223,  205,  244,   65,  129,   77,   82,

         106,  220,   55,  200,  108,  193,  171,  250,
          36,  225,  123,    8,   12,  189,  177,   74,

         120,  136,  149,  139,  227,   99,  232,  109,
         233,  203,  213,  254,   59,    0,   29,   57,

         242,  239,  183,   14,  102,   88,  208,  228,
         166,  119,  114,  248,  235,  117,   75,   10,

          49,   68,   80,  180,  143,  237,   31,   26,
         219,  153,  141,   51,  159,   17,  131,   20
      );

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Transform]----------------------------------------------------
   --|
   --|   Purpose:
   --|   Transforms MD2 state based on the input block.
   --|
   --|   Arguments:
   --|   Context           MD2_Context object.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access MD2_Context);

   ---------------------------------------------------------------------
   -- Body subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------

   function    Allocate_Context
      return   MD2_Context_Ptr
   is
   begin
      return new MD2_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out MD2_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access MD2_Context)
   is
      X              : Byte_Array(1 .. 48) := (others => 0);
      T              : Byte := 0;
   begin

      --|   Build encryption block from State, Block and State xor
      --|   Block.

      X(1 .. 16)  := Context.all.State;
      X(17 .. 32) := Context.all.Block;

      for I in 1 .. 16 loop
         X(I + 32)   := X(I) xor X(I + 16);
      end loop;

      --|   Encrypt block (18 rounds)

      for I in 0 .. 17 loop
         for J in X'Range loop
            X(J) := X(J) xor Pi_Subst(T);
            T := X(J);
         end loop;

         T := T + Byte(I);
      end loop;

      --|   Save new State

      Context.all.State := X(1 .. MD2_Block_Bytes);

      --|   Update checksum.

      T := Context.all.Checksum(MD2_Block_Bytes);

      for I in 1 .. MD2_Block_Bytes loop
         Context.all.Checksum(I) :=
            Context.all.Checksum(I) xor
            Pi_Subst(Context.all.Block(I) xor T);
         T := Context.all.Checksum(I);
      end loop;

      --|   Clear intermediate values.

      X := (others => 0);
   end Transform;

   ---------------------------------------------------------------------
   -- Specification declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------

   procedure  Hash_Start(
                  Context        : access MD2_Context)
   is
   begin
      Context.all.State    := (others => 0);
      Context.all.Checksum := (others => 0);
      Context.all.Index    := 0;
      Context.all.Block    := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access MD2_Context;
                  Bytes          : in     Byte_Array)
   is
      I              : Natural;
      J              : Natural;
      L              : Natural;
      R              : Natural;
   begin

      --|   If there is any input, process it.

      if Bytes'Length > 0 then

         --|   Initialize locals:
         --|   I  => Index of the last position occupied in context
         --|         internal block.
         --|   J  => Index of next input byte to process.
         --|   R  => Number of bytes in input that need to be processed.
         --|   L  => Number of free slots in context block.

         I     := Context.all.Index;
         J     := Bytes'First;
         R     := Bytes'Length;
         L     := MD2_Block_Bytes - I;

         --|   Update index.

         Context.all.Index := ((I + R) mod MD2_Block_Bytes);

         --|   If input contains more bytes than those that could be
         --|   stored in context block perform transformation.

         if R >= L then

            --|   Fill context buffer and transform.

            Context.all.Block(I + 1 .. MD2_Block_Bytes) :=
               Bytes(J .. J + L - 1);
            Transform(Context);
            J := J + L;
            R := R - L;

            --|   Process remaining input bytes in MD2_Block_Bytes
            --|   chunks.

            while R >= MD2_Block_Bytes loop
               Context.all.Block :=
                  Bytes(J .. J + MD2_Block_Bytes - 1);
               J := J + MD2_Block_Bytes;
               R := R - MD2_Block_Bytes;
               Transform(Context);
            end loop;

            I := 0;
         end if;

         --|   Copy remaining input bytes to context block.

         if Context.all.Index > 0 then
            Context.all.Block(I + 1 .. Context.all.Index) :=
               Bytes(J .. Bytes'Last);
         end if;
      end if;
   end Hash_Update;

   --+---[Hash_End]-----------------------------------------------------

   function    Hash_End(
                  Context        : access MD2_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      Pad_Len        : Natural;
      Pad_Byte       : Byte;
   begin

      --|   Perform padding and transform.

      Pad_Len  := MD2_Block_Bytes - Context.all.Index;
      Pad_Byte := Byte(Pad_Len);

      if Pad_Len > 0 then
         Context.all.Block(Context.all.Index + 1 .. MD2_Block_Bytes) :=
            (others => Pad_Byte);
         Transform(Context);
      end if;

      --|   Extend with checksum.

      Context.all.Block := Context.all.Checksum;
      Transform(Context);

      --|   Set digest.

      R := To_Message_Digest(Context.all.State);

      --|   Zeroize context.

      Context.State     := (others => 0);
      Context.Checksum  := (others => 0);
      Context.Index     := 0;
      Context.Block     := (others => 0);

      --|   Return digest.

      return R;
   end Hash_End;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out MD2_Context)
   is
   begin
      Object.Algo_Id    := ACF.Hash.MD2;
      Object.State      := (others => 0);
      Object.Checksum   := (others => 0);
      Object.Index      := 0;
      Object.Block      := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out MD2_Context)
   is
   begin
      Object.State      := (others => 0);
      Object.Checksum   := (others => 0);
      Object.Index      := 0;
      Object.Block      := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.MD2;
