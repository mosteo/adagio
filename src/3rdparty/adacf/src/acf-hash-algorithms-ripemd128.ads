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
--    File name         : acf-hash-algorithms-ripemd128.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 26th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RIPEMD-128 message digest algorithm.
--
--    RIPEMD and its successors were developed by the European RIPE
--    project. Its authors found collisions for a version of RIPEMD
--    restricted to two rounds. This attack can also be applied to MD4
--    and MD5. The original RIPEMD algorithm was then strengthened and
--    renamed to RIPEMD-160.
--
--    This package implements the original version of RIPEMD this
--    algorithm produces a 128-bit (16-byte) message digest.
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

package ACF.Hash.Algorithms.RIPEMD128 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD128_Context]--------------------------------------------
   --|   RIPEMD-128 message digest algorithm context type.
   --+------------------------------------------------------------------

   type RIPEMD128_Context is new Algorithm_Context with private;

   --+---[RIPEMD128_Context_Ptr]----------------------------------------
   --|   Access type to RIPEMD128_Context objects.
   --+------------------------------------------------------------------

   type RIPEMD128_Context_Ptr is access all RIPEMD128_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating RIPEMD128_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a RIPEMD128_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   RIPEMD128_Context_Ptr that references the newly allocated
   --|   object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   RIPEMD128_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated RIPEMD128_Context object.
   --|
   --|   Arguments:
   --|   Context           RIPEMD128_Context_Ptr that references the
   --|                     object to deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out RIPEMD128_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts RIPEMD-128 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD128_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access RIPEMD128_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes RIPEMD-128 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD128_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access RIPEMD128_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends RIPEMD-128 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD128_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access RIPEMD128_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD128_Digest_Bytes]---------------------------------------
   --|   Constant that defines the size in bytes of RIPEMD-128 message
   --|   digests.
   --+------------------------------------------------------------------

   RIPEMD128_Digest_Bytes        : constant Positive := 16;

   --+---[RIPEMD128_Block_Bytes]----------------------------------------
   --|   Constant that defines the size in bytes of RIPEMD-128
   --|   processing blocks.
   --+------------------------------------------------------------------

   RIPEMD128_Block_Bytes         : constant Positive := 64;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD128_Block]----------------------------------------------
   --|   Type for handling RIPEMD-128 input blocks.
   --+------------------------------------------------------------------

   subtype RIPEMD128_Block is Byte_Array(1 .. RIPEMD128_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling RIPEMD-128 state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 4);

   --+---[RIPEMD128_Context]--------------------------------------------
   --|   RIPEMD-128 computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          RIPEMD-128 state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type RIPEMD128_Context is new Algorithm_Context with
      record
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : RIPEMD128_Block := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes RIPEMD-128 digest context objects.
   --|
   --|   Arguments:
   --|   Object            RIPEMD128_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out RIPEMD128_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes RIPEMD-128 digest context objects.
   --|
   --|   Arguments:
   --|   Object            RIPEMD128_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out RIPEMD128_Context);

end ACF.Hash.Algorithms.RIPEMD128;
