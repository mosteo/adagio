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
--    File name         : acf-hash-algorithms-md4.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RSA-MD4 message digest algorithm.
--
--    The MD4 message digest algorithm was developed by RSA Data
--    Security Inc., and is described in RFC 1320. According to that
--    document:
--
--       "The algorithm takes as input a message of arbitrary length
--       and produces as output a 128-bit "fingerprint" or "message
--       digest" of the input. It is conjectured that is computationally
--       infeasible to produce two messages having the same message
--       message digest, or to produce any message having a given
--       prespecified target message digest. The MD4 algorithm is
--       intended for digital signature applications, where a large
--       file must be "compressed" in a secure manner before being
--       signed with a private (secret) key under a public-key
--       cryptosystem such as RSA."
--
--    Although MD4 is now considered insecure, its design is the basis
--    for the design of most other cryptographic hashes and therefore
--    merits description. First, the message to be operated on is padded
--    so that its length in bits plus 448 is divisible by 512. Then, in
--    what is called a Damgard/Merkle iterative structure, the message
--    is processed with a compression function in 512-bit blocks to
--    generate a digest value that is 128 bits (16 bytes) long.
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

package ACF.Hash.Algorithms.MD4 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD4_Context]--------------------------------------------------
   --|   MD4 message digest algorithm context type.
   --+------------------------------------------------------------------

   type MD4_Context is new Algorithm_Context with private;

   --+---[MD4_Context_Ptr]----------------------------------------------
   --|   Access type to MD4_Context objects.
   --+------------------------------------------------------------------

   type MD4_Context_Ptr is access all MD4_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating MD4_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a MD4_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   MD4_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   MD4_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated MD4_Context object.
   --|
   --|   Arguments:
   --|   Context           MD4_Context_Ptr that references the object to
   --|                     deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out MD4_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts MD4 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the MD4_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access MD4_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes MD4 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the MD4_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access MD4_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends MD4 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the MD4_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access MD4_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[MD4_Digest_Bytes]---------------------------------------------
   --|   Constant that defines the size in bytes of MD4 message digests.
   --+------------------------------------------------------------------

   MD4_Digest_Bytes              : constant Positive := 16;

   --+---[MD4_Block_Bytes]----------------------------------------------
   --|   Constant that defines the size in bytes of MD4 processing
   --|   blocks.
   --+------------------------------------------------------------------

   MD4_Block_Bytes               : constant Positive := 64;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD4_Block]----------------------------------------------------
   --|   Type for handling MD4 input blocks.
   --+------------------------------------------------------------------

   subtype MD4_Block is Byte_Array(1 .. MD4_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling MD4 state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 4);

   --+---[MD4_Context]--------------------------------------------------
   --|   MD4 computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          MD4 state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type MD4_Context is new Algorithm_Context with
      record
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : MD4_Block       := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes MD4 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD4_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out MD4_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes MD4 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD4_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out MD4_Context);

end ACF.Hash.Algorithms.MD4;
