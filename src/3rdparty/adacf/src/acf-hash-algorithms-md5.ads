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
--    File name         : acf-hash-algorithms-md5.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 25th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RSA-MD5 message digest algorithm.
--
--    The MD5 message digest algorithm was developed by RSA Data
--    Security Inc., and is described in RFC 1321. According to that
--    document:
--
--       "The algorithm takes as input a message of arbitrary length
--       and produces as output a 128-bit "fingerprint" or "message
--       digest" of the input. It is conjectured that is computationally
--       infeasible to produce two messages having the same message
--       message digest, or to produce any message having a given
--       prespecified target message digest. The MD5 algorithm is
--       intended for digital signature applications, where a large
--       file must be "compressed" in a secure manner before being
--       signed with a private (secret) key under a public-key
--       cryptosystem such as RSA."
--
--    While MD4 was designed for speed, a more conservative approach
--    was taken in the design of MD5. However, applying the same
--    techniques he used to attack MD4, Hans Dobbertin has shown that
--    collisions can be found for the MD5 compression function in about
--    10 hours on a PC. While these attacks have not been extended to
--    the full MD5 algorithm, they still do not inspire confidence
--    in the algorithm. RSA is quick to point out that these collision
--    attacks do not compromise the integrity of MD5 when used with
--    existing digital signatures. MD5 like MD4 produces a 128-bit
--    (16-bytes) message digest.
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

package ACF.Hash.Algorithms.MD5 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD5_Context]--------------------------------------------------
   --|   MD5 message digest algorithm context type.
   --+------------------------------------------------------------------

   type MD5_Context is new Algorithm_Context with private;

   --+---[MD5_Context_Ptr]----------------------------------------------
   --|   Access type to MD5_Context objects.
   --+------------------------------------------------------------------

   type MD5_Context_Ptr is access all MD5_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating MD5_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a MD5_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   MD5_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   MD5_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated MD5_Context object.
   --|
   --|   Arguments:
   --|   Context           MD5_Context_Ptr that references the object to
   --|                     deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out MD5_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts MD5 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the MD5_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access MD5_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes MD5 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the MD5_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access MD5_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends MD5 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the MD5_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access MD5_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[MD5_Digest_Bytes]---------------------------------------------
   --|   Constant that defines the size in bytes of MD5 message digests.
   --+------------------------------------------------------------------

   MD5_Digest_Bytes              : constant Positive := 16;

   --+---[MD5_Block_Bytes]----------------------------------------------
   --|   Constant that defines the size in bytes of MD5 processing
   --|   blocks.
   --+------------------------------------------------------------------

   MD5_Block_Bytes               : constant Positive := 64;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD5_Block]----------------------------------------------------
   --|   Type for handling MD5 input blocks.
   --+------------------------------------------------------------------

   subtype MD5_Block is Byte_Array(1 .. MD5_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling MD5 state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 4);

   --+---[MD5_Context]--------------------------------------------------
   --|   MD5 computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          MD5 state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type MD5_Context is new Algorithm_Context with
      record
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : MD5_Block       := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes MD5 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD5_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out MD5_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes MD5 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD5_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out MD5_Context);

end ACF.Hash.Algorithms.MD5;
