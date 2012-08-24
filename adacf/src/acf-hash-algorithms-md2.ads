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
--    File name         : acf-hash-algorithms-md2.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RSA-MD2 message digest algorithm.
--
--    The MD2 message digest algorithm was developed by RSA Data
--    Security Inc., and is described in RFC 1319. According to that
--    document:
--
--       "The algorithm takes as input a message of arbitrary length
--       and produces as output a 128-bit "fingerprint" or "message
--       digest" of the input. It is conjectured that is computationally
--       infeasible to produce two messages having the same message
--       message digest, or to produce any message having a given
--       prespecified target message digest. The MD2 algorithm is
--       intended for digital signature applications, where a large
--       file must be "compressed" in a secure manner before being
--       signed with a private (secret) key under a public-key
--       cryptosystem such as RSA."
--
--    MD2 is generally considered to be a dead algorithm. It was
--    designed to work in 8-bit processors and, in today's 32-bit world
--    is rarely used. It produces a 128-bit digest. MD2 is different in
--    design from MD4 and MD5, in that it first pads the message so
--    that its length in bits is divisible by 256. It then adds a
--    256-bit checksum. If this checksum is not added, the MD2 function
--    has been found to have collisions. There are no known attacks on
--    the full version of MD2.
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

package ACF.Hash.Algorithms.MD2 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD2_Context]--------------------------------------------------
   --|   MD2 message digest algorithm context type.
   --+------------------------------------------------------------------

   type MD2_Context is new Algorithm_Context with private;

   --+---[MD2_Context_Ptr]----------------------------------------------
   --|   Access type to MD2_Context objects.
   --+------------------------------------------------------------------

   type MD2_Context_Ptr is access all MD2_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating MD4_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a MD2_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   MD2_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   MD2_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated MD2_Context object.
   --|
   --|   Arguments:
   --|   Context           MD2_Context_Ptr that references the object to
   --|                     deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out MD2_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts MD2 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the MD2_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access MD2_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes MD2 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the MD2_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access MD2_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends MD2 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the MD2_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access MD2_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[MD2_Digest_Bytes]---------------------------------------------
   --|   Constant that defines the size in bytes of MD2 message digests.
   --+------------------------------------------------------------------

   MD2_Digest_Bytes              : constant Positive := 16;

   --+---[MD2_Block_Bytes]----------------------------------------------
   --|   Constant that defines the size in bytes of MD2 processing
   --|   blocks.
   --+------------------------------------------------------------------

   MD2_Block_Bytes               : constant Positive := 16;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[MD2_Block]----------------------------------------------------
   --|   Type for handling MD2 input blocks.
   --+------------------------------------------------------------------

   subtype MD2_Block is Byte_Array(1 .. MD2_Block_Bytes);

   --+---[MD2_Block_Index]----------------------------------------------
   --|   Type for indexing bytes within MD2_Blocks
   --+------------------------------------------------------------------

   subtype MD2_Block_Index is Natural range 0 .. MD2_Block_Bytes;

   --+---[MD2_Context]--------------------------------------------------
   --|   MD2 computation context. The extension part has the
   --|   following fields:
   --|
   --|   State          State register.
   --|   Checksum       Obvious
   --|   Index          Index of the last character mantained in the
   --|                  internal block.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type MD2_Context is new Algorithm_Context with
      record
         State                   : MD2_Block := (others => 0);
         Checksum                : MD2_Block := (others => 0);
         Index                   : MD2_Block_Index := 0;
         Block                   : MD2_Block := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes MD2 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD2_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out MD2_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes MD2 digest context objects.
   --|
   --|   Arguments:
   --|   Object            MD2_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out MD2_Context);

end ACF.Hash.Algorithms.MD2;
