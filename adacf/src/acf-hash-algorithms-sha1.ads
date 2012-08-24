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
--    File name         : acf-hash-slgorithms-sha1.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 26th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the SHA-1 (Secure Hash Algorithm) message digest
--    algorithm.
--
--    Similar in design to MD4 was developed by the United States
--    Government. Produces 160-bit (20-byte) message digests and
--    its considered quite good.
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

package ACF.Hash.Algorithms.SHA1 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[SHA1_Context]-------------------------------------------------
   --|   SHA1 message digest algorithm context type.
   --+------------------------------------------------------------------

   type SHA1_Context is new Algorithm_Context with private;

   --+---[SHA1_Context_Ptr]----------------------------------------------
   --|   Access type to SHA1_Context objects.
   --+------------------------------------------------------------------

   type SHA1_Context_Ptr is access all SHA1_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating SHA1_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a SHA1_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   SHA1_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   SHA1_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated SHA1_Context object.
   --|
   --|   Arguments:
   --|   Context           SHA1_Context_Ptr that references the object to
   --|                     deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out SHA1_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts SHA1 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the SHA1_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access SHA1_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes SHA1 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the SHA1_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access SHA1_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends SHA1 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the SHA1_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access SHA1_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[SHA1_Digest_Bytes]--------------------------------------------
   --|   Constant that defines the size in bytes of SHA1 message
   --|   digests.
   --+------------------------------------------------------------------

   SHA1_Digest_Bytes             : constant Positive := 20;

   --+---[SHA1_Block_Bytes]----------------------------------------------
   --|   Constant that defines the size in bytes of SHA1 processing
   --|   blocks.
   --+------------------------------------------------------------------

   SHA1_Block_Bytes              : constant Positive := 64;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[SHA1_Block]----------------------------------------------------
   --|   Type for handling SHA1 input blocks.
   --+------------------------------------------------------------------

   subtype SHA1_Block is Byte_Array(1 .. SHA1_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling SHA1 state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 5);

   --+---[SHA1_Context]--------------------------------------------------
   --|   SHA1 computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          SHA1 state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type SHA1_Context is new Algorithm_Context with
      record
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : SHA1_Block      := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes SHA1 digest context objects.
   --|
   --|   Arguments:
   --|   Object            SHA1_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out SHA1_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes SHA1 digest context objects.
   --|
   --|   Arguments:
   --|   Object            SHA1_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out SHA1_Context);

end ACF.Hash.Algorithms.SHA1;
