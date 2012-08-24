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
--    File name         : acf-hash-algorithms-tiger.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 27th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the TIGER message digest algorithm.
--
--    Tiger is a new hash algorithm developed by Ross Anderson and Eli
--    Biham. It is designed to work with 64-bit processors such as the
--    Digital Alpha and, unlike MD4, does not rely on rotations. In order
--    to provide drop-in compatibility with other hashes, Tiger can
--    generate a 128-bit, a 160-bit or a 192-bit digest.
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

package ACF.Hash.Algorithms.TIGER is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[TIGER_Context]------------------------------------------------
   --|   TIGER message digest algorithm context type.
   --+------------------------------------------------------------------

   type TIGER_Context is new Algorithm_Context with private;

   --+---[TIGER_Context_Ptr]--------------------------------------------
   --|   Access type to TIGER_Context objects.
   --+------------------------------------------------------------------

   type TIGER_Context_Ptr is access all TIGER_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating TIGER_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a TIGER_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   TIGER_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   TIGER_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated TIGER_Context object.
   --|
   --|   Arguments:
   --|   Context           TIGER_Context_Ptr that references the object
   --|                     to deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out TIGER_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts TIGER computation by initializing the context
   --|   object.
   --|
   --|   Arguments:
   --|   Context           Access to TIGER_Context to set up for
   --|                     computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access TIGER_Context);

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts TIGER computation by initializing the context
   --|   object.
   --|
   --|   Arguments:
   --|   Context           Access to TIGER_Context to set up for
   --|                     computation.
   --|   Passes            Positive value that sets the number of
   --|                     passes the algorithm has to perform. The
   --|                     minimum number of passes is 3 if the value
   --|                     of this argument is less than the minimum
   --|                     that minimum value is used instead.
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access TIGER_Context;
                  Passes         : in     Positive);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes TIGER over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the TIGER_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access TIGER_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends TIGER computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the TIGER_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access TIGER_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[TIGER_Digest_Bytes]-------------------------------------------
   --|   Constant that defines the size in bytes of TIGER message
   --|   digests.
   --+------------------------------------------------------------------

   TIGER_Digest_Bytes            : constant Positive := 24;

   --+---[TIGER_Block_Bytes]--------------------------------------------
   --|   Constant that defines the size in bytes of TIGER
   --|   processing blocks.
   --+------------------------------------------------------------------

   TIGER_Block_Bytes             : constant Positive := 64;

   --+---[Min_Passes]---------------------------------------------------
   --|   Constant that defines the minimum number of passes for
   --|   message digest computation.
   --+------------------------------------------------------------------

   Min_Passes                    : constant Positive := 3;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[TIGER_Block]--------------------------------------------------
   --|   Type for handling TIGER input blocks.
   --+------------------------------------------------------------------

   subtype TIGER_Block is Byte_Array(1 .. TIGER_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling TIGER state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Eight_Bytes_Array(1 .. 3);

   --+---[TIGER_Context]------------------------------------------------
   --|   TIGER computation context. The extension part has the
   --|   following fields:
   --|
   --|   Passes         Number of passes.
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          TIGER state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type TIGER_Context is new Algorithm_Context with
      record
         Passes                  : Positive        := Min_Passes;
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : TIGER_Block     := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes TIGER digest context objects.
   --|
   --|   Arguments:
   --|   Object            TIGER_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out TIGER_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes TIGER digest context objects.
   --|
   --|   Arguments:
   --|   Object            TIGER_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out TIGER_Context);

end ACF.Hash.Algorithms.TIGER;
