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
--    File name         : acf-hash-algorithms-haval.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : December 4th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Implements the HAVAL message digest algorithm version 1.
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

package ACF.Hash.Algorithms.HAVAL is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[HAVAL_Context]------------------------------------------------
   --|   HAVAL message digest algorithm context type.
   --+------------------------------------------------------------------

   type HAVAL_Context is new Algorithm_Context with private;

   --+---[HAVAL_Context_Ptr]--------------------------------------------
   --|   Access type to HAVAL_Context objects.
   --+------------------------------------------------------------------

   type HAVAL_Context_Ptr is access all HAVAL_Context;

   --+---[HAVAL_Passes]-------------------------------------------------
   --|   Type for handling the number of passes the algorithm performs.
   --+------------------------------------------------------------------

   subtype HAVAL_Passes is Positive range 3 .. 5;

   --+---[HAVAL_Digest_Size]--------------------------------------------
   --|   Enumerated type that identifies the generated HAVAL digest
   --|   size. Possible values are:
   --|
   --|   HAVAL_128            128-bit digest size.
   --|   HAVAL_160            160-bit digest size.
   --|   HAVAL_192            192-bit digest size.
   --|   HAVAL_224            224-bit digest size.
   --|   HAVAL_256            256-bit digest size.
   --+------------------------------------------------------------------

   type HAVAL_Digest_Size is
      (
         HAVAL_128,
         HAVAL_160,
         HAVAL_192,
         HAVAL_224,
         HAVAL_256
      );

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating HAVAL_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a HAVAL_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   HAVAL_Context_Ptr that references the newly allocated object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   HAVAL_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated HAVAL_Context object.
   --|
   --|   Arguments:
   --|   Context           HAVAL_Context_Ptr that references the object
   --|                     to deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out HAVAL_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts HAVAL computation by initializing the context. This
   --|   procedure sets up computation using 3 passes and 128-bit
   --|   message digests.
   --|
   --|   Arguments:
   --|   Context           Access to the HAVAL_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access HAVAL_Context);

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts HAVAL computation by initializing the context. This
   --|   procedure lets to choose the number of passes and the size of
   --|   the generated digest.
   --|
   --|   Arguments:
   --|   Context           Access to the HAVAL_Context object to set
   --|                     up for computation.
   --|   Passes            HAVAL_Passes that specifies the number of
   --|                     passes to perform.
   --|   Digest_Size       HAVAL_Digest_Size value that set the size
   --|                     of the digest to generate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access HAVAL_Context;
                  Passes         : in     HAVAL_Passes;
                  Digest_Size    : in     HAVAL_Digest_Size);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes HAVAL over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the HAVAL_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access HAVAL_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends HAVAL computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the HAVAL_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access HAVAL_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[HAVAL_Block_Bytes]--------------------------------------------
   --|   Constant that defines the size in bytes of HAVAL processing
   --|   blocks.
   --+------------------------------------------------------------------

   HAVAL_Block_Bytes             : constant Positive := 128;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[HAVAL_Block]--------------------------------------------------
   --|   Type for handling HAVAL input blocks.
   --+------------------------------------------------------------------

   subtype HAVAL_Block is Byte_Array(1 .. HAVAL_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling HAVAL state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 8);

   --+---[HAVAL_Context]------------------------------------------------
   --|   HAVAL computation context. The extension part has the
   --|   following fields:
   --|
   --+------------------------------------------------------------------

   type HAVAL_Context is new Algorithm_Context with
      record
         Passes                  : HAVAL_Passes       := 3;
         Digest_Size             : HAVAL_Digest_Size  := HAVAL_128;
         Bit_Count               : Eight_Bytes        := 0;
         State                   : State_Registers    := (others => 0);
         Block                   : HAVAL_Block        := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes HAVAL digest context objects.
   --|
   --|   Arguments:
   --|   Object            HAVAL_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out HAVAL_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes HAVAL digest context objects.
   --|
   --|   Arguments:
   --|   Object            HAVAL_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out HAVAL_Context);

end ACF.Hash.Algorithms.HAVAL;
