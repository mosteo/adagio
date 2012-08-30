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
--    File name         : acf-hash-algorithms-ripemd160.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 27th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the RIPEMD-160 message digest algorithm.
--
--    RIPEMD-160 is a 160-bit cryptographic hash function, designed by
--    Hans Dobbertin, Antoon Bosselaers, and  Bart Preneel. It is
--    intended to be used as a secure replacement for the 128-bit hash
--    functions MD4, MD5, and RIPEMD. MD4 and MD5 were developed by Ron
--    Rivest for RSA Data Security, while RIPEMD was developed in the
--    framework of the EU project RIPE (RACE Integrity Primitives
--    Evaluation, 1988-1992). There are two good reasons to consider such
--    a replacement:
--
--    o  A 128-bit hash result does not offer sufficient protection
--       anymore. A brute force collision search attack on a 128-bit hash
--       result requires 264 or about 2.1019 evaluations of the function.
--       In 1994 Paul van Oorschot and Mike Wiener showed that this
--       brute-force job can be done in less than a month with a $10
--       million investment ("Parallel collision search with applications
--       to hash functions and discrete logarithms,'' 2nd ACM Conference
--       on Computer and Communications Security, ACM Press, 1994, pp.
--       210-218). This cost is expected to halve every 18 months.
--
--    o  In the first half of 1995 Hans Dobbertin found collisions for a
--       version of RIPEMD restricted to two rounds out of three. Using
--       similar techniques Hans produced in the Fall of 1995 collisions
--       for (all 3 rounds of) MD4. The attack on MD4 requires only a few
--       seconds on a PC, and still leaves some freedom as to the choice
--       of the message, clearly ruling out MD4 as a collision resistant
--       hash function. Shortly afterwards, in the Spring of 1996, Hans
--       also found collisions for the compression function of MD5.
--       Although not yet extended to collisions for MD5 itself, this
--       attack casts serious doubts on the strength of MD5 as a
--       collision resistant hash function. RSA Data Security, for
--       which Ron Rivest developed MD4 and MD5, recommend that MD4
--       should not longer be used, and that MD5 should not be used for
--       future applications that require the hash function to
--       be collision-resistant.
--
--    RIPEMD-160 is a strengthened version of RIPEMD with a 160-bit hash
--    result, and is expected to be secure for the next ten years or
--    more. The design philosophy is to build as much as possible on
--    experience gained by evaluating MD4, MD5, and RIPEMD. Like its
--    predecessors, RIPEMD-160 is tuned for 32-bit processors, which we
--    feel will remain important in the coming decade.
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

package ACF.Hash.Algorithms.RIPEMD160 is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD160_Context]--------------------------------------------
   --|   RIPEMD-160 message digest algorithm context type.
   --+------------------------------------------------------------------

   type RIPEMD160_Context is new Algorithm_Context with private;

   --+---[RIPEMD160_Context_Ptr]----------------------------------------
   --|   Access type to RIPEMD160_Context objects.
   --+------------------------------------------------------------------

   type RIPEMD160_Context_Ptr is access all RIPEMD160_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating RIPEMD160_Context objects
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------
   --|   Purpose:
   --|   Allocates memory for a RIPEMD160_Context object and returns the
   --|   access to the newly allocated object.
   --|
   --|   Arguments:
   --|   None.
   --|
   --|   Returned value:
   --|   RIPEMD160_Context_Ptr that references the newly allocated
   --|   object.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Context
      return   RIPEMD160_Context_Ptr;

   --+---[Deallocate_Context]-------------------------------------------
   --|   Purpose:
   --|   Deallocates a previously allocated RIPEMD160_Context object.
   --|
   --|   Arguments:
   --|   Context           RIPEMD160_Context_Ptr that references the
   --|                     object to deallocate.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out RIPEMD160_Context_Ptr);

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts RIPEMD-160 computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD160_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access RIPEMD160_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes RIPEMD-160 over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD160_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access RIPEMD160_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends RIPEMD-160 computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the RIPEMD160_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access RIPEMD160_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD160_Digest_Bytes]---------------------------------------
   --|   Constant that defines the size in bytes of RIPEMD-160 message
   --|   digests.
   --+------------------------------------------------------------------

   RIPEMD160_Digest_Bytes        : constant Positive := 20;

   --+---[RIPEMD160_Block_Bytes]----------------------------------------
   --|   Constant that defines the size in bytes of RIPEMD-160
   --|   processing blocks.
   --+------------------------------------------------------------------

   RIPEMD160_Block_Bytes         : constant Positive := 64;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[RIPEMD160_Block]----------------------------------------------
   --|   Type for handling RIPEMD-160 input blocks.
   --+------------------------------------------------------------------

   subtype RIPEMD160_Block is Byte_Array(1 .. RIPEMD160_Block_Bytes);

   --+---[State_Registers]----------------------------------------------
   --|   Type for handling RIPEMD-160 state registers.
   --+------------------------------------------------------------------

   subtype State_Registers is Four_Bytes_Array(1 .. 5);

   --+---[RIPEMD160_Context]--------------------------------------------
   --|   RIPEMD-160 computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          RIPEMD-160 state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type RIPEMD160_Context is new Algorithm_Context with
      record
         Bit_Count               : Eight_Bytes     := 0;
         State                   : State_Registers := (others => 0);
         Block                   : RIPEMD160_Block := (others => 0);
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes RIPEMD-160 digest context objects.
   --|
   --|   Arguments:
   --|   Object            RIPEMD160_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out RIPEMD160_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes RIPEMD-160 digest context objects.
   --|
   --|   Arguments:
   --|   Object            RIPEMD160_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out RIPEMD160_Context);

end ACF.Hash.Algorithms.RIPEMD160;
