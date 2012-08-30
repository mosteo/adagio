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
--    File name         : acf-hash-algorithms.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Defines an abstract tagged type that is the base class of the
-- classes that provide the different hash algorithm contexts provided
-- by the ACF.
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

with Ada.Finalization;
with ACF.Types;                     use ACF.Types;
with ACF.Hash.Message_Digests;      use ACF.Hash.Message_Digests;

package ACF.Hash.Algorithms is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Algorithm_Context]--------------------------------------------
   --|   Abstract tagged type that is the base class for the digest
   --|   algorithm context classes. Digest context mantain the
   --|   information context needed for computing message digest.
   --+------------------------------------------------------------------

   type Algorithm_Context is abstract tagged limited private;

   --+---[Algorithm_Context_Ref]----------------------------------------
   --|   Wide class access type to Algorithm_Context objects.
   --+------------------------------------------------------------------

   type Algorithm_Context_Ref is access all Algorithm_Context'Class;

   --+---[Algorithm_Context_Ptr]----------------------------------------
   --|   Access type to Algorithm_Context objects.
   --+------------------------------------------------------------------

   type Algorithm_Context_Ptr is access all Algorithm_Context'Class;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts message digest computation by initializing the
   --|   context of the particular message digest algorithm.
   --|
   --|   Arguments:
   --|   Context           Access to the Algorithm_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access Algorithm_Context)
         is abstract;

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes the hash over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the Algorithm_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access Algorithm_Context;
                  Bytes          : in     Byte_Array)
         is abstract;

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends hash computations and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the Algorithm_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access Algorithm_Context)
      return   Message_Digest
         is abstract;

   ---------------------------------------------------------------------
   -- Non-dispatching operations
   ---------------------------------------------------------------------

   --+---[Get_Hash_Algorithm_Id]----------------------------------------
   --|   Purpose:
   --|   Returns the algorithm identifier that identifies the hash
   --|   algorithm implemented by a particular context.
   --|
   --|   Arguments:
   --|   Context           Access to the Algorithm_Context object
   --|                     for which the algorithm identifier is to be
   --|                     obtained.
   --|
   --|   Returned value:
   --|   Hash_Algorithm_Id value that identifies the algorithm
   --|   implemented.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Get_Hash_Algorithm_Id(
                  Context        : access Algorithm_Context'Class)
      return   Hash_Algorithm_Id;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Algorithm_Context]--------------------------------------------
   --|   Full definition of the Algorithm_Context tagged type. Its
   --|   record extension part contains the following fields:
   --|
   --|   Algo_Id              Hash_Algorithm_Id value that contains the
   --|                        algorithm identifier.
   --+------------------------------------------------------------------

   use Ada.Finalization;

   type Algorithm_Context is abstract new Limited_Controlled with
      record
         Algo_ID                 : Hash_Algorithm_Id;
      end record;

end ACF.Hash.Algorithms;
