------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------

with ACF.Hash.Algorithms.MD4;

package ACF.Hash.Algorithms.Ed2k is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   type Ed2k_Context is new Algorithm_Context with private;

   type Ed2k_Context_Ptr is access all Ed2k_Context;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Allocating and deallocating Ed2k_Context objects
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Dispatching operations
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------
   --|   Purpose:
   --|   Starts Ed2k computation by initializing the context.
   --|
   --|   Arguments:
   --|   Context           Access to the Ed2k_Context object to set
   --|                     up for computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Start(
                  Context        : access Ed2k_Context);

   --+---[Hash_Update]--------------------------------------------------
   --|   Purpose:
   --|   Computes Ed2k over an array of bytes.
   --|
   --|   Arguments:
   --|   Context           Access to the Ed2k_Context object that
   --|                     governs the computation.
   --|   Bytes             Byte_Array to compute the hash over.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Hash_Update(
                  Context        : access Ed2k_Context;
                  Bytes          : in     Byte_Array);

   --+---[Hash_End]-----------------------------------------------------
   --|   Purpose:
   --|   Ends Ed2k computation and returns the computed message
   --|   digest.
   --|
   --|   Arguments:
   --|   Context           Access to the Ed2k_Context object that
   --|                     governs the computation.
   --|
   --|   Returned value
   --|   Message_Digest resulting of computation.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hash_End(
                  Context        : access Ed2k_Context)
      return   Message_Digest;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   package md4 renames Acf.Hash.Algorithms.md4;

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[Ed2k_Digest_Bytes]--------------------------------------------
   --|   Constant that defines the size in bytes of Ed2k message
   --|   digests.
   --+------------------------------------------------------------------

   Ed2k_Digest_Bytes             : constant Positive := 16;

   --+---[Ed2k_Block_Bytes]----------------------------------------------
   --|   Constant that defines the size in bytes of Ed2k processing
   --|   blocks.
   --+------------------------------------------------------------------

   Ed2k_Block_Bytes              : constant Positive := 9_728_000;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Ed2k_Context]--------------------------------------------------
   --|   Ed2k computation context. The extension part has the
   --|   following fields:
   --|
   --|   Bit_Count      64-bit counter of processed bits.
   --|   State          Ed2k state registers.
   --|   Block          Internal block.
   --+------------------------------------------------------------------

   type Ed2k_Context is new Algorithm_Context with
      record
         Main_context, Partial_context : aliased MD4.MD4_Context;
         Remaining   : Natural := Ed2k_block_bytes;
         Size        : Natural := 0;
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Initialize]---------------------------------------------------
   --|   Purpose:
   --|   Initializes Ed2k digest context objects.
   --|
   --|   Arguments:
   --|   Object            Ed2k_Context object to initialize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out Ed2k_Context);

   --+---[Finalize]-----------------------------------------------------
   --|   Purpose:
   --|   Finalizes Ed2k digest context objects.
   --|
   --|   Arguments:
   --|   Object            Ed2k_Context object to finalize.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

     procedure   Finalize(
                    Object         : in out Ed2k_Context);

end ACF.Hash.Algorithms.Ed2k;
