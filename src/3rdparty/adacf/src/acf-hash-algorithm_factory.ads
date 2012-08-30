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
--    File name         : acf-hash-algorithm_factory.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Provides a factory method for creating hash algorithm context
-- objects.
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

with ACF.Hash;                   use ACF.Hash;
with ACF.Hash.Algorithms;        use ACF.Hash.Algorithms;

package ACF.Hash.Algorithm_Factory is

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   --+---[Create_Hash_Context]------------------------------------------
   --|   Purpose:
   --|   Creates and returns a specific hash algorithm context from
   --|   the algorithm identifier supplied as an argument.
   --|
   --|   Arguments:
   --|   For_Algorithm        Hash_Algorithm_Id value that identifies
   --|                        the message digest algorithm for which the
   --|                        context object is to be created.
   --|
   --|   Returned value:
   --|   Returns a Algorithm_Context_Ref value that references the
   --|   message digest context object created.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if an exception is raised during context
   --|   creation.
   --+------------------------------------------------------------------

   function    Create_Hash_Context(
                  For_Algorithm  : in     Hash_Algorithm_Id)
      return   Algorithm_Context_Ref;

end ACF.Hash.Algorithm_Factory;
