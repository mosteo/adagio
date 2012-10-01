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
--    File name         : acf-hash-algorithms.adb
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Implements the functionality declared in its spec.
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

package body ACF.Hash.Algorithms is

   ---------------------------------------------------------------------
   -- Public subprograms
   ---------------------------------------------------------------------

   --+---[Get_Hash_Algorithm_Id]----------------------------------------

   function    Get_Hash_Algorithm_Id(
                  Context        : access Algorithm_Context'Class)
      return   Hash_Algorithm_Id
   is
   begin
      return Context.all.Algo_ID;
   end Get_Hash_Algorithm_Id;

end ACF.Hash.Algorithms;
