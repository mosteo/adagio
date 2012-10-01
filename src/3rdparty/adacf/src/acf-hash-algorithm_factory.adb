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
--    File name         : acf-hash-algorithm_factory.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 22th., 2001
--    Current version   : 1.1
------------------------------------------------------------------------
-- Purpose:
-- Implements the factory method that creates hash algorithm context
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
-- 1.1   ADD   12042001 Added construction of HAVAL objects.
------------------------------------------------------------------------

with ACF.Exceptions;                use ACF.Exceptions;
with ACF.Hash.Algorithms.MD2;       use ACF.Hash.Algorithms.MD2;
with ACF.Hash.Algorithms.MD4;       use ACF.Hash.Algorithms.MD4;
with ACF.Hash.Algorithms.MD5;       use ACF.Hash.Algorithms.MD5;
with ACF.Hash.Algorithms.SHA1;      use ACF.Hash.Algorithms.SHA1;
with ACF.Hash.Algorithms.RIPEMD128; use ACF.Hash.Algorithms.RIPEMD128;
with ACF.Hash.Algorithms.RIPEMD160; use ACF.Hash.Algorithms.RIPEMD160;
with ACF.Hash.Algorithms.TIGER;     use ACF.Hash.Algorithms.TIGER;
with ACF.Hash.Algorithms.HAVAL;     use ACF.Hash.Algorithms.HAVAL;

package body ACF.Hash.Algorithm_Factory is

   ---------------------------------------------------------------------
   -- Spec declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Create_Hash_Context]------------------------------------------

   function    Create_Hash_Context(
                  For_Algorithm  : in     Hash_Algorithm_Id)
      return   Algorithm_Context_Ref
   is
   begin
      case For_Algorithm is
         when ACF.Hash.MD2 =>
            return new MD2_Context;
         when ACF.Hash.MD4 =>
            return new MD4_Context;
         when ACF.Hash.MD5 =>
            return new MD5_Context;
         when ACF.Hash.SHA1 =>
            return new SHA1_Context;
         when ACF.Hash.RIPEMD_128 =>
            return new RIPEMD128_Context;
         when ACF.Hash.RIPEMD_160 =>
            return new RIPEMD160_Context;
         when ACF.Hash.TIGER =>
            return new TIGER_Context;
         when ACF.Hash.HAVAL =>
            return new HAVAL_Context;
      end case;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Create_Hash_Context;

end ACF.Hash.Algorithm_Factory;
