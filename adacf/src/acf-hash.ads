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
--    File name         : acf-hash.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 20th., 2001
--    Current version   : 1.1
------------------------------------------------------------------------
-- Purpose:
-- This is the root package for message digest computation.
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
-- 1.0   ADD   11202001 Initial implementation
-- 1.1   ADD   12042001 Added identifier for HAVAL.
------------------------------------------------------------------------

package ACF.Hash is

   pragma Pure;

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Hash_Algorithm_Id]--------------------------------------------
   --|   Enumerated type that identifies the different message digest
   --|   algorithms implemented in the ACF. Possible values are:
   --|
   --|   MD2                  Identifier of the RSA MD2 Message Digest
   --|                        algorithm. It produces 128-bit message
   --|                        digests.
   --|   MD4                  Identifier of the RSA MD4 Message Digest
   --|                        algorithm. It produces 128-bit message
   --|                        digests.
   --|   MD5                  Identifier of the RSA MD5 Message Digest
   --|                        algorithm. It produces 128-bit message
   --|                        digests.
   --|   SHA1                 Identifier of the SHS SHA 1 Message Digest
   --|                        algorithm. It produces 160-bit message
   --|                        digests.
   --|   RIPEMD_128           Identifier of the RIPEMD-128 Message
   --|                        Digest algorithm. It produces 128-bit
   --|                        message digests.
   --|   RIPEMD_160           Identifier of the RIPEMD-160 Message
   --|                        Digest algorithm. It produces 160-bit
   --|                        message digests.
   --|   TIGER                Identifier of the TIGER Message Digest
   --|                        algorithm. It produces 192-bit message
   --|                        digests.
   --|   HAVAL                Identifier of the HAVAL message digest
   --|                        algorithm. It can produce 128, 160, 192
   --|                        224, or 256-bit message digests.
   --+------------------------------------------------------------------

   type Hash_Algorithm_Id is
      (
         MD2,
         MD4,
         MD5,
         SHA1,
         RIPEMD_128,
         RIPEMD_160,
         TIGER,
         HAVAL
      );

end ACF.Hash;
