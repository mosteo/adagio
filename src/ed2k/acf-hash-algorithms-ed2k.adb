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
--    File name         : acf-hash-algorithms-Ed2k.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 26th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the SHA-1 (Secure Hash Algorithm) message digest
--    algorithm.
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

with Acf.Hash.Message_digests;

package body ACF.Hash.Algorithms.Ed2k is

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access Ed2k_Context)
   is
   begin
      Context.Remaining := Ed2k_block_bytes;
      Md4.Hash_start (Context.Main_context'access);
      Md4.Hash_start (Context.Partial_context'access);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access Ed2k_Context;
                  Bytes          : in     Byte_Array)
   is
      Remaining : Natural := Context.Remaining;
   begin
      if Bytes'Length = 0 then
         return;
      end if;
      if Bytes'Length > Context.Remaining then
         Hash_update (Context, 
            Bytes (Bytes'First .. Bytes'First + Remaining - 1));
         declare
            Partial_hash : Acf.Hash.Message_digests.Message_digest;
         begin
            Partial_hash := MD4.Hash_end (Context.Partial_context'Access);
            MD4.Hash_update (Context.Main_context'Access, 
               Acf.Hash.Message_digests.To_byte_array (Partial_hash));
            Context.Remaining := Ed2k_block_bytes;
            MD4.Hash_start (Context.Partial_context'Access);
            Hash_update (Context, 
               Bytes (Bytes'First + Remaining .. Bytes'Last));
         end;
      else
         MD4.Hash_update (Context.Partial_context'Access, Bytes);
         Context.Size      := Context.Size + Bytes'Length;
         Context.Remaining := Context.Remaining - Bytes'Length;
      end if;
   end Hash_Update;

   --+---[Hash_End]-----------------------------------------------------

   function    Hash_End(
                  Context        : access Ed2k_Context)
      return   Message_Digest
   is
   begin
      if Context.Size > Ed2k_block_bytes then
         declare
            Partial_hash : Acf.Hash.Message_digests.Message_digest;
         begin
            Partial_hash := MD4.Hash_end (Context.Partial_context'Access);
            MD4.Hash_update (Context.Main_context'Access, 
               Acf.Hash.Message_digests.To_byte_array (Partial_hash));
            return Md4.Hash_end (Context.Main_context'access);
         end;
      else
         return Md4.Hash_end (Context.Partial_context'access);
      end if;
   end Hash_End;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out Ed2k_Context)
   is
      pragma Unreferenced (Object);
   begin
      null;
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out Ed2k_Context)
   is
      pragma Unreferenced (Object);
   begin
      null;
   end Finalize;

end ACF.Hash.Algorithms.Ed2k;
