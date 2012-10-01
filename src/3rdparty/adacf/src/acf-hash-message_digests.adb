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
--    File name         : acf-hash-message_digests.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 20th., 2001
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
-- 1.0   ADD   11202001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

with Interfaces; use Interfaces;

package body ACF.Hash.Message_Digests is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              Byte_Array,
                              Byte_Array_Ptr);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Allocate_Byte_Array]------------------------------------------
   --|   Purpose:
   --|   Dynamically allocates a byte array of a specific size and
   --|   returns an access value to that allocated array.
   --|
   --|   Arguments:
   --|   Size              Positive value that specifies the size of
   --|                     the array to allocate.
   --|
   --|   Returned value:
   --|   Returns a Byte_Array_Ptr that references the allocated array.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if the allocation fails.
   --+------------------------------------------------------------------

   function    Allocate_Byte_Array(
                  Size           : in     Positive)
      return   Byte_Array_Ptr;

   ---------------------------------------------------------------------
   -- Body subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Byte_Ptr]--------------------------------------------

   function    Allocate_Byte_Array(
                  Size           : in     Positive)
      return   Byte_Array_Ptr
   is
      R              : Byte_Array_Ptr := null;
   begin
      R := new Byte_Array(1 .. Size);
      R.all := (others => 0);
      return R;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Byte_Array;

   ---------------------------------------------------------------------
   -- Spec subprogram bodies
   ---------------------------------------------------------------------

   --+---[To_Message_Digest]--------------------------------------------

   function    To_Message_Digest(
                  Source         : in     Byte_Array)
      return   Message_Digest
   is
      R              : Message_Digest := Null_Message_Digest;
   begin
      if Source'Length > 0 then
         R.Bytes := Allocate_Byte_Array(Source'Length);
         R.Bytes.all := Source;
      end if;

      return R;
   exception
      when ACF_Storage_Error =>
         raise;
      when others =>
         --raise;
         raise ACF_Unexpected_Error;
   end To_Message_Digest;

   --+---[To_Message_Digest]--------------------------------------------

   function    To_Message_Digest(
                  Source         : in     String)
      return   Message_Digest
   is
      R              : Message_Digest;
      L              : Positive;
      M              : Integer;
      P              : Byte_Array_Ptr := null;
      N              : Byte;
      B              : Byte;
      H              : Boolean := True;
      J              : Positive;
   begin
      if Source'Length = 0 then
         return Null_Message_Digest;
      else
         M := Source'Length mod 2;

         if M /= 0 then
            raise ACF_Syntax_Error;
         end if;

         L := Source'Length / 2;
         P := Allocate_Byte_Array(L);
         J := 1;

         for I in Source'Range loop
            case Source(I) is
               when '0' .. '9' =>
                  N := Byte(Character'Pos(Source(I)) -
                            Character'Pos('0'));
               when 'a' .. 'f' =>
                  N := Byte(10 + Character'Pos(Source(I)) -
                                 Character'Pos('a'));
               when 'A' .. 'F' =>
                  N := Byte(10 + Character'Pos(Source(I)) -
                                 Character'Pos('A'));
               when others =>
                  raise ACF_Syntax_Error;
            end case;

            if H then
               B := Shift_Left(N, 4);
            else
               B := B or N;
               P.all(J) := B;
               J := J + 1;
            end if;

            H := not H;
         end loop;

         R.Bytes := P;

         return R;
      end if;
   exception
      when ACF_Storage_Error |
           ACF_Syntax_Error =>
         if P /= null then
            Free(P);
         end if;
         raise;
      when others =>
         if P /= null then
            Free(P);
         end if;
         raise ACF_Unexpected_Error;
   end To_Message_Digest;

   --+---[Clear]--------------------------------------------------------

   procedure   Clear(
                  The_Digest     : in out Message_Digest)
   is
   begin
      if The_Digest.Bytes /= null then
         The_Digest.Bytes.all := (others => 0);
      end if;
   end Clear;

   --+---[Set_Message_Digest]-------------------------------------------

   procedure   Set_Message_Digest(
                  The_Digest     : in out Message_Digest;
                  From           : in     Byte_Array)
   is
      T              : Byte_Array_Ptr := null;
   begin
      if From'Length > 0 then
         T := Allocate_Byte_Array(From'Length);
         T.all := From;
      end if;

      if The_Digest.Bytes /= null then
         Free(The_Digest.Bytes);
      end if;

      The_Digest.Bytes := T;
   exception
      when ACF_Storage_Error =>
         raise;
      when others =>
         raise ACF_Unexpected_Error;
   end Set_Message_Digest;

   --+---[To_Byte_Array]------------------------------------------------

   function    To_Byte_Array(
                  From_Digest    : in     Message_Digest)
      return   Byte_Array
   is
   begin
      if From_Digest.Bytes = null then
         raise ACF_Null_Argument_Error;
      else
         return From_Digest.Bytes.all;
      end if;
   end To_Byte_Array;

   --+---[To_Hex_String]------------------------------------------------

   function    To_Hex_String(
                  From_Digest    : in     Message_Digest;
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String
   is
   begin
      if From_Digest.Bytes = null then
         raise ACF_Null_Argument_Error;
      else
         declare
            R        : String(1 .. 2 * From_Digest.Bytes.all'Last);
            N        : Positive := R'First;
         begin
            for I in From_Digest.Bytes.all'Range loop
               R(N .. N + 1) :=
                  To_Hex_String(
                     Value => From_Digest.Bytes.all(I),
                     Digit_Case => Digit_Case);
               N := N + 2;
            end loop;

            return R;
         end;
      end if;
   end To_Hex_String;

   --+---["="]----------------------------------------------------------

   function    "="(
                  Left           : in     Message_Digest;
                  Right          : in     Message_Digest)
      return   Boolean
   is
   begin
      if Left.Bytes = Right.Bytes then
         return True;
      else
         if Left.Bytes = null or else
            Right.Bytes = null then
            return False;
         else
            if Left.Bytes.all'Length = Right.Bytes.all'Length then
               return Left.Bytes.all = Right.Bytes.all;
            else
               return False;
            end if;
         end if;
      end if;
   end "=";

   --+---[Get_Size]-----------------------------------------------------

   function    Get_Size(
                  Of_Digest      : in     Message_Digest)
      return   Natural
   is
   begin
      if Of_Digest.Bytes = null then
         return 0;
      else
         return Of_Digest.Bytes.all'Length;
      end if;
   end Get_Size;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out Message_Digest)
   is
   begin
      Object.Bytes := null;
   end Initialize;

   --+---[Adjust]-------------------------------------------------------

   procedure   Adjust(
                  Object         : in out Message_Digest)
   is
      T              : Byte_Array_Ptr := null;
   begin
      if Object.Bytes /= null then
         T := Allocate_Byte_Array(Object.Bytes.all'Length);
         T.all := Object.Bytes.all;
         Object.Bytes := T;
      end if;
   end Adjust;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out Message_Digest)
   is
   begin
      if Object.Bytes /= null then
         Free(Object.Bytes);
      end if;
   end Finalize;

end ACF.Hash.Message_Digests;
