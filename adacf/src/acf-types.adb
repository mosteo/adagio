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
--    File name         : acf-types.adb
--    File kind         : Ada package body.
--    Author            : Antonio Duran
--    Creation date     : November 20th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Implements the functionality declared in its specification.
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

with Ada.Characters.Handling;       use Ada.Characters.Handling;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;

package body ACF.Types is

   ---------------------------------------------------------------------
   -- Constants
   ---------------------------------------------------------------------

   --+---[Hex_Digits]---------------------------------------------------
   --|   Array containing the hexadecimal digits for each possible
   --|   value of a nibble.
   --+------------------------------------------------------------------

   Hex_Digits        : constant array(Byte range 16#00# .. 16#0F#)
                                    of Character :=
      (
         '0', '1', '2', '3', '4', '5', '6', '7',
         '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
      );

   ---------------------------------------------------------------------
   -- Subprogram bodies
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Obtaining parts of basic modular type's values
   ---------------------------------------------------------------------

   --+---[Lo_Nibble]----------------------------------------------------

   function    Lo_Nibble(
                  B              : in     Byte)
      return   Byte
   is
   begin
      return (B and 16#0F#);
   end Lo_Nibble;

   --+---[Hi_Nibble]----------------------------------------------------

   function    Hi_Nibble(
                  B              : in     Byte)
      return   Byte
   is
   begin
      return (Shift_Right(B, 4) and 16#0F#);
   end Hi_Nibble;

   --+---[Lo_Byte]------------------------------------------------------

   function    Lo_Byte(
                  T              : in     Two_Bytes)
      return   Byte
   is
   begin
      return Byte(T and 16#00FF#);
   end Lo_Byte;

   --+---[Hi_Byte]------------------------------------------------------

   function    Hi_Byte(
                  T              : in     Two_Bytes)
      return   Byte
   is
   begin
      return Byte(Shift_Right(T, 8) and 16#00FF#);
   end Hi_Byte;

   --+---[Lo_Two_Bytes]-------------------------------------------------

   function    Lo_Two_Bytes(
                  F              : in     Four_Bytes)
      return   Two_Bytes
   is
   begin
      return Two_Bytes(F and 16#0000_FFFF#);
   end Lo_Two_Bytes;

   --+---[Hi_Two_Bytes]-------------------------------------------------

   function    Hi_Two_Bytes(
                  F              : in     Four_Bytes)
      return   Two_Bytes
   is
   begin
      return Two_Bytes(Shift_Right(F, 16) and 16#0000_FFFF#);
   end Hi_Two_Bytes;

   --+---[Lo_Four_Bytes]------------------------------------------------

   function    Lo_Four_Bytes(
                  E              : in     Eight_Bytes)
      return   Four_Bytes
   is
   begin
      return Four_Bytes(E and 16#0000_0000_FFFF_FFFF#);
   end Lo_Four_Bytes;

   --+---[Hi_Four_Bytes]------------------------------------------------

   function    Hi_Four_Bytes(
                  E              : in     Eight_Bytes)
      return   Four_Bytes
   is
   begin
      return Four_Bytes(
                  Shift_Right(E, 32) and
                  16#0000_0000_FFFF_FFFF#);
   end Hi_Four_Bytes;

   ---------------------------------------------------------------------
   -- Building modular type's values
   ---------------------------------------------------------------------

   --+---[Make_Two_Bytes]-----------------------------------------------

   function    Make_Two_Bytes(
                  L              : in     Byte;
                  H              : in     Byte)
      return   Two_Bytes
   is
   begin
      return (
         Two_Bytes(L) or
         Shift_Left(Two_Bytes(H), 8));
   end Make_Two_Bytes;

   --+---[Make_Four_Bytes]----------------------------------------------

   function    Make_Four_Bytes(
                  LL             : in     Byte;
                  LH             : in     Byte;
                  HL             : in     Byte;
                  Hh             : in     Byte)
      return   Four_Bytes
   is
   begin
      return (
         Four_Bytes(LL) or
         Shift_Left(Four_Bytes(LH), 8) or
         Shift_Left(Four_Bytes(HL), 16) or
         Shift_Left(Four_Bytes(HH), 24));
   end Make_Four_Bytes;

   --+---[Make_Four_Bytes]----------------------------------------------

   function    Make_Four_Bytes(
                  L              : in     Two_Bytes;
                  H              : in     Two_Bytes)
      return   Four_Bytes
   is
   begin
      return (
         Four_Bytes(L) or
         Shift_Left(Four_Bytes(H), 16));
   end Make_Four_Bytes;

   --+---[Make_Eight_Bytes]---------------------------------------------

   function    Make_Eight_Bytes(
                  LLL            : in     Byte;
                  LLH            : in     Byte;
                  LHL            : in     Byte;
                  LHH            : in     Byte;
                  HLL            : in     Byte;
                  HLH            : in     Byte;
                  HHL            : in     Byte;
                  HHH            : in     Byte)
      return   Eight_Bytes
   is
   begin
      return (
         Eight_Bytes(LLL) or
         Shift_Left(Eight_Bytes(LLH), 8) or
         Shift_Left(Eight_Bytes(LHL), 16) or
         Shift_Left(Eight_Bytes(LHH), 24) or
         Shift_Left(Eight_Bytes(HLL), 32) or
         Shift_Left(Eight_Bytes(HLH), 40) or
         Shift_Left(Eight_Bytes(HHL), 48) or
         Shift_Left(Eight_Bytes(HHH), 56));
   end Make_Eight_Bytes;

   --+---[Make_Eight_Bytes]---------------------------------------------

   function    Make_Eight_Bytes(
                  LL             : in     Two_Bytes;
                  LH             : in     Two_Bytes;
                  HL             : in     Two_Bytes;
                  HH             : in     Two_Bytes)
      return   Eight_Bytes
   is
   begin
      return (
         Eight_Bytes(LL) or
         Shift_Left(Eight_Bytes(LH), 16) or
         Shift_Left(Eight_Bytes(HL), 32) or
         Shift_Left(Eight_Bytes(HH), 48));
   end Make_Eight_Bytes;

   --+---[Make_Eight_Bytes]---------------------------------------------

   function    Make_Eight_Bytes(
                  L              : in     Four_Bytes;
                  H              : in     Four_Bytes)
      return   Eight_Bytes
   is
   begin
      return (
         Eight_Bytes(L) or
         Shift_Left(Eight_Bytes(H), 32));
   end Make_Eight_Bytes;

   ---------------------------------------------------------------------
   -- Obtaining byte arrays
   ---------------------------------------------------------------------

   --+---[To_Byte_Array]------------------------------------------------

   function    To_Byte_Array(
                  B              : in     Byte;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array
   is
   begin
      return (1 => B);
   end To_Byte_Array;

   --+---[To_Byte_Array]------------------------------------------------

   function    To_Byte_Array(
                  T              : in     Two_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array
   is
   begin
      if Order = Little_Endian then
         return (1 => Hi_Byte(T), 2 => Lo_Byte(T));
      else
         return (1 => Lo_Byte(T), 2 => Hi_Byte(T));
      end if;
   end To_Byte_Array;

   --+---[To_Byte_Array]------------------------------------------------

   function    To_Byte_Array(
                  F              : in     Four_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array
   is
   begin
      if Order = Little_Endian then
         return (
            1 => Hi_Byte(Hi_Two_Bytes(F)),
            2 => Lo_Byte(Hi_Two_Bytes(F)),
            3 => Hi_Byte(Lo_Two_Bytes(F)),
            4 => Lo_Byte(Lo_Two_Bytes(F)));
      else
         return (
            1 => Lo_Byte(Lo_Two_Bytes(F)),
            2 => Hi_Byte(Lo_Two_Bytes(F)),
            3 => Lo_Byte(Hi_Two_Bytes(F)),
            4 => Hi_Byte(Hi_Two_Bytes(F)));
      end if;
   end To_Byte_Array;

   --+---[To_Byte_Array]------------------------------------------------

   function    To_Byte_Array(
                  E              : in     Eight_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array
   is
   begin
      if Order = Little_Endian then
         return (
            1 => Hi_Byte(Hi_Two_Bytes(Hi_Four_Bytes(E))),
            2 => Lo_Byte(Hi_Two_Bytes(Hi_Four_Bytes(E))),
            3 => Hi_Byte(Lo_Two_Bytes(Hi_Four_Bytes(E))),
            4 => Lo_Byte(Lo_Two_Bytes(Hi_Four_Bytes(E))),
            5 => Hi_Byte(Hi_Two_Bytes(Lo_Four_Bytes(E))),
            6 => Lo_Byte(Hi_Two_Bytes(Lo_Four_Bytes(E))),
            7 => Hi_Byte(Lo_Two_Bytes(Lo_Four_Bytes(E))),
            8 => Lo_Byte(Lo_Two_Bytes(Lo_Four_Bytes(E))));
      else
         return (
            1 => Lo_Byte(Lo_Two_Bytes(Lo_Four_Bytes(E))),
            2 => Hi_Byte(Lo_Two_Bytes(Lo_Four_Bytes(E))),
            3 => Lo_Byte(Hi_Two_Bytes(Lo_Four_Bytes(E))),
            4 => Hi_Byte(Hi_Two_Bytes(Lo_Four_Bytes(E))),
            5 => Lo_Byte(Lo_Two_Bytes(Hi_Four_Bytes(E))),
            6 => Hi_Byte(Lo_Two_Bytes(Hi_Four_Bytes(E))),
            7 => Lo_Byte(Hi_Two_Bytes(Hi_Four_Bytes(E))),
            8 => Hi_Byte(Hi_Two_Bytes(Hi_Four_Bytes(E))));
      end if;
   end To_Byte_Array;

   ---------------------------------------------------------------------
   -- Accessing and modifying individual bit values
   ---------------------------------------------------------------------

   --+---[Get_Bit_Value]------------------------------------------------

   function    Get_Bit_Value(
                  From           : in     Byte;
                  At_Position    : in     Natural)
      return   Boolean
   is
   begin
      if At_Position <= 7 then
         return ((From and Shift_Left(Byte(1), At_Position)) /= 0);
      else
         return False;
      end if;
   end Get_Bit_Value;

   --+---[Get_Bit_Value]------------------------------------------------

   function    Get_Bit_Value(
                  From           : in     Two_Bytes;
                  At_Position    : in     Natural)
      return   Boolean
   is
   begin
      if At_Position <= 15 then
         return ((From and Shift_Left(Two_Bytes(1), At_Position)) /= 0);
      else
         return False;
      end if;
   end Get_Bit_Value;

   --+---[Get_Bit_Value]------------------------------------------------

   function    Get_Bit_Value(
                  From           : in     Four_Bytes;
                  At_Position    : in     Natural)
      return   Boolean
   is
   begin
      if At_Position <= 31 then
         return (
            (From and Shift_Left(Four_Bytes(1), At_Position)) /= 0);
      else
         return False;
      end if;
   end Get_Bit_Value;

   --+---[Get_Bit_Value]------------------------------------------------

   function    Get_Bit_Value(
                  From           : in     Eight_Bytes;
                  At_Position    : in     Natural)
      return   Boolean
   is
   begin
      if At_Position <= 63 then
         return (
            (From and Shift_Left(Eight_Bytes(1), At_Position)) /= 0);
      else
         return False;
      end if;
   end Get_Bit_Value;

   --+---[Set_Bit_Value]------------------------------------------------

   procedure   Set_Bit_Value(
                  Into           : in out Byte;
                  At_Position    : in     Natural;
                  To             : in     Boolean)
   is
   begin
      if At_Position <= 7 then
         if To then
            Into := Into or (Rotate_Left(Byte(1), At_Position));
         else
            Into := Into and (Rotate_Left(Byte(16#FE#), At_Position));
         end if;
      end if;
   end Set_Bit_Value;

   --+---[Set_Bit_Value]------------------------------------------------

   procedure   Set_Bit_Value(
                  Into           : in out Two_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean)
   is
   begin
      if At_Position <= 15 then
         if To then
            Into := Into or
                    (Rotate_Left(Two_Bytes(1), At_Position));
         else
            Into := Into and
                    (Rotate_Left(Two_Bytes(16#FFFE#), At_Position));
         end if;
      end if;
   end Set_Bit_Value;

   --+---[Set_Bit_Value]------------------------------------------------

   procedure   Set_Bit_Value(
                  Into           : in out Four_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean)
   is
   begin
      if At_Position <= 31 then
         if To then
            Into := Into or
                    (Rotate_Left(Four_Bytes(1), At_Position));
         else
            Into := Into and
                    (Rotate_Left(Four_Bytes(16#FFFF_FFFE#),
                                 At_Position));
         end if;
      end if;
   end Set_Bit_Value;

   --+---[Set_Bit_Value]------------------------------------------------

   procedure   Set_Bit_Value(
                  Into           : in out Eight_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean)
   is
   begin
      if At_Position <= 63 then
         if To then
            Into := Into or
                    (Rotate_Left(Eight_Bytes(1), At_Position));
         else
            Into := Into and
                    (Rotate_Left(Eight_Bytes(16#FFFF_FFFF_FFFF_FFFE#),
                                 At_Position));
         end if;
      end if;
   end Set_Bit_Value;

   --+---[To_Hex_String]------------------------------------------------

   function    To_Hex_String(
                  Value          : in     Byte;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String
   is
      R              : Unbounded_String;
   begin
      Append(R, Hex_Digits(Hi_Nibble(Value)));
      Append(R, Hex_Digits(Lo_Nibble(Value)));

      if Digit_Case = Lower_Case then
         return (Preffix & To_String(R) & Suffix);
      else
         return (Preffix & To_Upper(To_String(R)) & Suffix);
      end if;
   end To_Hex_String;

   --+---[To_Hex_String]------------------------------------------------

   function    To_Hex_String(
                  Value          : in     Two_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String
   is
      R              : Unbounded_String;
      B              : Byte_Array := To_Byte_Array(Value);
   begin
      for I in B'Range loop
         Append(R, Hex_Digits(Hi_Nibble(B(I))));
         Append(R, Hex_Digits(Lo_Nibble(B(I))));
      end loop;

      if Digit_Case = Lower_Case then
         return (Preffix & To_String(R) & Suffix);
      else
         return (Preffix & To_Upper(To_String(R)) & Suffix);
      end if;
   end To_Hex_String;

   --+---[To_Hex_String]------------------------------------------------

   function    To_Hex_String(
                  Value          : in     Four_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String
   is
      R              : Unbounded_String;
      B              : Byte_Array := To_Byte_Array(Value);
   begin
      for I in B'Range loop
         Append(R, Hex_Digits(Hi_Nibble(B(I))));
         Append(R, Hex_Digits(Lo_Nibble(B(I))));
      end loop;

      if Digit_Case = Lower_Case then
         return (Preffix & To_String(R) & Suffix);
      else
         return (Preffix & To_Upper(To_String(R)) & Suffix);
      end if;
   end To_Hex_String;

   --+---[To_Hex_String]------------------------------------------------

   function    To_Hex_String(
                  Value          : in     Eight_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String
   is
      R              : Unbounded_String;
      B              : Byte_Array := To_Byte_Array(Value);
   begin
      for I in B'Range loop
         Append(R, Hex_Digits(Hi_Nibble(B(I))));
         Append(R, Hex_Digits(Lo_Nibble(B(I))));
      end loop;

      if Digit_Case = Lower_Case then
         return (Preffix & To_String(R) & Suffix);
      else
         return (Preffix & To_Upper(To_String(R)) & Suffix);
      end if;
   end To_Hex_String;

end ACF.Types;
