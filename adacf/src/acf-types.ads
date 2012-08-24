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
--    File name         : acf-types.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 20th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- This package contains basic type definitions and operations on
-- those types that are used in other parts of the framework.
------------------------------------------------------------------------
-- Portability issues:
-- This package defines a set of basic modular types for handling 8,
-- 16, 32, and 64-bit values by deriving from the corresponding
-- Interfaces.Unsigned_n types. Since the LRM does not specify the
-- minimum set of Unsigned_n types an implementation must provide,
-- there is a risk in porting this library to other architectures/
-- implementations.
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

with Interfaces;

package ACF.Types is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Basic modular types
   ---------------------------------------------------------------------

   --+---[Byte]---------------------------------------------------------
   --|   Modular type for handling 8-bit values.
   --+------------------------------------------------------------------

   subtype Byte is Interfaces.Unsigned_8;

   --+---[Two_Bytes]----------------------------------------------------
   --|   Modular type for handling 16-bit values.
   --+------------------------------------------------------------------

   type Two_Bytes is new Interfaces.Unsigned_16;

   --+---[Four_Bytes]---------------------------------------------------
   --|   Modular type for handling 32-bit values.
   --+------------------------------------------------------------------

   type Four_Bytes is new Interfaces.Unsigned_32;

   --+---[Eight_Bytes]--------------------------------------------------
   --|   Modular type for handling 64-bit values.
   --+------------------------------------------------------------------

   type Eight_Bytes is new Interfaces.Unsigned_64;

   ---------------------------------------------------------------------
   -- Array types
   ---------------------------------------------------------------------

   --+---[Byte_Array]---------------------------------------------------
   --|   Unconstrained, positive indexed, array type of Byte values.
   --+------------------------------------------------------------------

   type Byte_Array is array(Positive range <>) of Byte;
   pragma Pack(Byte_Array);

   --+---[Two_Bytes_Array]----------------------------------------------
   --|   Unconstrained, positive indexed, array type of Two_Bytes
   --|   values.
   --+------------------------------------------------------------------

   type Two_Bytes_Array is array(Positive range <>) of Two_Bytes;
   pragma Pack(Two_Bytes_Array);

   --+---[Four_Bytes_Array]---------------------------------------------
   --|   Unconstrained, positive indexed, array type of Four_Bytes
   --|   values.
   --+------------------------------------------------------------------

   type Four_Bytes_Array is array(Positive range <>) of Four_Bytes;
   pragma Pack(Four_Bytes_Array);

   --+---[Eight_Bytes_Array]--------------------------------------------
   --|   Unconstrained, positive indexed, array type of Eight_Bytes
   --|   values.
   --+------------------------------------------------------------------

   type Eight_Bytes_Array is array(Positive range <>) of Eight_Bytes;
   pragma Pack(Eight_Bytes_Array);

   ---------------------------------------------------------------------
   -- Other types
   ---------------------------------------------------------------------

   --+---[Byte_Order]---------------------------------------------------
   --|   Enumerated type that identifies the order of bytes in the
   --|   Byte_Array's resulting from the functions that transform
   --|   basic modular types into byte arrays. There are two possible
   --|   byte ordering schemas:
   --|
   --|   Little_Endian        Significance of bytes in the byte array
   --|                        decreases as the index of the array
   --|                        increases.
   --|
   --|   Big_Endian           Significance of bytes in the byte array
   --|                        increases as the index of the array
   --|                        increases.
   --|
   --|   Note that this ordering has nothing to do with the underlying
   --|   processor architecture.
   --+------------------------------------------------------------------

   type Byte_Order is (Little_Endian, Big_Endian);

   --+---[Hex_Digit_Case]-----------------------------------------------
   --|   Enumerated type that identifies the case of the hexadecimal
   --|   string representation resulting from To_Hex_String functions.
   --|   Possible values are:
   --|
   --|   Lower_Case           Hexadecimal digit letters are formatted
   --|                        in lower case.
   --|   Upper_Case           Hexadecimal digit letters are formatted
   --|                        in upper case.
   --+------------------------------------------------------------------

   type Hex_Digit_Case is (Lower_Case, Upper_Case);

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Obtaining parts of basic modular type's values
   ---------------------------------------------------------------------

   --+---[Lo_Nibble]----------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the least significant 4-bit part (nibble)
   --|   of an 8-bit (Byte) value. The result is returned as a Byte
   --|   value.
   --|
   --|   Arguments:
   --|   B                 Byte value to obtain the least significant
   --|                     4-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Byte value containing the least significant 4-bit
   --|   part of B.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Lo_Nibble(
                  B              : in     Byte)
      return   Byte;
   pragma Inline(Lo_Nibble);

   --+---[Hi_Nibble]----------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the most significant 4-bit part (nibble)
   --|   of an 8-bit (Byte) value. The result is returned as a Byte
   --|   value.
   --|
   --|   Arguments:
   --|   B                 Byte value to obtain the most significant
   --|                     4-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Byte value containing the most significant 4-bit
   --|   part of B.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hi_Nibble(
                  B              : in     Byte)
      return   Byte;
   pragma Inline(Hi_Nibble);

   --+---[Lo_Byte]------------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the least significant 8-bit part (Byte)
   --|   of a 16-bit (Two_Bytes) value.
   --|
   --|   Arguments:
   --|   T                 Two_Bytes value to obtain the least
   --|                     significant 8-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Byte value containing the least significant 8-bit
   --|   part of T.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Lo_Byte(
                  T              : in     Two_Bytes)
      return   Byte;
   pragma Inline(Lo_Byte);

   --+---[Hi_Byte]------------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the most significant 8-bit part (Byte)
   --|   of a 16-bit (Two_Bytes) value.
   --|
   --|   Arguments:
   --|   T                 Two_Bytes value to obtain the most
   --|                     significant 8-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Byte value containing the most significant 8-bit
   --|   part of T.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hi_Byte(
                  T              : in     Two_Bytes)
      return   Byte;
   pragma Inline(Hi_Byte);

   --+---[Lo_Two_Bytes]-------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the least significant 16-bit part
   --|   (Two_Bytes) of a 32-bit (Four_Bytes) value.
   --|
   --|   Arguments:
   --|   F                 Two_Bytes value to obtain the least
   --|                     significant 16-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Two_Bytes value containing the least significant
   --|   16-bit part of F.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Lo_Two_Bytes(
                  F              : in     Four_Bytes)
      return   Two_Bytes;
   pragma Inline(Lo_Two_Bytes);

   --+---[Hi_Two_Bytes]-------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the most significant 16-bit part
   --|   (Two_Bytes) of a 32-bit (Four_Bytes) value.
   --|
   --|   Arguments:
   --|   F                 Two_Bytes value to obtain the most
   --|                     significant 16-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Two_Bytes value containing the most significant
   --|   16-bit part of F.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hi_Two_Bytes(
                  F              : in     Four_Bytes)
      return   Two_Bytes;
   pragma Inline(Hi_Two_Bytes);

   --+---[Lo_Four_Bytes]------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the least significant 32-bit part
   --|   (Four_Bytes) of a 64-bit (Eight_Bytes) value.
   --|
   --|   Arguments:
   --|   E                 Eight_Bytes value to obtain the least
   --|                     significant 32-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Four_Bytes value containing the least significant
   --|   32-bit part of E.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Lo_Four_Bytes(
                  E              : in     Eight_Bytes)
      return   Four_Bytes;
   pragma Inline(Lo_Four_Bytes);

   --+---[Hi_Four_Bytes]------------------------------------------------
   --|
   --|   Purpose:
   --|   Obtains and returns the most significant 32-bit part
   --|   (Four_Bytes) of a 64-bit (Eight_Bytes) value.
   --|
   --|   Arguments:
   --|   E                 Eight_Bytes value to obtain the most
   --|                     significant 32-bit part from.
   --|
   --|   Returned value:
   --|   Returns a Four_Bytes value containing the most significant
   --|   32-bit part of E.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Hi_Four_Bytes(
                  E              : in     Eight_Bytes)
      return   Four_Bytes;
   pragma Inline(Hi_Four_Bytes);

   ---------------------------------------------------------------------
   -- Building modular type's values
   ---------------------------------------------------------------------

   --+---[Make_Two_Bytes]-----------------------------------------------
   --|
   --|   Purpose:
   --|   Builds and returns a 16-bit (Two_Bytes) value out of two
   --|   8-bit (Byte) values.
   --|
   --|   Arguments:
   --|   L                 Byte value which will provide the bits
   --|                     0 .. 7 of the value to be build.
   --|   H                 Byte value which will provide the bits
   --|                     8 .. 15 of the value to be build.
   --|
   --|   Returned value:
   --|   Returns a Two_Bytes value built out of the arguments supplied.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Make_Two_Bytes(
                  L              : in     Byte;
                  H              : in     Byte)
      return   Two_Bytes;
   pragma Inline(Make_Two_Bytes);

   --+---[Make_Four_Bytes]----------------------------------------------
   --|
   --|   Purpose:
   --|   Builds and returns a 32-bit (Four_Bytes) value. Two overloaded
   --|   forms are provided:
   --|
   --|   o  First form builds the 32-bit value out of 4 8-bit (Byte)
   --|      values.
   --|   o  Second form builds the 32-bit value out of two 16-bit
   --|      (Two_Bytes) values.
   --|
   --|   Arguments:
   --|   First form:
   --|   LL                Byte value which will provide the bits
   --|                     0 .. 7 of the value to be build.
   --|   LH                Byte value which will provide the bits
   --|                     8 .. 15 of the value to be build.
   --|   HL                Byte value which will provide the bits
   --|                     16 .. 23 of the value to be build.
   --|   HH                Byte value which will provide the bits
   --|                     24 .. 31 of the value to be build.
   --|   Second form:
   --|   L                 Two_Bytes value which will provide the bits
   --|                     0 .. 15 of the value to be build.
   --|   H                 Two_Bytes value which will provide the bits
   --|                     16 .. 31 of the value to be build.
   --|
   --|   Returned value:
   --|   Returns a Four_Bytes value built out of the arguments supplied.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Make_Four_Bytes(
                  LL             : in     Byte;
                  LH             : in     Byte;
                  HL             : in     Byte;
                  Hh             : in     Byte)
      return   Four_Bytes;
   pragma Inline(Make_Four_Bytes);

   function    Make_Four_Bytes(
                  L              : in     Two_Bytes;
                  H              : in     Two_Bytes)
      return   Four_Bytes;
   pragma Inline(Make_Four_Bytes);

   --+---[Make_Eight_Bytes]---------------------------------------------
   --|
   --|   Purpose:
   --|   Builds and returns a 64-bit (Eight_Bytes) value. Three
   --|   overloaded forms are provided:
   --|
   --|   o  First form builds the 64-bit value out of 8 8-bit (Byte)
   --|      values.
   --|   o  Second form builds the 64-bit value out of 4 16-bit
   --|      (Two_Bytes) values.
   --|   o  Third form builds the 64-bit value out of two 32-bit
   --|      (Four_Bytes) values.
   --|
   --|   Arguments:
   --|   First form:
   --|   LLL               Byte value which will provide the bits
   --|                     0 .. 7 of the value to be build.
   --|   LLH               Byte value which will provide the bits
   --|                     8 .. 15 of the value to be build.
   --|   LHL               Byte value which will provide the bits
   --|                     16 .. 23 of the value to be build.
   --|   LHH               Byte value which will provide the bits
   --|                     24 .. 31 of the value to be build.
   --|   HLL               Byte value which will provide the bits
   --|                     32 .. 39 of the value to be build.
   --|   HLH               Byte value which will provide the bits
   --|                     40 .. 47 of the value to be build.
   --|   HHL               Byte value which will provide the bits
   --|                     48 .. 55 of the value to be build.
   --|   HHH               Byte value which will provide the bits
   --|                     56 .. 63 of the value to be build.
   --|   Second form:
   --|   LL                Two_Bytes value which will provide the bits
   --|                     0 .. 15 of the value to be build.
   --|   LH                Two_Bytes value which will provide the bits
   --|                     16 .. 31 of the value to be build.
   --|   HL                Two_Bytes value which will provide the bits
   --|                     32.. 47 of the value to be build.
   --|   HH                Two_Bytes value which will provide the bits
   --|                     48 .. 63 of the value to be build.
   --|   Third form:
   --|   L                 Four_Bytes value which will provide the bits
   --|                     0 .. 31 of the value to be build.
   --|   H                 Four_Bytes value which will provide the bits
   --|                     32 .. 63 of the value to be build.
   --|
   --|   Returned value:
   --|   Returns an Eight_Bytes value built out of the arguments
   --|   supplied.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Make_Eight_Bytes(
                  LLL            : in     Byte;
                  LLH            : in     Byte;
                  LHL            : in     Byte;
                  LHH            : in     Byte;
                  HLL            : in     Byte;
                  HLH            : in     Byte;
                  HHL            : in     Byte;
                  HHH            : in     Byte)
      return   Eight_Bytes;
   pragma Inline(Make_Eight_Bytes);

   function    Make_Eight_Bytes(
                  LL             : in     Two_Bytes;
                  LH             : in     Two_Bytes;
                  HL             : in     Two_Bytes;
                  HH             : in     Two_Bytes)
      return   Eight_Bytes;
   pragma Inline(Make_Eight_Bytes);

   function    Make_Eight_Bytes(
                  L              : in     Four_Bytes;
                  H              : in     Four_Bytes)
      return   Eight_Bytes;
   pragma Inline(Make_Eight_Bytes);

   ---------------------------------------------------------------------
   -- Obtaining byte arrays
   ---------------------------------------------------------------------

   --+---[To_Byte_Array]------------------------------------------------
   --|
   --|   Purpose:
   --|   Returns a Byte_Array containing the bytes that conform a
   --|   basic modular type value. Four overloaded functions are
   --|   provided: one for each of the different basic modular types
   --|   declared in this package.
   --|
   --|   Arguments:
   --|   B, T, F, or E     Either a Byte, Two_Bytes, Four_Bytes, or
   --|                     Eight_Bytes value, depending on the
   --|                     overloaded form that is the basic modular
   --|                     value for which the composing bytes are to
   --|                     be obtained as a Byte_Array.
   --|   Order             Byte_Order value which determines the
   --|                     ordering of bytes in the resulting array.
   --|                     This argument is ignored when the first
   --|                     argument is a Byte.
   --|
   --|   Returned value:
   --|   Returns a Byte_Array containing the Bytes that conform the
   --|   first argument's value in the order specified by the second
   --|   argument. The resulting array index is always 1 based.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    To_Byte_Array(
                  B              : in     Byte;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array;

   function    To_Byte_Array(
                  T              : in     Two_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array;

   function    To_Byte_Array(
                  F              : in     Four_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array;

   function    To_Byte_Array(
                  E              : in     Eight_Bytes;
                  Order          : in     Byte_Order := Little_Endian)
      return   Byte_Array;

   ---------------------------------------------------------------------
   -- Accessing and modifying individual bit values
   ---------------------------------------------------------------------

   --+---[Get_Bit_Value]------------------------------------------------
   --|
   --|   Purpose:
   --|   Returns the value of a specific bit inside a value of a
   --|   basic modular type as a Boolean value (False -> 0, True -> 1).
   --|   Four overloaded functions are provided: one for each of the
   --|   different basic modular types declared in this package.
   --|
   --|   Arguments:
   --|   From              Either a Byte, Two_Bytes, Four_Bytes, or
   --|                     Eight_Bytes value, depending on the
   --|                     overloaded form, from which the value of a
   --|                     specific bit is to be obtained and returned.
   --|   At_Position       Natural number that specifies the position of
   --|                     the bit inside From. Positions are zero based
   --|                     being 0 the least significant bit.
   --|
   --|   Returned value:
   --|   Returns a Boolean value with the value of the bit that is
   --|   At_Position in From (False -> 0, True -> 1). If the value of
   --|   At_Position is greater than the maximum bit position for From
   --|   (7 for Byte, 15 for Two_Bytes, 31 for Four_Bytes, or 63 for
   --|   Eight_Bytes) the function will return False.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Get_Bit_Value(
                  From           : in     Byte;
                  At_Position    : in     Natural)
      return   Boolean;

   function    Get_Bit_Value(
                  From           : in     Two_Bytes;
                  At_Position    : in     Natural)
      return   Boolean;

   function    Get_Bit_Value(
                  From           : in     Four_Bytes;
                  At_Position    : in     Natural)
      return   Boolean;

   function    Get_Bit_Value(
                  From           : in     Eight_Bytes;
                  At_Position    : in     Natural)
      return   Boolean;

   --+---[Set_Bit_Value]------------------------------------------------
   --|
   --|   Purpose:
   --|   Allows to set an individual bit value inside of a  basic
   --|   modular type value. Four overloaded forms are provided: one for
   --|   each of the different basic modular types declared in this
   --|   package.
   --|
   --|   Arguments:
   --|   Into              Either a Byte, Two_Bytes, Four_Bytes, or
   --|                     Eight_Bytes value, depending on the
   --|                     overloaded form, for which the value of a
   --|                     specific bit is to be set.
   --|   At_Position       Natural number that specifies the position of
   --|                     the bit to set. Positions are zero based
   --|                     being 0 the least significant bit. If the
   --|                     value of At_Position is greater than the
   --|                     maximum possible bit position of Into, no
   --|                     operation is performed on Into.
   --|   To                Boolean value that determines the value the
   --|                     bit is to be set: False -> 0, True -> 1.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Set_Bit_Value(
                  Into           : in out Byte;
                  At_Position    : in     Natural;
                  To             : in     Boolean);

   procedure   Set_Bit_Value(
                  Into           : in out Two_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean);

   procedure   Set_Bit_Value(
                  Into           : in out Four_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean);

   procedure   Set_Bit_Value(
                  Into           : in out Eight_Bytes;
                  At_Position    : in     Natural;
                  To             : in     Boolean);

   ---------------------------------------------------------------------
   -- Converting to hexadecimal strings for formatted output
   ---------------------------------------------------------------------

   --+---[To_Hex_String]------------------------------------------------
   --|
   --|   Purpose:
   --|   Returns the hexadecimal string representation of a basic
   --|   modular type value. Four overloaded forms are provided: one for
   --|   each of the different basic modular types declared in this
   --|   package.
   --|
   --|   Arguments:
   --|   Value             Either a Byte, Two_Bytes, Four_Bytes, or
   --|                     Eight_Bytes value, depending on the
   --|                     overloaded form, for which the hexadecimal
   --|                     representation is to be obtained.
   --|   Preffix           String to prepend to the hexadecimal
   --|                     representation. For example use "16#" for
   --|                     obtaining an hexadecimal literal in Ada style
   --|                     or use "0x" to obtain an hexadecimal literal
   --|                     in C style.
   --|   Suffix            String to append to the hexadecimal
   --|                     representation. For example use "#" for
   --|                     obtaining an hexadecimal literal in Ada
   --|                     style.
   --|   Digit_Case        Case of the digits in the haxadecimal
   --|                     representation.
   --|
   --|   Returned value:
   --|   String with the hexadecimal representation of the Value.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    To_Hex_String(
                  Value          : in     Byte;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String;

   function    To_Hex_String(
                  Value          : in     Two_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String;

   function    To_Hex_String(
                  Value          : in     Four_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String;

   function    To_Hex_String(
                  Value          : in     Eight_Bytes;
                  Preffix        : in     String := "";
                  Suffix         : in     String := "";
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String;

end ACF.Types;
