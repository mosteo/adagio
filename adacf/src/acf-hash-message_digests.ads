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
--    File name         : acf-hash-message_digests.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : November 20th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Provides a type definition and operations on values of that type
-- to handle the message digest produced by the hashing algorithms
-- implemented in the ACF.
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

with Ada.Finalization;
with ACF.Types;                     use ACF.Types;

package ACF.Hash.Message_Digests is

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Message_Digest]-----------------------------------------------
   --|   Type for handling the message digests produced by the hashing
   --|   algorithms implemented in the ACF.
   --+------------------------------------------------------------------

   type Message_Digest is private;

   ---------------------------------------------------------------------
   -- Constants
   ---------------------------------------------------------------------

   --+---[Null_Message_Digest]------------------------------------------
   --|   Constant that represents a null message digest object.
   --+------------------------------------------------------------------

   Null_Message_Digest     : constant Message_Digest;

   ---------------------------------------------------------------------
   -- Subprogram specifications
   ---------------------------------------------------------------------

   --+---[To_Message_Digest]--------------------------------------------
   --|   Purpose:
   --|   Builds and returns a Message_Digest value from a Byte_Array.
   --|
   --|   Arguments:
   --|   Source            Byte_Array value from which the
   --|                     Message_Digest will be built.
   --|
   --|   Returned value:
   --|   Returns a Message_Digest value built out of the Source
   --|   Byte_Array. If Source'Length is 0 the function will return
   --|   a Null_Message_Digest.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if fails the allocation of the necessary
   --|      space.
   --+------------------------------------------------------------------

   function    To_Message_Digest(
                  Source         : in     Byte_Array)
      return   Message_Digest;

   --+---[To_Message_Digest]--------------------------------------------
   --|   Purpose:
   --|   Builds and returns a Message_Digest value from an hexadecimal
   --|   String that represents a byte array.
   --|
   --|   Arguments:
   --|   Source            String that contains the hexadecimal
   --|                     representation of a byte array. String
   --|                     length must be 0 or a multiple of 2. Each
   --|                     byte is represented by a pair of hexadecimal
   --|                     digits. Hexadecimal digit case is irrelevant.
   --|                     No whitespace, or other separators or
   --|                     format effectors are allowed at the
   --|                     beginning and the end of the string or
   --|                     between hex digits.
   --|
   --|   Returned value:
   --|   Returns a Message_Digest value built out of the Source
   --|   String. If Source'Length is 0 the function will return
   --|   a Null_Message_Digest.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if fails the allocation of the necessary
   --|      space.
   --|   ACF_Syntax_Error if Source does not conform the syntax
   --|      conventions or contains invalid hexadecimal digit
   --|      characters.
   --+------------------------------------------------------------------

   function    To_Message_Digest(
                  Source         : in     String)
      return   Message_Digest;

   --+---[Set_Message_Digest]-------------------------------------------
   --|   Purpose:
   --|   Sets the Message_Digest to the sequence of bytes contained in
   --|   a Byte_Array.
   --|
   --|   Arguments:
   --|   The_Digest        Message_Digest object to set.
   --|   From              Byte_Array to set the Message_Digest to.
   --|
   --|   Exceptions:
   --|   ACF_Storage_Error if fails the allocation of the necessary
   --|      space. In case this exception is raised The_Digest is
   --|      left unmodified.
   --+------------------------------------------------------------------

   procedure   Set_Message_Digest(
                  The_Digest     : in out Message_Digest;
                  From           : in     Byte_Array);

   --+---[Clear]--------------------------------------------------------
   --|   Purpose:
   --|   Zeroizes a Message_Digest object.
   --|
   --|   Arguments:
   --|   The_Digest        Message_Digest object to clear.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Clear(
                  The_Digest     : in out Message_Digest);

   --+---[To_Byte_Array]------------------------------------------------
   --|   Purpose:
   --|   Returns the byte array with the bytes of a Message_Digest.
   --|
   --|   Arguments:
   --|   From_Digest       Message_Digest object to obtain the byte
   --|                     array from.
   --|
   --|   Returned value:
   --|   Returns a Byte_Array with the sequence of bytes corresponding
   --|   to the Message_Digest. The array index is always 1 based.
   --|
   --|   Exceptions:
   --|   ACF_Null_Argument_Error is From_Digest is a
   --|      Null_Message_Digest.
   --+------------------------------------------------------------------

   function    To_Byte_Array(
                  From_Digest    : in     Message_Digest)
      return   Byte_Array;

   --+---[To_Hex_String]------------------------------------------------
   --|   Purpose:
   --|   Returns a string containing the hexadecimal representation of
   --|   a message digest.
   --|
   --|   Arguments:
   --|   From_Digest       Message_Digest to obtain the hex string
   --|                     representation from.
   --|   Digit_Case        Case of the digits in the haxadecimal
   --|                     representation (see ACF.Types).
   --|
   --|   Returned value:
   --|   String with the hexadecimal representation of the message
   --|   digest.
   --|
   --|   Exceptions:
   --|   ACF_Null_Argument_Error is From_Digest is a
   --|      Null_Message_Digest.
   --+------------------------------------------------------------------

   function    To_Hex_String(
                  From_Digest    : in     Message_Digest;
                  Digit_Case     : in     Hex_Digit_Case := Upper_Case)
      return   String;

   --+---["="]----------------------------------------------------------
   --|   Purpose:
   --|   Equality test for message digests.
   --|
   --|   Arguments:
   --|   Left, Right       Message_Digest objects to compare.
   --|
   --|   Returned value:
   --|   Boolean value with the result of comparision.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    "="(
                  Left           : in     Message_Digest;
                  Right          : in     Message_Digest)
      return   Boolean;

   --+---[Get_Size]-----------------------------------------------------
   --|   Purpose:
   --|   Returns the size in bytes of a Message_Digest object.
   --|
   --|   Arguments:
   --|   Of_Digest         Message_Digest to obtain the size from.
   --|
   --|   Returned value:
   --|   Natural value with the message digest length (0 if it is a
   --|   Null_Message_Digest.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Get_Size(
                  Of_Digest      : in     Message_Digest)
      return   Natural;

   ---------------------------------------------------------------------
   -- Private part
   ---------------------------------------------------------------------

private

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Byte_Array_Ptr]-----------------------------------------------
   --|   Access type to Byte_Array values.
   --+------------------------------------------------------------------

   type Byte_Array_Ptr is access all Byte_Array;

   --+---[Message_Digest]-----------------------------------------------
   --|   Type for handling the message digests produced by the hashing
   --|   algorithms implemented in the ACF. It is a type extension of
   --|   the Ada.Finalization.Controlled tagged type that contains the
   --|   following fields:
   --|
   --|   Bytes             Reference to the Byte_Array that contains the
   --|                     message digest bytes.
   --+------------------------------------------------------------------

   type Message_Digest is new Ada.Finalization.Controlled with
      record
         Bytes                   : Byte_Array_Ptr := null;
      end record;

   ---------------------------------------------------------------------
   -- Subprograms
   ---------------------------------------------------------------------

   --+---[Methods inherited of Ada.Dinalization.Controlled]-------------
   --|   Next three subprograms are the methods inherited from
   --|   Ada.Finalization.Controlled that control the initialization,
   --|   assignment, and finalization of Message_Digest objects.
   --+------------------------------------------------------------------

   procedure   Initialize(
                  Object         : in out Message_Digest);

   procedure   Adjust(
                  Object         : in out Message_Digest);

   procedure   Finalize(
                  Object         : in out Message_Digest);

   ---------------------------------------------------------------------
   -- Constants
   ---------------------------------------------------------------------

   --+---[Null_Message_Digest]------------------------------------------
   --|   Constant that represents a null message digest object.
   --+------------------------------------------------------------------

   Null_Message_Digest     : constant Message_Digest :=
                                 (Ada.Finalization.Controlled with
                                       Bytes => null);
end ACF.Hash.Message_Digests;