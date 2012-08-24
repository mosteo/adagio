-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/esql/gnu-db-esql_support.adb,v $
--  Description     : Simple ODBC interface for the esql translator          --
--  Author          : Michael Erdmann                                        --
--  Created         : 22.12.2000                                             --
--  Last Modified By: $Author: stephen_leake $
--  Last Modified On: $Date: 2003/09/28 17:35:38 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000 - 2003 Michael Erdmann                                --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains procedures to adopt the SQL_STANDARD data          --
--  type to the Ada Style data types.                                        --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;               use Ada.Text_IO;
with System;                    use System;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;

with GNU.DB.SQLCLI;             use GNU.DB.SQLCLI;

package body GNU.DB.ESQL_Support is

   Version : constant String :=
      "$Id: gnu-db-esql_support.adb,v 1.39 2003/09/28 17:35:38 stephen_leake Exp $";
   pragma Unreferenced (Version);

   package SQLCA is new System.Address_To_Access_Conversions (Object => SQLCA_Type);
   use SQLCA;

   function To_Char is new Ada.Unchecked_Conversion
        (Source => SQL.SQLCHAR, Target => Character);

   function To_SQLCHAR is new Ada.Unchecked_Conversion
        (Target => SQL.SQLCHAR, Source  => Character);

   ---------------
   -- To_String --
   ---------------
   procedure To_String (
      Item   : in SQL_STANDARD.CHAR;
      Target : out String) is
      -- Copy SQL characters to Ada string and pad the result with spaces
      Next       : Natural := Target'First;
      Ch         : Character;
   begin
      for I in Item'Range loop
         Ch := To_Char (Item (I));
         exit when Ch = Character'Val (0);
         if Next not in Target'Range then
            raise Constraint_Error;
         end if;
         Target (Next) := Ch;
         Next          := Next + 1;
      end loop;

      -- Fill the rest of Target with spaces
      for I in Next .. Target'Last loop
         Target (I) := Character'Val (32);
      end loop;
   end To_String;

   ---------------
   -- To_String --
   ---------------
   function To_String (
      Item : SQL_STANDARD.CHAR) return String is
      -- Convert to Ada string
      Result : String (1 .. Item'Length);
   begin
      To_String (Item, Result);
      return Result;
   end To_String;

   ----------
   -- Move --
   ----------
   procedure Move (
      S : in String;
      C : out SQL_STANDARD.CHAR ) is
      -- copy from string to sql char
      L : constant Positive := S'Length ;
   begin
     for I in C'Range loop
       if I > L
       then
         C(I) := To_SQLCHAR(Character'Val(32));
       else
         C(I) := To_SQLCHAR(S(I));
       end if;
     end loop;
   end Move;

   -------------
   -- Is_Null --
   -------------
   function Is_Null (
      s : SQL_STANDARD.INDICATOR_TYPE ) return Boolean is
      -- Return true if the indicator type indicates a null value.
   begin
      return s = SQL_NULL_DATA;
   end Is_Null;

   -----------
   -- DEBUG --
   -----------
   procedure DEBUG(
      Where : in String;
      Query : in String;
      RC    : in System.Address) is
      --
      -- This procedure generates debugging information. It is
      -- invoked directly after an execute.
      --
      SCA : constant SQLCA_Access := SQLCA_Access(To_Pointer( RC ));
   begin
      Put_Line( Where );
      Put_Line( Query );
      Put_Line( "SQL State : " & SCA.State  &
                " SQL Code :" & SQLCODE_TYPE'Image(SCA.SqlCode) );
      Put_Line( "---------------");
   end DEBUG;

   ---------------
   -- To_String --
   ---------------
   function To_String(
      This : in GNADE.VARCHAR ) return String is
      Len  : Positive := 1 ;
   begin
      if not Is_Null( This.Length ) and This.Length > 0 then
         Len := Positive( This.Length );
         return To_String( This.Value(1..Len) );
      else
         return "";
      end if;

   end To_String;

   ----------------
   -- To_VARCHAR --
   ----------------
   procedure To_VARCHAR(
      Value  : in  String;
      Result : out GNADE.VARCHAR ) is
      Len    : constant Positive := Value'Length;
   begin
      Move( Value, Result.Value );
      Result.Length := INDICATOR_TYPE(Len);
   end To_VARCHAR;

   -------------
   -- Is_Null --
   -------------
   function Is_Null(
      This : in GNADE.VARCHAR ) return Boolean is
   begin
      return Is_Null( This.Length );
   end Is_Null;

   ------------
   -- Length --
   ------------
   function Length(
      This : in GNADE.VARCHAR ) return Natural is
   begin
      if not Is_Null( This.Length ) then
         return Integer( This.Length );
      else
         return 0;
      end if;
   end Length;

   ---------------
   -- To_Binary --
   ---------------
   function To_Binary(
      Src    : in String ) return GNADE.BINARY is
      Result : GNADE.BINARY(1..Src'Length);
      Dest   : Positive := 1;
   begin
      for I in Src'Range loop
         Result(Dest) := To_SQLCHAR(  Src(I) );
         Dest := Dest + 1;
      end loop;

      return Result;
   end To_Binary;

   ---------------
   -- To_Binary --
   ---------------
   function To_Binary(
      This   : in GNADE.VARBINARY ) return GNADE.BINARY is
      Len  : Positive := 1 ;
   begin
      if not Is_Null( This.Length ) and This.Length > 0 then
         Len := Positive( This.Length );
         return This.Value(1..Len);
      else
         raise Is_Empty;
      end if;
   end To_Binary;

   ---------------
   -- To_Binary --
   ---------------
   procedure To_Binary(
      This   : in GNADE.VARBINARY;
      Dest   : out GNADE.BINARY;
      Len    : out Positive ) is
      J      : Positive := Dest'First;
   begin
      if not Is_Null( This.Length ) and This.Length > 0 then
         Len  := Positive( This.Length );
         for I in 1..Len loop
            Dest(J) := This.Value(I);

            J := J + 1;
            exit when not ( J in Dest'Range ) ;
         end loop;
      else
         raise Is_Empty;
      end if;
   end To_Binary;

   ------------------
   -- To_VARBINARY --
   ------------------
   procedure To_VARBINARY(
      Value  : in GNADE.BINARY;
      Result : out GNADE.VARBINARY ) is
      Dest   : Natural := 1;
   begin
      for Src in Value'Range loop
         Result.Value(Dest):= Value(Src);
         Dest := Dest + 1;
      end loop;

      Result.Length := INDICATOR_TYPE( Dest-1 );
   end To_VARBINARY;

   ------------
   -- Length --
   ------------
   function Length(
      This   : in GNADE.VARBINARY ) return Natural is
   begin
      if not Is_Null( This.Length ) then
         return Integer( This.Length );
      else
         return 0;
      end if;
   end Length;

end GNU.DB.ESQL_Support;


