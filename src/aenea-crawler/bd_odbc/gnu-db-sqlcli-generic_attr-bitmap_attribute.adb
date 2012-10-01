-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2001, 2003 Juergen Pfeifer
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
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
-------------------------------------------------------------------------------
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GNU.DB.SQLCLI.Generic_Attr.Bitmap_Attribute is

   function To_String (Object : Attribute_Value_Pair_Bitmap)
                      return String is
      type Str_Pointer is access String;
      procedure Append (X : in String);
      procedure Free is new Ada.Unchecked_Deallocation (String,
                                                        Str_Pointer);
      S : Str_Pointer;
      F : Boolean := True;

      procedure Append (X : in String) is
         O : Str_Pointer;
      begin
         O := S;
         S := new String'(O.all & X);
         Free (O);
      end Append;
      pragma Inline (Append);
   begin
      S := new String'("{");
      for I in E'Range loop
         if Object.Value (I) then
            if F then
               Append (E'Image (I));
               F := False;
            else
               Append ("," & E'Image (I));
            end if;
         end if;
      end loop;
      Append ("}");
      declare
        Str : constant String := S.all;
      begin
         Free (S);
         return Str;
      end;
   end To_String;

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair_Bitmap is
      package P_SQLUI is new
        System.Address_To_Access_Conversions (SQLUINTEGER);
      use System;
      use P_SQLUI;

      Value : aliased SQLUINTEGER;
      Len   : Base := Base (MaxLength);
      Set   : E_Bitmap := (others => False);
   begin
      Get (Handle,
           Attribute,
           To_Integer (To_Address (Object_Pointer'(Value'Access))),
           Len,
           Data,
           ErrorCode);

      for I in E_Bitmap'Range loop
         exit when Value = 0;
         if (Value / 2) /= 0 then
            Set (I) := True;
         end if;
         Value := Value / 2;
      end loop;

      return Attribute_Value_Pair_Bitmap'(Attribute => Attribute,
                                          Value     => Set);
   end GetAttr;

   procedure SetAttr (Handle    : in Context;
                      AV_Pair   : in Attribute_Value_Pair_Bitmap;
                      Data      : in Aux;
                      ErrorCode : out SQLRETURN) is
      use System;
      function Cvt is new Ada.Unchecked_Conversion (SQLUINTEGER, SQLPOINTER);
      Len : constant Base       := SQL_IS_UINTEGER;
      Val : aliased SQLUINTEGER := 0;
      Bit : SQLUINTEGER         := 1;
   begin
      for I in E_Bitmap'Range loop
         if AV_Pair.Value(I) then
            Val := Val + Bit;
         end if;
         Bit := Bit + Bit;
      end loop;
      Set (Handle, AV_Pair.Attribute,
           Cvt (Val),
           Len,
           Data,
           ErrorCode);
   end SetAttr;

end GNU.DB.SQLCLI.Generic_Attr.Bitmap_Attribute;
