-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2003 Juergen Pfeifer
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
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body GNU.DB.SQLCLI.Generic_Attr.Boolean_Attribute is

   function To_String (Object : Attribute_Value_Pair_Boolean_Scalar)
                      return String is
   begin
      return Boolean'Image (Object.Value);
   end To_String;

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair_Boolean_Scalar
   is
      pragma Unreferenced (MaxLength);
      package P is new System.Address_To_Access_Conversions (Bool_Type);

      Value : aliased Bool_Type;
      Len   : Base := Default_Len;
      Res   : Boolean := False;
   begin
      Get (Handle,
           Attribute,
           To_SQLPOINTER (P.To_Address
                          (P.Object_Pointer'(Value'Access))),
           Len,
           Data,
           ErrorCode);
      if Value /= 0 then
         Res := True;
      end if;
      return Attribute_Value_Pair_Boolean_Scalar'
        (Attribute => Attribute,
         Value     => Res);
   end GetAttr;

   procedure SetAttr (Handle    : in Context;
                      AV_Pair   : in Attribute_Value_Pair_Boolean_Scalar;
                      Data      : in Aux;
                      ErrorCode : out SQLRETURN)
   is
      function Cvt is new Ada.Unchecked_Conversion (SQLINTEGER, SQLPOINTER);
      Len : constant Base     := Default_Len;
      Val : aliased Bool_Type := 0;
   begin
      if AV_Pair.Value then
         Val := 1;
      end if;
      Set (Handle, AV_Pair.Attribute,
           Cvt (SQLINTEGER (Val)),
           Len,
           Data,
           ErrorCode);
   end SetAttr;

end GNU.DB.SQLCLI.Generic_Attr.Boolean_Attribute;
