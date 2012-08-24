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
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;

package body GNU.DB.SQLCLI.Generic_Attr.Enumerated_Attribute is

   function To_String (Object : Attribute_Value_Pair_Enum) return String is
   begin
      return E'Image (Object.Value);
   end To_String;

   function To_E    is new Ada.Unchecked_Conversion (E_Base, E);
   function To_Base is new Ada.Unchecked_Conversion (E, E_Base);

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair_Enum is
      pragma Unreferenced (MaxLength);
      package P is new System.Address_To_Access_Conversions (E_Base);
      Value   : aliased E_Base;
      AttrVal : E;
      Len     : Base := Default_Len;
   begin
      Get (Handle,
           Attribute,
           To_SQLPOINTER
           (P.To_Address (P.Object_Pointer'(Value'Access))),
           Len,
           Data,
           ErrorCode);
      AttrVal := To_E (Value);
      if Is_SQL_Ok (ErrorCode.all) then
         if not AttrVal'Valid then
            Raise_Invalid_Enum (ProcedureName => "EnumeratedAttribute.GetAttr",
                                EnumName      => E_Name,
                                EnumValue     =>
                                  E_Base'Image (To_Base (AttrVal)));
         end if;
      else
         --  we have to make sure to return a valid value, so in case
         --  of an error and an invalid enumeration value we set it to
         --  the first value of the enumeration
         if not AttrVal'Valid then
            AttrVal := E'First;
         end if;
      end if;
      return Attribute_Value_Pair_Enum'(Attribute => Attribute,
                                        Value     => AttrVal);
   end GetAttr;

   procedure SetAttr (Handle    : in  Context;
                      AV_Pair   : in  Attribute_Value_Pair_Enum;
                      Data      : in  Aux;
                      ErrorCode : out SQLRETURN)
   is
      Len : constant Base           := Default_Len;
      Val : aliased constant E_Base := To_Base (AV_Pair.Value);

      use System.Storage_Elements;
   begin
      --  Despite what the ODBC API says, in this case 'Value' must be
      --  the _value_ of the attribute, not a pointer to the value!
      Set (Handle,
           AV_Pair.Attribute,
           To_SQLPOINTER (To_Address (Integer_Address (Val))),
           Len,
           Data,
           ErrorCode);
   end SetAttr;

end GNU.DB.SQLCLI.Generic_Attr.Enumerated_Attribute;

