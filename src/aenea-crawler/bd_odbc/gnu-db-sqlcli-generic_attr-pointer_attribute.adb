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
with GNAT.Debug_Utilities;

package body GNU.DB.SQLCLI.Generic_Attr.Pointer_Attribute is

   package D renames GNAT.Debug_Utilities;

   function To_String (Object : Attribute_Value_Pair_Pointer)
                      return String is
      function Cvt is new Ada.Unchecked_Conversion (P_Object,
                                                    System.Address);
   begin
      return "Ptr@(" & D.Image (Cvt (Object.Value)) & ")";
   end To_String;

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair_Pointer is
      package P is new System.Address_To_Access_Conversions (P_Object);
      function Cvt is new Ada.Unchecked_Conversion (P.Object_Pointer,
                                                    P_Object);
      Value : aliased System.Address;
      Len   : Base := Base (MaxLength);
   begin
      Get (Handle,
           Attribute,
           To_SQLPOINTER (Value'Address),   --  this is ok here
           Len,
           Data,
           ErrorCode);
      return Attribute_Value_Pair_Pointer'
        (Attribute => Attribute,
         Value     => Cvt (P.To_Pointer (Value)));
   end GetAttr;

   procedure SetAttr (Handle    : in  Context;
                      AV_Pair   : in  Attribute_Value_Pair_Pointer;
                      Data      : in  Aux;
                      ErrorCode : out SQLRETURN)
   is
      package P is new System.Address_To_Access_Conversions (P_Object);
      Len : constant Base                := SQL_IS_POINTER;
      AVP : Attribute_Value_Pair_Pointer := AV_Pair;
   begin
      Set (Handle, AV_Pair.Attribute,
           To_SQLPOINTER
           (P.To_Address
            (P.Object_Pointer'(AVP.Value'Access))),
           Len,
           Data,
           ErrorCode);
   end SetAttr;

end GNU.DB.SQLCLI.Generic_Attr.Pointer_Attribute;

