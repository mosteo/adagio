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
with System.WCh_Con; use System.WCh_Con;
pragma Warnings (Off); --  System.WCh_Wts is an internal GNAT unit
with System.WCh_WtS; use System.WCh_WtS;
pragma Warnings (On);

package body GNU.DB.SQLCLI.Generic_Attr.Wide_String_Attribute is

   function To_String (Object : Attribute_Value_Pair_Wide_String)
                       return String is
   begin
      return Wide_String_To_String (Object.Value, WCEM_Hex);
   end To_String;

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair_Wide_String is

      pragma Assert (MaxLength > 0);
      pragma Warnings (Off);
      Str : aliased Wide_String (1 .. Positive (MaxLength));
      pragma Warnings (On);
      Len : Base := Base (Str'Length);
   begin
      Get (Handle,
           Attribute,
           To_SQLPOINTER (Str'Address),
           Len,
           Data,
           ErrorCode);
      if Len <= 0 or else Wide_Character'Pos (Str (Str'First)) = 0 then
         return Attribute_Value_Pair_Wide_String'(Attribute => Attribute,
                                                  Len   => 0,
                                                  Value => Wide_String'(""));
      else
         return Attribute_Value_Pair_Wide_String'
           (Attribute => Attribute,
            Len      => Natural (Len),
            Value    => Str (1 .. Natural (Len)));
      end if;
   end GetAttr;

   procedure SetAttr (Handle    : in  Context;
                      AV_Pair   : in  Attribute_Value_Pair_Wide_String;
                      Data      : in  Aux;
                      ErrorCode : out SQLRETURN) is
      Length : constant Base := Base (AV_Pair.Value'Length);
   begin
      Set (Handle, AV_Pair.Attribute,
           To_SQLPOINTER (AV_Pair.Value'Address),
           Length,
           Data,
           ErrorCode);
   end SetAttr;

end GNU.DB.SQLCLI.Generic_Attr.Wide_String_Attribute;
