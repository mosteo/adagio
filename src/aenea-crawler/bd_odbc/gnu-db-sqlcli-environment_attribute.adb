-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2002 Juergen Pfeifer
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
package body GNU.DB.SQLCLI.Environment_Attribute is

   procedure Get_Env_Attr (EnvironmentHandle : in SQLHENV;
                           Attribute         : in SQL_ENVIRONMENT_ATTRIBUTE;
                           Value             : in SQLPOINTER;
                           Length            : in out SQLINTEGER;
                           Data              : in SQLSMALLINT;
                           ErrorCode         : access SQLRETURN)
   is
      pragma Unreferenced (Data);
      function GetEnvAttr (EnvironmentHandle : SQLHENV;
                           Env_Type          : SQL_ENVIRONMENT_ATTRIBUTE;
                           ValuePtr          : SQLPOINTER;
                           Buffer_Length     : SQLINTEGER;
                           P_Length          : access SQLINTEGER)
                           return SQLRETURN;
      pragma Import (Stdcall, GetEnvAttr, "SQLGetEnvAttr");

      Len : aliased SQLINTEGER := Length;
      RC  : constant SQLRETURN := GetEnvAttr (EnvironmentHandle,
                                              Attribute,
                                              Value,
                                              Len,
                                              Len'Access);
   begin
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      end if;
   end Get_Env_Attr;

   procedure Set_Env_Attr (EnvironmentHandle : in  SQLHENV;
                           Attribute         : in  SQL_ENVIRONMENT_ATTRIBUTE;
                           Value             : in  SQLPOINTER;
                           Length            : in  SQLINTEGER;
                           Data              : in  SQLSMALLINT;
                           ErrorCode         : out SQLRETURN)
   is
      pragma Unreferenced (Data);
      function SetEnvAttr (EnvironmentHandle : SQLHENV;
                           Attribute         : SQL_ENVIRONMENT_ATTRIBUTE;
                           Value             : SQLPOINTER;
                           StringLength      : SQLINTEGER)
                           return SQLRETURN;
      pragma Import (Stdcall, SetEnvAttr, "SQLSetEnvAttr");

   begin
      ErrorCode := SetEnvAttr (EnvironmentHandle,
                               Attribute,
                               Value,
                               Length);
   end Set_Env_Attr;

   function SQLGetEnvAttr
     (EnvironmentHandle : SQLHENV;
      Attribute         : SQL_ENVIRONMENT_ATTRIBUTE;
      MaxLength         : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode         : access SQLRETURN)
     return Environment_Attribute'Class is
      use type Dispatch.Attr_Get_Func;
      F : constant Dispatch.Attr_Get_Func := Dispatch.Get_Func (Attribute);
   begin
      if F = null then
         Raise_SQL_Error ("SQLGetEnvAttr",
                          SQL_ENVIRONMENT_ATTRIBUTE'Image (Attribute) &
                          Attr_Not_Supported_Msg);
      else
         return F.all (EnvironmentHandle,
                       Attribute,
                       MaxLength,
                       0,
                       ErrorCode);
      end if;
   end SQLGetEnvAttr;

   function SQLGetEnvAttr
     (EnvironmentHandle : SQLHENV;
      Attribute         : SQL_ENVIRONMENT_ATTRIBUTE;
      MaxLength         : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Environment_Attribute'Class is

      RC : aliased SQLRETURN;
      Result : constant Environment_Attribute'Class :=
        SQLGetEnvAttr (EnvironmentHandle, Attribute, MaxLength, RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetEnvAttr",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
      return Result;
   end SQLGetEnvAttr;

   function SQLSetEnvAttr
     (EnvironmentHandle : in SQLHENV;
      AttrRec           : in Environment_Attribute'Class) return SQLRETURN
   is
      use type Dispatch.Attr_Set_Proc;
      F : constant Dispatch.Attr_Set_Proc :=
        Dispatch.Set_Proc (AttrRec.Attribute);
      RC : SQLRETURN;
   begin
      if F = null then
         Raise_SQL_Error
           ("SQLSetEnvAttr",
            SQL_ENVIRONMENT_ATTRIBUTE'Image (AttrRec.Attribute) &
            Attr_Not_Supported_Msg);
      else
         F.all (EnvironmentHandle, AttrRec, 0, RC);
      end if;
      return RC;
   end SQLSetEnvAttr;

   procedure SQLSetEnvAttr
     (EnvironmentHandle : in SQLHENV;
      AttrRec           : in Environment_Attribute'Class) is

      RC : constant SQLRETURN := SQLSetEnvAttr (EnvironmentHandle, AttrRec);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetEnvAttr",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
   end SQLSetEnvAttr;

begin
   EA_Boolean.Register (SQL_ATTR_OUTPUT_NTS);
end GNU.DB.SQLCLI.Environment_Attribute;
