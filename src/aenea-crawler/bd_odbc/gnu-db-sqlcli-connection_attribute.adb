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
with GNU.DB.SQLCLI.Dispatch.Debug;
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.Debug);
with GNU.DB.SQLCLI;
pragma Elaborate_All (GNU.DB.SQLCLI);

package body GNU.DB.SQLCLI.Connection_Attribute is

   package DD is new Dispatch.Debug;
   procedure Register_String_Attributes;

   procedure Get_Connect_Attr (ConnectionHandle : in SQLHDBC;
                               Attribute        : in SQL_CONNECTION_ATTRIBUTE;
                               Value            : in SQLPOINTER;
                               Length           : in out SQLINTEGER;
                               Data             : in SQLSMALLINT;
                               ErrorCode        : access SQLRETURN)
   is
      pragma Unreferenced (Data);
      function GetConnectAttr (ConnectionHandle  : SQLHDBC;
                               Attribute         : SQL_CONNECTION_ATTRIBUTE;
                               Value             : SQLPOINTER;
                               BufferLength      : SQLINTEGER;
                               pStringLength     : access SQLINTEGER)
                               return SQLRETURN;
      pragma Import (Stdcall, GetConnectAttr, "SQLGetConnectAttr");
      Len : aliased SQLINTEGER := Length;
      RC  : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
         Length := 0;
      else
         RC  := GetConnectAttr (ConnectionHandle,
                                Attribute,
                                Value,
                                Len,
                                Len'Access);
      end if;
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      end if;
   end Get_Connect_Attr;

   procedure Set_Connect_Attr (ConnectionHandle : in SQLHDBC;
                               Attribute        : in SQL_CONNECTION_ATTRIBUTE;
                               Value            : in SQLPOINTER;
                               Length           : in SQLINTEGER;
                               Data             : in SQLSMALLINT;
                               ErrorCode        : out SQLRETURN)
   is
      pragma Unreferenced (Data);
      function SetConnectAttr (ConnectionHandle : SQLHDBC;
                               Attribute        : SQL_CONNECTION_ATTRIBUTE;
                               Value            : SQLPOINTER;
                               StringLength     : SQLINTEGER)
                               return SQLRETURN;
      pragma Import (Stdcall, SetConnectAttr, "SQLSetConnectAttr");
      RC  : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
      else
         RC := SetConnectAttr (ConnectionHandle,
                               Attribute,
                               Value,
                               Length);
      end if;
      ErrorCode := RC;
   end Set_Connect_Attr;

   function SQLGetConnectAttr
     (ConnectionHandle : SQLHDBC;
      Attribute        : SQL_CONNECTION_ATTRIBUTE;
      MaxLength        : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode        : access SQLRETURN)
     return Connection_Attribute'Class
   is
      use type Dispatch.Attr_Get_Func;
      F : constant Dispatch.Attr_Get_Func := Dispatch.Get_Func (Attribute);
   begin
      if F = null then
         Raise_SQL_Error ("SQLGetConnectAttr",
                          SQL_CONNECTION_ATTRIBUTE'Image (Attribute) &
                          Attr_Not_Supported_Msg);
      else
         return F.all (ConnectionHandle,
                       Attribute,
                       MaxLength,
                       0,
                       ErrorCode);
      end if;
   end SQLGetConnectAttr;

   function SQLGetConnectAttr
     (ConnectionHandle : SQLHDBC;
      Attribute        : SQL_CONNECTION_ATTRIBUTE;
      MaxLength        : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Connection_Attribute'Class is

      RC : aliased SQLRETURN;
      Result : constant Connection_Attribute'Class :=
        SQLGetConnectAttr (ConnectionHandle, Attribute, MaxLength, RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetConnectAttr",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      return Result;
   end SQLGetConnectAttr;

   function SQLSetConnectAttr
     (ConnectionHandle : SQLHDBC;
      AttrRec          : Connection_Attribute'Class) return SQLRETURN is

      use type Dispatch.Attr_Set_Proc;
      F : constant Dispatch.Attr_Set_Proc :=
        Dispatch.Set_Proc (AttrRec.Attribute);
      ProcName  : constant String := "SQLSetConnectAttr";
      ErrorCode : SQLRETURN;
   begin
      if F = null then
         Raise_SQL_Error
           (ProcName,
            SQL_CONNECTION_ATTRIBUTE'Image (AttrRec.Attribute) &
            Attr_Not_Supported_Msg);
      else
         F.all (ConnectionHandle, AttrRec, 0, ErrorCode);
      end if;
      return ErrorCode;
   end SQLSetConnectAttr;

   procedure SQLSetConnectAttr
     (ConnectionHandle : SQLHDBC;
      AttrRec          : Connection_Attribute'Class) is

      RC : constant SQLRETURN := SQLSetConnectAttr (ConnectionHandle,
                                                    AttrRec);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetConnectAttr",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLSetConnectAttr;

   procedure Register_String_Attributes is
   begin
      if Unicode_Attr_Flag then
         CA_WString.Register (SQL_ATTR_TRANSLATE_LIB);
         CA_WString.Register (SQL_ATTR_TRACEFILE);
         CA_WString.Register (SQL_ATTR_CURRENT_CATALOG);
      else
         CA_String.Register (SQL_ATTR_TRANSLATE_LIB);
         CA_String.Register (SQL_ATTR_TRACEFILE);
         CA_String.Register (SQL_ATTR_CURRENT_CATALOG);
      end if;
   end Register_String_Attributes;

begin
   CA_Unsigned.Register (SQL_ATTR_CONNECTION_TIMEOUT);
   CA_Unsigned.Register (SQL_ATTR_LOGIN_TIMEOUT);
   CA_Unsigned.Register (SQL_ATTR_PACKET_SIZE);
   CA_Unsigned.Register (SQL_ATTR_TRANSLATE_OPTION);
   CA_Unsigned.Register (SQL_ATTR_ENLIST_IN_XA);
   CA_Unsigned.Register (SQL_ATTR_ENLIST_IN_DTC);
   CA_Unsigned.Register (SQL_ATTR_QUIET_MODE);

   CA_Boolean.Register (SQL_ATTR_CONNECTION_DEAD);
   CA_Boolean.Register (SQL_ATTR_METADATA_ID);
   CA_Boolean.Register (SQL_ATTR_AUTO_IPD);

   Register_String_Attributes;
   Register_Initializer (Register_String_Attributes'Access);
   DD.Verify;
end GNU.DB.SQLCLI.Connection_Attribute;
