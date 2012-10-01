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

package body GNU.DB.SQLCLI.Statement_Attribute is

   package DD is new Dispatch.Debug;
   procedure Register_String_Attributes;

   procedure Get_Stmt_Attr (StatementHandle : in SQLHSTMT;
                            Attribute       : in SQL_STATEMENT_ATTRIBUTE;
                            Value           : in SQLPOINTER;
                            Length          : in out SQLINTEGER;
                            Data            : in  SQLSMALLINT;
                            ErrorCode       : access SQLRETURN)
   is
      pragma Unreferenced (Data);
      function GetStmtAttr (StatementHandle : SQLHSTMT;
                            Attribute       : SQL_STATEMENT_ATTRIBUTE;
                            Value           : SQLPOINTER;
                            BufferLength    : SQLINTEGER;
                            pStringLength   : access SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, GetStmtAttr, "SQLGetStmtAttr");
      Len : aliased SQLINTEGER := Length;
      RC  : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
         Length := 0;
      else
         RC := GetStmtAttr (StatementHandle,
                            Attribute,
                            Value,
                            Len,
                            Len'Access);
      end if;
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      end if;
   end Get_Stmt_Attr;

   procedure Set_Stmt_Attr (StatementHandle : in  SQLHSTMT;
                            Attribute       : in  SQL_STATEMENT_ATTRIBUTE;
                            Value           : in  SQLPOINTER;
                            Length          : in  SQLINTEGER;
                            Data            : in  SQLSMALLINT;
                            ErrorCode       : out SQLRETURN)
   is
      pragma Unreferenced (Data);
      function SetStmtAttr (StatementHandle : SQLHSTMT;
                            Attribute       : SQL_STATEMENT_ATTRIBUTE;
                            Value           : SQLPOINTER;
                            StringLength    : SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, SetStmtAttr, "SQLSetStmtAttr");
      RC  : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
      else
         RC := SetStmtAttr (StatementHandle,
                            Attribute,
                            Value,
                            Length);
      end if;
      ErrorCode := RC;
   end Set_Stmt_Attr;

   function SQLGetStmtAttr
     (StatementHandle : SQLHSTMT;
      Attribute       : SQL_STATEMENT_ATTRIBUTE;
      MaxLength       : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode       : access SQLRETURN)
     return Statement_Attribute'Class is
      use type Dispatch.Attr_Get_Func;
      F : constant Dispatch.Attr_Get_Func := Dispatch.Get_Func (Attribute);
   begin
      if F = null then
            Raise_SQL_Error ("SQLGetStmtAttr",
                             SQL_STATEMENT_ATTRIBUTE'Image (Attribute) &
                             Attr_Not_Supported_Msg);
      else
         return F.all (StatementHandle,
                       Attribute,
                       MaxLength,
                       0,
                       ErrorCode);
      end if;
   end SQLGetStmtAttr;

   function SQLGetStmtAttr
     (StatementHandle : SQLHSTMT;
      Attribute       : SQL_STATEMENT_ATTRIBUTE;
      MaxLength       : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Statement_Attribute'Class is

      RC : aliased SQLRETURN;
      Result : constant Statement_Attribute'Class :=
        SQLGetStmtAttr (StatementHandle, Attribute, MaxLength, RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetStmtAttr",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return Result;
   end SQLGetStmtAttr;

   function SQLSetStmtAttr (StatementHandle : in SQLHSTMT;
                            AV_Pair         : in Statement_Attribute'Class)
                           return SQLRETURN
   is
      use type Dispatch.Attr_Set_Proc;
      F : constant Dispatch.Attr_Set_Proc :=
        Dispatch.Set_Proc (AV_Pair.Attribute);
      ErrorCode : SQLRETURN;
   begin
      if F = null then
         Raise_SQL_Error
           ("SQLSetStmtAttr",
            SQL_STATEMENT_ATTRIBUTE'Image (AV_Pair.Attribute) &
            Attr_Not_Supported_Msg);
      else
         F.all (StatementHandle, AV_Pair, 0, ErrorCode);
      end if;
      return ErrorCode;
   end SQLSetStmtAttr;

   procedure SQLSetStmtAttr (StatementHandle : in SQLHSTMT;
                             AV_Pair         : in Statement_Attribute'Class)
   is
      RC : constant SQLRETURN := SQLSetStmtAttr (StatementHandle, AV_Pair);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetStmtAttr",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSetStmtAttr;

   procedure Register_String_Attributes is
   begin
      if Unicode_Attr_Flag then
         null;
      else
         null;
      end if;
   end Register_String_Attributes;

begin
   SA_Unsigned.Register (SQL_ROWSET_SIZE);
   SA_Unsigned.Register (SQL_ATTR_ROW_BIND_TYPE);
   SA_Unsigned.Register (SQL_ATTR_PARAM_BIND_TYPE);
   SA_Unsigned.Register (SQL_ATTR_ROW_NUMBER);
   SA_Unsigned.Register (SQL_ATTR_KEYSET_SIZE);
   SA_Unsigned.Register (SQL_ATTR_MAX_LENGTH);
   SA_Unsigned.Register (SQL_ATTR_MAX_ROWS);
   SA_Unsigned.Register (SQL_ATTR_PARAMSET_SIZE);
   SA_Unsigned.Register (SQL_ATTR_QUERY_TIMEOUT);
   SA_Unsigned.Register (SQL_ATTR_ROW_ARRAY_SIZE);
   SA_Unsigned.Register (SQL_ATTR_FETCH_BOOKMARK_PTR);

   SA_PointerSUInt.Register (SQL_ATTR_ROW_STATUS_PTR);
   SA_PointerSUInt.Register (SQL_ATTR_PARAM_STATUS_PTR);
   SA_PointerSUInt.Register (SQL_ATTR_PARAM_OPERATION_PTR);
   SA_PointerSUInt.Register (SQL_ATTR_ROW_OPERATION_PTR);

   SA_PointerUInt.Register (SQL_ATTR_PARAMS_PROCESSED_PTR);
   SA_PointerUInt.Register (SQL_ATTR_ROW_BIND_OFFSET_PTR);
   SA_PointerUInt.Register (SQL_ATTR_ROWS_FETCHED_PTR);
   SA_PointerUInt.Register (SQL_ATTR_PARAM_BIND_OFFSET_PTR);

   SA_Context.Register (SQL_ATTR_APP_PARAM_DESC);
   SA_Context.Register (SQL_ATTR_APP_ROW_DESC);
   SA_Context.Register (SQL_ATTR_IMP_ROW_DESC);
   SA_Context.Register (SQL_ATTR_IMP_PARAM_DESC);

   SA_Boolean.Register (SQL_ATTR_METADATA_ID);
   SA_Boolean.Register (SQL_ATTR_ENABLE_AUTO_IPD);

   Register_String_Attributes;
   Register_Initializer (Register_String_Attributes'Access);
   DD.Verify;
end GNU.DB.SQLCLI.Statement_Attribute;
