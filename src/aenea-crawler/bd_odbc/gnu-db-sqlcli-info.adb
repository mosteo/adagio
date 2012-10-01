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
with Ada.Unchecked_Conversion;
with GNU.DB.SQLCLI.Dispatch.Debug;
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.Debug);
with GNU.DB.SQLCLI;
pragma Elaborate_All (GNU.DB.SQLCLI);

package body GNU.DB.SQLCLI.Info is

   package DD is new Dispatch.Debug;
   procedure Register_String_Attributes;

   ProcName  : constant String := "SQLGetInfo";
   ProcName2 : constant String := "SQLGetFunctions";

   procedure Get_Info (ConnectionHandle : in SQLHDBC;
                       InfoType         : in SQL_INFO_TYPE;
                       Value            : in SQLPOINTER;
                       Length           : in out SQLSMALLINT;
                       Data             : in SQLSMALLINT;
                       ErrorCode        : access SQLRETURN)
   is
      pragma Unreferenced (Data);
      function GetInfo (ConnectionHandle : SQLHDBC;
                        InfoType         : SQL_INFO_TYPE;
                        InfoValue        : SQLPOINTER;
                        BufferLength     : SQLSMALLINT;
                        StringLength     : access SQLSMALLINT)
                        return SQLRETURN;
      pragma Import (Stdcall, GetInfo, "SQLGetInfo");
      Len : aliased SQLSMALLINT := Length;
      RC  : SQLRETURN;
   begin
      if Len < 0 then
         Len := 0;
      end if;
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
         Length := 0;
      else
         RC := GetInfo (ConnectionHandle,
                        InfoType,
                        Value,
                        Len,
                        Len'Access);
      end if;
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      end if;
   end Get_Info;

   procedure Set_Info (ConnectionHandle : in  SQLHDBC;
                       InfoType         : in  SQL_INFO_TYPE;
                       Value            : in  SQLPOINTER;
                       Length           : in  SQLSMALLINT;
                       Data             : in  SQLSMALLINT;
                       ErrorCode        : out SQLRETURN)
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Length);
      pragma Unreferenced (Value);
      pragma Unreferenced (InfoType);
      pragma Unreferenced (ConnectionHandle);
   begin
      ErrorCode := -1;
      Raise_SQL_Error (ProcedureName => "",
                       ErrorMessage  => "Set Value not allowed");
   end Set_Info;

   function SQLGetInfo (ConnectionHandle  : SQLHDBC;
                        InfoType          : SQL_INFO_TYPE;
                        MaxLength         : SQLSMALLINT := 1024;
                        ErrorCode         : access SQLRETURN)
                       return Driver_Info'Class
   is
      use type Dispatch.Attr_Get_Func;
      F : constant Dispatch.Attr_Get_Func := Dispatch.Get_Func (InfoType);
   begin
      if F = null then
         Raise_SQL_Error (ProcName,
                          SQL_INFO_TYPE'Image (InfoType) &
                          Attr_Not_Supported_Msg);
      else
         return F.all (ConnectionHandle,
                       InfoType,
                       MaxLength,
                       0,
                       ErrorCode);
      end if;
   end SQLGetInfo;

   function SQLGetInfo (ConnectionHandle  : SQLHDBC;
                        InfoType          : SQL_INFO_TYPE;
                        MaxLength         : SQLSMALLINT := 1024)
                       return Driver_Info'Class
   is
      RC : aliased SQLRETURN;
      Result : constant Driver_Info'Class :=
        SQLGetInfo (ConnectionHandle, InfoType, MaxLength, RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => ProcName,
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      return Result;
   end SQLGetInfo;

   function SQLGetFunctions (ConnectionHandle : SQLHDBC;
                             Func             : SQL_API_FUNCTION)
                             return Boolean is
      function GetFunctions (ConnectionHandle : SQLHDBC;
                             FunctionId       : SQL_API_FUNCTION;
                             pSupported       : access SQL_BOOLEAN)
                             return SQLRETURN;
      pragma Import (Stdcall, GetFunctions, "SQLGetFunctions");

      Supported : aliased SQL_BOOLEAN;
      RC        : constant SQLRETURN := GetFunctions (ConnectionHandle,
                                                      Func,
                                                      Supported'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => ProcName2,
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      if Supported = SQL_TRUE then
         return True;
      else
         return False;
      end if;
   end SQLGetFunctions;

   procedure SQLGetFunctions
     (ConnectionHandle : in  SQLHDBC;
      Bitmap           : out SQL_API_FUNCTION_BITMAP) is
      function GetFunctions
        (ConnectionHandle : SQLHDBC;
         FunctionId       : SQLUSMALLINT;
         pSupported       : access SQL_API_FUNCTION_BITMAP)
         return SQLRETURN;
      pragma Import (Stdcall, GetFunctions, "SQLGetFunctions");

      BM : aliased SQL_API_FUNCTION_BITMAP;
      RC : constant SQLRETURN := GetFunctions (ConnectionHandle,
                                               SQL_API_ODBC3_ALL_FUNCTIONS,
                                               BM'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => ProcName2,
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      Bitmap := BM;
   end SQLGetFunctions;

   function SQLGetFunctions (Bitmap : SQL_API_FUNCTION_BITMAP;
                             Func   : SQL_API_FUNCTION) return Boolean is
      function To_Int is new Ada.Unchecked_Conversion (SQL_API_FUNCTION,
                                                       SQLUSMALLINT);
      I : constant SQLUSMALLINT := To_Int (Func) / SQLUSMALLINT'Size;
      J : constant SAFSA_Element := 2 **
        Natural ((To_Int (Func) mod SQLUSMALLINT'Size));
      E : constant SAFSA_Element := Bitmap (Natural (I)) and J;
   begin
      if E = 0 then
         return False;
      else
         return True;
      end if;
   end SQLGetFunctions;

   procedure Register_String_Attributes is
   begin
      if Unicode_Attr_Flag then
         DI_WString.Register (SQL_DATA_SOURCE_NAME);
         DI_WString.Register (SQL_DRIVER_NAME);
         DI_WString.Register (SQL_DRIVER_VER);
         DI_WString.Register (SQL_ODBC_VER);
         DI_WString.Register (SQL_SERVER_NAME);
         DI_WString.Register (SQL_SEARCH_PATTERN_ESCAPE);
         DI_WString.Register (SQL_DATABASE_NAME);
         DI_WString.Register (SQL_DBMS_NAME);
         DI_WString.Register (SQL_DBMS_VER);
         DI_WString.Register (SQL_IDENTIFIER_QUOTE_CHAR);
         DI_WString.Register (SQL_SCHEMA_TERM);
         DI_WString.Register (SQL_PROCEDURE_TERM);
         DI_WString.Register (SQL_QUALIFIER_NAME_SEPARATOR);
         DI_WString.Register (SQL_QUALIFIER_TERM);
         DI_WString.Register (SQL_TABLE_TERM);
         DI_WString.Register (SQL_USER_NAME);
         DI_WString.Register (SQL_DRIVER_ODBC_VER);
         DI_WString.Register (SQL_KEYWORDS);
         DI_WString.Register (SQL_SPECIAL_CHARACTERS);
         DI_WString.Register (SQL_DM_VER);
         DI_WString.Register (SQL_XOPEN_CLI_YEAR);
         DI_WString.Register (SQL_COLLATION_SEQ);
      else
         DI_String.Register (SQL_DATA_SOURCE_NAME);
         DI_String.Register (SQL_DRIVER_NAME);
         DI_String.Register (SQL_DRIVER_VER);
         DI_String.Register (SQL_ODBC_VER);
         DI_String.Register (SQL_SERVER_NAME);
         DI_String.Register (SQL_SEARCH_PATTERN_ESCAPE);
         DI_String.Register (SQL_DATABASE_NAME);
         DI_String.Register (SQL_DBMS_NAME);
         DI_String.Register (SQL_DBMS_VER);
         DI_String.Register (SQL_IDENTIFIER_QUOTE_CHAR);
         DI_String.Register (SQL_SCHEMA_TERM);
         DI_String.Register (SQL_PROCEDURE_TERM);
         DI_String.Register (SQL_QUALIFIER_NAME_SEPARATOR);
         DI_String.Register (SQL_QUALIFIER_TERM);
         DI_String.Register (SQL_TABLE_TERM);
         DI_String.Register (SQL_USER_NAME);
         DI_String.Register (SQL_DRIVER_ODBC_VER);
         DI_String.Register (SQL_KEYWORDS);
         DI_String.Register (SQL_SPECIAL_CHARACTERS);
         DI_String.Register (SQL_DM_VER);
         DI_String.Register (SQL_XOPEN_CLI_YEAR);
         DI_String.Register (SQL_COLLATION_SEQ);
      end if;
   end Register_String_Attributes;

begin
   --  All String attributes
   Register_String_Attributes;
   Register_Initializer (Register_String_Attributes'Access);

   --  All SQLUSMALLINT attributes
   DI_USmallint.Register (SQL_MAXIMUM_DRIVER_CONNECTIONS);
   DI_USmallint.Register (SQL_MAXIMUM_CONCURRENT_ACTIVITIES);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMN_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_CURSOR_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_SCHEMA_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_PROCEDURE_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_CATALOG_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_TABLE_NAME_LENGTH);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMNS_IN_GROUP_BY);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMNS_IN_INDEX);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMNS_IN_ORDER_BY);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMNS_IN_SELECT);
   DI_USmallint.Register (SQL_MAXIMUM_COLUMNS_IN_TABLE);
   DI_USmallint.Register (SQL_MAXIMUM_TABLES_IN_SELECT);
   DI_USmallint.Register (SQL_MAXIMUM_USER_NAME_LENGTH);
   DI_USmallint.Register (SQL_ACTIVE_ENVIRONMENTS);
   DI_USmallint.Register (SQL_MAXIMUM_IDENTIFIER_LENGTH);

   --  All Context attributes
   DI_Context.Register (SQL_DRIVER_HDBC);
   DI_Context.Register (SQL_DRIVER_HENV);
   DI_Context.Register (SQL_DRIVER_HSTMT);
   DI_Context.Register (SQL_DRIVER_HLIB);
   DI_Context.Register (SQL_DRIVER_HDESC);

   --  All SQLUINTEGER Attributes
   DI_UInteger.Register (SQL_MAXIMUM_INDEX_SIZE);
   DI_UInteger.Register (SQL_MAXIMUM_ROW_SIZE);
   DI_UInteger.Register (SQL_MAXIMUM_STATEMENT_LENGTH);
   DI_UInteger.Register (SQL_MAXIMUM_CHAR_LITERAL_LENGTH);
   DI_UInteger.Register (SQL_MAX_BINARY_LITERAL_LEN);
   DI_UInteger.Register (SQL_MAX_ASYNC_CONCURRENT_STATEMENTS);

   --  All Boolean_String Attributes (Y/N)
   DI_Boolean_String.Register (SQL_OUTER_JOINS);
   DI_Boolean_String.Register (SQL_ROW_UPDATES);
   DI_Boolean_String.Register (SQL_ACCESSIBLE_TABLES);
   DI_Boolean_String.Register (SQL_ACCESSIBLE_PROCEDURES);
   DI_Boolean_String.Register (SQL_PROCEDURES);
   DI_Boolean_String.Register (SQL_DATA_SOURCE_READ_ONLY);
   DI_Boolean_String.Register (SQL_EXPRESSIONS_IN_ORDERBY);
   DI_Boolean_String.Register (SQL_MULTIPLE_RESULT_SETS);
   DI_Boolean_String.Register (SQL_MULTIPLE_ACTIVE_TRANSACTIONS);
   DI_Boolean_String.Register (SQL_INTEGRITY);
   DI_Boolean_String.Register (SQL_COLUMN_ALIAS);
   DI_Boolean_String.Register (SQL_ORDER_BY_COLUMNS_IN_SELECT);
   DI_Boolean_String.Register (SQL_MAXIMUM_ROW_SIZE_INCLUDES_LONG);
   DI_Boolean_String.Register (SQL_NEED_LONG_DATA_LEN);
   DI_Boolean_String.Register (SQL_LIKE_ESCAPE_CLAUSE);
   DI_Boolean_String.Register (SQL_DESCRIBE_PARAMETER);
   DI_Boolean_String.Register (SQL_CATALOG_NAME);

   Dsp_Conversion_Target.Register (SQL_CONVERT_BINARY);
   Dsp_Conversion_Target.Register (SQL_CONVERT_BIT);
   Dsp_Conversion_Target.Register (SQL_CONVERT_CHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_DATE);
   Dsp_Conversion_Target.Register (SQL_CONVERT_DECIMAL);
   Dsp_Conversion_Target.Register (SQL_CONVERT_DOUBLE);
   Dsp_Conversion_Target.Register (SQL_CONVERT_FLOAT);
   Dsp_Conversion_Target.Register (SQL_CONVERT_INTEGER);
   Dsp_Conversion_Target.Register (SQL_CONVERT_LONGVARCHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_NUMERIC);
   Dsp_Conversion_Target.Register (SQL_CONVERT_REAL);
   Dsp_Conversion_Target.Register (SQL_CONVERT_SMALLINT);
   Dsp_Conversion_Target.Register (SQL_CONVERT_TIME);
   Dsp_Conversion_Target.Register (SQL_CONVERT_TIMESTAMP);
   Dsp_Conversion_Target.Register (SQL_CONVERT_TINYINT);
   Dsp_Conversion_Target.Register (SQL_CONVERT_VARBINARY);
   Dsp_Conversion_Target.Register (SQL_CONVERT_VARCHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_WCHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_INTERVAL_DAY_TIME);
   Dsp_Conversion_Target.Register (SQL_CONVERT_INTERVAL_YEAR_MONTH);
   Dsp_Conversion_Target.Register (SQL_CONVERT_WLONGVARCHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_WVARCHAR);
   Dsp_Conversion_Target.Register (SQL_CONVERT_LONGVARBINARY);

   Dsp_Cursor_Attribute1.Register (SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1);
   Dsp_Cursor_Attribute1.Register (SQL_KEYSET_CURSOR_ATTRIBUTES1);
   Dsp_Cursor_Attribute1.Register (SQL_STATIC_CURSOR_ATTRIBUTES1);

   Dsp_Cursor_Attribute2.Register (SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2);
   Dsp_Cursor_Attribute2.Register (SQL_KEYSET_CURSOR_ATTRIBUTES2);
   Dsp_Cursor_Attribute2.Register (SQL_STATIC_CURSOR_ATTRIBUTES2);

   Dsp_Cursor_Commit_Behavior.Register (SQL_CURSOR_ROLLBACK_BEHAVIOR);

   Dsp_Identifier_Case.Register (SQL_QUOTED_IDENTIFIER_CASE);

   Dsp_Time_Interval_Function.Register (SQL_TIMEDATE_DIFF_INTERVALS);
   DD.Verify;
end GNU.DB.SQLCLI.Info;
