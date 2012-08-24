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
with GNU.DB.SQLCLI.Connection_Attribute;

with GNU.DB.SQLCLI.Generic_Attr;
with GNU.DB.SQLCLI.Dispatch;
with GNU.DB.SQLCLI.Dispatch.A_Unsigned;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Wide_String;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
with GNU.DB.SQLCLI.Dispatch.A_Boolean;
with GNU.DB.SQLCLI.Dispatch.A_Context;
with GNU.DB.SQLCLI.Dispatch.A_Pointer;

pragma Elaborate_All (GNU.DB.SQLCLI.Generic_Attr);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Unsigned);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Wide_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Enumerated);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Boolean);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Context);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Pointer);

package GNU.DB.SQLCLI.Statement_Attribute is

   type SQL_STATEMENT_ATTRIBUTE is (SQL_ATTR_CURSOR_SENSITIVITY,
                                    SQL_ATTR_CURSOR_SCROLLABLE,
                                    SQL_ATTR_QUERY_TIMEOUT,
                                    SQL_ATTR_MAX_ROWS,
                                    SQL_ATTR_NOSCAN,
                                    SQL_ATTR_MAX_LENGTH,
                                    SQL_ATTR_ASYNC_ENABLE,
                                    SQL_ATTR_ROW_BIND_TYPE,
                                    SQL_ATTR_CURSOR_TYPE,
                                    SQL_ATTR_CONCURRENCY,
                                    SQL_ATTR_KEYSET_SIZE,
                                    SQL_ROWSET_SIZE,
                                    SQL_ATTR_SIMULATE_CURSOR,
                                    SQL_ATTR_RETRIEVE_DATA,
                                    SQL_ATTR_USE_BOOKMARKS,
                                    SQL_ATTR_ROW_NUMBER,
                                    SQL_ATTR_ENABLE_AUTO_IPD,
                                    SQL_ATTR_FETCH_BOOKMARK_PTR,
                                    SQL_ATTR_PARAM_BIND_OFFSET_PTR,
                                    SQL_ATTR_PARAM_BIND_TYPE,
                                    SQL_ATTR_PARAM_OPERATION_PTR,
                                    SQL_ATTR_PARAM_STATUS_PTR,
                                    SQL_ATTR_PARAMS_PROCESSED_PTR,
                                    SQL_ATTR_PARAMSET_SIZE,
                                    SQL_ATTR_ROW_BIND_OFFSET_PTR,
                                    SQL_ATTR_ROW_OPERATION_PTR,
                                    SQL_ATTR_ROW_STATUS_PTR,
                                    SQL_ATTR_ROWS_FETCHED_PTR,
                                    SQL_ATTR_ROW_ARRAY_SIZE,
                                    SQL_ATTR_APP_ROW_DESC,
                                    SQL_ATTR_APP_PARAM_DESC,
                                    SQL_ATTR_IMP_ROW_DESC,
                                    SQL_ATTR_IMP_PARAM_DESC,
                                    SQL_ATTR_METADATA_ID);
   for SQL_STATEMENT_ATTRIBUTE use (SQL_ATTR_CURSOR_SENSITIVITY    => -2,
                                    SQL_ATTR_CURSOR_SCROLLABLE     => -1,
                                    SQL_ATTR_QUERY_TIMEOUT         => 0,
                                    SQL_ATTR_MAX_ROWS              => 1,
                                    SQL_ATTR_NOSCAN                => 2,
                                    SQL_ATTR_MAX_LENGTH            => 3,
                                    SQL_ATTR_ASYNC_ENABLE          => 4,
                                    SQL_ATTR_ROW_BIND_TYPE         => 5,
                                    SQL_ATTR_CURSOR_TYPE           => 6,
                                    SQL_ATTR_CONCURRENCY           => 7,
                                    SQL_ATTR_KEYSET_SIZE           => 8,
                                    SQL_ROWSET_SIZE                => 9,
                                    SQL_ATTR_SIMULATE_CURSOR       => 10,
                                    SQL_ATTR_RETRIEVE_DATA         => 11,
                                    SQL_ATTR_USE_BOOKMARKS         => 12,
                                    SQL_ATTR_ROW_NUMBER            => 14,
                                    SQL_ATTR_ENABLE_AUTO_IPD       => 15,
                                    SQL_ATTR_FETCH_BOOKMARK_PTR    => 16,
                                    SQL_ATTR_PARAM_BIND_OFFSET_PTR => 17,
                                    SQL_ATTR_PARAM_BIND_TYPE       => 18,
                                    SQL_ATTR_PARAM_OPERATION_PTR   => 19,
                                    SQL_ATTR_PARAM_STATUS_PTR      => 20,
                                    SQL_ATTR_PARAMS_PROCESSED_PTR  => 21,
                                    SQL_ATTR_PARAMSET_SIZE         => 22,
                                    SQL_ATTR_ROW_BIND_OFFSET_PTR   => 23,
                                    SQL_ATTR_ROW_OPERATION_PTR     => 24,
                                    SQL_ATTR_ROW_STATUS_PTR        => 25,
                                    SQL_ATTR_ROWS_FETCHED_PTR      => 26,
                                    SQL_ATTR_ROW_ARRAY_SIZE        => 27,
                                    SQL_ATTR_APP_ROW_DESC          => 10010,
                                    SQL_ATTR_APP_PARAM_DESC        => 10011,
                                    SQL_ATTR_IMP_ROW_DESC          => 10012,
                                    SQL_ATTR_IMP_PARAM_DESC        => 10013,
                                    SQL_ATTR_METADATA_ID           => 10014);
   for SQL_STATEMENT_ATTRIBUTE'Size use SQLINTEGER'Size;

   --  Some old names
   SQL_STMT_OPT_MIN : constant SQL_STATEMENT_ATTRIBUTE
     := SQL_ATTR_QUERY_TIMEOUT;
   SQL_STMT_OPT_MAX : constant SQL_STATEMENT_ATTRIBUTE
     := SQL_ATTR_ROW_NUMBER;
   subtype SQL_STATEMENT_OPTION is SQL_STATEMENT_ATTRIBUTE
     range SQL_STMT_OPT_MIN .. SQL_STMT_OPT_MAX;


   procedure Get_Stmt_Attr (StatementHandle : in SQLHSTMT;
                            Attribute       : in SQL_STATEMENT_ATTRIBUTE;
                            Value           : in SQLPOINTER;
                            Length          : in out SQLINTEGER;
                            Data            : in SQLSMALLINT;
                            ErrorCode       : access SQLRETURN);
   pragma Inline (Get_Stmt_Attr);

   procedure Set_Stmt_Attr (StatementHandle : in  SQLHSTMT;
                            Attribute       : in  SQL_STATEMENT_ATTRIBUTE;
                            Value           : in  SQLPOINTER;
                            Length          : in  SQLINTEGER;
                            Data            : in  SQLSMALLINT;
                            ErrorCode       : out SQLRETURN);
   pragma Inline (Set_Stmt_Attr);

   package Statement_Attributes is new GNU.DB.SQLCLI.Generic_Attr
     (Context         => SQLHSTMT,
      T               => SQL_STATEMENT_ATTRIBUTE,
      Base            => SQLINTEGER,
      Aux             => SQLSMALLINT,
      Get             => Get_Stmt_Attr,
      Set             => Set_Stmt_Attr,
      Default_Context => Null_Handle);
   subtype Statement_Attribute is Statement_Attributes.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Statement_Attributes);

   package SA_String is new Dispatch.A_String;
   subtype Statement_Attribute_String is SA_String.Info;

   package SA_WString is new Dispatch.A_Wide_String;
   subtype Statement_Attribute_Wide_String is SA_WString.Info;

   package SA_Context is new Dispatch.A_Context;
   subtype Statement_Attribute_Handle is SA_Context.Info;

   package SA_Unsigned is new Dispatch.A_Unsigned (SQLUINTEGER);
   subtype Statement_Attribute_Unsigned is SA_Unsigned.Info;

   package SA_Boolean is new Dispatch.A_Boolean (SQLUINTEGER);
   subtype Statement_Attribute_Boolean is SA_Boolean.Info;

   package SA_PointerUInt is new
     Dispatch.A_Pointer (SQLUINTEGER, PTR_SQLUINTEGER);
   subtype Statement_Attribute_Pointer_To_UInt is SA_PointerUInt.Info;

   package SA_PointerSUInt is new
     Dispatch.A_Pointer (SQLUSMALLINT, PTR_SQLUSMALLINT);
   subtype Statement_Attribute_Pointer_To_USmallInt is SA_PointerSUInt.Info;

   function SQLGetStmtAttr
     (StatementHandle : SQLHSTMT;
      Attribute       : SQL_STATEMENT_ATTRIBUTE;
      MaxLength       : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode       : access SQLRETURN)
     return Statement_Attribute'Class;
   --  This version returns the ErrorCode in case of an ODBC Error

   function SQLGetStmtAttr
     (StatementHandle : SQLHSTMT;
      Attribute       : SQL_STATEMENT_ATTRIBUTE;
      MaxLength       : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Statement_Attribute'Class;
   --  This version raises an exception in case of an ODBC Error

   function SQLSetStmtAttr (StatementHandle : in SQLHSTMT;
                            AV_Pair         : in Statement_Attribute'Class)
                           return SQLRETURN;

   procedure SQLSetStmtAttr (StatementHandle : in SQLHSTMT;
                             AV_Pair         : in Statement_Attribute'Class);

   --  ---------------------------------------------------------------------
   --  SQL_BIND_TYPE options
   SQL_BIND_BY_COLUMN    : constant Statement_Attribute_Unsigned :=
     (Attribute => SQL_ATTR_PARAM_BIND_TYPE,
      Value => 0);

   SQL_BIND_TYPE_DEFAULT : constant Statement_Attribute_Unsigned :=
     SQL_BIND_BY_COLUMN;

   --  ---------------------------------------------------------------------
   --  SQL_ROWSET_SIZE options
   SQL_ROWSET_SIZE_DEFAULT : constant := 1;


   --  ----------------------------------------------------------------------


   package Dsp_Async_Enable is new
     Dispatch.A_Enumerated
     (SQL_ATTR_ASYNC_ENABLE,
      GNU.DB.SQLCLI.Connection_Attribute.ASYNC_ENABLE,
      SQLINTEGER,
      "ASYNC_ENABLE");
   subtype Statement_Attribute_Async_Enable is Dsp_Async_Enable.Info;


   --  ----------------------------------------------------------------------


   type CONCURRENCY is (SQL_CONCUR_READ_ONLY,
                        SQL_CONCUR_LOCK,
                        SQL_CONCUR_ROWVER,
                        SQL_CONCUR_VALUES);
   for CONCURRENCY use (SQL_CONCUR_READ_ONLY  => 1,
                        SQL_CONCUR_LOCK       => 2,
                        SQL_CONCUR_ROWVER     => 3,
                        SQL_CONCUR_VALUES     => 4);
   for CONCURRENCY'Size use SQLINTEGER'Size;
   SQL_CONCUR_DEFAULT : constant CONCURRENCY := SQL_CONCUR_READ_ONLY;

   package Dsp_Concurrency is new
     Dispatch.A_Enumerated (SQL_ATTR_CONCURRENCY,
                            CONCURRENCY,
                            SQLINTEGER,
                            "CONCURRENCY");
   subtype Statement_Attribute_Concurrency is Dsp_Concurrency.Info;


   --  ----------------------------------------------------------------------


   type CURSOR_SCROLLABLE is (SQL_NONSCROLLABLE,
                              SQL_SCROLLABLE);
   for CURSOR_SCROLLABLE'Size use SQLINTEGER'Size;

   package Dsp_Cursor_Scrollable is new
     Dispatch.A_Enumerated (SQL_ATTR_CURSOR_SCROLLABLE,
                            CURSOR_SCROLLABLE,
                            SQLINTEGER,
                            "CURSOR_SCROLLABLE");
   subtype Statement_Attribute_CSCR is Dsp_Cursor_Scrollable.Info;


   --  ----------------------------------------------------------------------


   type CURSOR_SENSITIVITY is (SQL_UNSPECIFIED,
                               SQL_INSENSITIVE,
                               SQL_SENSITIVE);
   for CURSOR_SENSITIVITY'Size use SQLINTEGER'Size;

   package Dsp_Cursor_Sensitivity is new
     Dispatch.A_Enumerated (SQL_ATTR_CURSOR_SENSITIVITY,
                            CURSOR_SENSITIVITY,
                            SQLINTEGER,
                            "CURSOR_SENSITIVITY");
   subtype Statement_Attribute_Cursor_Sensitivity is
     Dsp_Cursor_Sensitivity.Info;


   --  ----------------------------------------------------------------------


   type CURSOR_TYPE is (SQL_CURSOR_FORWARD_ONLY,
                        SQL_CURSOR_KEYSET_DRIVEN,
                        SQL_CURSOR_DYNAMIC,
                        SQL_CURSOR_STATIC);
   for CURSOR_TYPE'Size use SQLINTEGER'Size;
   SQL_CURSOR_TYPE_DEFAULT : constant CURSOR_TYPE := SQL_CURSOR_FORWARD_ONLY;

   package Dsp_Cursor_Type is new
     Dispatch.A_Enumerated (SQL_ATTR_CURSOR_TYPE,
                            CURSOR_TYPE,
                            SQLINTEGER,
                            "CURSOR_TYPE");
   subtype Statement_Attribute_Cursor_Type is Dsp_Cursor_Type.Info;


   --  ----------------------------------------------------------------------


   type NOSCAN_OPTION is (SQL_NOSCAN_OFF,
                          SQL_NOSCAN_ON);
   for NOSCAN_OPTION'Size use SQLUINTEGER'Size;
   SQL_NOSCAN_DEFAULT : constant NOSCAN_OPTION := SQL_NOSCAN_OFF;

   package Dsp_Noscan is new
     Dispatch.A_Enumerated (SQL_ATTR_NOSCAN,
                            NOSCAN_OPTION,
                            SQLINTEGER,
                            "NOSCAN_OPTION");
   subtype Statement_Attribute_NoScan is Dsp_Noscan.Info;


   --  ----------------------------------------------------------------------


   type SQL_RETRIEVE_DATA is (SQL_RD_OFF,
                              SQL_RD_ON);
   for SQL_RETRIEVE_DATA'Size use SQLUINTEGER'Size;
   SQL_RD_DEFAULT : constant SQL_RETRIEVE_DATA := SQL_RD_ON;

   package Dsp_Retrieve_Data is new
     Dispatch.A_Enumerated (SQL_ATTR_RETRIEVE_DATA,
                            SQL_RETRIEVE_DATA,
                            SQLINTEGER,
                            "SQL_RETRIEVE_DATA");
   subtype Statement_Attribute_Retrieve_Data is Dsp_Retrieve_Data.Info;


   --  ----------------------------------------------------------------------


   type SQL_SIMULATE_CURSOR is (SQL_SC_NON_UNIQUE,
                                SQL_SC_TRY_UNIQUE,
                                SQL_SC_UNIQUE);
   for SQL_SIMULATE_CURSOR'Size use SQLUINTEGER'Size;

   package Dsp_Simulate_Cursor is new
     Dispatch.A_Enumerated (SQL_ATTR_SIMULATE_CURSOR,
                            SQL_SIMULATE_CURSOR,
                            SQLINTEGER,
                            "SQL_SIMULATE_CURSOR");
   subtype Statement_Attribute_Simulate_Cursor is
     Dsp_Simulate_Cursor.Info;


   --  ----------------------------------------------------------------------


   type SQL_USE_BOOKMARK is (SQL_UB_OFF,
                             SQL_UB_ON,
                             SQL_UB_VARIABLE);
   for SQL_USE_BOOKMARK'Size use SQLUINTEGER'Size;
   SQL_UB_FIXED   : constant SQL_USE_BOOKMARK := SQL_UB_ON;
   SQL_UB_DEFAULT : constant SQL_USE_BOOKMARK := SQL_UB_OFF;

   package Dsp_Use_Bookmarks is new
     Dispatch.A_Enumerated (SQL_ATTR_USE_BOOKMARKS,
                            SQL_USE_BOOKMARK,
                            SQLINTEGER,
                            "SQL_USE_BOOKMARK");
   subtype Statement_Attribute_Use_Bookmarks is Dsp_Use_Bookmarks.Info;


   --  ----------------------------------------------------------------------

end GNU.DB.SQLCLI.Statement_Attribute;
