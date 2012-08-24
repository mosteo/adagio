-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2001 Juergen Pfeifer
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
with GNU.DB.SQLCLI.Statement_Attribute;

with GNU.DB.SQLCLI.Generic_Attr;
with GNU.DB.SQLCLI.Dispatch;
with GNU.DB.SQLCLI.Dispatch.A_Unsigned;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Wide_String;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
with GNU.DB.SQLCLI.Dispatch.A_Bitmap;
with GNU.DB.SQLCLI.Dispatch.A_Context;
with GNU.DB.SQLCLI.Dispatch.A_Boolean_String;

pragma Elaborate_All (GNU.DB.SQLCLI.Generic_Attr);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Unsigned);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Wide_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Enumerated);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Bitmap);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Context);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Boolean_String);

package GNU.DB.SQLCLI.Info is

   type SQL_INFO_TYPE is (SQL_MAXIMUM_DRIVER_CONNECTIONS,
                          SQL_MAXIMUM_CONCURRENT_ACTIVITIES,
                          SQL_DATA_SOURCE_NAME,
                          SQL_DRIVER_HDBC,
                          SQL_DRIVER_HENV,
                          SQL_DRIVER_HSTMT,
                          SQL_DRIVER_NAME,
                          SQL_DRIVER_VER,
                          SQL_FETCH_DIRECTION,
                          SQL_ODBC_API_CONFORMANCE,
                          SQL_ODBC_VER,
                          SQL_ROW_UPDATES,
                          SQL_ODBC_SAG_CLI_CONFORMANCE,
                          SQL_SERVER_NAME,
                          SQL_SEARCH_PATTERN_ESCAPE,
                          SQL_ODBC_SQL_CONFORMANCE,
                          SQL_DATABASE_NAME,
                          SQL_DBMS_NAME,
                          SQL_DBMS_VER,
                          SQL_ACCESSIBLE_TABLES,
                          SQL_ACCESSIBLE_PROCEDURES,
                          SQL_PROCEDURES,
                          SQL_CONCAT_NULL_BEHAVIOR,
                          SQL_CURSOR_COMMIT_BEHAVIOR,
                          SQL_CURSOR_ROLLBACK_BEHAVIOR,
                          SQL_DATA_SOURCE_READ_ONLY,
                          SQL_DEFAULT_TRANSACTION_ISOLATION,
                          SQL_EXPRESSIONS_IN_ORDERBY,
                          SQL_IDENTIFIER_CASE,
                          SQL_IDENTIFIER_QUOTE_CHAR,
                          SQL_MAXIMUM_COLUMN_NAME_LENGTH,
                          SQL_MAXIMUM_CURSOR_NAME_LENGTH,
                          SQL_MAXIMUM_SCHEMA_NAME_LENGTH,
                          SQL_MAXIMUM_PROCEDURE_NAME_LENGTH,
                          SQL_MAXIMUM_CATALOG_NAME_LENGTH,
                          SQL_MAXIMUM_TABLE_NAME_LENGTH,
                          SQL_MULTIPLE_RESULT_SETS,
                          SQL_MULTIPLE_ACTIVE_TRANSACTIONS,
                          SQL_OUTER_JOINS,
                          SQL_SCHEMA_TERM,
                          SQL_PROCEDURE_TERM,
                          SQL_QUALIFIER_NAME_SEPARATOR,
                          SQL_QUALIFIER_TERM,
                          SQL_SCROLL_CONCURRENCY,
                          SQL_SCROLL_OPTIONS,
                          SQL_TABLE_TERM,
                          SQL_TRANSACTION_CAPABLE,
                          SQL_USER_NAME,
                          SQL_CONVERT_FUNCTIONS,
                          SQL_NUMERIC_FUNCTIONS,
                          SQL_STRING_FUNCTIONS,
                          SQL_SYSTEM_FUNCTIONS,
                          SQL_TIMEDATE_FUNCTIONS,
                          SQL_CONVERT_BIGINT,
                          SQL_CONVERT_BINARY,
                          SQL_CONVERT_BIT,
                          SQL_CONVERT_CHAR,
                          SQL_CONVERT_DATE,
                          SQL_CONVERT_DECIMAL,
                          SQL_CONVERT_DOUBLE,
                          SQL_CONVERT_FLOAT,
                          SQL_CONVERT_INTEGER,
                          SQL_CONVERT_LONGVARCHAR,
                          SQL_CONVERT_NUMERIC,
                          SQL_CONVERT_REAL,
                          SQL_CONVERT_SMALLINT,
                          SQL_CONVERT_TIME,
                          SQL_CONVERT_TIMESTAMP,
                          SQL_CONVERT_TINYINT,
                          SQL_CONVERT_VARBINARY,
                          SQL_CONVERT_VARCHAR,
                          SQL_CONVERT_LONGVARBINARY,
                          SQL_TRANSACTION_ISOLATION_OPTION,
                          SQL_INTEGRITY,
                          SQL_CORRELATION_NAME,
                          SQL_NON_NULLABLE_COLUMNS,
                          SQL_DRIVER_HLIB,
                          SQL_DRIVER_ODBC_VER,
                          SQL_LOCK_TYPES,
                          SQL_POS_OPERATIONS,
                          SQL_POSITIONED_STATEMENTS,
                          SQL_GETDATA_EXTENSIONS,
                          SQL_BOOKMARK_PERSISTENCE,
                          SQL_STATIC_SENSITIVITY,
                          SQL_FILE_USAGE,
                          SQL_NULL_COLLATION,
                          SQL_ALTER_TABLE,
                          SQL_COLUMN_ALIAS,
                          SQL_GROUP_BY,
                          SQL_KEYWORDS,
                          SQL_ORDER_BY_COLUMNS_IN_SELECT,
                          SQL_SCHEMA_USAGE,
                          SQL_QUALIFIER_USAGE,
                          SQL_QUOTED_IDENTIFIER_CASE,
                          SQL_SPECIAL_CHARACTERS,
                          SQL_SUBQUERIES,
                          SQL_UNION_STATEMENT,
                          SQL_MAXIMUM_COLUMNS_IN_GROUP_BY,
                          SQL_MAXIMUM_COLUMNS_IN_INDEX,
                          SQL_MAXIMUM_COLUMNS_IN_ORDER_BY,
                          SQL_MAXIMUM_COLUMNS_IN_SELECT,
                          SQL_MAXIMUM_COLUMNS_IN_TABLE,
                          SQL_MAXIMUM_INDEX_SIZE,
                          SQL_MAXIMUM_ROW_SIZE_INCLUDES_LONG,
                          SQL_MAXIMUM_ROW_SIZE,
                          SQL_MAXIMUM_STATEMENT_LENGTH,
                          SQL_MAXIMUM_TABLES_IN_SELECT,
                          SQL_MAXIMUM_USER_NAME_LENGTH,
                          SQL_MAXIMUM_CHAR_LITERAL_LENGTH,
                          SQL_TIMEDATE_ADD_INTERVALS,
                          SQL_TIMEDATE_DIFF_INTERVALS,
                          SQL_NEED_LONG_DATA_LEN,
                          SQL_MAX_BINARY_LITERAL_LEN,
                          SQL_LIKE_ESCAPE_CLAUSE,
                          SQL_QUALIFIER_LOCATION,
                          SQL_OUTER_JOIN_CAPABILITIES,
                          SQL_ACTIVE_ENVIRONMENTS,
                          SQL_ALTER_DOMAIN,
                          SQL_SQL_CONFORMANCE,
                          SQL_DATETIME_LITERALS,
                          SQL_BATCH_ROW_COUNT,
                          SQL_BATCH_SUPPORT,
                          SQL_CONVERT_WCHAR,
                          SQL_CONVERT_INTERVAL_DAY_TIME,
                          SQL_CONVERT_INTERVAL_YEAR_MONTH,
                          SQL_CONVERT_WLONGVARCHAR,
                          SQL_CONVERT_WVARCHAR,
                          SQL_CREATE_ASSERTION,
                          SQL_CREATE_CHARACTER_SET,
                          SQL_CREATE_COLLATION,
                          SQL_CREATE_DOMAIN,
                          SQL_CREATE_SCHEMA,
                          SQL_CREATE_TABLE,
                          SQL_CREATE_TRANSLATION,
                          SQL_CREATE_VIEW,
                          SQL_DRIVER_HDESC,
                          SQL_DROP_ASSERTION,
                          SQL_DROP_CHARACTER_SET,
                          SQL_DROP_COLLATION,
                          SQL_DROP_DOMAIN,
                          SQL_DROP_SCHEMA,
                          SQL_DROP_TABLE,
                          SQL_DROP_TRANSLATION,
                          SQL_DROP_VIEW,
                          SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
                          SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
                          SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,
                          SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,
                          SQL_INDEX_KEYWORDS,
                          SQL_INFO_SCHEMA_VIEWS,
                          SQL_KEYSET_CURSOR_ATTRIBUTES1,
                          SQL_KEYSET_CURSOR_ATTRIBUTES2,
                          SQL_ODBC_INTERFACE_CONFORMANCE,
                          SQL_PARAM_ARRAY_ROW_COUNTS,
                          SQL_PARAM_ARRAY_SELECTS,
                          SQL_SQL92_DATETIME_FUNCTIONS,
                          SQL_SQL92_FOREIGN_KEY_DELETE_RULE,
                          SQL_SQL92_FOREIGN_KEY_UPDATE_RULE,
                          SQL_SQL92_GRANT,
                          SQL_SQL92_NUMERIC_VALUE_FUNCTIONS,
                          SQL_SQL92_PREDICATES,
                          SQL_SQL92_RELATIONAL_JOIN_OPERATORS,
                          SQL_SQL92_REVOKE,
                          SQL_SQL92_ROW_VALUE_CONSTRUCTOR,
                          SQL_SQL92_STRING_FUNCTIONS,
                          SQL_SQL92_VALUE_EXPRESSIONS,
                          SQL_STANDARD_CLI_CONFORMANCE,
                          SQL_STATIC_CURSOR_ATTRIBUTES1,
                          SQL_STATIC_CURSOR_ATTRIBUTES2,
                          SQL_AGGREGATE_FUNCTIONS,
                          SQL_DDL_INDEX,
                          SQL_DM_VER,
                          SQL_INSERT_STATEMENT,
                          SQL_XOPEN_CLI_YEAR,
                          SQL_CURSOR_SENSITIVITY,
                          SQL_DESCRIBE_PARAMETER,
                          SQL_CATALOG_NAME,
                          SQL_COLLATION_SEQ,
                          SQL_MAXIMUM_IDENTIFIER_LENGTH,
                          SQL_ASYNC_MODE,
                          SQL_MAX_ASYNC_CONCURRENT_STATEMENTS);

   for SQL_INFO_TYPE use (SQL_MAXIMUM_DRIVER_CONNECTIONS        => 0,
                          SQL_MAXIMUM_CONCURRENT_ACTIVITIES     => 1,
                          SQL_DATA_SOURCE_NAME                  => 2,
                          SQL_DRIVER_HDBC                       => 3,
                          SQL_DRIVER_HENV                       => 4,
                          SQL_DRIVER_HSTMT                      => 5,
                          SQL_DRIVER_NAME                       => 6,
                          SQL_DRIVER_VER                        => 7,
                          SQL_FETCH_DIRECTION                   => 8,
                          SQL_ODBC_API_CONFORMANCE              => 9,
                          SQL_ODBC_VER                          => 10,
                          SQL_ROW_UPDATES                       => 11,
                          SQL_ODBC_SAG_CLI_CONFORMANCE          => 12,
                          SQL_SERVER_NAME                       => 13,
                          SQL_SEARCH_PATTERN_ESCAPE             => 14,
                          SQL_ODBC_SQL_CONFORMANCE              => 15,
                          SQL_DATABASE_NAME                     => 16,
                          SQL_DBMS_NAME                         => 17,
                          SQL_DBMS_VER                          => 18,
                          SQL_ACCESSIBLE_TABLES                 => 19,
                          SQL_ACCESSIBLE_PROCEDURES             => 20,
                          SQL_PROCEDURES                        => 21,
                          SQL_CONCAT_NULL_BEHAVIOR              => 22,
                          SQL_CURSOR_COMMIT_BEHAVIOR            => 23,
                          SQL_CURSOR_ROLLBACK_BEHAVIOR          => 24,
                          SQL_DATA_SOURCE_READ_ONLY             => 25,
                          SQL_DEFAULT_TRANSACTION_ISOLATION     => 26,
                          SQL_EXPRESSIONS_IN_ORDERBY            => 27,
                          SQL_IDENTIFIER_CASE                   => 28,
                          SQL_IDENTIFIER_QUOTE_CHAR             => 29,
                          SQL_MAXIMUM_COLUMN_NAME_LENGTH        => 30,
                          SQL_MAXIMUM_CURSOR_NAME_LENGTH        => 31,
                          SQL_MAXIMUM_SCHEMA_NAME_LENGTH        => 32,
                          SQL_MAXIMUM_PROCEDURE_NAME_LENGTH     => 33,
                          SQL_MAXIMUM_CATALOG_NAME_LENGTH       => 34,
                          SQL_MAXIMUM_TABLE_NAME_LENGTH         => 35,
                          SQL_MULTIPLE_RESULT_SETS              => 36,
                          SQL_MULTIPLE_ACTIVE_TRANSACTIONS      => 37,
                          SQL_OUTER_JOINS                       => 38,
                          SQL_SCHEMA_TERM                       => 39,
                          SQL_PROCEDURE_TERM                    => 40,
                          SQL_QUALIFIER_NAME_SEPARATOR          => 41,
                          SQL_QUALIFIER_TERM                    => 42,
                          SQL_SCROLL_CONCURRENCY                => 43,
                          SQL_SCROLL_OPTIONS                    => 44,
                          SQL_TABLE_TERM                        => 45,
                          SQL_TRANSACTION_CAPABLE               => 46,
                          SQL_USER_NAME                         => 47,
                          SQL_CONVERT_FUNCTIONS                 => 48,
                          SQL_NUMERIC_FUNCTIONS                 => 49,
                          SQL_STRING_FUNCTIONS                  => 50,
                          SQL_SYSTEM_FUNCTIONS                  => 51,
                          SQL_TIMEDATE_FUNCTIONS                => 52,
                          SQL_CONVERT_BIGINT                    => 53,
                          SQL_CONVERT_BINARY                    => 54,
                          SQL_CONVERT_BIT                       => 55,
                          SQL_CONVERT_CHAR                      => 56,
                          SQL_CONVERT_DATE                      => 57,
                          SQL_CONVERT_DECIMAL                   => 58,
                          SQL_CONVERT_DOUBLE                    => 59,
                          SQL_CONVERT_FLOAT                     => 60,
                          SQL_CONVERT_INTEGER                   => 61,
                          SQL_CONVERT_LONGVARCHAR               => 62,
                          SQL_CONVERT_NUMERIC                   => 63,
                          SQL_CONVERT_REAL                      => 64,
                          SQL_CONVERT_SMALLINT                  => 65,
                          SQL_CONVERT_TIME                      => 66,
                          SQL_CONVERT_TIMESTAMP                 => 67,
                          SQL_CONVERT_TINYINT                   => 68,
                          SQL_CONVERT_VARBINARY                 => 69,
                          SQL_CONVERT_VARCHAR                   => 70,
                          SQL_CONVERT_LONGVARBINARY             => 71,
                          SQL_TRANSACTION_ISOLATION_OPTION      => 72,
                          SQL_INTEGRITY                         => 73,
                          SQL_CORRELATION_NAME                  => 74,
                          SQL_NON_NULLABLE_COLUMNS              => 75,
                          SQL_DRIVER_HLIB                       => 76,
                          SQL_DRIVER_ODBC_VER                   => 77,
                          SQL_LOCK_TYPES                        => 78,
                          SQL_POS_OPERATIONS                    => 79,
                          SQL_POSITIONED_STATEMENTS             => 80,
                          SQL_GETDATA_EXTENSIONS                => 81,
                          SQL_BOOKMARK_PERSISTENCE              => 82,
                          SQL_STATIC_SENSITIVITY                => 83,
                          SQL_FILE_USAGE                        => 84,
                          SQL_NULL_COLLATION                    => 85,
                          SQL_ALTER_TABLE                       => 86,
                          SQL_COLUMN_ALIAS                      => 87,
                          SQL_GROUP_BY                          => 88,
                          SQL_KEYWORDS                          => 89,
                          SQL_ORDER_BY_COLUMNS_IN_SELECT        => 90,
                          SQL_SCHEMA_USAGE                      => 91,
                          SQL_QUALIFIER_USAGE                   => 92,
                          SQL_QUOTED_IDENTIFIER_CASE            => 93,
                          SQL_SPECIAL_CHARACTERS                => 94,
                          SQL_SUBQUERIES                        => 95,
                          SQL_UNION_STATEMENT                   => 96,
                          SQL_MAXIMUM_COLUMNS_IN_GROUP_BY       => 97,
                          SQL_MAXIMUM_COLUMNS_IN_INDEX          => 98,
                          SQL_MAXIMUM_COLUMNS_IN_ORDER_BY       => 99,
                          SQL_MAXIMUM_COLUMNS_IN_SELECT         => 100,
                          SQL_MAXIMUM_COLUMNS_IN_TABLE          => 101,
                          SQL_MAXIMUM_INDEX_SIZE                => 102,
                          SQL_MAXIMUM_ROW_SIZE_INCLUDES_LONG    => 103,
                          SQL_MAXIMUM_ROW_SIZE                  => 104,
                          SQL_MAXIMUM_STATEMENT_LENGTH          => 105,
                          SQL_MAXIMUM_TABLES_IN_SELECT          => 106,
                          SQL_MAXIMUM_USER_NAME_LENGTH          => 107,
                          SQL_MAXIMUM_CHAR_LITERAL_LENGTH       => 108,
                          SQL_TIMEDATE_ADD_INTERVALS            => 109,
                          SQL_TIMEDATE_DIFF_INTERVALS           => 110,
                          SQL_NEED_LONG_DATA_LEN                => 111,
                          SQL_MAX_BINARY_LITERAL_LEN            => 112,
                          SQL_LIKE_ESCAPE_CLAUSE                => 113,
                          SQL_QUALIFIER_LOCATION                => 114,
                          SQL_OUTER_JOIN_CAPABILITIES           => 115,
                          SQL_ACTIVE_ENVIRONMENTS               => 116,
                          SQL_ALTER_DOMAIN                      => 117,
                          SQL_SQL_CONFORMANCE                   => 118,
                          SQL_DATETIME_LITERALS                 => 119,
                          SQL_BATCH_ROW_COUNT                   => 120,
                          SQL_BATCH_SUPPORT                     => 121,
                          SQL_CONVERT_WCHAR                     => 122,
                          SQL_CONVERT_INTERVAL_DAY_TIME         => 123,
                          SQL_CONVERT_INTERVAL_YEAR_MONTH       => 124,
                          SQL_CONVERT_WLONGVARCHAR              => 125,
                          SQL_CONVERT_WVARCHAR                  => 126,
                          SQL_CREATE_ASSERTION                  => 127,
                          SQL_CREATE_CHARACTER_SET              => 128,
                          SQL_CREATE_COLLATION                  => 129,
                          SQL_CREATE_DOMAIN                     => 130,
                          SQL_CREATE_SCHEMA                     => 131,
                          SQL_CREATE_TABLE                      => 132,
                          SQL_CREATE_TRANSLATION                => 133,
                          SQL_CREATE_VIEW                       => 134,
                          SQL_DRIVER_HDESC                      => 135,
                          SQL_DROP_ASSERTION                    => 136,
                          SQL_DROP_CHARACTER_SET                => 137,
                          SQL_DROP_COLLATION                    => 138,
                          SQL_DROP_DOMAIN                       => 139,
                          SQL_DROP_SCHEMA                       => 140,
                          SQL_DROP_TABLE                        => 141,
                          SQL_DROP_TRANSLATION                  => 142,
                          SQL_DROP_VIEW                         => 143,
                          SQL_DYNAMIC_CURSOR_ATTRIBUTES1        => 144,
                          SQL_DYNAMIC_CURSOR_ATTRIBUTES2        => 145,
                          SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1   => 146,
                          SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2   => 147,
                          SQL_INDEX_KEYWORDS                    => 148,
                          SQL_INFO_SCHEMA_VIEWS                 => 149,
                          SQL_KEYSET_CURSOR_ATTRIBUTES1         => 150,
                          SQL_KEYSET_CURSOR_ATTRIBUTES2         => 151,
                          SQL_ODBC_INTERFACE_CONFORMANCE        => 152,
                          SQL_PARAM_ARRAY_ROW_COUNTS            => 153,
                          SQL_PARAM_ARRAY_SELECTS               => 154,
                          SQL_SQL92_DATETIME_FUNCTIONS          => 155,
                          SQL_SQL92_FOREIGN_KEY_DELETE_RULE     => 156,
                          SQL_SQL92_FOREIGN_KEY_UPDATE_RULE     => 157,
                          SQL_SQL92_GRANT                       => 158,
                          SQL_SQL92_NUMERIC_VALUE_FUNCTIONS     => 159,
                          SQL_SQL92_PREDICATES                  => 160,
                          SQL_SQL92_RELATIONAL_JOIN_OPERATORS   => 161,
                          SQL_SQL92_REVOKE                      => 162,
                          SQL_SQL92_ROW_VALUE_CONSTRUCTOR       => 163,
                          SQL_SQL92_STRING_FUNCTIONS            => 164,
                          SQL_SQL92_VALUE_EXPRESSIONS           => 165,
                          SQL_STANDARD_CLI_CONFORMANCE          => 166,
                          SQL_STATIC_CURSOR_ATTRIBUTES1         => 167,
                          SQL_STATIC_CURSOR_ATTRIBUTES2         => 168,
                          SQL_AGGREGATE_FUNCTIONS               => 169,
                          SQL_DDL_INDEX                         => 170,
                          SQL_DM_VER                            => 171,
                          SQL_INSERT_STATEMENT                  => 172,
                          SQL_XOPEN_CLI_YEAR                    => 10000,
                          SQL_CURSOR_SENSITIVITY                => 10001,
                          SQL_DESCRIBE_PARAMETER                => 10002,
                          SQL_CATALOG_NAME                      => 10003,
                          SQL_COLLATION_SEQ                     => 10004,
                          SQL_MAXIMUM_IDENTIFIER_LENGTH         => 10005,
                          SQL_ASYNC_MODE                        => 10021,
                          SQL_MAX_ASYNC_CONCURRENT_STATEMENTS   => 10022);
   for SQL_INFO_TYPE'Size use SQLSMALLINT'Size;

   --  Various aliases for compatibility reasons
   SQL_MAX_DRIVER_CONNECTIONS           : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_DRIVER_CONNECTIONS;
   SQL_MAX_COLUMN_NAME_LEN              : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_COLUMN_NAME_LENGTH;
   SQL_MAX_CURSOR_NAME_LEN              : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_CURSOR_NAME_LENGTH;
   SQL_MAX_SCHEMA_NAME_LEN              : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_SCHEMA_NAME_LENGTH;
   SQL_TXN_CAPABLE                      : constant SQL_INFO_TYPE
     := SQL_TRANSACTION_CAPABLE;
   SQL_TXN_ISOLATION_OPTION             : constant SQL_INFO_TYPE
     := SQL_TRANSACTION_ISOLATION_OPTION;
   SQL_MAX_COLUMNS_IN_GROUP_BY          : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_COLUMNS_IN_GROUP_BY;
   SQL_MAX_COLUMNS_IN_INDEX             : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_COLUMNS_IN_INDEX;
   SQL_MAX_COLUMNS_IN_ORDER_BY          : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_COLUMNS_IN_ORDER_BY;
   SQL_MAX_COLUMNS_IN_SELECT            : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_COLUMNS_IN_SELECT;
   SQL_MAX_INDEX_SIZE                   : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_INDEX_SIZE;
   SQL_MAX_ROW_SIZE                     : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_ROW_SIZE;
   SQL_MAX_STATEMENT_LEN                : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_STATEMENT_LENGTH;
   SQL_MAX_TABLES_IN_SELECT             : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_TABLES_IN_SELECT;
   SQL_MAX_USER_NAME_LEN                : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_USER_NAME_LENGTH;
   SQL_OJ_CAPABILITIES                  : constant SQL_INFO_TYPE
     := SQL_OUTER_JOIN_CAPABILITIES;
   SQL_MAX_IDENTIFIER_LEN               : constant SQL_INFO_TYPE
     := SQL_MAXIMUM_IDENTIFIER_LENGTH;
   SQL_CATALOG_LOCATION                 : constant SQL_INFO_TYPE
     := SQL_QUALIFIER_LOCATION;
   SQL_CATALOG_NAME_SEPARATOR           : constant SQL_INFO_TYPE
     := SQL_QUALIFIER_NAME_SEPARATOR;
   SQL_CATALOG_TERM                     : constant SQL_INFO_TYPE
     := SQL_QUALIFIER_TERM;
   SQL_CATALOG_USAGE                    : constant SQL_INFO_TYPE
     := SQL_QUALIFIER_USAGE;
   SQL_OWNER_TERM                       : constant SQL_INFO_TYPE
     := SQL_SCHEMA_TERM;
   SQL_OWNER_USAGE                      : constant SQL_INFO_TYPE
     := SQL_SCHEMA_USAGE;
   SQL_UNION                            : constant SQL_INFO_TYPE
     := SQL_UNION_STATEMENT;

   procedure Get_Info (ConnectionHandle : in SQLHDBC;
                       InfoType         : in SQL_INFO_TYPE;
                       Value            : in SQLPOINTER;
                       Length           : in out SQLSMALLINT;
                       Data             : in SQLSMALLINT;
                       ErrorCode        : access SQLRETURN);
   pragma Inline (Get_Info);

   procedure Set_Info (ConnectionHandle : in  SQLHDBC;
                       InfoType         : in  SQL_INFO_TYPE;
                       Value            : in  SQLPOINTER;
                       Length           : in  SQLSMALLINT;
                       Data             : in  SQLSMALLINT;
                       ErrorCode        : out SQLRETURN);
   pragma Inline (Set_Info);

   package Driver_Information is
      new GNU.DB.SQLCLI.Generic_Attr (Context         => SQLHDBC,
                                      T               => SQL_INFO_TYPE,
                                      Base            => SQLSMALLINT,
                                      Aux             => SQLSMALLINT,
                                      Get             => Get_Info,
                                      Set             => Set_Info,
                                      Default_Context => Null_Handle);

   subtype Driver_Info is Driver_Information.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Driver_Information);
   --  Instantiate the dispatcher package that manages to map the
   --  proper derived types to the ODBC enumeration SQL_INFO_TYPE

   package DI_String is new Dispatch.A_String;
   subtype Driver_Info_String is DI_String.Info;

   package DI_WString is new Dispatch.A_Wide_String;
   subtype Driver_Info_Wide_String is DI_WString.Info;

   package DI_Boolean_String is new Dispatch.A_Boolean_String;
   subtype Driver_Info_Boolean is DI_Boolean_String.Info;

   package DI_Context is new Dispatch.A_Context;
   subtype Driver_Info_Handle  is DI_Context.Info;

   package DI_USmallint is new Dispatch.A_Unsigned (SQLUSMALLINT);
   subtype Driver_Info_Small_Unsigned is DI_USmallint.Info;

   package DI_UInteger is new Dispatch.A_Unsigned (SQLUINTEGER);
   subtype Driver_Info_Unsigned is DI_UInteger.Info;

   function SQLGetInfo (ConnectionHandle  : SQLHDBC;
                        InfoType          : SQL_INFO_TYPE;
                        MaxLength         : SQLSMALLINT := 1024;
                        ErrorCode         : access SQLRETURN)
                       return Driver_Info'Class;
   --  This version returns the ErrorCode in case of an ODBC error

   function SQLGetInfo (ConnectionHandle  : SQLHDBC;
                        InfoType          : SQL_INFO_TYPE;
                        MaxLength         : SQLSMALLINT := 1024)
                        return Driver_Info'Class;
   --  This version raises an exception in case of an ODBC error

   --  ----------------------------------------------------------------------

   type AGGREGATE_FUNCTION is (SQL_AF_AVG,
                               SQL_AF_COUNT,
                               SQL_AF_MAX,
                               SQL_AF_MIN,
                               SQL_AF_SUM,
                               SQL_AF_DISTINCT);

   type AGGREGATE_FUNCTION_BITMAP is
      array (AGGREGATE_FUNCTION) of Boolean;

   SQL_AF_ALL : constant AGGREGATE_FUNCTION_BITMAP :=
     (AGGREGATE_FUNCTION'Range => True);

   package Dsp_Aggregate_Function is new
     Dispatch.A_Bitmap (SQL_AGGREGATE_FUNCTIONS,
                        AGGREGATE_FUNCTION,
                        AGGREGATE_FUNCTION_BITMAP);
   subtype Aggregate_Function_Info is Dsp_Aggregate_Function.Info;


   --  ----------------------------------------------------------------------


   type ALTER_DOMAIN is (SQL_AD_CONSTRAINT_NAME_DEFINITION,
                         SQL_AD_ADD_DOMAIN_CONSTRAINT,
                         SQL_AD_DROP_DOMAIN_CONSTRAINT,
                         SQL_AD_ADD_DOMAIN_DEFAULT,
                         SQL_AD_DROP_DOMAIN_DEFAULT,
                         SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED,
                         SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE,
                         SQL_AD_ADD_CONSTRAINT_DEFERRABLE,
                         SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE);

   type ALTER_DOMAIN_BITMAP is
     array (ALTER_DOMAIN) of Boolean;

   package Dsp_Alter_Domain is new
     Dispatch.A_Bitmap (SQL_ALTER_DOMAIN,
                        ALTER_DOMAIN,
                        ALTER_DOMAIN_BITMAP);
   subtype Alter_Domain_Info is Dsp_Alter_Domain.Info;


   --  ----------------------------------------------------------------------


   type ALTER_TABLE is (SQL_AT_ADD_COLUMN,
                        SQL_AT_DROP_COLUMN,
                        SQL_AT_RESERVED1,
                        SQL_AT_ADD_CONSTRAINT,
                        SQL_AT_RESERVED2,
                        SQL_AT_ADD_COLUMN_SINGLE,
                        SQL_AT_ADD_COLUMN_DEFAULT,
                        SQL_AT_ADD_COLUMN_COLLATION,
                        SQL_AT_SET_COLUMN_DEFAULT,
                        SQL_AT_DROP_COLUMN_DEFAULT,
                        SQL_AT_DROP_COLUMN_CASCADE,
                        SQL_AT_DROP_COLUMN_RESTRICT,
                        SQL_AT_ADD_TABLE_CONSTRAINT,
                        SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE,
                        SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT,
                        SQL_AT_CONSTRAINT_NAME_DEFINITION,
                        SQL_AT_CONSTRAINT_INITIALLY_DEFERRED,
                        SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE,
                        SQL_AT_CONSTRAINT_DEFERRABLE,
                        SQL_AT_CONSTRAINT_NON_DEFERRABLE);

   type ALTER_TABLE_BITMAP is
     array (ALTER_TABLE) of Boolean;

   package Dsp_Alter_Table is new
     Dispatch.A_Bitmap (SQL_ALTER_TABLE,
                        ALTER_TABLE,
                        ALTER_TABLE_BITMAP);
   subtype Alter_Table_Info is Dsp_Alter_Table.Info;


   --  ----------------------------------------------------------------------


   type ASYNC_MODE is (SQL_AM_NONE,
                       SQL_AM_CONNECTION,
                       SQL_AM_STATEMENT);
   for ASYNC_MODE'Size use SQLINTEGER'Size;

   package Dsp_Async_Mode is new
     Dispatch.A_Enumerated (SQL_ASYNC_MODE,
                            ASYNC_MODE,
                            SQLINTEGER,
                            "ASYNC_MODE");
   subtype Async_Mode_Info is Dsp_Async_Mode.Info;


   --  ----------------------------------------------------------------------


   type BATCH_ROW_COUNT is (SQL_BRC_PROCEDURES,
                            SQL_BRC_EXPLICIT,
                            SQL_BRC_ROLLED_UP);

   type BATCH_ROW_COUNT_BITMAP is
     array (BATCH_ROW_COUNT) of Boolean;

   package Dsp_Batch_Row_Count is new
     Dispatch.A_Bitmap (SQL_BATCH_ROW_COUNT,
                        BATCH_ROW_COUNT,
                        BATCH_ROW_COUNT_BITMAP);
   subtype Batch_Row_Count_Info is Dsp_Batch_Row_Count.Info;


   --  ----------------------------------------------------------------------


   type BATCH_SUPPORT is (SQL_BS_SELECT_EXPLICIT,
                          SQL_BS_ROW_COUNT_EXPLICIT,
                          SQL_BS_SELECT_PROC,
                          SQL_BS_ROW_COUNT_PROC);

   type BATCH_SUPPORT_BITMAP is
     array (BATCH_SUPPORT) of Boolean;

   package Dsp_Batch_Support is new
     Dispatch.A_Bitmap (SQL_BATCH_SUPPORT,
                        BATCH_SUPPORT,
                        BATCH_SUPPORT_BITMAP);
   subtype Batch_Support_Info is Dsp_Batch_Support.Info;


   --  ----------------------------------------------------------------------


   type BOOKMARK_PERSISTENCE is (SQL_BP_CLOSE,
                                 SQL_BP_DELETE,
                                 SQL_BP_DROP,
                                 SQL_BP_TRANSACTION,
                                 SQL_BP_UPDATE,
                                 SQL_BP_OTHER_HSTMT,
                                 SQL_BP_SCROLL);

   type BOOKMARK_PERSISTENCE_BITMAP is
     array (BOOKMARK_PERSISTENCE) of Boolean;

   package Dsp_Bookmark_Per is new
     Dispatch.A_Bitmap (SQL_BOOKMARK_PERSISTENCE,
                        BOOKMARK_PERSISTENCE,
                        BOOKMARK_PERSISTENCE_BITMAP);
   subtype Bookmark_Persistence_Info is Dsp_Bookmark_Per.Info;



   --  ----------------------------------------------------------------------

   type POSITION_LOCKTYPE_BITMAP is array (POSITION_LOCKTYPE) of Boolean;

   package Dsp_LockType is new
     Dispatch.A_Bitmap (SQL_LOCK_TYPES,
                        POSITION_LOCKTYPE,
                        POSITION_LOCKTYPE_BITMAP);
   subtype Position_LockType_Info is Dsp_LockType.Info;

   --  ----------------------------------------------------------------------

   type POSITION_OPERATION_BITMAP is array (POSITION_OPERATION) of Boolean;

   package Dsp_PosOperation is new
     Dispatch.A_Bitmap (SQL_POS_OPERATIONS,
                        POSITION_OPERATION,
                        POSITION_OPERATION_BITMAP);
   subtype Position_Operation_Info is Dsp_PosOperation.Info;
   --  ----------------------------------------------------------------------

   type POSITIONED_STATEMENT is (POSITIONED_DELETE,
                                 POSITIONED_UPDATE,
                                 SELECT_FOR_UPDATE);

   type POSITIONED_STATEMENT_BITMAP is array (POSITIONED_STATEMENT) of Boolean;

   package Dsp_PosStatements is new
     Dispatch.A_Bitmap (SQL_POSITIONED_STATEMENTS,
                        POSITIONED_STATEMENT,
                        POSITIONED_STATEMENT_BITMAP);
   subtype Positioned_Statement_Info is Dsp_PosStatements.Info;

   --  ----------------------------------------------------------------------

   type FETCH_DIRECTION_BITMAP is array (FETCH_DIRECTION) of Boolean;

   package Dsp_FetchDirection is new
     Dispatch.A_Bitmap (SQL_FETCH_DIRECTION,
                        FETCH_DIRECTION,
                        FETCH_DIRECTION_BITMAP);
   subtype Fetch_Direction_Info is Dsp_FetchDirection.Info;

   --  ----------------------------------------------------------------------


   type CONCAT_NULL_BEHAVIOR is (SQL_CB_NULL,
                                 SQL_CB_NON_NULL);
   for CONCAT_NULL_BEHAVIOR'Size use SQLSMALLINT'Size;

   package Dsp_Concat_Null_Behavior is new
     Dispatch.A_Enumerated (SQL_CONCAT_NULL_BEHAVIOR,
                            CONCAT_NULL_BEHAVIOR,
                            SQLSMALLINT,
                            "CONCAT_NULL_BEHAVIOR");
   subtype Concat_Null_Behaviour_Info is Dsp_Concat_Null_Behavior.Info;


   --  ----------------------------------------------------------------------


   type CONVERSION_TARGET is (SQL_CVT_CHAR,
                              SQL_CVT_NUMERIC,
                              SQL_CVT_DECIMAL,
                              SQL_CVT_INTEGER,
                              SQL_CVT_SMALLINT,
                              SQL_CVT_FLOAT,
                              SQL_CVT_REAL,
                              SQL_CVT_DOUBLE,
                              SQL_CVT_VARCHAR,
                              SQL_CVT_LONGVARCHAR,
                              SQL_CVT_BINARY,
                              SQL_CVT_VARBINARY,
                              SQL_CVT_BIT,
                              SQL_CVT_TINYINT,
                              SQL_CVT_BIGINT,
                              SQL_CVT_DATE,
                              SQL_CVT_TIME,
                              SQL_CVT_TIMESTAMP,
                              SQL_CVT_LONGVARBINARY,
                              SQL_CVT_INTERVAL_YEAR_MONTH,
                              SQL_CVT_INTERVAL_DAY_TIME,
                              SQL_CVT_WCHAR,
                              SQL_CVT_WLONGVARCHAR,
                              SQL_CVT_WVARCHAR);

   type CONVERSION_TARGET_BITMAP is
      array (CONVERSION_TARGET) of Boolean;

   package Dsp_Conversion_Target is new
     Dispatch.A_Bitmap (SQL_CONVERT_BIGINT,
                        CONVERSION_TARGET,
                        CONVERSION_TARGET_BITMAP);
   subtype Conversion_Target_Info is Dsp_Conversion_Target.Info;



   --  ----------------------------------------------------------------------


   type CONVERT_FUNCTION is (SQL_FN_CVT_CONVERT,
                             SQL_FN_CVT_CAST);

   type CONVERT_FUNCTION_BITMAP is
     array (CONVERT_FUNCTION) of Boolean;

   package Dsp_Convert_Functions is new
     Dispatch.A_Bitmap (SQL_CONVERT_FUNCTIONS,
                        CONVERT_FUNCTION,
                        CONVERT_FUNCTION_BITMAP);
   subtype Convert_Functions_Info is Dsp_Convert_Functions.Info;


   --  ----------------------------------------------------------------------


   type CORRELATION_NAME is (SQL_CN_NONE,
                             SQL_CN_DIFFERENT,
                             SQL_CN_ANY);
   for CORRELATION_NAME'Size use SQLSMALLINT'Size;

   package Dsp_Correlation_Name is new
     Dispatch.A_Enumerated (SQL_CORRELATION_NAME,
                            CORRELATION_NAME,
                            SQLSMALLINT,
                                              "CORRELATION_NAME");
   subtype Correlation_Name_Info is Dsp_Correlation_Name.Info;


   --  ----------------------------------------------------------------------


   type CREATE_ASSERTION is (SQL_CA_CREATE_ASSERTION,
                             SQL_CA_RESERVED1,
                             SQL_CA_RESERVED2,
                             SQL_CA_RESERVED3,
                             SQL_CA_CONSTRAINT_INITIALLY_DEFERRED,
                             SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE,
                             SQL_CA_CONSTRAINT_DEFERRABLE,
                             SQL_CA_CONSTRAINT_NON_DEFERRABLE);

   type CREATE_ASSERTION_BITMAP is
     array (CREATE_ASSERTION) of Boolean;

   package Dsp_Create_Assertion is new
     Dispatch.A_Bitmap (SQL_CREATE_ASSERTION,
                        CREATE_ASSERTION,
                        CREATE_ASSERTION_BITMAP);
   subtype Create_Assertion_Info is Dsp_Create_Assertion.Info;


   --  ----------------------------------------------------------------------


   type CREATE_CHARACTER_SET is (SQL_CCS_CREATE_CHARACTER_SET,
                                 SQL_CCS_COLLATE_CLAUSE,
                                 SQL_CCS_LIMITED_COLLATION);

   type CREATE_CHARACTER_SET_BITMAP is
     array (CREATE_CHARACTER_SET) of Boolean;

   package Dsp_Create_Character_Set is new
     Dispatch.A_Bitmap (SQL_CREATE_CHARACTER_SET,
                        CREATE_CHARACTER_SET,
                        CREATE_CHARACTER_SET_BITMAP);
   subtype Create_Character_Set_Info is Dsp_Create_Character_Set.Info;


   --  ----------------------------------------------------------------------


   type CREATE_COLLATION is (SQL_CCOL_CREATE_COLLATION);

   type CREATE_COLLATION_BITMAP is
     array (CREATE_COLLATION) of Boolean;

   package Dsp_Create_Collation is new
     Dispatch.A_Bitmap (SQL_CREATE_COLLATION,
                        CREATE_COLLATION,
                        CREATE_COLLATION_BITMAP);
   subtype Create_Collation_Info is Dsp_Create_Collation.Info;


   --  ----------------------------------------------------------------------


   type CREATE_DOMAIN is (SQL_CDO_CREATE_DOMAIN,
                          SQL_CDO_DEFAULT,
                          SQL_CDO_CONSTRAINT,
                          SQL_CDO_COLLATION,
                          SQL_CDO_CONSTRAINT_NAME_DEFINITION,
                          SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED,
                          SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE,
                          SQL_CDO_CONSTRAINT_DEFERRABLE,
                          SQL_CDO_CONSTRAINT_NON_DEFERRABLE);

   type CREATE_DOMAIN_BITMAP is
     array (CREATE_DOMAIN) of Boolean;

   package Dsp_Create_Domain is new
     Dispatch.A_Bitmap (SQL_CREATE_DOMAIN,
                        CREATE_DOMAIN,
                        CREATE_DOMAIN_BITMAP);
   subtype Create_Domain_Info is Dsp_Create_Domain.Info;


   --  ----------------------------------------------------------------------


   type CREATE_SCHEMA is (SQL_CS_CREATE_SCHEMA,
                          SQL_CS_AUTHORIZATION,
                          SQL_CS_DEFAULT_CHARACTER_SET);

   type CREATE_SCHEMA_BITMAP is
     array (CREATE_SCHEMA) of Boolean;

   package Dsp_Create_Schema is new
     Dispatch.A_Bitmap (SQL_CREATE_SCHEMA,
                        CREATE_SCHEMA,
                        CREATE_SCHEMA_BITMAP);
   subtype Create_Schema_Info is Dsp_Create_Schema.Info;


   --  ----------------------------------------------------------------------


   type CREATE_TABLE is (SQL_CT_CREATE_TABLE,
                         SQL_CT_COMMIT_PRESERVE,
                         SQL_CT_COMMIT_DELETE,
                         SQL_CT_GLOBAL_TEMPORARY,
                         SQL_CT_LOCAL_TEMPORARY,
                         SQL_CT_CONSTRAINT_INITIALLY_DEFERRED,
                         SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE,
                         SQL_CT_CONSTRAINT_DEFERRABLE,
                         SQL_CT_CONSTRAINT_NON_DEFERRABLE,
                         SQL_CT_COLUMN_CONSTRAINT,
                         SQL_CT_COLUMN_DEFAULT,
                         SQL_CT_COLUMN_COLLATION,
                         SQL_CT_TABLE_CONSTRAINT,
                         SQL_CT_CONSTRAINT_NAME_DEFINITION);

   type CREATE_TABLE_BITMAP is
     array (CREATE_TABLE) of Boolean;

   package Dsp_Create_Table is new
     Dispatch.A_Bitmap (SQL_CREATE_TABLE,
                        CREATE_TABLE,
                        CREATE_TABLE_BITMAP);
   subtype Create_Table_Info is Dsp_Create_Table.Info;


   --  ----------------------------------------------------------------------


   type CREATE_TRANSLATION is (SQL_CTR_CREATE_TRANSLATION);

   type CREATE_TRANSLATION_BITMAP is
     array (CREATE_TRANSLATION) of Boolean;

   package Dsp_Create_Translation is new
     Dispatch.A_Bitmap (SQL_CREATE_TRANSLATION,
                        CREATE_TRANSLATION,
                        CREATE_TRANSLATION_BITMAP);
   subtype Create_Translation_Info is Dsp_Create_Translation.Info;


   --  ----------------------------------------------------------------------


   type CREATE_VIEW is (SQL_CV_CREATE_VIEW,
                        SQL_CV_CHECK_OPTION,
                        SQL_CV_CASCADED,
                        SQL_CV_LOCAL);

   type CREATE_VIEW_BITMAP is
     array (CREATE_VIEW) of Boolean;

   package Dsp_Create_View is new
     Dispatch.A_Bitmap (SQL_CREATE_VIEW,
                        CREATE_VIEW,
                        CREATE_VIEW_BITMAP);
   subtype Create_View_Info is Dsp_Create_View.Info;


   --  ----------------------------------------------------------------------
   type STATIC_SENSITIVITY is (SQL_SS_ADDITIONS,
                               SQL_SS_DELETIONS,
                               SQL_SS_UPDATES);

   type STATIC_SENSITIVITY_BITMAP is array (STATIC_SENSITIVITY) of Boolean;

   package Dsp_StaticSensitivity is new
     Dispatch.A_Bitmap (SQL_STATIC_SENSITIVITY,
                        STATIC_SENSITIVITY,
                        STATIC_SENSITIVITY_BITMAP);
   subtype Static_Sensitivity_Info is Dsp_StaticSensitivity.Info;


   --  ----------------------------------------------------------------------
   type SCROLL_CONCURRENCY is (SQL_SCCO_READ_ONLY,
                               SQL_SCCO_LOCK,
                               SQL_SCCO_OPT_ROWVER,
                               SQL_SCCO_OPT_VALUES);

   type SCROLL_CONCURRENCY_BITMAP is array (SCROLL_CONCURRENCY) of Boolean;

   package Dsp_ScrollConcurrency is new
     Dispatch.A_Bitmap (SQL_SCROLL_CONCURRENCY,
                        SCROLL_CONCURRENCY,
                        SCROLL_CONCURRENCY_BITMAP);
   subtype Scroll_Concurrency_Info is Dsp_ScrollConcurrency.Info;

   --  ----------------------------------------------------------------------

   --  Bitmasks for
   --  SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
   --  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,
   --  SQL_KEYSET_CURSOR_ATTRIBUTES1
   --  SQL_STATIC_CURSOR_ATTRIBUTES1
   type CURSOR_ATTRIBUTES1 is (SQL_CA1_NEXT,
                               SQL_CA1_ABSOLUTE,
                               SQL_CA1_RELATIVE,
                               SQL_CA1_BOOKMARK,
                               SQL_CA1_RESERVED1,
                               SQL_CA1_RESERVED2,
                               SQL_CA1_LOCK_NO_CHANGE,
                               SQL_CA1_LOCK_EXCLUSIVE,
                               SQL_CA1_LOCK_UNLOCK,
                               SQL_CA1_POS_POSITION,
                               SQL_CA1_POS_UPDATE,
                               SQL_CA1_POS_DELETE,
                               SQL_CA1_POS_REFRESH,
                               SQL_CA1_POSITIONED_UPDATE,
                               SQL_CA1_POSITIONED_DELETE,
                               SQL_CA1_SELECT_FOR_UPDATE,
                               SQL_CA1_BULK_ADD,
                               SQL_CA1_BULK_UPDATE_BY_BOOKMARK,
                               SQL_CA1_BULK_DELETE_BY_BOOKMARK,
                               SQL_CA1_BULK_FETCH_BY_BOOKMARK);

   type CURSOR_ATTRIBUTES1_BITMAP is
     array (CURSOR_ATTRIBUTES1) of Boolean;

   subtype SUPPORTED_FETCH_ORIENTATIONS is CURSOR_ATTRIBUTES1
     range SQL_CA1_NEXT .. SQL_CA1_BOOKMARK;

   subtype SUPPORTED_SETPOS_LOCKS is CURSOR_ATTRIBUTES1
     range SQL_CA1_LOCK_NO_CHANGE .. SQL_CA1_LOCK_UNLOCK;

   subtype SUPPORTED_SETPOS_OPERATIONS is CURSOR_ATTRIBUTES1
     range SQL_CA1_POS_POSITION .. SQL_CA1_POS_REFRESH;

   subtype SUPPORTED_POSITIONED_UPDATED is CURSOR_ATTRIBUTES1
     range SQL_CA1_POSITIONED_UPDATE .. SQL_CA1_SELECT_FOR_UPDATE;

   subtype SUPPORTED_BULK_OPERATIONS is CURSOR_ATTRIBUTES1
     range SQL_CA1_BULK_ADD .. SQL_CA1_BULK_FETCH_BY_BOOKMARK;

   package Dsp_Cursor_Attribute1 is new
     Dispatch.A_Bitmap (SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
                        CURSOR_ATTRIBUTES1,
                        CURSOR_ATTRIBUTES1_BITMAP);

   --  ---------------------------------------------------------------------
   --  Bitmasks for
   --  SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
   --  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,
   --  SQL_KEYSET_CURSOR_ATTRIBUTES2
   --  SQL_STATIC_CURSOR_ATTRIBUTES2
   type CURSOR_ATTRIBUTES2 is (SQL_CA2_READ_ONLY_CONCURRENCY,
                               SQL_CA2_LOCK_CONCURRENCY,
                               SQL_CA2_OPT_ROWVER_CONCURRENCY,
                               SQL_CA2_OPT_VALUES_CONCURRENCY,
                               SQL_CA2_SENSITIVITY_ADDITIONS,
                               SQL_CA2_SENSITIVITY_DELETIONS,
                               SQL_CA2_SENSITIVITY_UPDATES,
                               SQL_CA2_MAX_ROWS_SELECT,
                               SQL_CA2_MAX_ROWS_INSERT,
                               SQL_CA2_MAX_ROWS_DELETE,
                               SQL_CA2_MAX_ROWS_UPDATE,
                               SQL_CA2_MAX_ROWS_CATALOG,
                               SQL_CA2_CRC_EXACT,
                               SQL_CA2_CRC_APPROXIMATE,
                               SQL_CA2_SIMULATE_NON_UNIQUE,
                               SQL_CA2_SIMULATE_TRY_UNIQUE,
                               SQL_CA2_SIMULATE_UNIQUE);

   type CURSOR_ATTRIBUTES2_BITMAP is
     array (CURSOR_ATTRIBUTES2) of Boolean;

   subtype SUPPORTED_SCROLL_CONCURRENCY is CURSOR_ATTRIBUTES2
     range SQL_CA2_READ_ONLY_CONCURRENCY .. SQL_CA2_OPT_VALUES_CONCURRENCY;

   subtype CURSORS_OWN_SENSITIVITY is CURSOR_ATTRIBUTES2
     range SQL_CA2_SENSITIVITY_ADDITIONS .. SQL_CA2_SENSITIVITY_UPDATES;

   subtype MAX_ROWS_SEMANTICS is CURSOR_ATTRIBUTES2
     range SQL_CA2_MAX_ROWS_SELECT .. SQL_CA2_MAX_ROWS_CATALOG;

   SQL_CA2_MAX_ROWS_AFFECTS_ALL : constant CURSOR_ATTRIBUTES2_BITMAP :=
     (MAX_ROWS_SEMANTICS'Range => True, others => False);

   subtype DIAG_CURSOR_ROW_COUNT_SEMANTIC is CURSOR_ATTRIBUTES2
     range SQL_CA2_CRC_EXACT .. SQL_CA2_CRC_APPROXIMATE;

   subtype SIMULATABLE_POSITIONED_STATEMENTS is CURSOR_ATTRIBUTES2
     range SQL_CA2_SIMULATE_NON_UNIQUE .. SQL_CA2_SIMULATE_UNIQUE;

   package Dsp_Cursor_Attribute2 is new
     Dispatch.A_Bitmap (SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
                        CURSOR_ATTRIBUTES2,
                        CURSOR_ATTRIBUTES2_BITMAP);

   subtype Cursor_Attribute_Info1 is Dsp_Cursor_Attribute1.Info;
   subtype Cursor_Attribute_Info2 is Dsp_Cursor_Attribute2.Info;


   --  ----------------------------------------------------------------------


   type CURSOR_COMMIT_BEHAVIOR is (SQL_CB_DELETE,
                                   SQL_CB_CLOSE,
                                   SQL_CB_PRESERVE);
   for CURSOR_COMMIT_BEHAVIOR'Size use SQLSMALLINT'Size;

   package Dsp_Cursor_Commit_Behavior is new
     Dispatch.A_Enumerated (SQL_CURSOR_COMMIT_BEHAVIOR,
                            CURSOR_COMMIT_BEHAVIOR,
                            SQLSMALLINT,
                            "CURSOR_COMMIT_BEHAVIOR");
   subtype Cursor_Commit_Behavior_Info is
     Dsp_Cursor_Commit_Behavior.Info;
   subtype Cursor_Rollback_Behavior_Info is
     Dsp_Cursor_Commit_Behavior.Info;


   --  ----------------------------------------------------------------------


   package Dsp_Cursor_Sensitivity is new
     Dispatch.A_Enumerated
     (SQL_CURSOR_SENSITIVITY,
      GNU.DB.SQLCLI.Statement_Attribute.CURSOR_SENSITIVITY,
      SQLINTEGER,
      "CURSOR_SENSITIVITY");
   subtype Cursor_Sensitivity_Info is Dsp_Cursor_Sensitivity.Info;


   --  ----------------------------------------------------------------------


   type DATETIME_LITERAL is (SQL_DL_SQL92_DATE,
                             SQL_DL_SQL92_TIME,
                             SQL_DL_SQL92_TIMESTAMP,
                             SQL_DL_SQL92_INTERVAL_YEAR,
                             SQL_DL_SQL92_INTERVAL_MONTH,
                             SQL_DL_SQL92_INTERVAL_DAY,
                             SQL_DL_SQL92_INTERVAL_HOUR,
                             SQL_DL_SQL92_INTERVAL_MINUTE,
                             SQL_DL_SQL92_INTERVAL_SECOND,
                             SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH,
                             SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR,
                             SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE,
                             SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND,
                             SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE,
                             SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND,
                             SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND);

   type DATETIME_LITERAL_BITMAP is
     array (DATETIME_LITERAL) of Boolean;

   package Dsp_DateTime_Literals is new
     Dispatch.A_Bitmap (SQL_DATETIME_LITERALS,
                        DATETIME_LITERAL,
                        DATETIME_LITERAL_BITMAP);
   subtype DateTime_Literals_Info is Dsp_DateTime_Literals.Info;


   --  ----------------------------------------------------------------------


   type DDL_INDEX is (SQL_DI_CREATE_INDEX,
                      SQL_DI_DROP_INDEX);

   type DDL_INDEX_BITMAP is
     array (DDL_INDEX) of Boolean;

   package Dsp_DDL_Index is new
     Dispatch.A_Bitmap (SQL_DDL_INDEX,
                        DDL_INDEX,
                        DDL_INDEX_BITMAP);
   subtype DDL_Index_Info is Dsp_DDL_Index.Info;


   --  ----------------------------------------------------------------------


   type DROP_ASSERTION is (SQL_DA_DROP_ASSERTION);

   type DROP_ASSERTION_BITMAP is
     array (DROP_ASSERTION) of Boolean;

   package Dsp_Drop_Assertion is new
     Dispatch.A_Bitmap (SQL_DROP_ASSERTION,
                        DROP_ASSERTION,
                        DROP_ASSERTION_BITMAP);
   subtype Drop_Assertion_Info is Dsp_Drop_Assertion.Info;


   --  ----------------------------------------------------------------------


   type DROP_CHARACTER_SET is (SQL_DCS_DROP_CHARACTER_SET);

   type DROP_CHARACTER_SET_BITMAP is
     array (DROP_CHARACTER_SET) of Boolean;

   package Dsp_Drop_Character_Set is new
     Dispatch.A_Bitmap (SQL_DROP_CHARACTER_SET,
                        DROP_CHARACTER_SET,
                        DROP_CHARACTER_SET_BITMAP);
   subtype Drop_Character_Set_Info is Dsp_Drop_Character_Set.Info;


   --  ----------------------------------------------------------------------


   type DROP_COLLATION is (SQL_DC_DROP_COLLATION);

   type DROP_COLLATION_BITMAP is
     array (DROP_COLLATION) of Boolean;

   package Dsp_Drop_Collation is new
     Dispatch.A_Bitmap (SQL_DROP_COLLATION,
                        DROP_COLLATION,
                        DROP_COLLATION_BITMAP);
   subtype Drop_Collation_Info is Dsp_Drop_Collation.Info;


   --  ----------------------------------------------------------------------


   type DROP_DOMAIN is (SQL_DD_DROP_DOMAIN,
                        SQL_DD_RESTRICT,
                        SQL_DD_CASCADE);

   type DROP_DOMAIN_BITMAP is
     array (DROP_DOMAIN) of Boolean;

   package Dsp_Drop_Domain is new
     Dispatch.A_Bitmap (SQL_DROP_DOMAIN,
                        DROP_DOMAIN,
                        DROP_DOMAIN_BITMAP);
   subtype Drop_Domain_Info is Dsp_Drop_Domain.Info;


   --  ----------------------------------------------------------------------


   type DROP_SCHEMA is (SQL_DS_DROP_SCHEMA,
                        SQL_DS_RESTRICT,
                        SQL_DS_CASCADE);

   type DROP_SCHEMA_BITMAP is
     array (DROP_SCHEMA) of Boolean;

   package Dsp_Drop_Schema is new
     Dispatch.A_Bitmap (SQL_DROP_SCHEMA,
                        DROP_SCHEMA,
                        DROP_SCHEMA_BITMAP);
   subtype Drop_Schema_Info is Dsp_Drop_Schema.Info;


   --  ----------------------------------------------------------------------


   type DROP_TABLE is (SQL_DT_DROP_TABLE,
                       SQL_DT_RESTRICT,
                       SQL_DT_CASCADE);

   type DROP_TABLE_BITMAP is
     array (DROP_TABLE) of Boolean;

   package Dsp_Drop_Table is new
     Dispatch.A_Bitmap (SQL_DROP_TABLE,
                        DROP_TABLE,
                        DROP_TABLE_BITMAP);
   subtype Drop_Table_Info is Dsp_Drop_Table.Info;


   --  ----------------------------------------------------------------------


   type DROP_TRANSLATION is (SQL_DTR_DROP_TRANSLATION);

   type DROP_TRANSLATION_BITMAP is
      array (DROP_TRANSLATION) of Boolean;

   package Dsp_Drop_Translation is new
     Dispatch.A_Bitmap (SQL_DROP_TRANSLATION,
                        DROP_TRANSLATION,
                        DROP_TRANSLATION_BITMAP);
   subtype Drop_Translation_Info is Dsp_Drop_Translation.Info;


   --  ----------------------------------------------------------------------


   type DROP_VIEW is (SQL_DV_DROP_VIEW,
                      SQL_DV_RESTRICT,
                      SQL_DV_CASCADE);

   type DROP_VIEW_BITMAP is
     array (DROP_VIEW) of Boolean;

   package Dsp_Drop_View is new
     Dispatch.A_Bitmap (SQL_DROP_VIEW,
                        DROP_VIEW,
                        DROP_VIEW_BITMAP);
   subtype Drop_View_Info is Dsp_Drop_View.Info;


   --  ----------------------------------------------------------------------


   type FILE_USAGE is (SQL_FILE_NOT_SUPPORTED,
                       SQL_FILE_TABLE,
                       SQL_FILE_CATALOG);
   for FILE_USAGE'Size use SQLSMALLINT'Size;
   SQL_FILE_QUALIFIER : constant FILE_USAGE := SQL_FILE_CATALOG;

   package Dsp_File_Usage is new
     Dispatch.A_Enumerated (SQL_FILE_USAGE,
                            FILE_USAGE,
                            SQLSMALLINT,
                            "FILE_USAGE");
   subtype File_Usage_Info is Dsp_File_Usage.Info;


   --  ----------------------------------------------------------------------


   type GETDATA_EXTENSION is (SQL_GD_ANY_COLUMN,
                              SQL_GD_ANY_ORDER,
                              SQL_GD_BLOCK,
                              SQL_GD_BOUND);

   type GETDATA_EXTENSION_BITMAP is
     array (GETDATA_EXTENSION) of Boolean;

   package Dsp_GetData_Extension is new
     Dispatch.A_Bitmap (SQL_GETDATA_EXTENSIONS,
                        GETDATA_EXTENSION,
                        GETDATA_EXTENSION_BITMAP);
   subtype GetData_Extension_Info is Dsp_GetData_Extension.Info;


   --  ----------------------------------------------------------------------


   type GROUP_BY is (SQL_GB_NOT_SUPPORTED,
                     SQL_GB_GROUP_BY_EQUALS_SELECT,
                     SQL_GB_GROUP_BY_CONTAINS_SELECT,
                     SQL_GB_NO_RELATION,
                     SQL_GB_COLLATE);
   for GROUP_BY'Size use SQLSMALLINT'Size;

   package Dsp_Group_By is new
     Dispatch.A_Enumerated (SQL_GROUP_BY,
                            GROUP_BY,
                            SQLSMALLINT,
                            "GROUP_BY");
   subtype Group_By_Info is Dsp_Group_By.Info;


   --  ----------------------------------------------------------------------


   type IDENTIFIER_CASE is (SQL_IC_UPPER,
                            SQL_IC_LOWER,
                            SQL_IC_SENSITIVE,
                            SQL_IC_MIXED);
   for IDENTIFIER_CASE'Size use SQLSMALLINT'Size;
   for IDENTIFIER_CASE use (SQL_IC_UPPER       => 1,
                            SQL_IC_LOWER       => 2,
                            SQL_IC_SENSITIVE   => 3,
                            SQL_IC_MIXED       => 4);

   package Dsp_Identifier_Case is new
     Dispatch.A_Enumerated (SQL_IDENTIFIER_CASE,
                            IDENTIFIER_CASE,
                            SQLSMALLINT,
                            "IDENTIFIER_CASE");
   subtype Identifier_Case_Info        is Dsp_Identifier_Case.Info;
   subtype Identifier_Quoted_Case_Info is Dsp_Identifier_Case.Info;


   --  ----------------------------------------------------------------------


   type INDEX_KEYWORD is (SQL_IK_NONE,
                          SQL_IK_ASC,
                          SQL_IK_DESC);

   type INDEX_KEYWORD_BITMAP is
     array (INDEX_KEYWORD) of Boolean;

   SQL_IK_ALL : constant INDEX_KEYWORD_BITMAP := (INDEX_KEYWORD'Range => True);

   package Dsp_Index_Keyword is new
     Dispatch.A_Bitmap (SQL_INDEX_KEYWORDS,
                        INDEX_KEYWORD,
                        INDEX_KEYWORD_BITMAP);
   subtype Index_Keyword_Info is Dsp_Index_Keyword.Info;


   --  ----------------------------------------------------------------------


   type INFO_SCHEMA_VIEW is (SQL_ISV_ASSERTIONS,
                             SQL_ISV_CHARACTER_SETS,
                             SQL_ISV_CHECK_CONSTRAINTS,
                             SQL_ISV_COLLATIONS,
                             SQL_ISV_COLUMN_DOMAIN_USAGE,
                             SQL_ISV_COLUMN_PRIVILEGES,
                             SQL_ISV_COLUMNS,
                             SQL_ISV_CONSTRAINT_COLUMN_USAGE,
                             SQL_ISV_CONSTRAINT_TABLE_USAGE,
                             SQL_ISV_DOMAIN_CONSTRAINTS,
                             SQL_ISV_DOMAINS,
                             SQL_ISV_KEY_COLUMN_USAGE,
                             SQL_ISV_REFERENTIAL_CONSTRAINTS,
                             SQL_ISV_SCHEMATA,
                             SQL_ISV_SQL_LANGUAGES,
                             SQL_ISV_TABLE_CONSTRAINTS,
                             SQL_ISV_TABLE_PRIVILEGES,
                             SQL_ISV_TABLES,
                             SQL_ISV_TRANSLATIONS,
                             SQL_ISV_USAGE_PRIVILEGES,
                             SQL_ISV_VIEW_COLUMN_USAGE,
                             SQL_ISV_VIEW_TABLE_USAGE,
                             SQL_ISV_VIEWS);

   type INFO_SCHEMA_VIEW_BITMAP is
     array (INFO_SCHEMA_VIEW) of Boolean;

   package Dsp_Info_Schema_Views is new
     Dispatch.A_Bitmap (SQL_INFO_SCHEMA_VIEWS,
                        INFO_SCHEMA_VIEW,
                        INFO_SCHEMA_VIEW_BITMAP);
   subtype Info_Schema_Views_Info is Dsp_Info_Schema_Views.Info;


   --  ----------------------------------------------------------------------


   type INSERT_STATEMENT is (SQL_IS_INSERT_LITERALS,
                             SQL_IS_INSERT_SEARCHED,
                             SQL_IS_SELECT_INTO);

   type INSERT_STATEMENT_BITMAP is
     array (INSERT_STATEMENT) of Boolean;

   package Dsp_Insert_Statement is new
     Dispatch.A_Bitmap (SQL_INSERT_STATEMENT,
                        INSERT_STATEMENT,
                        INSERT_STATEMENT_BITMAP);
   subtype Insert_Statement_Info is Dsp_Insert_Statement.Info;


   --  ----------------------------------------------------------------------


   type NON_NULLABLE_COLUMNS is (SQL_NNC_NULL,
                                 SQL_NNC_NON_NULL);
   for NON_NULLABLE_COLUMNS'Size use SQLSMALLINT'Size;

   package Dsp_Non_Nullable_Columns is new
     Dispatch.A_Enumerated (SQL_NON_NULLABLE_COLUMNS,
                            NON_NULLABLE_COLUMNS,
                            SQLSMALLINT,
                            "NON_NULLABLE_COLUMNS");
   subtype Non_Nullable_Columns_Info is Dsp_Non_Nullable_Columns.Info;


   --  ----------------------------------------------------------------------


   type NULL_COLLATION is (SQL_NC_HIGH,
                           SQL_NC_LOW,
                           SQL_NC_START,
                           SQL_NC_END);
   for NULL_COLLATION use (SQL_NC_HIGH   => 0,
                           SQL_NC_LOW    => 1,
                           SQL_NC_START  => 2,
                           SQL_NC_END    => 4);
   for NULL_COLLATION'Size use SQLSMALLINT'Size;

   package Dsp_Null_Collation is new
     Dispatch.A_Enumerated (SQL_NULL_COLLATION,
                            NULL_COLLATION,
                            SQLSMALLINT,
                            "NULL_COLLATION");
   subtype Null_Collation_Info is Dsp_Null_Collation.Info;


   --  ----------------------------------------------------------------------

   --  ---------------------------------------------------------------------
   --  SQL_NUMERIC_FUNCTIONS functions
   type NUMERIC_FUNCTION is (SQL_FN_NUM_ABS,
                             SQL_FN_NUM_ACOS,
                             SQL_FN_NUM_ASIN,
                             SQL_FN_NUM_ATAN,
                             SQL_FN_NUM_ATAN2,
                             SQL_FN_NUM_CEILING,
                             SQL_FN_NUM_COS,
                             SQL_FN_NUM_COT,
                             SQL_FN_NUM_EXP,
                             SQL_FN_NUM_FLOOR,
                             SQL_FN_NUM_LOG,
                             SQL_FN_NUM_MOD,
                             SQL_FN_NUM_SIGN,
                             SQL_FN_NUM_SIN,
                             SQL_FN_NUM_SQRT,
                             SQL_FN_NUM_TAN,
                             SQL_FN_NUM_PI,
                             SQL_FN_NUM_RAND,
                             SQL_FN_NUM_DEGREES,
                             SQL_FN_NUM_LOG10,
                             SQL_FN_NUM_POWER,
                             SQL_FN_NUM_RADIANS,
                             SQL_FN_NUM_ROUND,
                             SQL_FN_NUM_TRUNCATE);

   type NUMERIC_FUNCTION_BITMAP is
     array (NUMERIC_FUNCTION) of Boolean;

   package Dsp_Numeric_Function is new
     Dispatch.A_Bitmap (SQL_NUMERIC_FUNCTIONS,
                        NUMERIC_FUNCTION,
                        NUMERIC_FUNCTION_BITMAP);
   subtype Numeric_Function_Info is Dsp_Numeric_Function.Info;


   --  ----------------------------------------------------------------------


   type OJ_CAPABILITY is (SQL_OJ_LEFT,
                          SQL_OJ_RIGHT,
                          SQL_OJ_FULL,
                          SQL_OJ_NESTED,
                          SQL_OJ_NOT_ORDERED,
                          SQL_OJ_INNER,
                          SQL_OJ_ALL_COMPARISON_OPS);

   type OJ_CAPABILITY_BITMAP is
     array (OJ_CAPABILITY) of Boolean;

   package Dsp_Outer_Join_Capability is new
     Dispatch.A_Bitmap (SQL_OUTER_JOIN_CAPABILITIES,
                        OJ_CAPABILITY,
                        OJ_CAPABILITY_BITMAP);
   subtype Outer_Join_Capability_Info is Dsp_Outer_Join_Capability.Info;


   --  ----------------------------------------------------------------------


   type PARAM_ARRAY_ROW_COUNTS is (SQL_PARC_BATCH,
                                   SQL_PARC_NO_BATCH);
   for PARAM_ARRAY_ROW_COUNTS use (SQL_PARC_BATCH     => 1,
                                   SQL_PARC_NO_BATCH  => 2);
   for PARAM_ARRAY_ROW_COUNTS'Size use SQLINTEGER'Size;

   package Dsp_Param_Array_Row_Counts is new
     Dispatch.A_Enumerated (SQL_PARAM_ARRAY_ROW_COUNTS,
                            PARAM_ARRAY_ROW_COUNTS,
                            SQLINTEGER,
                                              "PARAM_ARRAY_ROW_COUNTS");
   subtype Param_Array_Row_Counts_Info is Dsp_Param_Array_Row_Counts.Info;


   --  ----------------------------------------------------------------------


   type PARAM_ARRAY_SELECT_BATCH is (SQL_PAS_BATCH,
                                     SQL_PAS_NO_BATCH,
                                     SQL_PAS_NO_SELECT);
   for PARAM_ARRAY_SELECT_BATCH use (SQL_PAS_BATCH     => 1,
                                     SQL_PAS_NO_BATCH  => 2,
                                     SQL_PAS_NO_SELECT => 3);
   for PARAM_ARRAY_SELECT_BATCH'Size use SQLINTEGER'Size;

   package Dsp_Param_Array_Select_Batch is new
     Dispatch.A_Enumerated (SQL_PARAM_ARRAY_SELECTS,
                            PARAM_ARRAY_SELECT_BATCH,
                            SQLINTEGER,
                                              "PARAM_ARRAY_SELECT_BATCH");
   subtype Param_Array_Select_Batch_Info is Dsp_Param_Array_Select_Batch.Info;


   --  ----------------------------------------------------------------------


   type QUALIFIER_LOCATION is (SQL_QL_START,
                               SQL_QL_END);
   for QUALIFIER_LOCATION use (SQL_QL_START   => 1,
                               SQL_QL_END     => 2);
   for QUALIFIER_LOCATION'Size use SQLSMALLINT'Size;

   package Dsp_Qualifier_Location is new
     Dispatch.A_Enumerated (SQL_QUALIFIER_LOCATION,
                            QUALIFIER_LOCATION,
                            SQLSMALLINT,
                            "QUALIFIER_LOCATION");
   subtype Qualifier_Location_Info is Dsp_Qualifier_Location.Info;


   --  ----------------------------------------------------------------------


   type QUALIFIER_USAGE is (SQL_QU_DML_STATEMENTS,
                            SQL_QU_PROCEDURE_INVOCATION,
                            SQL_QU_TABLE_DEFINITION,
                            SQL_QU_INDEX_DEFINITION,
                            SQL_QU_PRIVILEGE_DEFINITION);

   SQL_CU_DML_STATEMENTS       : constant QUALIFIER_USAGE
     := SQL_QU_DML_STATEMENTS;
   SQL_CU_PROCEDURE_INVOCATION : constant QUALIFIER_USAGE
     := SQL_QU_PROCEDURE_INVOCATION;
   SQL_CU_TABLE_DEFINITION     : constant QUALIFIER_USAGE
     := SQL_QU_TABLE_DEFINITION;
   SQL_CU_INDEX_DEFINITION     : constant QUALIFIER_USAGE
     := SQL_QU_INDEX_DEFINITION;
   SQL_CU_PRIVILEGE_DEFINITION : constant QUALIFIER_USAGE
     := SQL_QU_PRIVILEGE_DEFINITION;

   type QUALIFIER_USAGE_BITMAP is
     array (QUALIFIER_USAGE) of Boolean;

   package Dsp_Qualifier_Usage is new
     Dispatch.A_Bitmap (SQL_QUALIFIER_USAGE,
                        QUALIFIER_USAGE,
                        QUALIFIER_USAGE_BITMAP);
   subtype Qualifier_Usage_Info is Dsp_Qualifier_Usage.Info;


   --  ----------------------------------------------------------------------


   type SCHEMA_USAGE is (SQL_SU_DML_STATEMENTS,
                         SQL_SU_PROCEDURE_INVOCATION,
                         SQL_SU_TABLE_DEFINITION,
                         SQL_SU_INDEX_DEFINITION,
                         SQL_SU_PRIVILEGE_DEFINITION);

   SQL_OU_DML_STATEMENTS       : constant SCHEMA_USAGE
     := SQL_SU_DML_STATEMENTS;
   SQL_OU_PROCEDURE_INVOCATION : constant SCHEMA_USAGE
     := SQL_SU_PROCEDURE_INVOCATION;
   SQL_OU_TABLE_DEFINITION     : constant SCHEMA_USAGE
     := SQL_SU_TABLE_DEFINITION;
   SQL_OU_INDEX_DEFINITION     : constant SCHEMA_USAGE
     := SQL_SU_INDEX_DEFINITION;
   SQL_OU_PRIVILEGE_DEFINITION : constant SCHEMA_USAGE
     := SQL_SU_PRIVILEGE_DEFINITION;

   type SCHEMA_USAGE_BITMAP is
     array (SCHEMA_USAGE) of Boolean;

   package Dsp_Schema_Usage is new
     Dispatch.A_Bitmap (SQL_SCHEMA_USAGE,
                        SCHEMA_USAGE,
                        SCHEMA_USAGE_BITMAP);
   subtype Schema_Usage_Info is Dsp_Schema_Usage.Info;


   --  ----------------------------------------------------------------------


   type SCROLL_OPTION is (SQL_SO_FORWARD_ONLY,
                          SQL_SO_KEYSET_DRIVEN,
                          SQL_SO_DYNAMIC,
                          SQL_SO_MIXED,
                          SQL_SO_STATIC);

   type SCROLL_OPTION_BITMAP is
     array (SCROLL_OPTION) of Boolean;

   package Dsp_Scroll_Option is new
     Dispatch.A_Bitmap (SQL_SCROLL_OPTIONS,
                        SCROLL_OPTION,
                        SCROLL_OPTION_BITMAP);
   subtype Scroll_Option_Info is Dsp_Scroll_Option.Info;


   --  ----------------------------------------------------------------------


   type SQL92_DATETIME_FUNCTION is (SQL_SDF_CURRENT_DATE,
                                    SQL_SDF_CURRENT_TIME,
                                    SQL_SDF_CURRENT_TIMESTAMP);

   type SQL92_DATETIME_FUNCTION_BITMAP is
     array (SQL92_DATETIME_FUNCTION) of Boolean;

   package Dsp_SQL92_DateTime_Function is new
     Dispatch.A_Bitmap (SQL_SQL92_DATETIME_FUNCTIONS,
                        SQL92_DATETIME_FUNCTION,
                        SQL92_DATETIME_FUNCTION_BITMAP);
   subtype SQL92_DateTime_Function_Info is
     Dsp_SQL92_DateTime_Function.Info;


   --  ----------------------------------------------------------------------


   type SQL92_FOREIGN_KEY_DELETE_RULE is (SQL_SFKD_CASCADE,
                                          SQL_SFKD_NO_ACTION,
                                          SQL_SFKD_SET_DEFAULT,
                                          SQL_SFKD_SET_NULL);

   type SQL92_FOREIGN_KEY_DELETE_RULE_BITMAP is
     array (SQL92_FOREIGN_KEY_DELETE_RULE) of Boolean;

   package Dsp_SQL92_Foreign_Key_Delete_Rule is new
     Dispatch.A_Bitmap (SQL_SQL92_FOREIGN_KEY_DELETE_RULE,
                        SQL92_FOREIGN_KEY_DELETE_RULE,
                        SQL92_FOREIGN_KEY_DELETE_RULE_BITMAP);
   subtype SQL92_Foreign_Key_Delete_Rule_Info is
     Dsp_SQL92_Foreign_Key_Delete_Rule.Info;


   --  ----------------------------------------------------------------------


   type SQL92_FOREIGN_KEY_UPDATE_RULE is (SQL_SFKU_CASCADE,
                                          SQL_SFKU_NO_ACTION,
                                          SQL_SFKU_SET_DEFAULT,
                                          SQL_SFKU_SET_NULL);

   type SQL92_FOREIGN_KEY_UPDATE_RULE_BITMAP is
     array (SQL92_FOREIGN_KEY_UPDATE_RULE) of Boolean;


   package Dsp_SQL92_Foreign_Key_Update_Rule is new
     Dispatch.A_Bitmap (SQL_SQL92_FOREIGN_KEY_UPDATE_RULE,
                        SQL92_FOREIGN_KEY_UPDATE_RULE,
                        SQL92_FOREIGN_KEY_UPDATE_RULE_BITMAP);
   subtype SQL92_Foreign_Key_Update_Rule_Info is
     Dsp_SQL92_Foreign_Key_Update_Rule.Info;


   --  ----------------------------------------------------------------------


   type SQL92_GRANT is (SQL_SG_USAGE_ON_DOMAIN,
                        SQL_SG_USAGE_ON_CHARACTER_SET,
                        SQL_SG_USAGE_ON_COLLATION,
                        SQL_SG_USAGE_ON_TRANSLATION,
                        SQL_SG_WITH_GRANT_OPTION,
                        SQL_SG_DELETE_TABLE,
                        SQL_SG_INSERT_TABLE,
                        SQL_SG_INSERT_COLUMN,
                        SQL_SG_REFERENCES_TABLE,
                        SQL_SG_REFERENCES_COLUMN,
                        SQL_SG_SELECT_TABLE,
                        SQL_SG_UPDATE_TABLE,
                        SQL_SG_UPDATE_COLUMN);

   type SQL92_GRANT_BITMAP is
     array (SQL92_GRANT) of Boolean;

   package Dsp_SQL92_Grant is new
     Dispatch.A_Bitmap (SQL_SQL92_GRANT,
                        SQL92_GRANT,
                        SQL92_GRANT_BITMAP);
   subtype SQL92_Grant_Info is Dsp_SQL92_Grant.Info;


   --  ----------------------------------------------------------------------


   type SQL92_NUMERIC_VALUE_FUNCTION is (SQL_SNVF_BIT_LENGTH,
                                         SQL_SNVF_CHAR_LENGTH,
                                         SQL_SNVF_CHARACTER_LENGTH,
                                         SQL_SNVF_EXTRACT,
                                         SQL_SNVF_OCTET_LENGTH,
                                         SQL_SNVF_POSITION);

   type SQL92_NUMERIC_VALUE_FUNCTION_BITMAP is
     array (SQL92_NUMERIC_VALUE_FUNCTION) of Boolean;


   package Dsp_SQL92_Numeric_Value_Function is new
     Dispatch.A_Bitmap (SQL_SQL92_NUMERIC_VALUE_FUNCTIONS,
                        SQL92_NUMERIC_VALUE_FUNCTION,
                        SQL92_NUMERIC_VALUE_FUNCTION_BITMAP);
   subtype SQL92_Numeric_Value_Function_Info is
     Dsp_SQL92_Numeric_Value_Function.Info;


   --  ----------------------------------------------------------------------


   type SQL92_PREDICATE is (SQL_SP_EXISTS,
                            SQL_SP_ISNOTNULL,
                            SQL_SP_ISNULL,
                            SQL_SP_MATCH_FULL,
                            SQL_SP_MATCH_PARTIAL,
                            SQL_SP_MATCH_UNIQUE_FULL,
                            SQL_SP_MATCH_UNIQUE_PARTIAL,
                            SQL_SP_OVERLAPS,
                            SQL_SP_UNIQUE,
                            SQL_SP_LIKE,
                            SQL_SP_IN,
                            SQL_SP_BETWEEN,
                            SQL_SP_COMPARISON,
                            SQL_SP_QUANTIFIED_COMPARISON);

   type SQL92_PREDICATE_BITMAP is
     array (SQL92_PREDICATE) of Boolean;

   package Dsp_SQL92_Predicate is new
     Dispatch.A_Bitmap (SQL_SQL92_PREDICATES,
                        SQL92_PREDICATE,
                        SQL92_PREDICATE_BITMAP);
   subtype SQL92_Predicate_Info is Dsp_SQL92_Predicate.Info;


   --  ----------------------------------------------------------------------


   type SQL92_RELATIONAL_JOIN_OPERATOR is (SQL_SRJO_CORRESPONDING_CLAUSE,
                                           SQL_SRJO_CROSS_JOIN,
                                           SQL_SRJO_EXCEPT_JOIN,
                                           SQL_SRJO_FULL_OUTER_JOIN,
                                           SQL_SRJO_INNER_JOIN,
                                           SQL_SRJO_INTERSECT_JOIN,
                                           SQL_SRJO_LEFT_OUTER_JOIN,
                                           SQL_SRJO_NATURAL_JOIN,
                                           SQL_SRJO_RIGHT_OUTER_JOIN,
                                           SQL_SRJO_UNION_JOIN);

   type SQL92_RELATIONAL_JOIN_OPERATOR_BITMAP is
     array (SQL92_RELATIONAL_JOIN_OPERATOR) of Boolean;


   package Dsp_SQL92_Relational_Join_Operator is new
     Dispatch.A_Bitmap (SQL_SQL92_RELATIONAL_JOIN_OPERATORS,
                        SQL92_RELATIONAL_JOIN_OPERATOR,
                        SQL92_RELATIONAL_JOIN_OPERATOR_BITMAP);
   subtype SQL92_Relational_Join_Operator_Info is
     Dsp_SQL92_Relational_Join_Operator.Info;


   --  ----------------------------------------------------------------------


   type SQL92_REVOKE is (SQL_SR_USAGE_ON_DOMAIN,
                         SQL_SR_USAGE_ON_CHARACTER_SET,
                         SQL_SR_USAGE_ON_COLLATION,
                         SQL_SR_USAGE_ON_TRANSLATION,
                         SQL_SR_GRANT_OPTION_FOR,
                         SQL_SR_CASCADE,
                         SQL_SR_RESTRICT,
                         SQL_SR_DELETE_TABLE,
                         SQL_SR_INSERT_TABLE,
                         SQL_SR_INSERT_COLUMN,
                         SQL_SR_REFERENCES_TABLE,
                         SQL_SR_REFERENCES_COLUMN,
                         SQL_SR_SELECT_TABLE,
                         SQL_SR_UPDATE_TABLE,
                         SQL_SR_UPDATE_COLUMN);

   type SQL92_REVOKE_BITMAP is
     array (SQL92_REVOKE) of Boolean;

   package Dsp_SQL92_Revoke is new
     Dispatch.A_Bitmap (SQL_SQL92_REVOKE,
                        SQL92_REVOKE,
                        SQL92_REVOKE_BITMAP);
   subtype SQL92_Revoke_Info is Dsp_SQL92_Revoke.Info;


   --  ----------------------------------------------------------------------


   type SQL92_ROW_VALUE_CONSTRUCTOR is (SQL_SRVC_VALUE_EXPRESSION,
                                        SQL_SRVC_NULL,
                                        SQL_SRVC_DEFAULT,
                                        SQL_SRVC_ROW_SUBQUERY);

   type SQL92_ROW_VALUE_CONSTRUCTOR_BITMAP is
      array (SQL92_ROW_VALUE_CONSTRUCTOR) of Boolean;

   package Dsp_SQL92_Row_Value_Constructor is new
     Dispatch.A_Bitmap (SQL_SQL92_ROW_VALUE_CONSTRUCTOR,
                        SQL92_ROW_VALUE_CONSTRUCTOR,
                        SQL92_ROW_VALUE_CONSTRUCTOR_BITMAP);
   subtype SQL92_Row_Value_Constructor_Info is
     Dsp_SQL92_Row_Value_Constructor.Info;


   --  ----------------------------------------------------------------------


   type SQL92_STRING_FUNCTION is (SQL_SSF_CONVERT,
                                  SQL_SSF_LOWER,
                                  SQL_SSF_UPPER,
                                  SQL_SSF_SUBSTRING,
                                  SQL_SSF_TRANSLATE,
                                  SQL_SSF_TRIM_BOTH,
                                  SQL_SSF_TRIM_LEADING,
                                  SQL_SSF_TRIM_TRAILING);

   type SQL92_STRING_FUNCTION_BITMAP is
     array (SQL92_STRING_FUNCTION) of Boolean;

   package Dsp_SQL92_String_Function is new
     Dispatch.A_Bitmap (SQL_SQL92_STRING_FUNCTIONS,
                        SQL92_STRING_FUNCTION,
                        SQL92_STRING_FUNCTION_BITMAP);
   subtype SQL92_String_Function_Info is
     Dsp_SQL92_String_Function.Info;


   --  ----------------------------------------------------------------------


   type SQL92_VALUE_EXPRESSION is (SQL_SVE_CASE,
                                   SQL_SVE_CAST,
                                   SQL_SVE_COALESCE,
                                   SQL_SVE_NULLIF);

   type SQL92_VALUE_EXPRESSION_BITMAP is
      array (SQL92_VALUE_EXPRESSION) of Boolean;

   package Dsp_SQL92_Value_Expression is new
     Dispatch.A_Bitmap (SQL_SQL92_VALUE_EXPRESSIONS,
                        SQL92_VALUE_EXPRESSION,
                        SQL92_VALUE_EXPRESSION_BITMAP);
   subtype SQL92_Value_Expression_Info is Dsp_SQL92_Value_Expression.Info;


   --  ----------------------------------------------------------------------
   type ODBC_SAG_CLI_CONFORMANCE is (SQL_OSCC_NOT_COMPLIANT,
                                     SQL_OSCC_COMPLIANT);
   for ODBC_SAG_CLI_CONFORMANCE'Size use SQLSMALLINT'Size;

   package Dsp_ODBC_SAG_CLI_Conformance is new
     Dispatch.A_Enumerated (SQL_ODBC_SAG_CLI_CONFORMANCE,
                            ODBC_SAG_CLI_CONFORMANCE,
                            SQLSMALLINT,
                            "SQL_ODBC_SAG_CLI_CONFORMANCE");
   subtype ODBC_SAG_CLI_Conformance_Info is
     Dsp_ODBC_SAG_CLI_Conformance.Info;

   --  ----------------------------------------------------------------------

   type ODBC_API_CONFORMANCE is (SQL_OAC_NONE,
                                 SQL_OAC_LEVEL1,
                                 SQL_OAC_LEVEL2);
   for ODBC_API_CONFORMANCE'Size use SQLSMALLINT'Size;

   package Dsp_ODBC_API_Conformance is new
     Dispatch.A_Enumerated (SQL_ODBC_API_CONFORMANCE,
                            ODBC_API_CONFORMANCE,
                            SQLSMALLINT,
                            "SQL_ODBC_API_CONFORMANCE");
   subtype ODBC_API_Conformance_Info is
     Dsp_ODBC_API_Conformance.Info;

   --  ----------------------------------------------------------------------

   type ODBC_SQL_CONFORMANCE is (SQL_OSC_MINIMUM,
                                 SQL_OSC_CORE,
                                 SQL_OSC_EXTENDED);
   for ODBC_SQL_CONFORMANCE'Size use SQLSMALLINT'Size;

   package Dsp_ODBC_SQL_Conformance is new
     Dispatch.A_Enumerated (SQL_ODBC_SQL_CONFORMANCE,
                            ODBC_SQL_CONFORMANCE,
                            SQLSMALLINT,
                            "SQL_ODBC_SQL_CONFORMANCE");
   subtype ODBC_SQL_Conformance_Info is
     Dsp_ODBC_SQL_Conformance.Info;

   --  ----------------------------------------------------------------------


   type SQL_CONFORMANCE is (SQL_SC_SQL92_ENTRY,
                            SQL_SC_FIPS127_2_TRANSITIONAL,
                            SQL_SC_SQL92_INTERMEDIATE,
                            SQL_SC_SQL92_FULL);

   type SQL_CONFORMANCE_BITMAP is
     array (SQL_CONFORMANCE) of Boolean;

   package Dsp_SQL_Conformance is new
     Dispatch.A_Bitmap (SQL_SQL_CONFORMANCE,
                        SQL_CONFORMANCE,
                        SQL_CONFORMANCE_BITMAP);
   subtype SQL_Conformance_Info is Dsp_SQL_Conformance.Info;


   --  ----------------------------------------------------------------------


   type SQL_INTERFACE_CONFORMANCE is (SQL_OIC_CORE,
                                      SQL_OIC_LEVEL1,
                                      SQL_OIC_LEVEL2);
   for SQL_INTERFACE_CONFORMANCE use (SQL_OIC_CORE     => 1,
                                      SQL_OIC_LEVEL1   => 2,
                                      SQL_OIC_LEVEL2   => 3);
   for SQL_INTERFACE_CONFORMANCE'Size use SQLINTEGER'Size;

   package Dsp_SQL_Interface_Conformance is new
     Dispatch.A_Enumerated (SQL_ODBC_INTERFACE_CONFORMANCE,
                            SQL_INTERFACE_CONFORMANCE,
                            SQLINTEGER,
                            "SQL_INTERFACE_CONFORMANCE");
   subtype SQL_Interface_Conformance_Info is
     Dsp_SQL_Interface_Conformance.Info;


   --  ----------------------------------------------------------------------


   type STANDARD_CLI_CONFORMANCE is (SQL_SCC_XOPEN_CLI_VERSION1,
                                     SQL_SCC_ISO92_CLI);

   type STANDARD_CLI_CONFORMANCE_BITMAP is
     array (STANDARD_CLI_CONFORMANCE) of Boolean;

   package Dsp_Standard_CLI_Conformace is new
     Dispatch.A_Bitmap (SQL_STANDARD_CLI_CONFORMANCE,
                        STANDARD_CLI_CONFORMANCE,
                        STANDARD_CLI_CONFORMANCE_BITMAP);
   subtype Standard_CLI_Conformace_Info is Dsp_Standard_CLI_Conformace.Info;


   --  ----------------------------------------------------------------------


   type STRING_FUNCTION is (SQL_FN_STR_CONCAT,
                            SQL_FN_STR_INSERT,
                            SQL_FN_STR_LEFT,
                            SQL_FN_STR_LTRIM,
                            SQL_FN_STR_LENGTH,
                            SQL_FN_STR_LOCATE,
                            SQL_FN_STR_LCASE,
                            SQL_FN_STR_REPEAT,
                            SQL_FN_STR_REPLACE,
                            SQL_FN_STR_RIGHT,
                            SQL_FN_STR_RTRIM,
                            SQL_FN_STR_SUBSTRING,
                            SQL_FN_STR_UCASE,
                            SQL_FN_STR_ASCII,
                            SQL_FN_STR_CHAR,
                            SQL_FN_STR_DIFFERENCE,
                            SQL_FN_STR_LOCATE_2,
                            SQL_FN_STR_SOUNDEX,
                            SQL_FN_STR_SPACE,
                            SQL_FN_STR_BIT_LENGTH,
                            SQL_FN_STR_CHAR_LENGTH,
                            SQL_FN_STR_CHARACTER_LENGTH,
                            SQL_FN_STR_OCTET_LENGTH,
                            SQL_FN_STR_POSITION);

   type STRING_FUNCTION_BITMAP is
     array (STRING_FUNCTION) of Boolean;

   package Dsp_String_Function is new
     Dispatch.A_Bitmap (SQL_STRING_FUNCTIONS,
                        STRING_FUNCTION,
                        STRING_FUNCTION_BITMAP);
   subtype String_Function_Info is Dsp_String_Function.Info;


   --  ----------------------------------------------------------------------


   type SUBQUERY is (SQL_SQ_COMPARISON,
                     SQL_SQ_EXISTS,
                     SQL_SQ_IN,
                     SQL_SQ_QUANTIFIED,
                     SQL_SQ_CORRELATED_SUBQUERIES);

   type SUBQUERY_BITMAP is
     array (SUBQUERY) of Boolean;

   package Dsp_SubQuery is new Dispatch.A_Bitmap (SQL_SUBQUERIES,
                                                  SUBQUERY,
                                                  SUBQUERY_BITMAP);
   subtype SubQuery_Info is Dsp_SubQuery.Info;


   --  ----------------------------------------------------------------------


   type SYSTEM_FUNCTION is (SQL_FN_SYS_USERNAME,
                            SQL_FN_SYS_DBNAME,
                            SQL_FN_SYS_IFNULL);

   type SYSTEM_FUNCTION_BITMAP is
     array (SYSTEM_FUNCTION) of Boolean;

   package Dsp_System_Function is new
     Dispatch.A_Bitmap (SQL_SYSTEM_FUNCTIONS,
                        SYSTEM_FUNCTION,
                        SYSTEM_FUNCTION_BITMAP);
   subtype System_Function_Info is Dsp_System_Function.Info;


   --  ----------------------------------------------------------------------


   type TIME_INTERVAL_FUNCTION is (SQL_FN_TSI_FRAC_SECOND,
                                   SQL_FN_TSI_SECOND,
                                   SQL_FN_TSI_MINUTE,
                                   SQL_FN_TSI_HOUR,
                                   SQL_FN_TSI_DAY,
                                   SQL_FN_TSI_WEEK,
                                   SQL_FN_TSI_MONTH,
                                   SQL_FN_TSI_QUARTER,
                                   SQL_FN_TSI_YEAR);

   type TIME_INTERVAL_FUNCTION_BITMAP is
     array (TIME_INTERVAL_FUNCTION) of Boolean;

   package Dsp_Time_Interval_Function is new
     Dispatch.A_Bitmap (SQL_TIMEDATE_ADD_INTERVALS,
                        TIME_INTERVAL_FUNCTION,
                        TIME_INTERVAL_FUNCTION_BITMAP);
   subtype Time_Interval_Function_Info is Dsp_Time_Interval_Function.Info;




   --  ----------------------------------------------------------------------


   type TIMEDATE_FUNCTION is (SQL_FN_TD_NOW,
                              SQL_FN_TD_CURDATE,
                              SQL_FN_TD_DAYOFMONTH,
                              SQL_FN_TD_DAYOFWEEK,
                              SQL_FN_TD_DAYOFYEAR,
                              SQL_FN_TD_MONTH,
                              SQL_FN_TD_QUARTER,
                              SQL_FN_TD_WEEK,
                              SQL_FN_TD_YEAR,
                              SQL_FN_TD_CURTIME,
                              SQL_FN_TD_HOUR,
                              SQL_FN_TD_MINUTE,
                              SQL_FN_TD_SECOND,
                              SQL_FN_TD_TIMESTAMPADD,
                              SQL_FN_TD_TIMESTAMPDIFF,
                              SQL_FN_TD_DAYNAME,
                              SQL_FN_TD_MONTHNAME,
                              SQL_FN_TD_CURRENT_DATE,
                              SQL_FN_TD_CURRENT_TIME,
                              SQL_FN_TD_CURRENT_TIMESTAMP,
                              SQL_FN_TD_EXTRACT);

   type TIMEDATE_FUNCTION_BITMAP is
     array (TIMEDATE_FUNCTION) of Boolean;

   package Dsp_TimeDate_Function is new
     Dispatch.A_Bitmap (SQL_TIMEDATE_FUNCTIONS,
                        TIMEDATE_FUNCTION,
                        TIMEDATE_FUNCTION_BITMAP);
   subtype TimeDate_Function_Info is Dsp_TimeDate_Function.Info;


   --  ----------------------------------------------------------------------


   type TXN_CAPABLE is (SQL_TC_NONE,
                        SQL_TC_DML,
                        SQL_TC_ALL,
                        SQL_TC_DDL_COMMIT,
                        SQL_TC_DDL_IGNORE);
   for TXN_CAPABLE'Size use SQLSMALLINT'Size;

   package Dsp_TXN_Capable is new
     Dispatch.A_Enumerated (SQL_TRANSACTION_CAPABLE,
                            TXN_CAPABLE,
                            SQLSMALLINT,
                            "TXN_CAPABLE");
   subtype TXN_Capable_Info is Dsp_TXN_Capable.Info;


   --  ----------------------------------------------------------------------


   type TXN_ISOLATION_OPTION_BITMAP is array
     (GNU.DB.SQLCLI.Connection_Attribute.TXN_ISOLATION_OPTION)
     of Boolean;

   package Dsp_TXN_Isolation is new Dispatch.A_Bitmap
     (SQL_TRANSACTION_ISOLATION_OPTION,
      GNU.DB.SQLCLI.Connection_Attribute.TXN_ISOLATION_OPTION,
      TXN_ISOLATION_OPTION_BITMAP);
   subtype TXN_Isolation_Info is Dsp_TXN_Isolation.Info;

   package Dsp_TXN_Isolation_Default is new
     Dispatch.A_Enumerated
     (SQL_DEFAULT_TRANSACTION_ISOLATION,
      GNU.DB.SQLCLI.Connection_Attribute.TXN_ISOLATION_OPTION,
      SQLINTEGER,
      "TXN_ISOLATION_OPTION");
   subtype TXN_Default_Isolation_Info is Dsp_TXN_Isolation_Default.Info;




   --  ----------------------------------------------------------------------


   type UNION is (SQL_U_UNION,
                  SQL_U_UNION_ALL);

   type UNION_BITMAP is
     array (UNION) of Boolean;

   package Dsp_Union is new
     Dispatch.A_Bitmap (SQL_UNION_STATEMENT,
                        UNION,
                        UNION_BITMAP);
   subtype Union_Info is Dsp_Union.Info;

   --  ---------------------------------------------------------------------
   --  SQLGetFunctions() values to identify ODBC APIs
   type SQL_API_FUNCTION is (SQL_API_SQLALLOCCONNECT,
                             SQL_API_SQLALLOCENV,
                             SQL_API_SQLALLOCSTMT,
                             SQL_API_SQLBINDCOL,
                             SQL_API_SQLCANCEL,
                             SQL_API_SQLCOLATTRIBUTE,
                             SQL_API_SQLCONNECT,
                             SQL_API_SQLDESCRIBECOL,
                             SQL_API_SQLDISCONNECT,
                             SQL_API_SQLERROR,
                             SQL_API_SQLEXECDIRECT,
                             SQL_API_SQLEXECUTE,
                             SQL_API_SQLFETCH,
                             SQL_API_SQLFREECONNECT,
                             SQL_API_SQLFREEENV,
                             SQL_API_SQLFREESTMT,
                             SQL_API_SQLGETCURSORNAME,
                             SQL_API_SQLNUMRESULTCOLS,
                             SQL_API_SQLPREPARE,
                             SQL_API_SQLROWCOUNT,
                             SQL_API_SQLSETCURSORNAME,
                             SQL_API_SQLSETPARAM,
                             SQL_API_SQLTRANSACT,
                             SQL_API_SQLBULKOPERATIONS,
                             SQL_API_SQLCOLUMNS,
                             SQL_API_SQLDRIVERCONNECT,
                             SQL_API_SQLGETCONNECTOPTION,
                             SQL_API_SQLGETDATA,
                             SQL_API_SQLGETFUNCTIONS,
                             SQL_API_SQLGETINFO,
                             SQL_API_SQLGETSTMTOPTION,
                             SQL_API_SQLGETTYPEINFO,
                             SQL_API_SQLPARAMDATA,
                             SQL_API_SQLPUTDATA,
                             SQL_API_SQLSETCONNECTOPTION,
                             SQL_API_SQLSETSTMTOPTION,
                             SQL_API_SQLSPECIALCOLUMNS,
                             SQL_API_SQLSTATISTICS,
                             SQL_API_SQLTABLES,
                             SQL_API_SQLBROWSECONNECT,
                             SQL_API_SQLCOLUMNPRIVILEGES,
                             SQL_API_SQLDATASOURCES,
                             SQL_API_SQLDESCRIBEPARAM,
                             SQL_API_SQLEXTENDEDFETCH,
                             SQL_API_SQLFOREIGNKEYS,
                             SQL_API_SQLMORERESULTS,
                             SQL_API_SQLNATIVESQL,
                             SQL_API_SQLNUMPARAMS,
                             SQL_API_SQLPARAMOPTIONS,
                             SQL_API_SQLPRIMARYKEYS,
                             SQL_API_SQLPROCEDURECOLUMNS,
                             SQL_API_SQLPROCEDURES,
                             SQL_API_SQLSETPOS,
                             SQL_API_SQLSETSCROLLOPTIONS,
                             SQL_API_SQLTABLEPRIVILEGES,
                             SQL_API_SQLDRIVERS,
                             SQL_API_SQLBINDPARAMETER,
                             SQL_API_SQLALLOCHANDLESTD,
                             SQL_API_SQLALLOCHANDLE,
                             SQL_API_SQLBINDPARAM,
                             SQL_API_SQLCLOSECURSOR,
                             SQL_API_SQLCOPYDESC,
                             SQL_API_SQLENDTRAN,
                             SQL_API_SQLFREEHANDLE,
                             SQL_API_SQLGETCONNECTATTR,
                             SQL_API_SQLGETDESCFIELD,
                             SQL_API_SQLGETDESCREC,
                             SQL_API_SQLGETDIAGFIELD,
                             SQL_API_SQLGETDIAGREC,
                             SQL_API_SQLGETENVATTR,
                             SQL_API_SQLGETSTMTATTR,
                             SQL_API_SQLSETCONNECTATTR,
                             SQL_API_SQLSETDESCFIELD,
                             SQL_API_SQLSETDESCREC,
                             SQL_API_SQLSETENVATTR,
                             SQL_API_SQLSETSTMTATTR,
                             SQL_API_SQLFETCHSCROLL);

   for SQL_API_FUNCTION use (SQL_API_SQLALLOCCONNECT        => 1,
                             SQL_API_SQLALLOCENV            => 2,
                             SQL_API_SQLALLOCSTMT           => 3,
                             SQL_API_SQLBINDCOL             => 4,
                             SQL_API_SQLCANCEL              => 5,
                             SQL_API_SQLCOLATTRIBUTE        => 6,
                             SQL_API_SQLCONNECT             => 7,
                             SQL_API_SQLDESCRIBECOL         => 8,
                             SQL_API_SQLDISCONNECT          => 9,
                             SQL_API_SQLERROR               => 10,
                             SQL_API_SQLEXECDIRECT          => 11,
                             SQL_API_SQLEXECUTE             => 12,
                             SQL_API_SQLFETCH               => 13,
                             SQL_API_SQLFREECONNECT         => 14,
                             SQL_API_SQLFREEENV             => 15,
                             SQL_API_SQLFREESTMT            => 16,
                             SQL_API_SQLGETCURSORNAME       => 17,
                             SQL_API_SQLNUMRESULTCOLS       => 18,
                             SQL_API_SQLPREPARE             => 19,
                             SQL_API_SQLROWCOUNT            => 20,
                             SQL_API_SQLSETCURSORNAME       => 21,
                             SQL_API_SQLSETPARAM            => 22,
                             SQL_API_SQLTRANSACT            => 23,
                             SQL_API_SQLBULKOPERATIONS      => 24,
                             SQL_API_SQLCOLUMNS             => 40,
                             SQL_API_SQLDRIVERCONNECT       => 41,
                             SQL_API_SQLGETCONNECTOPTION    => 42,
                             SQL_API_SQLGETDATA             => 43,
                             SQL_API_SQLGETFUNCTIONS        => 44,
                             SQL_API_SQLGETINFO             => 45,
                             SQL_API_SQLGETSTMTOPTION       => 46,
                             SQL_API_SQLGETTYPEINFO         => 47,
                             SQL_API_SQLPARAMDATA           => 48,
                             SQL_API_SQLPUTDATA             => 49,
                             SQL_API_SQLSETCONNECTOPTION    => 50,
                             SQL_API_SQLSETSTMTOPTION       => 51,
                             SQL_API_SQLSPECIALCOLUMNS      => 52,
                             SQL_API_SQLSTATISTICS          => 53,
                             SQL_API_SQLTABLES              => 54,
                             SQL_API_SQLBROWSECONNECT       => 55,
                             SQL_API_SQLCOLUMNPRIVILEGES    => 56,
                             SQL_API_SQLDATASOURCES         => 57,
                             SQL_API_SQLDESCRIBEPARAM       => 58,
                             SQL_API_SQLEXTENDEDFETCH       => 59,
                             SQL_API_SQLFOREIGNKEYS         => 60,
                             SQL_API_SQLMORERESULTS         => 61,
                             SQL_API_SQLNATIVESQL           => 62,
                             SQL_API_SQLNUMPARAMS           => 63,
                             SQL_API_SQLPARAMOPTIONS        => 64,
                             SQL_API_SQLPRIMARYKEYS         => 65,
                             SQL_API_SQLPROCEDURECOLUMNS    => 66,
                             SQL_API_SQLPROCEDURES          => 67,
                             SQL_API_SQLSETPOS              => 68,
                             SQL_API_SQLSETSCROLLOPTIONS    => 69,
                             SQL_API_SQLTABLEPRIVILEGES     => 70,
                             SQL_API_SQLDRIVERS             => 71,
                             SQL_API_SQLBINDPARAMETER       => 72,
                             SQL_API_SQLALLOCHANDLESTD      => 73,
                             SQL_API_SQLALLOCHANDLE         => 1001,
                             SQL_API_SQLBINDPARAM           => 1002,
                             SQL_API_SQLCLOSECURSOR         => 1003,
                             SQL_API_SQLCOPYDESC            => 1004,
                             SQL_API_SQLENDTRAN             => 1005,
                             SQL_API_SQLFREEHANDLE          => 1006,
                             SQL_API_SQLGETCONNECTATTR      => 1007,
                             SQL_API_SQLGETDESCFIELD        => 1008,
                             SQL_API_SQLGETDESCREC          => 1009,
                             SQL_API_SQLGETDIAGFIELD        => 1010,
                             SQL_API_SQLGETDIAGREC          => 1011,
                             SQL_API_SQLGETENVATTR          => 1012,
                             SQL_API_SQLGETSTMTATTR         => 1014,
                             SQL_API_SQLSETCONNECTATTR      => 1016,
                             SQL_API_SQLSETDESCFIELD        => 1017,
                             SQL_API_SQLSETDESCREC          => 1018,
                             SQL_API_SQLSETENVATTR          => 1019,
                             SQL_API_SQLSETSTMTATTR         => 1020,
                             SQL_API_SQLFETCHSCROLL         => 1021);
   for SQL_API_FUNCTION'Size use SQLUSMALLINT'Size;

   type SQL_API_FUNCTION_BITMAP is private;

   function SQLGetFunctions (ConnectionHandle : SQLHDBC;
                             Func             : SQL_API_FUNCTION)
                             return Boolean;

   procedure SQLGetFunctions
     (ConnectionHandle : in  SQLHDBC;
      Bitmap           : out SQL_API_FUNCTION_BITMAP);

   function SQLGetFunctions (Bitmap : SQL_API_FUNCTION_BITMAP;
                             Func   : SQL_API_FUNCTION) return Boolean;
   pragma Inline (SQLGetFunctions);

private

   SQL_API_ODBC3_ALL_FUNCTIONS      : constant SQLUSMALLINT := 999;
   SQL_API_ODBC3_ALL_FUNCTIONS_SIZE : constant := 250; --  array of 250 words

   type SAFSA_Element is mod 2 ** SQLUSMALLINT'Size;

   type SQL_API_FUNCTION_BITMAP is
      array (0 .. (SQL_API_ODBC3_ALL_FUNCTIONS_SIZE - 1)) of SAFSA_Element;
   pragma Pack (SQL_API_FUNCTION_BITMAP);
   pragma Assert (SQL_API_FUNCTION_BITMAP'Size = 4000);

end GNU.DB.SQLCLI.Info;
