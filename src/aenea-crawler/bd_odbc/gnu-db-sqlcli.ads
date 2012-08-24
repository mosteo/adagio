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
with System.Storage_Elements;
with Interfaces.C;

package GNU.DB.SQLCLI is

   --  The name "SQLCLI" comes from the SQL (Standard Query Language)
   --  Call-Level Interface. Note that ODBC (Open Database
   --  Connectivity) is a similar standard.

   type SQLPOINTER is private;

   type SQLINTEGER   is range -(2 ** 31) .. +(2 ** 31) - 1;
   for SQLINTEGER'Size use 32;

   type SQLUINTEGER  is mod 2 ** SQLINTEGER'Size;
   for SQLUINTEGER'Size use SQLINTEGER'Size;

   type SQLSMALLINT  is range -(2 ** 15) .. +(2 ** 15) - 1;
   for SQLSMALLINT'Size use 16;

   type SQLUSMALLINT is mod 2 ** SQLSMALLINT'Size;
   for SQLUSMALLINT'Size use SQLSMALLINT'Size;

   type SQLBIGINT    is range -(2 ** 63) .. +(2 ** 63) - 1;
   for SQLBIGINT'Size use 64;

   type SQLUBIGINT   is mod 2 ** SQLBIGINT'Size;
   for SQLUBIGINT'Size use SQLBIGINT'Size;

   type SQLTINYINT is range -(2 ** 7) .. +(2 ** 7) - 1;
   for SQLTINYINT'Size use 8;

   type SQLUTINYINT is mod 2 ** SQLTINYINT'Size;
   for SQLUTINYINT'Size use SQLTINYINT'Size;

   type SQLDOUBLE    is new Interfaces.C.double;
   type SQLREAL      is new Interfaces.C.C_float;

   type SQLCHAR      is new Interfaces.C.unsigned_char;
   type SQLSCHAR     is new Interfaces.C.signed_char;
   type SQLTCHAR     is new Wide_Character;

   --  Component Types
   type SQLDATE      is new SQLUTINYINT;
   type SQLDECIMAL   is new SQLUTINYINT;
   type SQLNUMERIC   is new SQLUTINYINT;
   type SQLTIME      is new SQLUTINYINT;
   type SQLTIMESTAMP is new SQLUTINYINT;
   type SQLVARCHAR   is new Interfaces.C.unsigned_char;

   type SQL_OPTION_BITMASK is mod 2 ** SQLUINTEGER'Size;

   type SQL_BOOLEAN is (SQL_FALSE, SQL_TRUE);
   for SQL_BOOLEAN'Size use SQLSMALLINT'Size;

   type PTR_SQLUINTEGER    is access all SQLUINTEGER;
   type PTR_SQLUSMALLINT   is access all SQLUSMALLINT;
   type PTR_SQLINTEGER     is access all SQLINTEGER;
   type PTR_SQLSMALLINT    is access all SQLSMALLINT;
   type PTR_SQLCHAR        is access all SQLCHAR;
   type PTR_SQLTCHAR       is access all SQLTCHAR;
   type PTR_SQLDOUBLE      is access all SQLDOUBLE;
   type PTR_SQLREAL        is access all SQLREAL;

   function To_SQLPOINTER (S : PTR_SQLTCHAR)     return SQLPOINTER;
   function To_SQLPOINTER (S : PTR_SQLCHAR)      return SQLPOINTER;
   function To_SQLPOINTER (S : PTR_SQLUINTEGER)  return SQLPOINTER;
   function To_SQLPOINTER (S : PTR_SQLINTEGER)   return SQLPOINTER;
   function To_SQLPOINTER (S : PTR_SQLUSMALLINT) return SQLPOINTER;
   function To_SQLPOINTER (S : PTR_SQLSMALLINT)  return SQLPOINTER;
   function To_SQLPOINTER (S : access String)           return SQLPOINTER;
   function To_SQLPOINTER (S : access Wide_String)      return SQLPOINTER;
   function To_SQLPOINTER (S : System.Address)   return SQLPOINTER;
   pragma Inline_Always (To_SQLPOINTER);

   function To_Address (P : SQLPOINTER) return System.Address;
   pragma Inline_Always (To_Address);

   function To_PTR_SQLCHAR  (S : access String) return PTR_SQLCHAR;
   function To_PTR_SQLCHAR  (S : System.Address) return PTR_SQLCHAR;
   pragma Inline_Always (To_PTR_SQLCHAR);

   function To_PTR_SQLTCHAR (S : access Wide_String) return PTR_SQLTCHAR;
   function To_PTR_SQLTCHAR (S : System.Address) return PTR_SQLTCHAR;
   pragma Inline_Always (To_PTR_SQLTCHAR);

   type SQL_DATE_STRUCT is record
      year   : SQLSMALLINT;
      month  : SQLUSMALLINT;
      day    : SQLUSMALLINT;
   end record;
   pragma Convention (C, SQL_DATE_STRUCT);

   type SQL_TIME_STRUCT is record
      hour   : SQLUSMALLINT;
      minute : SQLUSMALLINT;
      second : SQLUSMALLINT;
   end record;
   pragma Convention (C, SQL_TIME_STRUCT);

   type SQL_TIME_FRACTION is range 0 .. 999_999_999;
   for SQL_TIME_FRACTION'Size use SQLUINTEGER'Size;

   type SQL_TIMESTAMP_STRUCT is record
      year     : SQLSMALLINT;
      month    : SQLUSMALLINT;
      day      : SQLUSMALLINT;
      hour     : SQLUSMALLINT;
      minute   : SQLUSMALLINT;
      second   : SQLUSMALLINT;
      fraction : SQL_TIME_FRACTION;
   end record;
   pragma Convention (C, SQL_TIMESTAMP_STRUCT);

   type SQLINTERVAL is (SQL_IS_YEAR,
                        SQL_IS_MONTH,
                        SQL_IS_DAY,
                        SQL_IS_HOUR,
                        SQL_IS_MINUTE,
                        SQL_IS_SECOND,
                        SQL_IS_YEAR_TO_MONTH,
                        SQL_IS_DAY_TO_HOUR,
                        SQL_IS_DAY_TO_MINUTE,
                        SQL_IS_DAY_TO_SECOND,
                        SQL_IS_HOUR_TO_MINUTE,
                        SQL_IS_HOUR_TO_SECOND,
                        SQL_IS_MINUTE_TO_SECOND);
   for SQLINTERVAL use (SQL_IS_YEAR             => 1,
                        SQL_IS_MONTH            => 2,
                        SQL_IS_DAY              => 3,
                        SQL_IS_HOUR             => 4,
                        SQL_IS_MINUTE           => 5,
                        SQL_IS_SECOND           => 6,
                        SQL_IS_YEAR_TO_MONTH    => 7,
                        SQL_IS_DAY_TO_HOUR      => 8,
                        SQL_IS_DAY_TO_MINUTE    => 9,
                        SQL_IS_DAY_TO_SECOND    => 10,
                        SQL_IS_HOUR_TO_MINUTE   => 11,
                        SQL_IS_HOUR_TO_SECOND   => 12,
                        SQL_IS_MINUTE_TO_SECOND => 13);
   for SQLINTERVAL'Size use Interfaces.C.int'Size;

   type SQL_YEAR_MONTH_STRUCT is record
      year  : SQLUINTEGER;
      month : SQLUINTEGER;
   end record;
   pragma Convention (C, SQL_YEAR_MONTH_STRUCT);

   type SQL_DAY_SECOND_STRUCT is record
      day      : SQLUINTEGER;
      hour     : SQLUINTEGER;
      minute   : SQLUINTEGER;
      second   : SQLUINTEGER;
      fraction : SQL_TIME_FRACTION;
   end record;
   pragma Convention (C, SQL_DAY_SECOND_STRUCT);

   type SQL_SIGNED_YEAR_MONTH_STRUCT is record
      interval_sign : SQL_BOOLEAN; --  SQL_TRUE if negative
      year          : SQLUINTEGER;
      month         : SQLUINTEGER;
   end record;
   pragma Convention (C, SQL_SIGNED_YEAR_MONTH_STRUCT);

   type SQL_SIGNED_DAY_SECOND_STRUCT is record
      interval_sign : SQL_BOOLEAN; --  SQL_TRUE if negative
      day           : SQLUINTEGER;
      hour          : SQLUINTEGER;
      minute        : SQLUINTEGER;
      second        : SQLUINTEGER;
      fraction      : SQL_TIME_FRACTION;
   end record;
   pragma Convention (C, SQL_SIGNED_DAY_SECOND_STRUCT);

   type SQL_INTERVAL_STRUCT (Which : SQLINTERVAL := SQL_IS_YEAR) is record
      case Which is
         when
           SQL_IS_YEAR  |
           SQL_IS_MONTH |
           SQL_IS_YEAR_TO_MONTH =>
            year_month    : SQL_SIGNED_YEAR_MONTH_STRUCT;
            when
              SQL_IS_DAY            |
              SQL_IS_HOUR           |
              SQL_IS_MINUTE         |
              SQL_IS_SECOND         |
              SQL_IS_DAY_TO_HOUR    |
              SQL_IS_DAY_TO_MINUTE  |
              SQL_IS_DAY_TO_SECOND  |
              SQL_IS_HOUR_TO_MINUTE |
              SQL_IS_HOUR_TO_SECOND |
              SQL_IS_MINUTE_TO_SECOND =>
            day_second    : SQL_SIGNED_DAY_SECOND_STRUCT;
      end case;
   end record;
   pragma Convention (C, SQL_INTERVAL_STRUCT);
   pragma Unchecked_Union (SQL_INTERVAL_STRUCT);

   SQL_MAX_NUMERIC_LEN : constant := 16;
   type SQL_NUMERIC_ARRAY is
      array (0 .. (SQL_MAX_NUMERIC_LEN - 1)) of aliased SQLNUMERIC;
   pragma Convention (C, SQL_NUMERIC_ARRAY);

   type SQL_NUMERIC_STRUCT is record
      precision   : SQLUTINYINT;
      scale       : SQLTINYINT;
      sign        : SQLUTINYINT; --  1 if positive, 0 if negative
      val         : SQL_NUMERIC_ARRAY;
   end record;
   pragma Convention (C, SQL_NUMERIC_STRUCT);

   Data_Error                : exception;
   No_Data                   : exception;
   Need_Data                 : exception;
   Still_Executing           : exception;
   Invalid_Handle            : exception;
   --  These exceptions correspond to the well defined SQLRETURN codes
   --  in the ODBC headers

   Database_Error            : exception;  --  General Database error
   Unhandled_Enum            : exception;  --  ODBC returned unknown value
   Type_Error                : exception;  --  Runtime typecheck Error
   No_Unicode_Support        : exception;  --  Driver Mgr. has no Unicode
   --  These exceptions are generated by the binding

   Generate_Detailed_Exceptions : Boolean;
   --  When this is set to True (default), instead of the general
   --  Database_Error exception the SQLSTATE gets evaluated and a more
   --  detailed exception (see below) is raised.

   Wrong_Parameter_Count     : exception;
   Invalid_Count_Field       : exception;
   Restricted_Type_Violation : exception;
   Invalid_Descriptor_Index  : exception;
   Invalid_Default_Parameter : exception;
   Prepared_Stmt_Not_Cursor  : exception;
   Column_List_Mismatch      : exception;
   Format_Error              : exception;
   Syntax_Error              : exception;
   Table_Already_Exists      : exception;
   Table_Not_Found           : exception;
   Index_Already_Exists      : exception;
   Index_Not_Found           : exception;
   Column_Already_Exists     : exception;
   Column_Not_Found          : exception;
   Connection_Error          : exception;
   Invalid_Cursor_State      : exception;
   Invalid_Cursor_Name       : exception;
   Duplicate_Cursor_Name     : exception;
   Invalid_Transaction_State : exception;
   Invalid_Auth_Spec         : exception;
   Invalid_Catalog_Name      : exception;
   Invalid_Schema_Name       : exception;
   Serialization_Failure     : exception;
   Integrity_Violation       : exception;
   Unknown_Stmt_Completion   : exception;
   With_Check_Violation      : exception;
   Operation_Canceled        : exception;
   Invalid_SQL_Datatype      : exception;
   Memory_Allocation_Error   : exception;
   Invalid_App_Buffer        : exception;
   Statement_Not_Prepared    : exception;
   Invalid_Null_Pointer      : exception;
   Invalid_Function_Sequence : exception;
   Timeout_Expired           : exception;
   Driver_Error              : exception;
   Invalid_Buffer_Length     : exception;
   Not_Implemented           : exception;
   Invalid_Information_Type  : exception;
   Functiontype_Out_Of_Range : exception;
   --  These exceptions are generated based on the value in the SQLSTATE
   --  diagnostic field.


   type SQLHANDLE  is private;
   SQL_NULL_HANDLE : constant SQLHANDLE;

   subtype SQLHENV  is SQLHANDLE;
   subtype SQLHDBC  is SQLHANDLE;
   subtype SQLHSTMT is SQLHANDLE;
   subtype SQLHDESC is SQLHANDLE;
   subtype SQLHWND  is SQLHANDLE;

   SQL_NULL_HENV   : constant SQLHENV;
   SQL_NULL_HDBC   : constant SQLHDBC;
   SQL_NULL_HSTMT  : constant SQLHSTMT;
   SQL_NULL_HDESC  : constant SQLHDESC;
   SQL_NULL_HWND   : constant SQLHWND;

   SQL_SQLSTATE_SIZE : constant := 5;
   subtype SQLSTATE is String (1 .. SQL_SQLSTATE_SIZE);
   EMPTY_SQLSTATE : constant SQLSTATE := "00000";
   subtype WIDE_SQLSTATE is Wide_String (1 .. SQL_SQLSTATE_SIZE);

   type SQLRETURN is new SQLSMALLINT;

   SQL_SUCCESS            : constant SQLRETURN := 0;
   SQL_SUCCESS_WITH_INFO  : constant SQLRETURN := 1;
   SQL_STILL_EXECUTING    : constant SQLRETURN := 2;
   SQL_NEED_DATA          : constant SQLRETURN := 99;
   SQL_NO_DATA            : constant SQLRETURN := 100;
   SQL_ERROR              : constant SQLRETURN := -1;
   SQL_INVALID_HANDLE     : constant SQLRETURN := -2;
   SQL_NO_DATA_FOUND      : constant SQLRETURN := SQL_NO_DATA;  --  Old name

   --  Special length values
   SQL_NULL_DATA        : constant := -1;
   SQL_DATA_AT_EXEC     : constant := -2;
   SQL_NTS              : constant := -3;
   SQL_NTSL             : constant := -3;

   type SQLINTEGER_ARRAY   is array (Natural range <>) of aliased SQLINTEGER;
   type SQLUINTEGER_ARRAY  is array (Natural range <>) of aliased SQLUINTEGER;
   type SQLSMALLINT_ARRAY  is array (Natural range <>) of aliased SQLSMALLINT;
   type SQLUSMALLINT_ARRAY is array (Natural range <>) of aliased SQLUSMALLINT;


   type SQL_HANDLE_TYPE is (SQL_HANDLE_ENV,
                            SQL_HANDLE_DBC,
                            SQL_HANDLE_STMT,
                            SQL_HANDLE_DESC);
   for SQL_HANDLE_TYPE use (SQL_HANDLE_ENV   => 1,
                            SQL_HANDLE_DBC   => 2,
                            SQL_HANDLE_STMT  => 3,
                            SQL_HANDLE_DESC  => 4);
   for SQL_HANDLE_TYPE'Size use SQLSMALLINT'Size;

   type SQL_ENDTRAN_HANDLE_TYPE is new SQL_HANDLE_TYPE
     range SQL_HANDLE_ENV .. SQL_HANDLE_DBC;

   function SQLAllocHandle (HandleType   : in  SQL_HANDLE_TYPE;
                            InputHandle  : in  SQLHANDLE;
                            OutputHandle : access SQLHANDLE) return SQLRETURN;

   procedure SQLAllocHandle (HandleType   : in  SQL_HANDLE_TYPE;
                             InputHandle  : in  SQLHANDLE;
                             OutputHandle : out SQLHANDLE);
   --  Obtains an environment, connection, statement, or descriptor handle.
   pragma Inline (SQLAllocHandle);

   function SQLFreeHandle (HandleType : SQL_HANDLE_TYPE;
                           Handle     : SQLHANDLE) return SQLRETURN;

   procedure SQLFreeHandle (HandleType : in SQL_HANDLE_TYPE;
                            Handle     : in out SQLHANDLE);
   --  Releases an environment, connection, statement, or descriptor handle.
   pragma Inline (SQLFreeHandle);

   type FreeStmt_Option is (SQL_CLOSE,
                            SQL_DROP,
                            SQL_UNBIND,
                            SQL_RESET_PARAMS);
   for FreeStmt_Option'Size use SQLSMALLINT'Size;

   function SQLFreeStmt (StatementHandle : SQLHSTMT;
                         Option          : FreeStmt_Option) return SQLRETURN;

   procedure SQLFreeStmt (StatementHandle   : in out SQLHSTMT;
                          Option            : in FreeStmt_Option := SQL_CLOSE);

   function SQLConnect (ConnectionHandle  : in SQLHDBC;
                        ServerName        : in String;
                        UserName          : in String;
                        Authentication    : in String) return SQLRETURN;

   procedure SQLConnect (ConnectionHandle  : SQLHDBC;
                         ServerName        : String;
                         UserName          : String;
                         Authentication    : String);

   function SQLConnect (ConnectionHandle  : in SQLHDBC;
                        ServerName        : in Wide_String;
                        UserName          : in Wide_String;
                        Authentication    : in Wide_String) return SQLRETURN;

   procedure SQLConnect (ConnectionHandle  : SQLHDBC;
                         ServerName        : Wide_String;
                         UserName          : Wide_String;
                         Authentication    : Wide_String);
   --  Connects to a specific driver by data source name, user ID
   --  and password.
   pragma Inline (SQLConnect);

   function SQLDisconnect (ConnectionHandle : SQLHDBC) return SQLRETURN;

   procedure SQLDisconnect (ConnectionHandle : in SQLHDBC);
   --  Closes the connection.
   pragma Inline (SQLDisconnect);

   type SQL_EXTENDED_FETCH_DIRECTION is (SQL_FETCH_NEXT,
                                         SQL_FETCH_FIRST,
                                         SQL_FETCH_LAST,
                                         SQL_FETCH_PRIOR,
                                         SQL_FETCH_ABSOLUTE,
                                         SQL_FETCH_RELATIVE,
                                         SQL_FETCH_RESUME,
                                         SQL_FETCH_BOOKMARK,
                                         SQL_FETCH_FIRST_USER,
                                         SQL_FETCH_FIRST_SYSTEM);
   for SQL_EXTENDED_FETCH_DIRECTION use (SQL_FETCH_NEXT         => 1,
                                         SQL_FETCH_FIRST        => 2,
                                         SQL_FETCH_LAST         => 3,
                                         SQL_FETCH_PRIOR        => 4,
                                         SQL_FETCH_ABSOLUTE     => 5,
                                         SQL_FETCH_RELATIVE     => 6,
                                         SQL_FETCH_RESUME       => 7,
                                         SQL_FETCH_BOOKMARK     => 8,
                                         SQL_FETCH_FIRST_USER   => 31,
                                         SQL_FETCH_FIRST_SYSTEM => 32);
   for SQL_EXTENDED_FETCH_DIRECTION'Size use SQLSMALLINT'Size;

   type FETCH_DIRECTION is new SQL_EXTENDED_FETCH_DIRECTION
     range SQL_FETCH_NEXT .. SQL_FETCH_BOOKMARK;

   type SIMPLE_FETCH_DIRECTION is new FETCH_DIRECTION
     range SQL_FETCH_NEXT .. SQL_FETCH_FIRST;

   SQL_MAX_DSN_LENGTH : constant := 32; --  maximum data source name size

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out String;
      Description       : out String;
      ErrorCode         : out SQLRETURN);
   --  This version returns the error code in an out paramater

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out String;
      Description       : out String);
   --  This version raises an exception;

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out Wide_String;
      Description       : out Wide_String;
      ErrorCode         : out SQLRETURN);
   --  This version returns the error code in an out paramater

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out Wide_String;
      Description       : out Wide_String);
   --  This version raises an exception;


   type SQL_DATA_TYPE is (SQL_GUID,
                          SQL_WLONGVARCHAR,
                          SQL_WVARCHAR,
                          SQL_WCHAR,
                          SQL_BIT,
                          SQL_TINYINT,
                          SQL_BIGINT,
                          SQL_LONGVARBINARY,
                          SQL_VARBINARY,
                          SQL_BINARY,
                          SQL_LONGVARCHAR,
                          SQL_UNKNOWN_TYPE,
                          SQL_CHAR,
                          SQL_NUMERIC,
                          SQL_DECIMAL,
                          SQL_INTEGER,
                          SQL_SMALLINT,
                          SQL_FLOAT,
                          SQL_REAL,
                          SQL_DOUBLE,
                          SQL_DATETIME,
                          SQL_TIME,
                          SQL_TIMESTAMP,
                          SQL_VARCHAR,
                          SQL_TYPE_DATE,
                          SQL_TYPE_TIME,
                          SQL_TYPE_TIMESTAMP,
                          SQL_DEFAULT,
                          SQL_INTERVAL_YEAR,
                          SQL_INTERVAL_MONTH,
                          SQL_INTERVAL_DAY,
                          SQL_INTERVAL_HOUR,
                          SQL_INTERVAL_MINUTE,
                          SQL_INTERVAL_SECOND,
                          SQL_INTERVAL_YEAR_TO_MONTH,
                          SQL_INTERVAL_DAY_TO_HOUR,
                          SQL_INTERVAL_DAY_TO_MINUTE,
                          SQL_INTERVAL_DAY_TO_SECOND,
                          SQL_INTERVAL_HOUR_TO_MINUTE,
                          SQL_INTERVAL_HOUR_TO_SECOND,
                          SQL_INTERVAL_MINUTE_TO_SECOND);
   for SQL_DATA_TYPE use (SQL_GUID                      => -11,
                          SQL_WLONGVARCHAR              => -10,
                          SQL_WVARCHAR                  => -9,
                          SQL_WCHAR                     => -8,
                          SQL_BIT                       => -7,
                          SQL_TINYINT                   => -6,
                          SQL_BIGINT                    => -5,
                          SQL_LONGVARBINARY             => -4,
                          SQL_VARBINARY                 => -3,
                          SQL_BINARY                    => -2,
                          SQL_LONGVARCHAR               => -1,
                          SQL_UNKNOWN_TYPE              => 0,
                          SQL_CHAR                      => 1,
                          SQL_NUMERIC                   => 2,
                          SQL_DECIMAL                   => 3,
                          SQL_INTEGER                   => 4,
                          SQL_SMALLINT                  => 5,
                          SQL_FLOAT                     => 6,
                          SQL_REAL                      => 7,
                          SQL_DOUBLE                    => 8,
                          SQL_DATETIME                  => 9,
                          SQL_TIME                      => 10,
                          SQL_TIMESTAMP                 => 11,
                          SQL_VARCHAR                   => 12,
                          SQL_TYPE_DATE                 => 91,
                          SQL_TYPE_TIME                 => 92,
                          SQL_TYPE_TIMESTAMP            => 93,
                          SQL_DEFAULT                   => 99,
                          SQL_INTERVAL_YEAR             => 101,
                          SQL_INTERVAL_MONTH            => 102,
                          SQL_INTERVAL_DAY              => 103,
                          SQL_INTERVAL_HOUR             => 104,
                          SQL_INTERVAL_MINUTE           => 105,
                          SQL_INTERVAL_SECOND           => 106,
                          SQL_INTERVAL_YEAR_TO_MONTH    => 107,
                          SQL_INTERVAL_DAY_TO_HOUR      => 108,
                          SQL_INTERVAL_DAY_TO_MINUTE    => 109,
                          SQL_INTERVAL_DAY_TO_SECOND    => 110,
                          SQL_INTERVAL_HOUR_TO_MINUTE   => 111,
                          SQL_INTERVAL_HOUR_TO_SECOND   => 112,
                          SQL_INTERVAL_MINUTE_TO_SECOND => 113);
   for SQL_DATA_TYPE'Size use SQLSMALLINT'Size;

   SQL_ALL_TYPES : constant SQL_DATA_TYPE := SQL_UNKNOWN_TYPE;

   type SQL_C_DATA_TYPE is (SQL_C_UTINYINT,
                            SQL_C_UBIGINT,
                            SQL_C_STINYINT,
                            SQL_C_SBIGINT,
                            SQL_C_ULONG,
                            SQL_C_USHORT,
                            SQL_C_SLONG,
                            SQL_C_SSHORT,
                            SQL_C_GUID,
                            SQL_C_WCHAR,
                            SQL_C_BIT,
                            SQL_C_TINYINT,
                            SQL_C_BINARY,
                            SQL_C_CHAR,
                            SQL_C_NUMERIC,
                            SQL_C_LONG,
                            SQL_C_SHORT,
                            SQL_C_FLOAT,
                            SQL_C_DOUBLE,
                            SQL_C_DATE,
                            SQL_C_TIME,
                            SQL_C_TIMESTAMP,
                            SQL_C_TYPE_DATE,
                            SQL_C_TYPE_TIME,
                            SQL_C_TYPE_TIMESTAMP,
                            SQL_C_DEFAULT,
                            SQL_C_INTERVAL_YEAR,
                            SQL_C_INTERVAL_MONTH,
                            SQL_C_INTERVAL_DAY,
                            SQL_C_INTERVAL_HOUR,
                            SQL_C_INTERVAL_MINUTE,
                            SQL_C_INTERVAL_SECOND,
                            SQL_C_INTERVAL_YEAR_TO_MONTH,
                            SQL_C_INTERVAL_DAY_TO_HOUR,
                            SQL_C_INTERVAL_DAY_TO_MINUTE,
                            SQL_C_INTERVAL_DAY_TO_SECOND,
                            SQL_C_INTERVAL_HOUR_TO_MINUTE,
                            SQL_C_INTERVAL_HOUR_TO_SECOND,
                            SQL_C_INTERVAL_MINUTE_TO_SECOND);
   for SQL_C_DATA_TYPE use (SQL_C_UTINYINT                  => -28,
                            SQL_C_UBIGINT                   => -27,
                            SQL_C_STINYINT                  => -26,
                            SQL_C_SBIGINT                   => -25,
                            SQL_C_ULONG                     => -18,
                            SQL_C_USHORT                    => -17,
                            SQL_C_SLONG                     => -16,
                            SQL_C_SSHORT                    => -15,
                            SQL_C_GUID                      => -11,
                            SQL_C_WCHAR                     => -8,
                            SQL_C_BIT                       => -7,
                            SQL_C_TINYINT                   => -6,
                            SQL_C_BINARY                    => -2,
                            SQL_C_CHAR                      => 1,
                            SQL_C_NUMERIC                   => 2,
                            SQL_C_LONG                      => 4,
                            SQL_C_SHORT                     => 5,
                            SQL_C_FLOAT                     => 7,
                            SQL_C_DOUBLE                    => 8,
                            SQL_C_DATE                      => 9,
                            SQL_C_TIME                      => 10,
                            SQL_C_TIMESTAMP                 => 11,
                            SQL_C_TYPE_DATE                 => 91,
                            SQL_C_TYPE_TIME                 => 92,
                            SQL_C_TYPE_TIMESTAMP            => 93,
                            SQL_C_DEFAULT                   => 99,
                            SQL_C_INTERVAL_YEAR             => 101,
                            SQL_C_INTERVAL_MONTH            => 102,
                            SQL_C_INTERVAL_DAY              => 103,
                            SQL_C_INTERVAL_HOUR             => 104,
                            SQL_C_INTERVAL_MINUTE           => 105,
                            SQL_C_INTERVAL_SECOND           => 106,
                            SQL_C_INTERVAL_YEAR_TO_MONTH    => 107,
                            SQL_C_INTERVAL_DAY_TO_HOUR      => 108,
                            SQL_C_INTERVAL_DAY_TO_MINUTE    => 109,
                            SQL_C_INTERVAL_DAY_TO_SECOND    => 110,
                            SQL_C_INTERVAL_HOUR_TO_MINUTE   => 111,
                            SQL_C_INTERVAL_HOUR_TO_SECOND   => 112,
                            SQL_C_INTERVAL_MINUTE_TO_SECOND => 113);
   for SQL_C_DATA_TYPE'Size use SQLSMALLINT'Size;

   SQL_C_BOOKMARK    : constant SQL_C_DATA_TYPE := SQL_C_ULONG;
   SQL_C_VARBOOKMARK : constant SQL_C_DATA_TYPE := SQL_C_BINARY;

   --  Values of NULLABLE field in descriptor
   type SQL_NULLABLE_INFO is (SQL_NO_NULLS,
                              SQL_NULLABLE,
                              --  Value returned by SQLGetTypeInfo to
                              --  denote that it is not known whether or not
                              --  a data type supports null values.
                              SQL_NULLABLE_UNKNOWN);
   for SQL_NULLABLE_INFO'Size use SQLSMALLINT'Size;

   type SQL_NULLABLE_FIELD is new SQL_NULLABLE_INFO
     range SQL_NO_NULLS .. SQL_NULLABLE;

   -- | ----------------------------------------------------------------------
   -- |
   --  Values returned by SQLGetTypeInfo to show WHERE clause supported
   type SQL_WHERE_INFO is (SQL_PRED_NONE,
                           SQL_PRED_CHAR,
                           SQL_PRED_BASIC);
   for SQL_WHERE_INFO'Size use SQLSMALLINT'Size;

   function SQLGetTypeInfo (StatementHandle : SQLHSTMT;
                            DataType        : SQL_DATA_TYPE) return SQLRETURN;

   procedure SQLGetTypeInfo (StatementHandle : SQLHSTMT;
                             DataType        : SQL_DATA_TYPE);
   --  Returns information about supported data types.
   pragma Inline (SQLGetTypeInfo);

   -- | ----------------------------------------------------------------------
   -- |
   type SQL_Column_Number is new SQLUSMALLINT;
   Bookmark_Column : constant SQL_Column_Number := 0;

   function SQLBindCol (StatementHandle : SQLHSTMT;
                        ColumnNumber    : SQL_Column_Number;
                        TargetType      : SQL_C_DATA_TYPE;
                        TargetValue     : SQLPOINTER;
                        BufferLength    : SQLINTEGER;
                        pStrLen_or_Ind  : access SQLINTEGER) return SQLRETURN;

   procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                         ColumnNumber     : in SQL_Column_Number;
                         TargetType       : in SQL_C_DATA_TYPE;
                         TargetValuePtr   : in SQLPOINTER;
                         BufferLength     : in SQLINTEGER;
                         StrLen_Or_IndPtr : access SQLINTEGER);
   pragma Inline (SQLBindCol);

   function SQLBindCol (StatementHandle  : SQLHSTMT;
                        ColumnNumber     : SQL_Column_Number;
                        TargetValue      : access String;
                        StrLen_Or_IndPtr : access SQLINTEGER)
                       return SQLRETURN;

   procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                         ColumnNumber     : in SQL_Column_Number;
                         TargetValue      : access String;
                         StrLen_Or_IndPtr : access SQLINTEGER);

   -- | ----------------------------------------------------------------------
   -- |
   type SQL_Parameter_Number is new SQLUSMALLINT range 1 .. SQLUSMALLINT'Last;
   type SQL_Parameter_Type is (SQL_PARAM_TYPE_UNKNOWN,
                               SQL_PARAM_INPUT,
                               SQL_PARAM_INPUT_OUTPUT,
                               SQL_RESULT_COL,
                               SQL_PARAM_OUTPUT,
                               SQL_RETURN_VALUE);
   for SQL_Parameter_Type'Size use SQLSMALLINT'Size;

   function SQLBindParameter (StatementHandle  : in SQLHSTMT;
                              ParameterNumber  : in SQL_Parameter_Number;
                              InputOutputType  : in SQL_Parameter_Type;
                              ValueType        : in SQL_C_DATA_TYPE;
                              ParameterType    : in SQL_DATA_TYPE;
                              ColumnSize       : in SQLUINTEGER;
                              DecimalDigits    : in SQLSMALLINT;
                              Value            : in SQLPOINTER;
                              BufferLength     : in SQLINTEGER;
                              StrLen_Or_IndPtr : access SQLINTEGER)
                             return SQLRETURN;

   procedure SQLBindParameter (StatementHandle  : in SQLHSTMT;
                               ParameterNumber  : in SQL_Parameter_Number;
                               InputOutputType  : in SQL_Parameter_Type;
                               ValueType        : in SQL_C_DATA_TYPE;
                               ParameterType    : in SQL_DATA_TYPE;
                               ColumnSize       : in SQLUINTEGER;
                               DecimalDigits    : in SQLSMALLINT;
                               Value            : in SQLPOINTER;
                               BufferLength     : in SQLINTEGER;
                               StrLen_Or_IndPtr : access SQLINTEGER);
   pragma Inline (SQLBindParameter);

   function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                              ParameterNumber : in     SQL_Parameter_Number;
                              Value           : access String;
                              Length          : access SQLINTEGER;
                              InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                             return SQLRETURN;

   procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                               ParameterNumber : in     SQL_Parameter_Number;
                               Value           : access String;
                               Length          : access SQLINTEGER;
                               InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT);

   -- | ----------------------------------------------------------------------
   -- |
   generic
      type Int is range <>;
   package IntegerBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Int;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Int;
                            IndPtr           : access SQLINTEGER);

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Int;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                                return SQLRETURN;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Int;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT);

   end IntegerBinding;

   -- | ----------------------------------------------------------------------
   -- |
   generic
      type Unsigned is mod <>;
   package UnsignedBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Unsigned;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Unsigned;
                            IndPtr           : access SQLINTEGER);

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Unsigned;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT)
                                return SQLRETURN;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Unsigned;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT);

   end UnsignedBinding;

   -- | ----------------------------------------------------------------------
   -- |
   generic
      type Flt is digits <>;
   package FloatBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Flt;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Flt;
                            IndPtr           : access SQLINTEGER);

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Flt;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT)
                                return SQLRETURN;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Flt;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT);

   end FloatBinding;

   -- | ----------------------------------------------------------------------
   -- |
   generic
      type Enum is (<>);
   package EnumBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Enum;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Enum;
                            IndPtr           : access SQLINTEGER);

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Enum;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT)
                                return SQLRETURN;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Enum;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type    := SQL_PARAM_INPUT);

   end EnumBinding;


   -- | ----------------------------------------------------------------------
   -- |
   procedure SQLDescribeParam (StatementHandle : in  SQLHSTMT;
                               ParameterNumber : in  SQL_Parameter_Number;
                               ParameterType   : out SQL_DATA_TYPE;
                               ColumnSize      : out SQLUINTEGER;
                               DecimalDigits   : out SQLSMALLINT;
                               Nullable        : out SQL_NULLABLE_INFO;
                               RC              : out SQLRETURN);
   procedure SQLDescribeParam (StatementHandle : in  SQLHSTMT;
                               ParameterNumber : in  SQL_Parameter_Number;
                               ParameterType   : out SQL_DATA_TYPE;
                               ColumnSize      : out SQLUINTEGER;
                               DecimalDigits   : out SQLSMALLINT;
                               Nullable        : out SQL_NULLABLE_INFO);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLCancel (StatementHandle : SQLHSTMT) return SQLRETURN;

   procedure SQLCancel (StatementHandle : in SQLHSTMT);
   pragma Inline (SQLCancel);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLCloseCursor (StatementHandle : SQLHSTMT) return SQLRETURN;

   procedure SQLCloseCursor (StatementHandle : in SQLHSTMT);
   pragma Inline (SQLCloseCursor);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLExecute (StatementHandle : SQLHSTMT) return SQLRETURN;

   procedure SQLExecute (StatementHandle : in SQLHSTMT);
   pragma Inline (SQLExecute);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLExecDirect (StatementHandle : SQLHSTMT;
                           StatementText   : String) return SQLRETURN;

   procedure SQLExecDirect (StatementHandle : in SQLHSTMT;
                            StatementText   : in String);

   function SQLExecDirect (StatementHandle : SQLHSTMT;
                           StatementText   : Wide_String) return SQLRETURN;

   procedure SQLExecDirect (StatementHandle : in SQLHSTMT;
                            StatementText   : in Wide_String);
   pragma Inline (SQLExecDirect);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLFetch (StatementHandle : SQLHSTMT) return SQLRETURN;

   procedure SQLFetch   (StatementHandle : in SQLHSTMT);
   pragma Inline (SQLFetch);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLFetchScroll (StatementHandle  : in SQLHSTMT;
                            FetchDirection   : in FETCH_DIRECTION;
                            FetchOffset      : in SQLINTEGER := 0)
                           return SQLRETURN;

   procedure SQLFetchScroll (StatementHandle : in SQLHSTMT;
                             FetchDirection  : in FETCH_DIRECTION;
                             FetchOffset     : in SQLINTEGER := 0);
   pragma Inline (SQLFetchScroll);

   -- | ----------------------------------------------------------------------
   -- |
   type ROW_STATUS is (SQL_ROW_SUCCESS,
                       SQL_ROW_DELETED,
                       SQL_ROW_UPDATED,
                       SQL_ROW_NOROW,
                       SQL_ROW_ADDED,
                       SQL_ROW_ERROR,
                       SQL_ROW_SUCCESS_WITH_INFO);
   for ROW_STATUS'Size use SQLUSMALLINT'Size;

   type RowStatusArray is array (Natural) of aliased ROW_STATUS;
   pragma Convention (C, RowStatusArray);
   type RowStatusArrayPtr is access all RowStatusArray;
   --  This is a non-fat pointer

   function SQLPrepare (StatementHandle : SQLHSTMT;
                        StatementText   : String) return SQLRETURN;

   procedure SQLPrepare (StatementHandle : SQLHSTMT;
                         StatementText   : String);

   function SQLPrepare (StatementHandle : SQLHSTMT;
                        StatementText   : Wide_String) return SQLRETURN;

   procedure SQLPrepare (StatementHandle : SQLHSTMT;
                         StatementText   : Wide_String);
   pragma Inline (SQLPrepare);

   -- | ----------------------------------------------------------------------
   -- |
   procedure SQLGetCursorName (StatementHandle : in  SQLHSTMT;
                               NameBuffer      : out String;
                               BufferLength    : out SQLSMALLINT;
                               ErrorCode       : out SQLRETURN);
   --  This version doesn't raise an exception but returns the ErrorCode
   --  in an out parameter

   function  SQLGetCursorName (StatementHandle : SQLHSTMT;
                               MaxNameLength    : SQLSMALLINT := 256)
                              return String;
   --  This version may raise an exception

   procedure SQLGetCursorName (StatementHandle : in  SQLHSTMT;
                               NameBuffer      : out Wide_String;
                               BufferLength    : out SQLSMALLINT;
                               ErrorCode       : out SQLRETURN);
   --  This version doesn't raise an exception but returns the ErrorCode
   --  in an out parameter

   function  SQLGetCursorName (StatementHandle : SQLHSTMT;
                               MaxNameLength    : SQLSMALLINT := 256)
                              return Wide_String;
   --  This version may raise an exception

   -- | ----------------------------------------------------------------------
   -- |
   function SQLSetCursorName (StatementHandle : SQLHSTMT;
                              CursorName      : String) return SQLRETURN;

   procedure SQLSetCursorName (StatementHandle : in SQLHSTMT;
                               CursorName      : in String);

   function SQLSetCursorName (StatementHandle : SQLHSTMT;
                              CursorName      : Wide_String) return SQLRETURN;

   procedure SQLSetCursorName (StatementHandle : in SQLHSTMT;
                               CursorName      : in Wide_String);
   pragma Inline (SQLSetCursorName);

   -- | ----------------------------------------------------------------------
   -- |
   type SQL_COMPLETION_TYPE is (SQL_COMMIT,
                                SQL_ROLLBACK);
   for SQL_COMPLETION_TYPE'Size use SQLSMALLINT'Size;

   function SQLEndTran
     (HandleType     : SQL_ENDTRAN_HANDLE_TYPE := SQL_HANDLE_DBC;
      Handle         : SQLHANDLE;
      CompletionType : SQL_COMPLETION_TYPE := SQL_COMMIT) return SQLRETURN;

   procedure SQLEndTran
     (HandleType     : in SQL_ENDTRAN_HANDLE_TYPE := SQL_HANDLE_DBC;
      Handle         : in SQLHANDLE;
      CompletionType : in SQL_COMPLETION_TYPE := SQL_COMMIT);
   pragma Inline (SQLEndTran);

   --
   --  Commodity
   --
   procedure SQLCommit   (ConnectionHandle : SQLHDBC);
   procedure SQLRollback (ConnectionHandle : SQLHDBC);
   pragma Inline (SQLCommit);
   pragma Inline (SQLRollback);
   --  Note : These versions may raise exceptions. If you don't want
   --         exceptions use the function SQLEndTran explicitly.

   -- | ----------------------------------------------------------------------
   -- |
   function SQLNumParams (StatementHandle : SQLHSTMT;
                          pNum            : access SQLSMALLINT)
                         return SQLRETURN;

   function SQLNumParams (StatementHandle : SQLHSTMT) return SQLSMALLINT;
   pragma Inline (SQLNumParams);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLNumResultCols (StatementHandle : SQLHSTMT;
                              pColumnCount    : access SQL_Column_Number)
                             return SQLRETURN;

   function SQLNumResultCols (StatementHandle : SQLHSTMT)
                             return SQL_Column_Number;
   pragma Inline (SQLNumResultCols);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLRowCount (StatementHandle : SQLHSTMT;
                         pRowCount       : access SQLINTEGER) return SQLRETURN;

   function SQLRowCount (StatementHandle : SQLHSTMT) return SQLINTEGER;
   pragma Inline (SQLRowCount);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLGetData (StatementHandle : in SQLHSTMT;
                        ColumnNumber    : in SQL_Column_Number;
                        TargetType      : in SQL_C_DATA_TYPE;
                        TargetValue     : in SQLPOINTER;
                        BufferLength    : in SQLINTEGER;
                        StrLen_Or_Ind   : access SQLINTEGER) return SQLRETURN;

   procedure SQLGetData (StatementHandle : in SQLHSTMT;
                         ColumnNumber    : in SQL_Column_Number;
                         TargetType      : in SQL_C_DATA_TYPE;
                         TargetValue     : in SQLPOINTER;
                         BufferLength    : in SQLINTEGER;
                         StrLen_Or_Ind   : access SQLINTEGER);
   pragma Inline (SQLGetData);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLPutData (StatementHandle : SQLHSTMT;
                        Data            : SQLPOINTER;
                        StrLen_or_Ind   : SQLINTEGER) return SQLRETURN;

   procedure SQLPutData (StatementHandle : in SQLHSTMT;
                         Data            : in SQLPOINTER;
                         StrLen_Or_Ind   : SQLINTEGER);
   pragma Inline (SQLPutData);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLParamData (StatementHandle : SQLHSTMT;
                          pValue          : access SQLPOINTER)
                         return SQLRETURN;

   function SQLParamData (StatementHandle : in SQLHSTMT) return SQLPOINTER;
   pragma Inline (SQLParamData);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLCopyDesc (SourceHandle : SQLHDESC;
                         TargetHandle : SQLHDESC) return SQLRETURN;

   procedure SQLCopyDesc (SourceHandle : in SQLHDESC;
                          TargetHandle : in SQLHDESC);
   pragma Inline (SQLCopyDesc);

   -- | ----------------------------------------------------------------------
   -- |
   type SQL_STATISTICS_UNIQUE_OPTION is (SQL_INDEX_UNIQUE,
                                         SQL_INDEX_ALL);
   for SQL_STATISTICS_UNIQUE_OPTION'Size use SQLSMALLINT'Size;

   type SQL_STATISTICS_PAGES_OPTION is (SQL_QUICK,
                                        SQL_ENSURE);
   for SQL_STATISTICS_PAGES_OPTION'Size use SQLSMALLINT'Size;

   function SQLStatistics
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      Unique          : SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
     return SQLRETURN;

   procedure SQLStatistics
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in String;
      SchemaName      : in String;
      TableName       : in String;
      Unique          : in SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : in SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK);
   pragma Inline (SQLStatistics);

   function SQLStatistics
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      Unique          : SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
     return SQLRETURN;

   procedure SQLStatistics
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String;
      SchemaName      : in Wide_String;
      TableName       : in Wide_String;
      Unique          : in SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : in SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK);
   pragma Inline (SQLStatistics);


   -- | ----------------------------------------------------------------------
   -- |
   SQL_ALL_PATTERN     : constant String := "%";
   SQL_ALL_CATALOGS    : String renames SQL_ALL_PATTERN;
   SQL_ALL_SCHEMAS     : String renames SQL_ALL_PATTERN;
   SQL_ALL_TABLES      : String renames SQL_ALL_PATTERN;
   SQL_ALL_TABLE_TYPES : String renames SQL_ALL_PATTERN;
   SQL_ALL_PROCEDURES  : String renames SQL_ALL_PATTERN;
   SQL_ALL_COLUMNS     : String renames SQL_ALL_PATTERN;

   W_SQL_ALL_PATTERN     : constant Wide_String := "%";
   W_SQL_ALL_CATALOGS    : Wide_String renames W_SQL_ALL_PATTERN;
   W_SQL_ALL_SCHEMAS     : Wide_String renames W_SQL_ALL_PATTERN;
   W_SQL_ALL_TABLES      : Wide_String renames W_SQL_ALL_PATTERN;
   W_SQL_ALL_TABLE_TYPES : Wide_String renames W_SQL_ALL_PATTERN;
   W_SQL_ALL_PROCEDURES  : Wide_String renames W_SQL_ALL_PATTERN;
   W_SQL_ALL_COLUMNS     : Wide_String renames W_SQL_ALL_PATTERN;

   function SQLTables (StatementHandle : SQLHSTMT;
                       CatalogName     : String := SQL_ALL_CATALOGS;
                       SchemaName      : String := SQL_ALL_SCHEMAS;
                       TableName       : String;
                       TableType       : String := SQL_ALL_TABLE_TYPES)
                      return SQLRETURN;

   procedure SQLTables (StatementHandle : in SQLHSTMT;
                        CatalogName     : in String := SQL_ALL_CATALOGS;
                        SchemaName      : in String := SQL_ALL_SCHEMAS;
                        TableName       : in String;
                        TableType       : in String := SQL_ALL_TABLE_TYPES);

   function SQLTables
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String := W_SQL_ALL_CATALOGS;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String;
      TableType       : Wide_String := W_SQL_ALL_TABLE_TYPES)
      return SQLRETURN;

   procedure SQLTables
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String := W_SQL_ALL_CATALOGS;
      SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : in Wide_String;
      TableType       : in Wide_String := W_SQL_ALL_TABLE_TYPES);
   pragma Inline (SQLTables);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLProcedures (StatementHandle : SQLHSTMT;
                           CatalogName     : String;
                           SchemaName      : String := SQL_ALL_SCHEMAS;
                           ProcName        : String := SQL_ALL_PROCEDURES)
                          return SQLRETURN;

   procedure SQLProcedures (StatementHandle : SQLHSTMT;
                            CatalogName     : String;
                            SchemaName      : String := SQL_ALL_SCHEMAS;
                            ProcName        : String := SQL_ALL_PROCEDURES);

   function SQLProcedures
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES)
     return SQLRETURN;

   procedure SQLProcedures
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES);
   pragma Inline (SQLProcedures);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      ProcName        : String := SQL_ALL_PROCEDURES;
      ColumnName      : String := SQL_ALL_COLUMNS) return SQLRETURN;

   procedure SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      ProcName        : String := SQL_ALL_PROCEDURES;
      ColumnName      : String := SQL_ALL_COLUMNS);

   function SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS) return SQLRETURN;

   procedure SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS);
   pragma Inline (SQLProcedureColumns);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      TableName       : String := SQL_ALL_TABLES) return SQLRETURN;

   procedure SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      TableName       : String := SQL_ALL_TABLES);

   function SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String := W_SQL_ALL_TABLES) return SQLRETURN;

   procedure SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String := W_SQL_ALL_TABLES);
   pragma Inline (SQLTablePrivileges);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      ColumnName      : String := SQL_ALL_COLUMNS) return SQLRETURN;

   procedure SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      ColumnName      : String := SQL_ALL_COLUMNS);

   function SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS) return SQLRETURN;

   procedure SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS);
   pragma Inline (SQLColumnPrivileges);

   -- | ----------------------------------------------------------------------
   -- |
   type Special_Column_Type is (SQL_BEST_ROWID,
                                SQL_ROWVER);
   for Special_Column_Type use (SQL_BEST_ROWID  => 1,
                                SQL_ROWVER      => 2);
   for Special_Column_Type'Size use SQLSMALLINT'Size;

   type ROWID_SCOPE is (SQL_SCOPE_CURROW,
                        SQL_SCOPE_TRANSACTION,
                        SQL_SCOPE_SESSION);
   for ROWID_SCOPE'Size use SQLSMALLINT'Size;

   function SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                               IdentifierType  : in Special_Column_Type;
                               CatalogName     : in String;
                               SchemaName      : in String;
                               TableName       : in String;
                               Scope           : in ROWID_SCOPE;
                               Nullable        : in SQL_NULLABLE_FIELD)
                              return SQLRETURN;

   procedure SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                                IdentifierType  : in Special_Column_Type;
                                CatalogName     : in String;
                                SchemaName      : in String;
                                TableName       : in String;
                                Scope           : in ROWID_SCOPE;
                                Nullable        : in SQL_NULLABLE_FIELD);

   function SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                               IdentifierType  : in Special_Column_Type;
                               CatalogName     : in Wide_String;
                               SchemaName      : in Wide_String;
                               TableName       : in Wide_String;
                               Scope           : in ROWID_SCOPE;
                               Nullable        : in SQL_NULLABLE_FIELD)
                              return SQLRETURN;

   procedure SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                                IdentifierType  : in Special_Column_Type;
                                CatalogName     : in Wide_String;
                                SchemaName      : in Wide_String;
                                TableName       : in Wide_String;
                                Scope           : in ROWID_SCOPE;
                                Nullable        : in SQL_NULLABLE_FIELD);
   pragma Inline (SQLSpecialColumns);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                            CatalogName     : String;
                            SchemaName      : String;
                            TableName       : String) return SQLRETURN;

   procedure SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                             CatalogName     : String;
                             SchemaName      : String;
                             TableName       : String);

   function SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                            CatalogName     : Wide_String;
                            SchemaName      : Wide_String;
                            TableName       : Wide_String) return SQLRETURN;

   procedure SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                             CatalogName     : Wide_String;
                             SchemaName      : Wide_String;
                             TableName       : Wide_String);
   pragma Inline (SQLPrimaryKeys);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLForeignKeys (StatementHandle        : SQLHSTMT;
                            PrimaryCatalogName     : String;
                            PrimarySchemaName      : String;
                            PrimaryTableName       : String;
                            ForeignCatalogName     : String;
                            ForeignSchemaName      : String;
                            ForeignTableName       : String) return SQLRETURN;

   procedure SQLForeignKeys (StatementHandle        : SQLHSTMT;
                             PrimaryCatalogName     : String;
                             PrimarySchemaName      : String;
                             PrimaryTableName       : String;
                             ForeignCatalogName     : String;
                             ForeignSchemaName      : String;
                             ForeignTableName       : String);

   function SQLForeignKeys (StatementHandle        : SQLHSTMT;
                            PrimaryCatalogName     : Wide_String;
                            PrimarySchemaName      : Wide_String;
                            PrimaryTableName       : Wide_String;
                            ForeignCatalogName     : Wide_String;
                            ForeignSchemaName      : Wide_String;
                            ForeignTableName       : Wide_String)
                            return SQLRETURN;

   procedure SQLForeignKeys (StatementHandle        : SQLHSTMT;
                             PrimaryCatalogName     : Wide_String;
                             PrimarySchemaName      : Wide_String;
                             PrimaryTableName       : Wide_String;
                             ForeignCatalogName     : Wide_String;
                             ForeignSchemaName      : Wide_String;
                             ForeignTableName       : Wide_String);
   pragma Inline (SQLForeignKeys);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO;
                            ErrorCode       : access SQLRETURN)
                           return String;
   --  This version returns the ErrorCode when an ODBC error occured

   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO)
                            return String;
   --  This version raises an exception when an ODBC error occured

   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO;
                            ErrorCode       : access SQLRETURN)
                           return Wide_String;
   --  This version returns the ErrorCode when an ODBC error occured

   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO)
                            return Wide_String;
   --  This version raises an exception when an ODBC error occured

   -- | ----------------------------------------------------------------------
   -- |
   function SQLColumns (StatementHandle : in SQLHSTMT;
                        CatalogName     : in String;
                        SchemaName      : in String := SQL_ALL_SCHEMAS;
                        TableName       : in String := SQL_ALL_TABLES;
                        ColumnName      : in String := SQL_ALL_COLUMNS)
                       return SQLRETURN;

   procedure SQLColumns (StatementHandle : in SQLHSTMT;
                         CatalogName     : in String;
                         SchemaName      : in String := SQL_ALL_SCHEMAS;
                         TableName       : in String := SQL_ALL_TABLES;
                         ColumnName      : in String := SQL_ALL_COLUMNS);

   function SQLColumns (StatementHandle : in SQLHSTMT;
                        CatalogName     : in Wide_String;
                        SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
                        TableName       : in Wide_String := W_SQL_ALL_TABLES;
                        ColumnName      : in Wide_String := W_SQL_ALL_COLUMNS)
                       return SQLRETURN;

   procedure SQLColumns
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String;
      SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : in Wide_String := W_SQL_ALL_TABLES;
      ColumnName      : in Wide_String := W_SQL_ALL_COLUMNS);
   pragma Inline (SQLColumns);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : String;
                          MaxLength        : SQLINTEGER := 1024;
                          ErrorCode        : access SQLRETURN)
     return String;
   --  This version returns the ErrorCode when an ODBC error occured

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : String;
                          MaxLength        : SQLINTEGER := 1024)
     return String;
   --  This version raises an exception when an ODBC error occured

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : Wide_String;
                          MaxLength        : SQLINTEGER := 1024;
                          ErrorCode        : access SQLRETURN)
     return Wide_String;
   --  This version returns the ErrorCode when an ODBC error occured

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : Wide_String;
                          MaxLength        : SQLINTEGER := 1024)
     return Wide_String;
   --  This version raises an exception when an ODBC error occured

   -- | ----------------------------------------------------------------------
   -- |
   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out String;
                         DriverAttributes    : out String);
   --  This version raises an exception when an ODBC error occured

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out String;
                         DriverAttributes    : out String;
                         RC                  : out SQLRETURN);

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out Wide_String;
                         DriverAttributes    : out Wide_String);
   --  This version raises an exception when an ODBC error occured

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out Wide_String;
                         DriverAttributes    : out Wide_String;
                         RC                  : out SQLRETURN);

   -- | ----------------------------------------------------------------------
   -- |
   type POSITIONAL_OPERATION is (SQL_POSITION,
                                 SQL_REFRESH,
                                 SQL_UPDATE,
                                 SQL_DELETE,
                                 SQL_ADD,
                                 SQL_UPDATE_BY_BOOKMARK,
                                 SQL_DELETE_BY_BOOKMARK,
                                 SQL_FETCH_BY_BOOKMARK);
   for POSITIONAL_OPERATION use (SQL_POSITION             => 0,
                                 SQL_REFRESH              => 1,
                                 SQL_UPDATE               => 2,
                                 SQL_DELETE               => 3,
                                 SQL_ADD                  => 4,
                                 SQL_UPDATE_BY_BOOKMARK   => 5,
                                 SQL_DELETE_BY_BOOKMARK   => 6,
                                 SQL_FETCH_BY_BOOKMARK    => 7);
   for POSITIONAL_OPERATION'Size use SQLUSMALLINT'Size;

   type BULK_OPERATION is new POSITIONAL_OPERATION
     range SQL_ADD .. POSITIONAL_OPERATION'Last;

   function SQLBulkOperations (StatementHandle  : SQLHSTMT;
                               Operation        : BULK_OPERATION)
                              return SQLRETURN;

   procedure SQLBulkOperations (StatementHandle  : SQLHSTMT;
                                Operation        : BULK_OPERATION);
   pragma Inline (SQLBulkOperations);

   -- | ----------------------------------------------------------------------
   -- |
   function SQLMoreResults (StatementHandle : SQLHSTMT) return SQLRETURN;
   pragma Inline (SQLMoreResults);

   -- | ----------------------------------------------------------------------
   -- |
   type POSITION_OPERATION is new POSITIONAL_OPERATION
     range POSITIONAL_OPERATION'First .. SQL_ADD;

   type POSITION_LOCKTYPE is (SQL_LOCK_NO_CHANGE,
                              SQL_LOCK_EXCLUSIVE,
                              SQL_LOCK_UNLOCK);
   for POSITION_LOCKTYPE use (SQL_LOCK_NO_CHANGE  => 0,
                              SQL_LOCK_EXCLUSIVE  => 1,
                              SQL_LOCK_UNLOCK     => 2);
   for POSITION_LOCKTYPE'Size use SQLUSMALLINT'Size;

   function SQLSetPos (StatementHandle  : in SQLHSTMT;
                       RowNumber        : in SQLUSMALLINT;
                       Operation        : in POSITION_OPERATION;
                       LockType         : in POSITION_LOCKTYPE)
                      return SQLRETURN;

   procedure SQLSetPos (StatementHandle  : in SQLHSTMT;
                        RowNumber        : in SQLUSMALLINT;
                        Operation        : in POSITION_OPERATION;
                        LockType         : in POSITION_LOCKTYPE);
   pragma Inline (SQLSetPos);

   -- | ----------------------------------------------------------------------
   -- |
   type DRIVER_COMPLETION is (SQL_DRIVER_NOPROMPT,
                              SQL_DRIVER_COMPLETE,
                              SQL_DRIVER_PROMPT,
                              SQL_DRIVER_COMPLETE_REQUIRED);
   for DRIVER_COMPLETION'Size use SQLUSMALLINT'Size;

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN);

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT);

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN);

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT);

   -- | ----------------------------------------------------------------------
   -- |
   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN);

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT);

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN);

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT);

   -- | ----------------------------------------------------------------------
   -- |
   --  whether an attribute is a pointer or not
   SQL_IS_POINTER    : constant := -4;
   SQL_IS_UINTEGER   : constant := -5;
   SQL_IS_INTEGER    : constant := -6;
   SQL_IS_USMALLINT  : constant := -7;
   SQL_IS_SMALLINT   : constant := -8;

   function SQL_LEN_DATA_AT_EXEC (Length : SQLINTEGER) return SQLINTEGER;
   pragma Inline (SQL_LEN_DATA_AT_EXEC);

   function SQL_LEN_BINARY_ATTR (Length : SQLINTEGER) return SQLINTEGER;
   pragma Inline (SQL_LEN_BINARY_ATTR);

   -- | ----------------------------------------------------------------------
   -- |
   function Null_Handle return SQLHANDLE;
   pragma Inline (Null_Handle);


   function SQL_Error_Message (HandleType  : SQL_HANDLE_TYPE;
                               Handle      : SQLHSTMT;
                               State       : access SQLSTATE)
                               return String;
   --  Ada95 Helper Routine
   --  Return the formatted error message and fill the SQLSTATE


   -- | ----------------------------------------------------------------------
   -- |
   procedure SQLFixNTS (Column  : in out String;
                        R_Index : SQLINTEGER := SQLINTEGER'Last;
                        Pad     : Character  := ' ');
   --  Ada95 Helper Routine
   --  Unfortunately almost all ODBC implementation do not support to set
   --  the environment attribute SQL_OUTPUT_NTS to false; they simply assume
   --  C Strings are the only form of string passed to ODBC. So we offer this
   --  helper that replaces potentially added ASCII.NULs by spaces.
   --  In addtion to the Column to fix you can optionally pass the rightmost
   --  position in the string where to start replacing NULs by spaces;
   --  the default is to start at the rightmost position of the string.
   --  You may also specify another padding character than a space.

   procedure SQLFixNTS (Column  : in out Wide_String;
                        R_Index : SQLINTEGER := SQLINTEGER'Last;
                        Pad     : Wide_Character  := Wide_Character'(' '));
   pragma Inline (SQLFixNTS);

   -- | ----------------------------------------------------------------------
   -- |
   procedure Unicode_Attributes (Flag : in Boolean := True);
   --  Set whether or not we should use Unicode for attributes that return
   --  string values.

   SQL_MAX_OPTION_STRING_LENGTH : constant := 256;

   type ASYNC_ENABLE is (SQL_ASYNC_ENABLE_OFF,
                         SQL_ASYNC_ENABLE_ON);
   --  |
   --  | values for SQL_ATTR_ASYNC_ENABLE
   --  |
   for ASYNC_ENABLE'Size use SQLINTEGER'Size;
   SQL_ASYNC_ENABLE_DEFAULT : constant ASYNC_ENABLE := SQL_ASYNC_ENABLE_OFF;

private
   --  Flag whether or not we use Unicode for attributes that return string
   --  values.
   Unicode_Attr_Flag : Boolean := False;

   type SQLPOINTER is new System.Storage_Elements.Integer_Address;

   --  Field width for labels in debug output
   Debug_Label_Width : constant := 40;

   type SQLHANDLE  is new System.Storage_Elements.Integer_Address;

   SQL_NULL_HANDLE : constant SQLHANDLE := 0;
   SQL_NULL_HENV   : constant SQLHENV   := 0;
   SQL_NULL_HDBC   : constant SQLHDBC   := 0;
   SQL_NULL_HSTMT  : constant SQLHSTMT  := 0;
   SQL_NULL_HDESC  : constant SQLHDESC  := 0;
   SQL_NULL_HWND   : constant SQLHWND   := 0;

   SQL_MAX_MESSAGE_SIZE        : constant := 256;

   SQL_LEN_DATA_AT_EXEC_OFFSET : constant := -100;
   SQL_LEN_BINARY_ATTR_OFFSET  : constant := -100;

   SQL_ADA95_BINDING_ERROR : constant SQLRETURN := SQLRETURN'First;
   SQL_ADA95_DATA_ERROR    : constant SQLRETURN := SQL_ADA95_BINDING_ERROR + 1;
   SQL_ADA95_INVALID_ENUM  : constant SQLRETURN := SQL_ADA95_BINDING_ERROR + 2;
   SQL_ADA95_TYPE_ERROR    : constant SQLRETURN := SQL_ADA95_BINDING_ERROR + 3;
   SQL_ADA95_NO_UNICODE    : constant SQLRETURN := SQL_ADA95_BINDING_ERROR + 4;

   procedure Raise_SQL_Error
     (ProcedureName : in String;
      ErrorMessage  : in String    := "";
      RC            : in SQLRETURN := SQL_ADA95_BINDING_ERROR;
      State         : in SQLSTATE  := EMPTY_SQLSTATE);
   pragma No_Return (Raise_SQL_Error);

   procedure Check_SQL_Error
     (RC                : in SQLRETURN;
      ProcedureName     : in String;
      ErrorMessage      : in String := "";
      HandleType        : in SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
      Handle            : in SQLHANDLE := SQL_NULL_HANDLE);

   function Is_SQL_Ok (ErrorCode : SQLRETURN) return Boolean;
   pragma Inline (Is_SQL_Ok);

   procedure Raise_Invalid_Enum (ProcedureName : in String;
                                 EnumName      : in String;
                                 EnumValue     : in String);
   pragma No_Return (Raise_Invalid_Enum);

   Attr_Not_Supported_Msg : constant String :=
     " is not supported by Ada95 binding.";

   subtype C_SQLSTATE  is Interfaces.C.char_array (1 .. SQL_SQLSTATE_SIZE);
   function To_Ada (State : C_SQLSTATE) return SQLSTATE;
   --  Convert the C character array State into an fixed length Ada String
   pragma Inline_Always (To_Ada);

   subtype C_WSQLSTATE is WIDE_SQLSTATE;

   function Length_Indicator (Size : Natural) return Integer;
   pragma Inline_Always (Length_Indicator);

   type Attr_Init is access procedure;
   procedure Register_Initializer (Ini : in Attr_Init);

end GNU.DB.SQLCLI;
