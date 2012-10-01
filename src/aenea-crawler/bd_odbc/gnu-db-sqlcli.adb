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
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with Interfaces.C; use Interfaces.C;

package body GNU.DB.SQLCLI is
   pragma Linker_Options ("-L/gnat/lib/win32 -lodbc32");

   package P_SQLTCHAR is new System.Address_To_Access_Conversions (SQLTCHAR);
   package P_SQLCHAR  is new System.Address_To_Access_Conversions (SQLCHAR);
   package P_SQLUI  is new System.Address_To_Access_Conversions (SQLUINTEGER);
   package P_SQLI   is new System.Address_To_Access_Conversions (SQLINTEGER);
   package P_SQLUSI is new System.Address_To_Access_Conversions (SQLUSMALLINT);
   package P_SQLSI  is new System.Address_To_Access_Conversions (SQLSMALLINT);

   function To_Address (P : SQLPOINTER) return System.Address is
   begin
      return System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (P));
   end To_Address;

   function To_SQLPOINTER (S : System.Address) return SQLPOINTER is
   begin
      return To_Integer (S);
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLCHAR) return SQLPOINTER is
      use P_SQLCHAR;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLTCHAR) return SQLPOINTER is
      use P_SQLTCHAR;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLUINTEGER) return SQLPOINTER is
      use P_SQLUI;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLINTEGER) return SQLPOINTER is
      use P_SQLI;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLUSMALLINT) return SQLPOINTER is
      use P_SQLUSI;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : PTR_SQLSMALLINT) return SQLPOINTER is
      use P_SQLSI;
   begin
      return To_Integer (To_Address (Object_Pointer (S)));
   end To_SQLPOINTER;


   function To_PTR_SQLCHAR (S : System.Address) return PTR_SQLCHAR is
   begin
      return PTR_SQLCHAR (P_SQLCHAR.To_Pointer (S));
   end To_PTR_SQLCHAR;

   function To_PTR_SQLTCHAR (S : System.Address) return PTR_SQLTCHAR is
   begin
      return PTR_SQLTCHAR (P_SQLTCHAR.To_Pointer (S));
   end To_PTR_SQLTCHAR;

   function To_PTR_SQLCHAR (S : access String) return PTR_SQLCHAR is
   begin
      return To_PTR_SQLCHAR (S.all'Address);
   end To_PTR_SQLCHAR;

   function To_PTR_SQLTCHAR (S : access Wide_String) return PTR_SQLTCHAR is
   begin
      return To_PTR_SQLTCHAR (S.all'Address);
   end To_PTR_SQLTCHAR;

   function To_SQLPOINTER (S : access String) return SQLPOINTER is
      use P_SQLCHAR;
   begin
      return To_Integer (To_Address (Object_Pointer (To_PTR_SQLCHAR (S))));
   end To_SQLPOINTER;

   function To_SQLPOINTER (S : access Wide_String) return SQLPOINTER is
      use P_SQLTCHAR;
   begin
      return To_Integer (To_Address (Object_Pointer (To_PTR_SQLTCHAR (S))));
   end To_SQLPOINTER;

   function Null_Handle return SQLHANDLE is
   begin
      return SQL_NULL_HANDLE;
   end Null_Handle;

   function To_Ada (State : C_SQLSTATE) return SQLSTATE is
      use Ada.Strings.Fixed;
      S : constant String := Interfaces.C.To_Ada (State, False);
      R : SQLSTATE;
   begin
      Move (Source => S, Target => R);
      SQLFixNTS (R);
      return R;
   end To_Ada;

   function Length_Indicator (Size : Natural) return Integer is
   begin
      if Size = SQLINTEGER'Size then
         return Integer (SQL_IS_INTEGER);
      elsif Size = SQLSMALLINT'Size then
         return Integer (SQL_IS_SMALLINT);
      else
         return 0;
      end if;
   end Length_Indicator;

   function SQL_Error_Message (HandleType  : SQL_HANDLE_TYPE;
                               Handle      : SQLHSTMT;
                               State       : access SQLSTATE)
                               return String
   is
      use Ada.Strings.Fixed;
      function GetDiagField (HandleType     : SQL_HANDLE_TYPE;
                             Handle         : SQLHANDLE;
                             RecNumber      : SQLSMALLINT;
                             DiagIdentifier : SQLSMALLINT;
                             DiagInfo       : SQLPOINTER;
                             BufferLength   : SQLSMALLINT;
                             pStringLength  : access SQLSMALLINT)
                             return SQLRETURN;
      pragma Import (Stdcall, GetDiagField, "SQLGetDiagField");

      function GetDiagRec (HandleType   : SQL_HANDLE_TYPE;
                           Handle       : SQLHANDLE;
                           RecNumber    : SQLSMALLINT;
                           pSqlstate    : access C_SQLSTATE;
                           pNativeError : access SQLINTEGER;
                           pMessageText : PTR_SQLCHAR;
                           BufferLength : SQLSMALLINT;
                           pTextLength  : access SQLSMALLINT)
                           return SQLRETURN;
      pragma Import (Stdcall, GetDiagRec, "SQLGetDiagRec");

      V_SQL_DIAG_SERVER_NAME : constant := 11;
      RNr        : constant SQLSMALLINT := 1;
      Err_Native : aliased SQLINTEGER;
      Len        : aliased SQLSMALLINT := SQL_MAX_MESSAGE_SIZE;
      D_Len      : aliased SQLSMALLINT := SQL_MAX_MESSAGE_SIZE;

      Msg        : String (1 .. SQL_MAX_MESSAGE_SIZE)
        := SQL_MAX_MESSAGE_SIZE * ' ';
      DiagMsg    : String (1 .. SQL_MAX_MESSAGE_SIZE) := Msg;

      C_State    : aliased C_SQLSTATE;
      RC         : SQLRETURN;
   begin
      -- bug: #437590, MySQL 3.23, MS ODBC Driver Manager 3.510
      --   if the GetDiagRec failes the State field is filled with
      --   junk, which causes SQL Error conditions to be triggered.
      --   In order to by pass this bug in the MySQL/Windows/DM
      --   configuration we reset the state field to empty = success class.
      --
      RC := GetDiagRec (HandleType, Handle, RNr, C_State'Access,
                        Err_Native'Access,
                        To_PTR_SQLCHAR (Msg'Address),
                        Len, Len'Access);
      if not Is_SQL_Ok (RC) then
         State.all := EMPTY_SQLSTATE;
         Len := 0;
      else
         State.all := GNU.DB.SQLCLI.To_Ada (C_State);
         if Len > SQL_MAX_MESSAGE_SIZE then
            Len := SQL_MAX_MESSAGE_SIZE;
         end if;
      end if;

      RC := GetDiagField (HandleType, Handle, RNr,
                          V_SQL_DIAG_SERVER_NAME,
                          To_SQLPOINTER (DiagMsg'Address),
                          D_Len, D_Len'Access);
      if not Is_SQL_Ok (RC) then
         D_Len := 0;
      end if;
      return
        "[Server=" & DiagMsg (1 .. Integer (D_Len)) &
        "][State=" & State.all & "]" & Msg (1 .. Integer (Len));
   end SQL_Error_Message;

   procedure Raise_SQL_Error
     (ProcedureName : in String;
      ErrorMessage  : in String    := "";
      RC            : in SQLRETURN := SQL_ADA95_BINDING_ERROR;
      State         : in SQLSTATE  := EMPTY_SQLSTATE)
      is separate;

   function Is_SQL_Ok (ErrorCode : SQLRETURN) return Boolean is
   begin
      if ErrorCode = SQL_SUCCESS or else ErrorCode = SQL_SUCCESS_WITH_INFO then
         return True;
      else
         return False;
      end if;
   end Is_SQL_Ok;

   procedure Check_SQL_Error
     (RC                : in SQLRETURN;
      ProcedureName     : in String;
      ErrorMessage      : in String := "";
      HandleType        : in SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
      Handle            : in SQLHANDLE := SQL_NULL_HANDLE)
   is
   begin
      if not Is_SQL_Ok (RC) then
         if RC = SQL_ERROR then
            declare
               State : aliased SQLSTATE;
               Msg   : constant String :=
                 ErrorMessage & SQL_Error_Message (HandleType,
                                                   Handle,
                                                   State'Access);
            begin
               Raise_SQL_Error (ProcedureName, Msg, RC, State);
            end;
         else
            Raise_SQL_Error (ProcedureName,
                             ErrorMessage,
                             RC);
         end if;
      end if;
   end Check_SQL_Error;


   function SQLAllocHandle (HandleType   : in  SQL_HANDLE_TYPE;
                            InputHandle  : in  SQLHANDLE;
                            OutputHandle : access SQLHANDLE)
                           return SQLRETURN is
      function AllocHandle (HandleType    : SQL_HANDLE_TYPE;
                            InputHandle   : SQLHANDLE;
                            pOutputHandle : access SQLHANDLE)
                           return SQLRETURN;
      pragma Import (Stdcall, AllocHandle, "SQLAllocHandle");
   begin
      return AllocHandle (HandleType, InputHandle, OutputHandle);
   end SQLAllocHandle;

   procedure SQLAllocHandle (HandleType   : in  SQL_HANDLE_TYPE;
                             InputHandle  : in  SQLHANDLE;
                             OutputHandle : out SQLHANDLE) is
      H_Out : aliased SQLHANDLE := SQL_NULL_HANDLE;
      RC    : constant SQLRETURN := SQLAllocHandle (HandleType,
                                                    InputHandle,
                                                    H_Out'Access);
      Error_Handle_Type : SQL_HANDLE_TYPE;
   begin
      if HandleType /= SQL_HANDLE_TYPE'First then
         Error_Handle_Type := SQL_HANDLE_TYPE'Pred (HandleType);
      else
         Error_Handle_Type := SQL_HANDLE_TYPE'First;
      end if;
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLAllocHandle",
                       HandleType    => Error_Handle_Type,
                       Handle        => InputHandle);
      OutputHandle := H_Out;
   end SQLAllocHandle;

   function SQLFreeHandle (HandleType : SQL_HANDLE_TYPE;
                           Handle     : SQLHANDLE)
                          return SQLRETURN is
      function FreeHandle (HandleType : SQL_HANDLE_TYPE;
                           Handle     : SQLHANDLE)
                          return SQLRETURN;
      pragma Import (Stdcall, FreeHandle, "SQLFreeHandle");
   begin
      return FreeHandle (HandleType, Handle);
   end SQLFreeHandle;

   procedure SQLFreeHandle (HandleType : in SQL_HANDLE_TYPE;
                            Handle     : in out SQLHANDLE)
   is
      RC : constant SQLRETURN := SQLFreeHandle (HandleType, Handle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLFreeHandle",
                       HandleType    => HandleType,
                       Handle        => Handle);
      Handle := SQL_NULL_HANDLE;
   end SQLFreeHandle;

   function SQLFreeStmt (StatementHandle : SQLHSTMT;
                         Option          : FreeStmt_Option)
                        return SQLRETURN is
      function FreeStmt (StatementHandle : SQLHSTMT;
                         Option          : FreeStmt_Option)
                        return SQLRETURN;
      pragma Import (Stdcall, FreeStmt, "SQLFreeStmt");
   begin
      return FreeStmt (StatementHandle, Option);
   end SQLFreeStmt;

   procedure SQLFreeStmt (StatementHandle   : in out SQLHSTMT;
                          Option            : in FreeStmt_Option := SQL_CLOSE)
   is
      RC : constant SQLRETURN := SQLFreeStmt (StatementHandle, Option);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLFreeStmt",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      if Option = SQL_DROP then
         StatementHandle := SQL_NULL_HSTMT;
      end if;
   end SQLFreeStmt;

   function SQLConnect (ConnectionHandle  : in SQLHDBC;
                        ServerName        : in String;
                        UserName          : in String;
                        Authentication    : in String)
                       return SQLRETURN is
      function Connect (ConnectionHandle  : SQLHDBC;
                        pServerName       : PTR_SQLCHAR;
                        NameLength1       : SQLSMALLINT;
                        pUserName         : PTR_SQLCHAR;
                        NameLength2       : SQLSMALLINT;
                        pAuthentication   : PTR_SQLCHAR;
                        NameLength3       : SQLSMALLINT)
                       return SQLRETURN;
      pragma Import (Stdcall, Connect, "SQLConnect");
   begin
      return Connect (ConnectionHandle,
                      To_PTR_SQLCHAR (ServerName'Address),
                      SQLSMALLINT (ServerName'Length),
                      To_PTR_SQLCHAR (UserName'Address),
                      SQLSMALLINT (UserName'Length),
                      To_PTR_SQLCHAR (Authentication'Address),
                      SQLSMALLINT (Authentication'Length));
   end SQLConnect;

   procedure SQLConnect (ConnectionHandle  : in SQLHDBC;
                         ServerName        : in String;
                         UserName          : in String;
                         Authentication    : in String)
   is
      RC : constant SQLRETURN := SQLConnect (ConnectionHandle,
                                             ServerName,
                                             UserName,
                                             Authentication);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLConnect;

   function SQLConnect (ConnectionHandle  : in SQLHDBC;
                        ServerName        : in Wide_String;
                        UserName          : in Wide_String;
                        Authentication    : in Wide_String)
                       return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLConnect;

   procedure SQLConnect (ConnectionHandle  : in SQLHDBC;
                         ServerName        : in Wide_String;
                         UserName          : in Wide_String;
                         Authentication    : in Wide_String)
   is
      RC : constant SQLRETURN := SQLConnect (ConnectionHandle,
                                             ServerName,
                                             UserName,
                                             Authentication);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLConnect;

   function SQLDisconnect (ConnectionHandle : SQLHDBC) return SQLRETURN is
      function Disconnect (ConnectionHandle : SQLHDBC) return SQLRETURN;
      pragma Import (Stdcall, Disconnect, "SQLDisconnect");
   begin
      return Disconnect (ConnectionHandle);
   end SQLDisconnect;

   procedure SQLDisconnect (ConnectionHandle : in SQLHDBC) is
      RC : constant SQLRETURN := SQLDisconnect (ConnectionHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDisconnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLDisconnect;

   function SQLGetTypeInfo (StatementHandle : SQLHSTMT;
                            DataType        : SQL_DATA_TYPE)
                           return SQLRETURN is
      function GetTypeInfo (StatementHandle : SQLHSTMT;
                            DataType        : SQL_DATA_TYPE)
                           return SQLRETURN;
      pragma Import (Stdcall, GetTypeInfo, "SQLGetTypeInfo");
   begin
      if Unicode_Attr_Flag then
         return SQL_ADA95_NO_UNICODE;
      else
         return GetTypeInfo (StatementHandle, DataType);
      end if;
   end SQLGetTypeInfo;

   procedure SQLGetTypeInfo (StatementHandle : in SQLHSTMT;
                             DataType        : in SQL_DATA_TYPE) is
      RC : constant SQLRETURN := SQLGetTypeInfo (StatementHandle, DataType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetTypeInfo",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLGetTypeInfo;

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out String;
      Description       : out String;
      ErrorCode         : out SQLRETURN) is

      use Ada.Strings;
      use Ada.Strings.Fixed;

      function DataSources (EnvironmentHandle : SQLHENV;
                            Direction         : SQL_EXTENDED_FETCH_DIRECTION;
                            pServerName       : PTR_SQLCHAR;
                            BufferLength1     : SQLSMALLINT;
                            pNameLength1      : access SQLSMALLINT;
                            pDescription      : PTR_SQLCHAR;
                            BufferLength2     : SQLSMALLINT;
                            pNameLength2      : access SQLSMALLINT)
                           return SQLRETURN;
      pragma Import (Stdcall, DataSources, "SQLDataSources");

      pragma Warnings (Off);
      Str1 : aliased String (1 .. (1 + SQL_MAX_DSN_LENGTH));
      Str2 : aliased String (1 .. 256);
      pragma Warnings (On);
      Len1 : aliased SQLSMALLINT := SQLSMALLINT (ServerName'Length);
      Len2 : aliased SQLSMALLINT := SQLSMALLINT (Str2'Length);
      RC   : constant SQLRETURN :=
        DataSources (EnvironmentHandle, Direction,
                     To_PTR_SQLCHAR (Str1'Address), Len1, Len1'Access,
                     To_PTR_SQLCHAR (Str2'Address), Len2, Len2'Access);
   begin
      ErrorCode := RC;
      if Is_SQL_Ok (RC) then
         Move (Source => Str1 (1 .. Integer (Len1)),
               Target => ServerName);
         Move (Source => Str2 (1 .. Integer (Len2)),
               Target => Description,
               Drop => Right);
      end if;
   end SQLDataSources;

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out String;
      Description       : out String) is

      RC : SQLRETURN;
   begin
      SQLDataSources (EnvironmentHandle,
                      Direction,
                      ServerName,
                      Description,
                      RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDataSources",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
   end SQLDataSources;

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out Wide_String;
      Description       : out Wide_String;
      ErrorCode         : out SQLRETURN) is

      use Ada.Strings;
      use Ada.Strings.Wide_Fixed;
   begin
      ErrorCode   := SQL_ADA95_NO_UNICODE;
      ServerName  := ServerName'Length * Wide_Character'(' ');
      Description := Description'Length * Wide_Character'(' ');
   end SQLDataSources;

   procedure SQLDataSources
     (EnvironmentHandle : in SQLHENV;
      Direction         : in SQL_EXTENDED_FETCH_DIRECTION;
      ServerName        : out Wide_String;
      Description       : out Wide_String) is

      RC : SQLRETURN;
   begin
      SQLDataSources (EnvironmentHandle,
                      Direction,
                      ServerName,
                      Description,
                      RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDataSources",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
   end SQLDataSources;

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out String;
                         DriverAttributes    : out String;
                         RC                  : out SQLRETURN)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      function Drivers (EnvironmentHandle : SQLHENV;
                        Direction         : SIMPLE_FETCH_DIRECTION;
                        pDesc             : PTR_SQLCHAR;
                        Len1              : SQLSMALLINT;
                        pLen1             : access SQLSMALLINT;
                        pAttr             : PTR_SQLCHAR;
                        Len2              : SQLSMALLINT;
                        pLen2             : access SQLSMALLINT)
                        return SQLRETURN;
      pragma Import (Stdcall, Drivers, "SQLDrivers");

      pragma Warnings (Off);
      Str1 : aliased String (1 .. 256);
      Str2 : aliased String (1 .. 256);
      pragma Warnings (On);
      Len1 : aliased SQLSMALLINT := SQLSMALLINT (DriverDescription'Length);
      Len2 : aliased SQLSMALLINT := SQLSMALLINT (DriverAttributes'Length);
      Err  : constant SQLRETURN :=
        Drivers (EnvironmentHandle, Direction,
                 To_PTR_SQLCHAR (Str1'Address), Len1, Len1'Access,
                 To_PTR_SQLCHAR (Str2'Address), Len2, Len2'Access);
   begin
      RC := Err;
      if Is_SQL_Ok (Err) then
         Move (Source => Str1 (1 .. Integer (Len1)),
               Target => DriverDescription);
         Move (Source => Str2 (1 .. Integer (Len2)),
               Target => DriverAttributes,
               Drop   => Right);
      end if;
   end SQLDrivers;

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out String;
                         DriverAttributes    : out String)
   is
      RC : SQLRETURN;
   begin
      SQLDrivers (EnvironmentHandle, Direction, DriverDescription,
                  DriverAttributes, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDrivers",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
   end SQLDrivers;


   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out Wide_String;
                         DriverAttributes    : out Wide_String;
                         RC                  : out SQLRETURN)
   is

      use Ada.Strings;
      use Ada.Strings.Wide_Fixed;
   begin
      RC   := SQL_ADA95_NO_UNICODE;
      DriverDescription  := DriverDescription'Length * Wide_Character'(' ');
      DriverAttributes   := DriverAttributes'Length * Wide_Character'(' ');
   end SQLDrivers;

   procedure SQLDrivers (EnvironmentHandle   : in  SQLHENV;
                         Direction           : in  SIMPLE_FETCH_DIRECTION;
                         DriverDescription   : out Wide_String;
                         DriverAttributes    : out Wide_String)
   is
      RC : SQLRETURN;
   begin
      SQLDrivers (EnvironmentHandle, Direction, DriverDescription,
                  DriverAttributes, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDrivers",
                       HandleType    => SQL_HANDLE_ENV,
                       Handle        => EnvironmentHandle);
   end SQLDrivers;

   function SQLBindCol (StatementHandle : SQLHSTMT;
                        ColumnNumber    : SQL_Column_Number;
                        TargetType      : SQL_C_DATA_TYPE;
                        TargetValue     : SQLPOINTER;
                        BufferLength    : SQLINTEGER;
                        pStrLen_or_Ind  : access SQLINTEGER)
                       return SQLRETURN is
      function BindCol (StatementHandle : SQLHSTMT;
                        ColumnNumber    : SQL_Column_Number;
                        TargetType      : SQL_C_DATA_TYPE;
                        TargetValue     : SQLPOINTER;
                        BufferLength    : SQLINTEGER;
                        pStrLen_or_Ind  : access SQLINTEGER)
                       return SQLRETURN;
      pragma Import (Stdcall, BindCol, "SQLBindCol");
   begin
      return BindCol (StatementHandle,
                      ColumnNumber,
                      TargetType,
                      TargetValue,
                      BufferLength,
                      pStrLen_or_Ind);
   end SQLBindCol;

   procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                         ColumnNumber     : in SQL_Column_Number;
                         TargetType       : in SQL_C_DATA_TYPE;
                         TargetValuePtr   : in SQLPOINTER;
                         BufferLength     : in SQLINTEGER;
                         StrLen_Or_IndPtr : access SQLINTEGER) is
      RC  : constant SQLRETURN := SQLBindCol (StatementHandle,
                                              ColumnNumber,
                                              TargetType,
                                              TargetValuePtr,
                                              BufferLength,
                                              StrLen_Or_IndPtr);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBindCol",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLBindCol;


   function SQLBindCol (StatementHandle  : SQLHSTMT;
                        ColumnNumber     : SQL_Column_Number;
                        TargetValue      : access String;
                        StrLen_Or_IndPtr : access SQLINTEGER)
                       return SQLRETURN
   is
   begin
      return SQLBindCol (StatementHandle,
                         ColumnNumber,
                         SQL_C_CHAR,
                         To_SQLPOINTER (TargetValue),
                         SQLINTEGER (TargetValue.all'Length),
                         StrLen_Or_IndPtr);
   end SQLBindCol;

   procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                         ColumnNumber     : in SQL_Column_Number;
                         TargetValue      : access String;
                         StrLen_Or_IndPtr : access SQLINTEGER)
   is
      RC : constant SQLRETURN := SQLBindCol (StatementHandle,
                                             ColumnNumber,
                                             TargetValue,
                                             StrLen_Or_IndPtr);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBindCol",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLBindCol;

   package body IntegerBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Int;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN
      is
         function BindCol (StatementHandle : SQLHSTMT;
                           ColumnNumber    : SQL_Column_Number;
                           TargetType      : SQL_C_DATA_TYPE;
                           TargetValue     : access Int;
                           BufferLength    : SQLINTEGER;
                           pStrLen_or_Ind  : access SQLINTEGER)
                          return SQLRETURN;
         pragma Import (Stdcall, BindCol, "SQLBindCol");
         DT  : SQL_C_DATA_TYPE;
      begin
         if Int'Size = SQLTINYINT'Size then
            DT := SQL_C_TINYINT;
         elsif Int'Size = SQLSMALLINT'Size then
            DT := SQL_C_SHORT;
         elsif Int'Size = SQLINTEGER'Size then
            DT := SQL_C_LONG;
         elsif Int'Size = SQLBIGINT'Size then
            DT := SQL_C_SBIGINT;
         else
            raise Constraint_Error;
         end if;
         return BindCol (StatementHandle,
                         ColumnNumber,
                         DT,
                         TargetValue,
                         SQLINTEGER (Int'Size / 8),
                         IndPtr);
      end SQLBindCol;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Int;
                            IndPtr           : access SQLINTEGER)
      is
         RC : constant SQLRETURN := SQLBindCol (StatementHandle,
                                                ColumnNumber,
                                                TargetValue,
                                                IndPtr);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindCol",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindCol;

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Int;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                                return SQLRETURN
      is
         C_DataType   : SQL_C_DATA_TYPE;
         SQL_DataType : SQL_DATA_TYPE;
         ColumnSize   : SQLUINTEGER;
      begin
         if Int'Size = SQLTINYINT'Size then
            C_DataType   := SQL_C_TINYINT;
            SQL_DataType := SQL_TINYINT;
            ColumnSize   := 3;
         elsif Int'Size = SQLSMALLINT'Size then
            C_DataType   := SQL_C_SHORT;
            SQL_DataType := SQL_SMALLINT;
            ColumnSize   := 5;
         elsif Int'Size = SQLINTEGER'Size then
            C_DataType   := SQL_C_LONG;
            SQL_DataType := SQL_INTEGER;
            ColumnSize   := 10;
         elsif Int'Size = SQLBIGINT'Size then
            C_DataType   := SQL_C_SBIGINT;
            SQL_DataType := SQL_BIGINT;
            ColumnSize   := 19;
         else
            raise Constraint_Error;
         end if;
         return SQLBindParameter (StatementHandle,
                                  ParameterNumber,
                                  InputOutputType,
                                  ValueType        => C_DataType,
                                  ParameterType    => SQL_DataType,
                                  ColumnSize       => ColumnSize,
                                  DecimalDigits    => 0,
                                  Value            => To_SQLPOINTER (Value.all'Address),
                                  BufferLength     => 0,
                                  StrLen_Or_IndPtr => Indicator);

      end SQLBindParameter;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Int;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
      is
         RC : constant SQLRETURN :=
           SQLBindParameter (StatementHandle, ParameterNumber, Value, Indicator, InputOutputType);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindParameter",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindParameter;

   end IntegerBinding;


   package body UnsignedBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Unsigned;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN
      is
         function BindCol (StatementHandle : SQLHSTMT;
                           ColumnNumber    : SQL_Column_Number;
                           TargetType      : SQL_C_DATA_TYPE;
                           TargetValue     : access Unsigned;
                           BufferLength    : SQLINTEGER;
                           pStrLen_or_Ind  : access SQLINTEGER)
                          return SQLRETURN;
         pragma Import (Stdcall, BindCol, "SQLBindCol");
         DT  : SQL_C_DATA_TYPE;
      begin
         if Unsigned'Size = SQLTINYINT'Size then
            DT := SQL_C_UTINYINT;
         elsif Unsigned'Size = SQLSMALLINT'Size then
            DT := SQL_C_USHORT;
         elsif Unsigned'Size = SQLINTEGER'Size then
            DT := SQL_C_ULONG;
         elsif Unsigned'Size = SQLBIGINT'Size then
            DT := SQL_C_UBIGINT;
         else
            raise Constraint_Error;
         end if;
         return BindCol (StatementHandle,
                         ColumnNumber,
                         DT,
                         TargetValue,
                         SQLINTEGER (Unsigned'Size / 8),
                         IndPtr);

      end SQLBindCol;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Unsigned;
                            IndPtr           : access SQLINTEGER)
      is
         RC : constant SQLRETURN := SQLBindCol (StatementHandle,
                                                ColumnNumber,
                                                TargetValue,
                                                IndPtr);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindCol",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);

      end SQLBindCol;

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Unsigned;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                                return SQLRETURN
      is
         C_DataType   : SQL_C_DATA_TYPE;
         SQL_DataType : SQL_DATA_TYPE;
         ColumnSize   : SQLUINTEGER;
      begin
         if Unsigned'Size = SQLTINYINT'Size then
            C_DataType   := SQL_C_UTINYINT;
            SQL_DataType := SQL_TINYINT;
            ColumnSize   := 3;
         elsif Unsigned'Size = SQLSMALLINT'Size then
            C_DataType   := SQL_C_USHORT;
            SQL_DataType := SQL_SMALLINT;
            ColumnSize   := 5;
         elsif Unsigned'Size = SQLINTEGER'Size then
            C_DataType   := SQL_C_ULONG;
            SQL_DataType := SQL_INTEGER;
            ColumnSize   := 10;
         elsif Unsigned'Size = SQLBIGINT'Size then
            C_DataType   := SQL_C_UBIGINT;
            SQL_DataType := SQL_BIGINT;
            ColumnSize   := 20;
         else
            raise Constraint_Error;
         end if;
         return SQLBindParameter (StatementHandle,
                                  ParameterNumber,
                                  InputOutputType,
                                  ValueType        => C_DataType,
                                  ParameterType    => SQL_DataType,
                                  ColumnSize       => ColumnSize,
                                  DecimalDigits    => 0,
                                  Value            => To_SQLPOINTER (Value.all'Address),
                                  BufferLength     => 0,
                                  StrLen_Or_IndPtr => Indicator);

      end SQLBindParameter;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Unsigned;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
      is
         RC : constant SQLRETURN :=
           SQLBindParameter (StatementHandle, ParameterNumber, Value, Indicator, InputOutputType);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindParameter",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindParameter;

   end UnsignedBinding;

   package body FloatBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Flt;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN
      is
         function BindCol (StatementHandle : SQLHSTMT;
                           ColumnNumber    : SQL_Column_Number;
                           TargetType      : SQL_C_DATA_TYPE;
                           TargetValue     : access Flt;
                           BufferLength    : SQLINTEGER;
                           pStrLen_or_Ind  : access SQLINTEGER)
                          return SQLRETURN;
         pragma Import (Stdcall, BindCol, "SQLBindCol");
         DT  : SQL_C_DATA_TYPE;
      begin
         if Flt'Size = SQLDOUBLE'Size then
            DT := SQL_C_DOUBLE;
         elsif Flt'Size = SQLREAL'Size then
            DT := SQL_C_FLOAT;
         else
            raise Constraint_Error;
         end if;
         return BindCol (StatementHandle,
                         ColumnNumber,
                         DT,
                         TargetValue,
                         SQLINTEGER (Flt'Size / 8),
                         IndPtr);
      end SQLBindCol;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Flt;
                            IndPtr           : access SQLINTEGER)
      is
         RC : constant SQLRETURN := SQLBindCol (StatementHandle,
                                                ColumnNumber,
                                                TargetValue,
                                                IndPtr);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindCol",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindCol;

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Flt;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                                return SQLRETURN
      is
         C_DataType   : SQL_C_DATA_TYPE;
         SQL_DataType : SQL_DATA_TYPE;
      begin
         if Flt'Size = SQLDOUBLE'Size then
            C_DataType   := SQL_C_DOUBLE;
            SQL_DataType := SQL_DOUBLE;
         elsif Flt'Size = SQLREAL'Size then
            C_DataType   := SQL_C_FLOAT;
            SQL_DataType := SQL_FLOAT;
         else
            raise Constraint_Error;
         end if;
         return SQLBindParameter (StatementHandle,
                                  ParameterNumber,
                                  InputOutputType,
                                  ValueType        => C_DataType,
                                  ParameterType    => SQL_DataType,
                                  ColumnSize       => 15,
                                  DecimalDigits    => 0,
                                  Value            => To_SQLPOINTER (Value.all'Address),
                                  BufferLength     => 0,
                                  StrLen_Or_IndPtr => Indicator);

      end SQLBindParameter;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Flt;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
      is
         RC : constant SQLRETURN :=
           SQLBindParameter (StatementHandle, ParameterNumber, Value, Indicator, InputOutputType);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindParameter",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindParameter;

   end FloatBinding;

   package body EnumBinding is
      function SQLBindCol (StatementHandle  : in SQLHSTMT;
                           ColumnNumber     : in SQL_Column_Number;
                           TargetValue      : access Enum;
                           IndPtr           : access SQLINTEGER)
                          return SQLRETURN
      is
         function BindCol (StatementHandle : SQLHSTMT;
                           ColumnNumber    : SQL_Column_Number;
                           TargetType      : SQL_C_DATA_TYPE;
                           TargetValue     : access Enum;
                           BufferLength    : SQLINTEGER;
                           pStrLen_or_Ind  : access SQLINTEGER)
                          return SQLRETURN;
         pragma Import (Stdcall, BindCol, "SQLBindCol");
         DT  : SQL_C_DATA_TYPE;
      begin
         if Enum'Size = SQLTINYINT'Size then
            DT := SQL_C_TINYINT;
         elsif Enum'Size = SQLSMALLINT'Size then
            DT := SQL_C_SHORT;
         elsif Enum'Size = SQLINTEGER'Size then
            DT := SQL_C_LONG;
         elsif Enum'Size = SQLBIGINT'Size then
            DT := SQL_C_SBIGINT;
         else
            raise Constraint_Error;
         end if;
         return BindCol (StatementHandle,
                         ColumnNumber,
                         DT,
                         TargetValue,
                         SQLINTEGER (Enum'Size / 8),
                         IndPtr);
      end SQLBindCol;

      procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                            ColumnNumber     : in SQL_Column_Number;
                            TargetValue      : access Enum;
                            IndPtr           : access SQLINTEGER)
      is
         RC : constant SQLRETURN := SQLBindCol (StatementHandle,
                                                ColumnNumber,
                                                TargetValue,
                                                IndPtr);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindCol",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindCol;

      function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                 ParameterNumber : in     SQL_Parameter_Number;
                                 Value           : access Enum;
                                 Indicator       : access SQLINTEGER;
                                 InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                                return SQLRETURN
      is
         C_DataType   : SQL_C_DATA_TYPE;
         SQL_DataType : SQL_DATA_TYPE;
         ColumnSize   : SQLUINTEGER;
      begin
         if Enum'Size = SQLTINYINT'Size then
            C_DataType   := SQL_C_UTINYINT;
            SQL_DataType := SQL_TINYINT;
            ColumnSize   := 3;
         elsif Enum'Size = SQLSMALLINT'Size then
            C_DataType   := SQL_C_USHORT;
            SQL_DataType := SQL_SMALLINT;
            ColumnSize   := 5;
         elsif Enum'Size = SQLINTEGER'Size then
            C_DataType   := SQL_C_ULONG;
            SQL_DataType := SQL_INTEGER;
            ColumnSize   := 10;
         elsif Enum'Size = SQLBIGINT'Size then
            C_DataType   := SQL_C_UBIGINT;
            SQL_DataType := SQL_BIGINT;
            ColumnSize   := 20;
         else
            raise Constraint_Error;
         end if;
         return SQLBindParameter (StatementHandle,
                                  ParameterNumber,
                                  InputOutputType,
                                  ValueType        => C_DataType,
                                  ParameterType    => SQL_DataType,
                                  ColumnSize       => ColumnSize,
                                  DecimalDigits    => 0,
                                  Value            => To_SQLPOINTER (Value.all'Address),
                                  BufferLength     => 0,
                                  StrLen_Or_IndPtr => Indicator);

      end SQLBindParameter;

      procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                                  ParameterNumber : in     SQL_Parameter_Number;
                                  Value           : access Enum;
                                  Indicator       : access SQLINTEGER;
                                  InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
      is
         RC : constant SQLRETURN :=
           SQLBindParameter (StatementHandle, ParameterNumber, Value, Indicator, InputOutputType);
      begin
         Check_SQL_Error (RC            => RC,
                          ProcedureName => "SQLBindParameter",
                          HandleType    => SQL_HANDLE_STMT,
                          Handle        => StatementHandle);
      end SQLBindParameter;

   end EnumBinding;

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
                             return SQLRETURN is
      function BindParameter (StatementHandle  : in SQLHSTMT;
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
      pragma Import (Stdcall, BindParameter, "SQLBindParameter");
   begin
      return BindParameter (StatementHandle,
                            ParameterNumber,
                            InputOutputType,
                            ValueType,
                            ParameterType,
                            ColumnSize,
                            DecimalDigits,
                            Value,
                            BufferLength,
                            StrLen_Or_IndPtr);
   end SQLBindParameter;

   procedure SQLBindParameter (StatementHandle  : in SQLHSTMT;
                               ParameterNumber  : in SQL_Parameter_Number;
                               InputOutputType  : in SQL_Parameter_Type;
                               ValueType        : in SQL_C_DATA_TYPE;
                               ParameterType    : in SQL_DATA_TYPE;
                               ColumnSize       : in SQLUINTEGER;
                               DecimalDigits    : in SQLSMALLINT;
                               Value            : in SQLPOINTER;
                               BufferLength     : in SQLINTEGER;
                               StrLen_Or_IndPtr : access SQLINTEGER) is
      RC : constant SQLRETURN := SQLBindParameter (StatementHandle,
                                                   ParameterNumber,
                                                   InputOutputType,
                                                   ValueType,
                                                   ParameterType,
                                                   ColumnSize,
                                                   DecimalDigits,
                                                   Value,
                                                   BufferLength,
                                                   StrLen_Or_IndPtr);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBindParameter",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLBindParameter;

   function SQLBindParameter (StatementHandle : in     SQLHSTMT;
                              ParameterNumber : in     SQL_Parameter_Number;
                              Value           : access String;
                              Length          : access SQLINTEGER;
                              InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
                             return SQLRETURN
   is begin
      return SQLBindParameter (StatementHandle,
                               ParameterNumber,
                               InputOutputType,
                               ValueType        => SQL_C_CHAR,
                               ParameterType    => SQL_VARCHAR,
                               ColumnSize       => 0,
                               DecimalDigits    => 0,
                               Value            => To_SQLPOINTER (Value),
                               BufferLength     => Value.all'Length,
                               StrLen_Or_IndPtr => Length);

   end SQLBindParameter;

   procedure SQLBindParameter (StatementHandle : in     SQLHSTMT;
                               ParameterNumber : in     SQL_Parameter_Number;
                               Value           : access String;
                               Length          : access SQLINTEGER;
                               InputOutputType : in     SQL_Parameter_Type   := SQL_PARAM_INPUT)
   is
      RC : constant SQLRETURN := SQLBindParameter (StatementHandle,
                                                   ParameterNumber,
                                                   Value,
                                                   Length,
                                                   InputOutputType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBindParameter",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLBindParameter;


   procedure SQLDescribeParam (StatementHandle : in  SQLHSTMT;
                               ParameterNumber : in  SQL_Parameter_Number;
                               ParameterType   : out SQL_DATA_TYPE;
                               ColumnSize      : out SQLUINTEGER;
                               DecimalDigits   : out SQLSMALLINT;
                               Nullable        : out SQL_NULLABLE_INFO;
                               RC              : out SQLRETURN)
   is
      ProcName : constant String := "SQLDescribeParam";
      function DescribeParam (StatementHandle  : SQLHSTMT;
                              ParameterNumber  : SQL_Parameter_Number;
                              ParameterType    : access SQL_DATA_TYPE;
                              ColumnSize       : access SQLUINTEGER;
                              DecimalDigits    : access SQLSMALLINT;
                              Nullable         : access SQL_NULLABLE_INFO)
                             return SQLRETURN;
      pragma Import (Stdcall, DescribeParam, "SQLDescribeParam");

      function SDT2Int is new
        Ada.Unchecked_Conversion (SQL_DATA_TYPE, SQLSMALLINT);
      function NU2Int is new
        Ada.Unchecked_Conversion (SQL_NULLABLE_INFO, SQLSMALLINT);

      DT : aliased SQL_DATA_TYPE;
      CS : aliased SQLUINTEGER;
      DD : aliased SQLSMALLINT;
      NU : aliased SQL_NULLABLE_INFO;
      EC : constant SQLRETURN := DescribeParam (StatementHandle,
                                                ParameterNumber,
                                                DT'Access,
                                                CS'Access,
                                                DD'Access,
                                                NU'Access);
   begin
      RC := EC;
      if Is_SQL_Ok (EC) then
         ColumnSize := CS;
         DecimalDigits := DD;
         if DT'Valid then
            ParameterType := DT;
         else Raise_Invalid_Enum (ProcName, "SQL_DATA_TYPE",
                                SQLSMALLINT'Image (SDT2Int (DT)));
         end if;
         if NU'Valid then
            Nullable := NU;
         else
            Raise_Invalid_Enum (ProcName, "SQL_NULLABLE_INFO",
                                SQLSMALLINT'Image (NU2Int (NU)));
         end if;
      end if;
   end SQLDescribeParam;

   procedure SQLDescribeParam (StatementHandle : in  SQLHSTMT;
                               ParameterNumber : in  SQL_Parameter_Number;
                               ParameterType   : out SQL_DATA_TYPE;
                               ColumnSize      : out SQLUINTEGER;
                               DecimalDigits   : out SQLSMALLINT;
                               Nullable        : out SQL_NULLABLE_INFO)
   is
      RC : SQLRETURN;
   begin
      SQLDescribeParam (StatementHandle, ParameterNumber, ParameterType,
                        ColumnSize, DecimalDigits, Nullable, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDescribeParam",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLDescribeParam;

   function SQLCancel (StatementHandle : SQLHSTMT) return SQLRETURN is
      function Cancel (StatementHandle : SQLHSTMT) return SQLRETURN;
      pragma Import (Stdcall, Cancel, "SQLCancel");
   begin
      return Cancel (StatementHandle);
   end SQLCancel;

   procedure SQLCancel (StatementHandle : in SQLHSTMT) is
      RC : constant SQLRETURN := SQLCancel (StatementHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLCancel",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLCancel;

   function SQLCloseCursor (StatementHandle : SQLHSTMT) return SQLRETURN is
      function CloseCursor (StatementHandle : SQLHSTMT) return SQLRETURN;
      pragma Import (Stdcall, CloseCursor, "SQLCloseCursor");
   begin
      return CloseCursor (StatementHandle);
   end SQLCloseCursor;

   procedure SQLCloseCursor (StatementHandle : in SQLHSTMT) is
      RC : constant SQLRETURN := SQLCloseCursor (StatementHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLCloseCursor",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLCloseCursor;

   function SQLPrepare (StatementHandle : SQLHSTMT;
                        StatementText   : String) return SQLRETURN is
      function Prepare (StatementHandle  : SQLHSTMT;
                        StatementText    : PTR_SQLCHAR;
                        TextLength       : SQLINTEGER) return SQLRETURN;
      pragma Import (Stdcall, Prepare, "SQLPrepare");
   begin
      return Prepare (StatementHandle,
                      To_PTR_SQLCHAR (StatementText'Address),
                      SQLINTEGER (StatementText'Length));
   end SQLPrepare;

   procedure SQLPrepare (StatementHandle : SQLHSTMT;
                         StatementText   : String) is

      RC : constant SQLRETURN := SQLPrepare (StatementHandle,
                                             StatementText);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLPrepare",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLPrepare;

   function SQLPrepare (StatementHandle : SQLHSTMT;
                        StatementText   : Wide_String) return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLPrepare;

   procedure SQLPrepare (StatementHandle : SQLHSTMT;
                         StatementText   : Wide_String) is

      RC : constant SQLRETURN := SQLPrepare (StatementHandle,
                                             StatementText);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLPrepare",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLPrepare;

   function SQLExecute (StatementHandle : SQLHSTMT) return SQLRETURN is
      function Execute (StatementHandle : SQLHSTMT) return SQLRETURN;
      pragma Import (Stdcall, Execute, "SQLExecute");
   begin
      return Execute (StatementHandle);
   end SQLExecute;

   procedure SQLExecute (StatementHandle : in SQLHSTMT) is
      RC : constant SQLRETURN := SQLExecute (StatementHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLExecute",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLExecute;

   function SQLFetch (StatementHandle : SQLHSTMT) return SQLRETURN is
      function Fetch (StatementHandle : SQLHSTMT) return SQLRETURN;
      pragma Import (Stdcall, Fetch, "SQLFetch");
   begin
      return Fetch (StatementHandle);
   end SQLFetch;

   procedure SQLFetch (StatementHandle : in SQLHSTMT) is
      RC : constant SQLRETURN := SQLFetch (StatementHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLFetch",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLFetch;

   function SQLFetchScroll (StatementHandle  : SQLHSTMT;
                            FetchDirection   : FETCH_DIRECTION;
                            FetchOffset      : SQLINTEGER := 0)
                           return SQLRETURN is
      function FetchScroll (StatementHandle  : SQLHSTMT;
                            FetchDirection   : FETCH_DIRECTION;
                            FetchOffset      : SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, FetchScroll, "SQLFetchScroll");
   begin
      return FetchScroll (StatementHandle, FetchDirection, FetchOffset);
   end SQLFetchScroll;

   procedure SQLFetchScroll (StatementHandle : in SQLHSTMT;
                             FetchDirection  : in FETCH_DIRECTION;
                             FetchOffset     : in SQLINTEGER := 0) is
      RC : constant SQLRETURN := SQLFetchScroll (StatementHandle,
                                                 FetchDirection,
                                                 FetchOffset);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLFetchScroll",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLFetchScroll;

   procedure SQLGetCursorName (StatementHandle : in  SQLHSTMT;
                               NameBuffer      : out String;
                               BufferLength    : out SQLSMALLINT;
                               ErrorCode       : out SQLRETURN) is

      function GetCursorName (StatementHandle   : SQLHSTMT;
                              pCursorName       : PTR_SQLCHAR;
                              BufferLength      : SQLSMALLINT;
                              pNameLength       : access SQLSMALLINT)
                             return SQLRETURN;
      pragma Import (Stdcall, GetCursorName, "SQLGetCursorName");
      Len : aliased SQLSMALLINT := SQLSMALLINT (NameBuffer'Length);
      RC  : constant SQLRETURN := GetCursorName (StatementHandle,
                                                 To_PTR_SQLCHAR (NameBuffer'Address),
                                                 Len,
                                                 Len'Access);
   begin
      ErrorCode := RC;
      if Is_SQL_Ok (RC) then
         BufferLength := Len;
         for I in Natural (Len) .. (NameBuffer'Length - 1) loop
            NameBuffer (NameBuffer'First + I) := ' ';
         end loop;
      end if;
   end SQLGetCursorName;

   function SQLGetCursorName (StatementHandle : SQLHSTMT;
                              MaxNameLength   : SQLSMALLINT := 256)
                              return String is
      pragma Assert (MaxNameLength > 0);
      Str : String (1 .. Positive (MaxNameLength));
      RC  : SQLRETURN;
      Len : SQLSMALLINT;
   begin
      SQLGetCursorName (StatementHandle, Str, Len, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetCursorName",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      if Len = 0 then
         return "";
      else
         return Str (1 .. Positive (Len));
      end if;
   end SQLGetCursorName;

   procedure SQLGetCursorName (StatementHandle : in  SQLHSTMT;
                               NameBuffer      : out Wide_String;
                               BufferLength    : out SQLSMALLINT;
                               ErrorCode       : out SQLRETURN) is
   begin
      ErrorCode    := SQL_ADA95_NO_UNICODE;
      BufferLength := 0;
      for I in NameBuffer'Range loop
         NameBuffer (I) := Wide_Character'(' ');
      end loop;
   end SQLGetCursorName;

   function SQLGetCursorName (StatementHandle : SQLHSTMT;
                              MaxNameLength   : SQLSMALLINT := 256)
                              return Wide_String is
      pragma Assert (MaxNameLength > 0);
      Str : Wide_String (1 .. Positive (MaxNameLength));
      RC  : SQLRETURN;
      Len : SQLSMALLINT;
   begin
      SQLGetCursorName (StatementHandle, Str, Len, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetCursorName",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      if Len = 0 then
         return Wide_String'("");
      else
         return Str (1 .. Positive (Len));
      end if;
   end SQLGetCursorName;

   function SQLSetCursorName (StatementHandle : SQLHSTMT;
                              CursorName      : String) return SQLRETURN is
      function SetCursorName (StatementHandle : SQLHSTMT;
                              pCursorName     : PTR_SQLCHAR;
                              NameLength      : SQLSMALLINT)
                             return SQLRETURN;
      pragma Import (Stdcall, SetCursorName, "SQLSetCursorName");
   begin
      return SetCursorName (StatementHandle,
                            To_PTR_SQLCHAR (CursorName'Address),
                            SQLSMALLINT (CursorName'Length));
   end SQLSetCursorName;

   procedure SQLSetCursorName (StatementHandle : in SQLHSTMT;
                               CursorName      : in String)
   is
      RC : constant SQLRETURN := SQLSetCursorName (StatementHandle,
                                                   CursorName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetCursorName",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSetCursorName;

   function SQLSetCursorName (StatementHandle : SQLHSTMT;
                              CursorName      : Wide_String) return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLSetCursorName;

   procedure SQLSetCursorName (StatementHandle : in SQLHSTMT;
                               CursorName      : in Wide_String)
   is
      RC : constant SQLRETURN := SQLSetCursorName (StatementHandle,
                                                   CursorName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetCursorName",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSetCursorName;

   function SQLExecDirect (StatementHandle : SQLHSTMT;
                           StatementText   : String) return SQLRETURN is
      function ExecDirect (StatementHandle : SQLHSTMT;
                           pStatementText  : PTR_SQLCHAR;
                           TextLength      : SQLINTEGER)
                          return SQLRETURN;
      pragma Import (Stdcall, ExecDirect, "SQLExecDirect");
   begin
      return ExecDirect (StatementHandle,
                         To_PTR_SQLCHAR (StatementText'Address),
                         SQLINTEGER (StatementText'Length));
   end SQLExecDirect;

   procedure SQLExecDirect (StatementHandle : in SQLHSTMT;
                            StatementText   : in String) is
      RC : constant SQLRETURN := SQLExecDirect (StatementHandle,
                                                StatementText);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLExecDirect",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLExecDirect;

   function SQLExecDirect (StatementHandle : SQLHSTMT;
                           StatementText   : Wide_String) return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLExecDirect;

   procedure SQLExecDirect (StatementHandle : in SQLHSTMT;
                            StatementText   : in Wide_String) is
      RC : constant SQLRETURN := SQLExecDirect (StatementHandle,
                                                StatementText);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLExecDirect",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLExecDirect;

   function SQLEndTran
     (HandleType     : SQL_ENDTRAN_HANDLE_TYPE := SQL_HANDLE_DBC;
      Handle         : SQLHANDLE;
      CompletionType : SQL_COMPLETION_TYPE := SQL_COMMIT) return SQLRETURN is
      function EndTran (HandleType     : SQL_ENDTRAN_HANDLE_TYPE;
                        Handle         : SQLHANDLE;
                        CompletionType : SQL_COMPLETION_TYPE)
                       return SQLRETURN;
      pragma Import (Stdcall, EndTran, "SQLEndTran");
   begin
      return EndTran (HandleType, Handle, CompletionType);
   end SQLEndTran;

   procedure SQLEndTran
     (HandleType     : in SQL_ENDTRAN_HANDLE_TYPE := SQL_HANDLE_DBC;
      Handle         : in SQLHANDLE;
      CompletionType : in SQL_COMPLETION_TYPE := SQL_COMMIT) is
      RC : constant SQLRETURN := SQLEndTran (HandleType,
                                             Handle,
                                             CompletionType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLEndTran",
                       HandleType    => SQL_HANDLE_TYPE (HandleType),
                       Handle        => Handle);
   end SQLEndTran;

   procedure SQLCommit   (ConnectionHandle : SQLHDBC) is
   begin
      SQLEndTran (HandleType     => SQL_HANDLE_DBC,
                  Handle         => ConnectionHandle,
                  CompletionType => SQL_COMMIT);
   end SQLCommit;

   procedure SQLRollback (ConnectionHandle : SQLHDBC) is
   begin
      SQLEndTran (HandleType     => SQL_HANDLE_DBC,
                  Handle         => ConnectionHandle,
                  CompletionType => SQL_ROLLBACK);
   end SQLRollback;

   function SQLNumParams (StatementHandle : SQLHSTMT;
                          pNum            : access SQLSMALLINT)
                         return SQLRETURN is
      function NumParams (StatementHandle : SQLHSTMT;
                          pNum            : access SQLSMALLINT)
                         return SQLRETURN;
      pragma Import (Stdcall, NumParams, "SQLNumParams");
   begin
      return NumParams (StatementHandle, pNum);
   end SQLNumParams;

   function SQLNumParams (StatementHandle : SQLHSTMT) return SQLSMALLINT is
      R  : aliased SQLSMALLINT;
      RC : constant SQLRETURN := SQLNumParams (StatementHandle, R'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLNumParams",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return R;
   end SQLNumParams;

   function SQLNumResultCols (StatementHandle : SQLHSTMT;
                              pColumnCount    : access SQL_Column_Number)
                             return SQLRETURN is
      function NumResultCols (StatementHandle : SQLHSTMT;
                              pColumnCount    : access SQL_Column_Number)
                             return SQLRETURN;
      pragma Import (Stdcall, NumResultCols, "SQLNumResultCols");
   begin
      return NumResultCols (StatementHandle, pColumnCount);
   end SQLNumResultCols;

   function SQLNumResultCols (StatementHandle : SQLHSTMT) return
     SQL_Column_Number
   is
      Count : aliased SQL_Column_Number;
      RC    : constant SQLRETURN := SQLNumResultCols (StatementHandle,
                                                      Count'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLNumResultCols",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return Count;
   end SQLNumResultCols;

   function SQLRowCount (StatementHandle : SQLHSTMT;
                         pRowCount       : access SQLINTEGER)
                        return SQLRETURN is
      function RowCount (StatementHandle : SQLHSTMT;
                         pRowCount       : access SQLINTEGER)
                         return SQLRETURN;
      pragma Import (Stdcall, RowCount, "SQLRowCount");
   begin
      return RowCount (StatementHandle, pRowCount);
   end SQLRowCount;

   function SQLRowCount (StatementHandle : SQLHSTMT) return SQLINTEGER is
      Count : aliased SQLINTEGER;
      RC    : constant SQLRETURN := SQLRowCount (StatementHandle,
                                                 Count'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLRowCount",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return Count;
   end SQLRowCount;

   function SQLGetData (StatementHandle : in SQLHSTMT;
                        ColumnNumber    : in SQL_Column_Number;
                        TargetType      : in SQL_C_DATA_TYPE;
                        TargetValue     : in SQLPOINTER;
                        BufferLength    : in SQLINTEGER;
                        StrLen_Or_Ind   : access SQLINTEGER)
                       return SQLRETURN is
      function GetData (StatementHandle : in SQLHSTMT;
                        ColumnNumber    : in SQL_Column_Number;
                        TargetType      : in SQL_C_DATA_TYPE;
                        TargetValue     : in SQLPOINTER;
                        BufferLength    : in SQLINTEGER;
                        StrLen_Or_Ind   : access SQLINTEGER)
                        return SQLRETURN;
      pragma Import (Stdcall, GetData, "SQLGetData");
   begin
      return GetData (StatementHandle,
                      ColumnNumber,
                      TargetType,
                      TargetValue,
                      BufferLength,
                      StrLen_Or_Ind);
   end SQLGetData;

   procedure SQLGetData (StatementHandle : in SQLHSTMT;
                         ColumnNumber    : in SQL_Column_Number;
                         TargetType      : in SQL_C_DATA_TYPE;
                         TargetValue     : in SQLPOINTER;
                         BufferLength    : in SQLINTEGER;
                         StrLen_Or_Ind   : access SQLINTEGER) is
      RC : constant SQLRETURN := SQLGetData (StatementHandle,
                                             ColumnNumber,
                                             TargetType,
                                             TargetValue,
                                             BufferLength,
                                             StrLen_Or_Ind);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetData",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLGetData;

   function SQLPutData (StatementHandle : SQLHSTMT;
                        Data            : SQLPOINTER;
                        StrLen_or_Ind   : SQLINTEGER)
                       return SQLRETURN is
      function PutData (StatementHandle : SQLHSTMT;
                        Data            : SQLPOINTER;
                        StrLen_or_Ind   : SQLINTEGER)
                       return SQLRETURN;
      pragma Import (Stdcall, PutData, "SQLPutData");
   begin
      return PutData (StatementHandle, Data, StrLen_or_Ind);
   end SQLPutData;

   procedure SQLPutData (StatementHandle : in SQLHSTMT;
                         Data            : in SQLPOINTER;
                         StrLen_Or_Ind   : SQLINTEGER) is
      RC : constant SQLRETURN := SQLPutData (StatementHandle,
                                             Data,
                                             StrLen_Or_Ind);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLPutData",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLPutData;

   function SQLParamData (StatementHandle : SQLHSTMT;
                          pValue          : access SQLPOINTER)
                         return SQLRETURN is
      function ParamData (StatementHandle : SQLHSTMT;
                          pValue          : access SQLPOINTER)
                         return SQLRETURN;
      pragma Import (Stdcall, ParamData, "SQLParamData");
   begin
      return ParamData (StatementHandle, pValue);
   end SQLParamData;

   function SQLParamData (StatementHandle : in SQLHSTMT) return SQLPOINTER is
      Res : aliased SQLPOINTER;
      RC  : constant SQLRETURN := SQLParamData (StatementHandle, Res'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLParamData",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return Res;
   end SQLParamData;

   function SQLCopyDesc (SourceHandle : SQLHDESC;
                         TargetHandle : SQLHDESC)
                        return SQLRETURN is
      function CopyDesc (SourceHandle : SQLHDESC;
                         TargetHandle : SQLHDESC)
                        return SQLRETURN;
      pragma Import (Stdcall, CopyDesc, "SQLCopyDesc");
   begin
      return CopyDesc (SourceHandle, TargetHandle);
   end SQLCopyDesc;

   procedure SQLCopyDesc (SourceHandle : in SQLHDESC;
                          TargetHandle : in SQLHDESC) is
      RC : constant SQLRETURN := SQLCopyDesc (SourceHandle, TargetHandle);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLCopyDesc",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => TargetHandle);
   end SQLCopyDesc;

   function SQLStatistics
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      Unique          : SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
     return SQLRETURN is

      function Statistics (StatementHandle : SQLHSTMT;
                           pCatalogName    : PTR_SQLCHAR;
                           NameLength1     : SQLSMALLINT;
                           pSchemaName     : PTR_SQLCHAR;
                           NameLength2     : SQLSMALLINT;
                           pTableName      : PTR_SQLCHAR;
                           NameLength3     : SQLSMALLINT;
                           Unique          : SQL_STATISTICS_UNIQUE_OPTION;
                           Reserved        : SQL_STATISTICS_PAGES_OPTION)
                           return SQLRETURN;
      pragma Import (Stdcall, Statistics, "SQLStatistics");
   begin
      return Statistics (StatementHandle,
                         To_PTR_SQLCHAR (CatalogName'Address),
                         SQLSMALLINT (CatalogName'Length),
                         To_PTR_SQLCHAR (SchemaName'Address),
                         SQLSMALLINT (SchemaName'Length),
                         To_PTR_SQLCHAR (TableName'Address),
                         SQLSMALLINT (TableName'Length),
                         Unique,
                         PagesImportance);
   end SQLStatistics;

   procedure SQLStatistics
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in String;
      SchemaName      : in String;
      TableName       : in String;
      Unique          : in SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : in SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
   is
      RC : constant SQLRETURN :=
        SQLStatistics (StatementHandle,
                       CatalogName,
                       SchemaName,
                       TableName,
                       Unique,
                       PagesImportance);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLStatistics",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLStatistics;

   function SQLStatistics
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      Unique          : SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
     return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLStatistics;

   procedure SQLStatistics
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String;
      SchemaName      : in Wide_String;
      TableName       : in Wide_String;
      Unique          : in SQL_STATISTICS_UNIQUE_OPTION := SQL_INDEX_ALL;
      PagesImportance : in SQL_STATISTICS_PAGES_OPTION  := SQL_QUICK)
   is
      RC : constant SQLRETURN :=
        SQLStatistics (StatementHandle,
                       CatalogName,
                       SchemaName,
                       TableName,
                       Unique,
                       PagesImportance);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLStatistics",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLStatistics;

   function SQLTables (StatementHandle : SQLHSTMT;
                       CatalogName     : String := SQL_ALL_CATALOGS;
                       SchemaName      : String := SQL_ALL_SCHEMAS;
                       TableName       : String;
                       TableType       : String := SQL_ALL_TABLE_TYPES)
                      return SQLRETURN is

      function Tables (StatementHandle : SQLHSTMT;
                       pCatalogName    : PTR_SQLCHAR;
                       NameLength1     : SQLSMALLINT;
                       pSchemaName     : PTR_SQLCHAR;
                       NameLength2     : SQLSMALLINT;
                       pTableName      : PTR_SQLCHAR;
                       NameLength3     : SQLSMALLINT;
                       pTableType      : PTR_SQLCHAR;
                       NameLength4     : SQLSMALLINT)
                      return SQLRETURN;
      pragma Import (Stdcall, Tables, "SQLTables");
   begin
      return Tables (StatementHandle,
                     To_PTR_SQLCHAR (CatalogName'Address),
                     SQLSMALLINT (CatalogName'Length),
                     To_PTR_SQLCHAR (SchemaName'Address),
                     SQLSMALLINT (SchemaName'Length),
                     To_PTR_SQLCHAR (TableName'Address),
                     SQLSMALLINT (TableName'Length),
                     To_PTR_SQLCHAR (TableType'Address),
                     SQLSMALLINT (TableType'Length));
   end SQLTables;

   procedure SQLTables (StatementHandle : in SQLHSTMT;
                        CatalogName     : in String := SQL_ALL_CATALOGS;
                        SchemaName      : in String := SQL_ALL_SCHEMAS;
                        TableName       : in String;
                        TableType       : in String := SQL_ALL_TABLE_TYPES)
   is
      RC : constant SQLRETURN := SQLTables (StatementHandle,
                                            CatalogName,
                                            SchemaName,
                                            TableName,
                                            TableType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLTables",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLTables;

   function SQLTables
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String := W_SQL_ALL_CATALOGS;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String;
      TableType       : Wide_String := W_SQL_ALL_TABLE_TYPES)
      return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLTables;

   procedure SQLTables
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String := W_SQL_ALL_CATALOGS;
      SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : in Wide_String;
      TableType       : in Wide_String := W_SQL_ALL_TABLE_TYPES)
   is
      RC : constant SQLRETURN := SQLTables (StatementHandle,
                                            CatalogName,
                                            SchemaName,
                                            TableName,
                                            TableType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLTables",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLTables;

   function SQLProcedures (StatementHandle : SQLHSTMT;
                           CatalogName     : String;
                           SchemaName      : String := SQL_ALL_SCHEMAS;
                           ProcName        : String := SQL_ALL_PROCEDURES)
                          return SQLRETURN
   is
      function Procedures (StatementHandle : SQLHSTMT;
                           pCatalogName    : PTR_SQLCHAR;
                           NameLength1     : SQLSMALLINT;
                           pSchemaName     : PTR_SQLCHAR;
                           NameLength2     : SQLSMALLINT;
                           pProcName       : PTR_SQLCHAR;
                           NameLength3     : SQLSMALLINT)
                          return SQLRETURN;
      pragma Import (Stdcall, Procedures, "SQLProcedures");
   begin
      return Procedures (StatementHandle,
                         To_PTR_SQLCHAR (CatalogName'Address),
                         SQLSMALLINT (CatalogName'Length),
                         To_PTR_SQLCHAR (SchemaName'Address),
                         SQLSMALLINT (SchemaName'Length),
                         To_PTR_SQLCHAR (ProcName'Address),
                         SQLSMALLINT (ProcName'Length));
   end SQLProcedures;

   procedure SQLProcedures (StatementHandle : SQLHSTMT;
                            CatalogName     : String;
                            SchemaName      : String := SQL_ALL_SCHEMAS;
                            ProcName        : String := SQL_ALL_PROCEDURES)
   is
      RC : constant SQLRETURN := SQLProcedures (StatementHandle,
                                                CatalogName,
                                                SchemaName,
                                                ProcName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLProcedures",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLProcedures;

   function SQLProcedures
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES)
     return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLProcedures;

   procedure SQLProcedures
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES)
   is
      RC : constant SQLRETURN := SQLProcedures (StatementHandle,
                                                CatalogName,
                                                SchemaName,
                                                ProcName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLProcedures",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLProcedures;


   function SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      ProcName        : String := SQL_ALL_PROCEDURES;
      ColumnName      : String := SQL_ALL_COLUMNS)
     return SQLRETURN
   is
      function ProcedureColumns (StatementHandle : SQLHSTMT;
                                 pCatalogName    : PTR_SQLCHAR;
                                 NameLength1     : SQLSMALLINT;
                                 pSchemaName     : PTR_SQLCHAR;
                                 NameLength2     : SQLSMALLINT;
                                 pProcName       : PTR_SQLCHAR;
                                 NameLength3     : SQLSMALLINT;
                                 PColumnName     : PTR_SQLCHAR;
                                 NameLength4     : SQLSMALLINT)
                                return SQLRETURN;
      pragma Import (Stdcall, ProcedureColumns,
                       "SQLProcedureColumns");
   begin
      return ProcedureColumns (StatementHandle,
                               To_PTR_SQLCHAR (CatalogName'Address),
                               SQLSMALLINT (CatalogName'Length),
                               To_PTR_SQLCHAR (SchemaName'Address),
                               SQLSMALLINT (SchemaName'Length),
                               To_PTR_SQLCHAR (ProcName'Address),
                               SQLSMALLINT (ProcName'Length),
                               To_PTR_SQLCHAR (ColumnName'Address),
                               SQLSMALLINT (ColumnName'Length));
   end SQLProcedureColumns;

   procedure SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      ProcName        : String := SQL_ALL_PROCEDURES;
      ColumnName      : String := SQL_ALL_COLUMNS)
   is
      RC : constant SQLRETURN := SQLProcedureColumns (StatementHandle,
                                                      CatalogName,
                                                      SchemaName,
                                                      ProcName,
                                                      ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLProcedureColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLProcedureColumns;

   function SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS)
     return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLProcedureColumns;

   procedure SQLProcedureColumns
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      ProcName        : Wide_String := W_SQL_ALL_PROCEDURES;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS)
   is
      RC : constant SQLRETURN := SQLProcedureColumns (StatementHandle,
                                                      CatalogName,
                                                      SchemaName,
                                                      ProcName,
                                                      ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLProcedureColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLProcedureColumns;

   function SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      TableName       : String := SQL_ALL_TABLES) return SQLRETURN
   is
      function TablePrivileges (StatementHandle : SQLHSTMT;
                                pCatalogName    : PTR_SQLCHAR;
                                NameLength1     : SQLSMALLINT;
                                pSchemaName     : PTR_SQLCHAR;
                                NameLength2     : SQLSMALLINT;
                                pTableName      : PTR_SQLCHAR;
                                NameLength3     : SQLSMALLINT)
                               return SQLRETURN;
      pragma Import (Stdcall, TablePrivileges,
                       "SQLTablePrivileges");
   begin
      return TablePrivileges (StatementHandle,
                              To_PTR_SQLCHAR (CatalogName'Address),
                              SQLSMALLINT (CatalogName'Length),
                              To_PTR_SQLCHAR (SchemaName'Address),
                              SQLSMALLINT (SchemaName'Length),
                              To_PTR_SQLCHAR (TableName'Address),
                              SQLSMALLINT (TableName'Length));
   end SQLTablePrivileges;

   procedure SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String := SQL_ALL_SCHEMAS;
      TableName       : String := SQL_ALL_TABLES)
   is
      RC : constant SQLRETURN := SQLTablePrivileges (StatementHandle,
                                                     CatalogName,
                                                     SchemaName,
                                                     TableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLTablePrivileges",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLTablePrivileges;

   function SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String := W_SQL_ALL_TABLES) return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLTablePrivileges;

   procedure SQLTablePrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : Wide_String := W_SQL_ALL_TABLES)
   is
      RC : constant SQLRETURN := SQLTablePrivileges (StatementHandle,
                                                     CatalogName,
                                                     SchemaName,
                                                     TableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLTablePrivileges",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLTablePrivileges;

   function SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      ColumnName      : String := SQL_ALL_COLUMNS) return SQLRETURN
   is
      function ColumnPrivileges (StatementHandle : SQLHSTMT;
                                 pCatalogName    : PTR_SQLCHAR;
                                 NameLength1     : SQLSMALLINT;
                                 pSchemaName     : PTR_SQLCHAR;
                                 NameLength2     : SQLSMALLINT;
                                 pTableName      : PTR_SQLCHAR;
                                 NameLength3     : SQLSMALLINT;
                                 PColumnName     : PTR_SQLCHAR;
                                 NameLength4     : SQLSMALLINT)
                                return SQLRETURN;
      pragma Import (Stdcall, ColumnPrivileges,
                       "SQLColumnPrivileges");
   begin
      return ColumnPrivileges (StatementHandle,
                               To_PTR_SQLCHAR (CatalogName'Address),
                               SQLSMALLINT (CatalogName'Length),
                               To_PTR_SQLCHAR (SchemaName'Address),
                               SQLSMALLINT (SchemaName'Length),
                               To_PTR_SQLCHAR (TableName'Address),
                               SQLSMALLINT (TableName'Length),
                               To_PTR_SQLCHAR (ColumnName'Address),
                               SQLSMALLINT (ColumnName'Length));
   end SQLColumnPrivileges;

   procedure SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : String;
      SchemaName      : String;
      TableName       : String;
      ColumnName      : String := SQL_ALL_COLUMNS)
   is
      RC : constant SQLRETURN := SQLColumnPrivileges (StatementHandle,
                                                      CatalogName,
                                                      SchemaName,
                                                      TableName,
                                                      ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColumnPrivileges",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColumnPrivileges;

   function SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS) return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLColumnPrivileges;

   procedure SQLColumnPrivileges
     (StatementHandle : SQLHSTMT;
      CatalogName     : Wide_String;
      SchemaName      : Wide_String;
      TableName       : Wide_String;
      ColumnName      : Wide_String := W_SQL_ALL_COLUMNS)
   is
      RC : constant SQLRETURN := SQLColumnPrivileges (StatementHandle,
                                                      CatalogName,
                                                      SchemaName,
                                                      TableName,
                                                      ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColumnPrivileges",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColumnPrivileges;

   function SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                               IdentifierType  : in Special_Column_Type;
                               CatalogName     : in String;
                               SchemaName      : in String;
                               TableName       : in String;
                               Scope           : in ROWID_SCOPE;
                               Nullable        : in SQL_NULLABLE_FIELD)
                              return SQLRETURN is

      function SpecialColumns (StatementHandle : SQLHSTMT;
                               IdentifierType  : Special_Column_Type;
                               pCatalogName    : PTR_SQLCHAR;
                               NameLength1     : SQLSMALLINT;
                               pSchemaName     : PTR_SQLCHAR;
                               NameLength2     : SQLSMALLINT;
                               pTableName      : PTR_SQLCHAR;
                               NameLength3     : SQLSMALLINT;
                               Scope           : ROWID_SCOPE;
                               Nullable        : SQL_NULLABLE_FIELD)
                              return SQLRETURN;
      pragma Import (Stdcall, SpecialColumns, "SQLSpecialColumns");
   begin
      return SpecialColumns (StatementHandle,
                             IdentifierType,
                             To_PTR_SQLCHAR (CatalogName'Address),
                             SQLSMALLINT (CatalogName'Length),
                             To_PTR_SQLCHAR (SchemaName'Address),
                             SQLSMALLINT (SchemaName'Length),
                             To_PTR_SQLCHAR (TableName'Address),
                             SQLSMALLINT (TableName'Length),
                             Scope,
                             Nullable);
   end SQLSpecialColumns;

   procedure SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                                IdentifierType  : in Special_Column_Type;
                                CatalogName     : in String;
                                SchemaName      : in String;
                                TableName       : in String;
                                Scope           : in ROWID_SCOPE;
                                Nullable        : in SQL_NULLABLE_FIELD)
   is
      RC : constant SQLRETURN := SQLSpecialColumns (StatementHandle,
                                                    IdentifierType,
                                                    CatalogName,
                                                    SchemaName,
                                                    TableName,
                                                    Scope,
                                                    Nullable);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSpecialColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSpecialColumns;

   function SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                               IdentifierType  : in Special_Column_Type;
                               CatalogName     : in Wide_String;
                               SchemaName      : in Wide_String;
                               TableName       : in Wide_String;
                               Scope           : in ROWID_SCOPE;
                               Nullable        : in SQL_NULLABLE_FIELD)
                              return SQLRETURN is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLSpecialColumns;

   procedure SQLSpecialColumns (StatementHandle : in SQLHSTMT;
                                IdentifierType  : in Special_Column_Type;
                                CatalogName     : in Wide_String;
                                SchemaName      : in Wide_String;
                                TableName       : in Wide_String;
                                Scope           : in ROWID_SCOPE;
                                Nullable        : in SQL_NULLABLE_FIELD)
   is
      RC : constant SQLRETURN := SQLSpecialColumns (StatementHandle,
                                                    IdentifierType,
                                                    CatalogName,
                                                    SchemaName,
                                                    TableName,
                                                    Scope,
                                                    Nullable);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSpecialColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSpecialColumns;

   function SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                            CatalogName     : String;
                            SchemaName      : String;
                            TableName       : String) return SQLRETURN
   is
      function PrimaryKeys (StatementHandle : SQLHSTMT;
                            pCatalogName    : PTR_SQLCHAR;
                            NameLength1     : SQLSMALLINT;
                            pSchemaName     : PTR_SQLCHAR;
                            NameLength2     : SQLSMALLINT;
                            pTableName      : PTR_SQLCHAR;
                            NameLength3     : SQLSMALLINT)
                            return SQLRETURN;
      pragma Import (Stdcall, PrimaryKeys, "SQLPrimaryKeys");
   begin
      return PrimaryKeys (StatementHandle,
                          To_PTR_SQLCHAR (CatalogName'Address),
                          SQLSMALLINT (CatalogName'Length),
                          To_PTR_SQLCHAR (SchemaName'Address),
                          SQLSMALLINT (SchemaName'Length),
                          To_PTR_SQLCHAR (TableName'Address),
                          SQLSMALLINT (TableName'Length));

   end SQLPrimaryKeys;

   procedure SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                             CatalogName     : String;
                             SchemaName      : String;
                             TableName       : String)
   is
      RC : constant SQLRETURN := SQLPrimaryKeys (StatementHandle,
                                                 CatalogName,
                                                 SchemaName,
                                                 TableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLPrimaryKeys",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLPrimaryKeys;

   function SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                            CatalogName     : Wide_String;
                            SchemaName      : Wide_String;
                            TableName       : Wide_String) return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLPrimaryKeys;

   procedure SQLPrimaryKeys (StatementHandle : SQLHSTMT;
                             CatalogName     : Wide_String;
                             SchemaName      : Wide_String;
                             TableName       : Wide_String)
   is
      RC : constant SQLRETURN := SQLPrimaryKeys (StatementHandle,
                                                 CatalogName,
                                                 SchemaName,
                                                 TableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLPrimaryKeys",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLPrimaryKeys;

   function SQLForeignKeys (StatementHandle        : SQLHSTMT;
                            PrimaryCatalogName     : String;
                            PrimarySchemaName      : String;
                            PrimaryTableName       : String;
                            ForeignCatalogName     : String;
                            ForeignSchemaName      : String;
                            ForeignTableName       : String) return SQLRETURN
   is
      function ForeignKeys (StatementHandle : SQLHSTMT;
                            pCatalogName    : PTR_SQLCHAR;
                            NameLength1     : SQLSMALLINT;
                            pSchemaName     : PTR_SQLCHAR;
                            NameLength2     : SQLSMALLINT;
                            pTableName      : PTR_SQLCHAR;
                            NameLength3     : SQLSMALLINT;
                            pCatalogName2   : PTR_SQLCHAR;
                            NameLength4     : SQLSMALLINT;
                            pSchemaName2    : PTR_SQLCHAR;
                            NameLength5     : SQLSMALLINT;
                            pTableName2     : PTR_SQLCHAR;
                            NameLength6     : SQLSMALLINT)
                            return SQLRETURN;
      pragma Import (Stdcall, ForeignKeys, "SQLForeignKeys");
   begin
      return ForeignKeys (StatementHandle,
                          To_PTR_SQLCHAR (PrimaryCatalogName'Address),
                          SQLSMALLINT (PrimaryCatalogName'Length),
                          To_PTR_SQLCHAR (PrimarySchemaName'Address),
                          SQLSMALLINT (PrimarySchemaName'Length),
                          To_PTR_SQLCHAR (PrimaryTableName'Address),
                          SQLSMALLINT (PrimaryTableName'Length),
                          To_PTR_SQLCHAR (ForeignCatalogName'Address),
                          SQLSMALLINT (ForeignCatalogName'Length),
                          To_PTR_SQLCHAR (ForeignSchemaName'Address),
                          SQLSMALLINT (ForeignSchemaName'Length),
                          To_PTR_SQLCHAR (ForeignTableName'Address),
                          SQLSMALLINT (ForeignTableName'Length));
   end SQLForeignKeys;

   procedure SQLForeignKeys (StatementHandle        : SQLHSTMT;
                             PrimaryCatalogName     : String;
                             PrimarySchemaName      : String;
                             PrimaryTableName       : String;
                             ForeignCatalogName     : String;
                             ForeignSchemaName      : String;
                             ForeignTableName       : String)
   is
      RC : constant SQLRETURN := SQLForeignKeys (StatementHandle,
                                                 PrimaryCatalogName,
                                                 PrimarySchemaName,
                                                 PrimaryTableName,
                                                 ForeignCatalogName,
                                                 ForeignSchemaName,
                                                 ForeignTableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLForeignKeys",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLForeignKeys;


   function SQLForeignKeys (StatementHandle        : SQLHSTMT;
                            PrimaryCatalogName     : Wide_String;
                            PrimarySchemaName      : Wide_String;
                            PrimaryTableName       : Wide_String;
                            ForeignCatalogName     : Wide_String;
                            ForeignSchemaName      : Wide_String;
                            ForeignTableName       : Wide_String)
                            return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLForeignKeys;

   procedure SQLForeignKeys (StatementHandle        : SQLHSTMT;
                             PrimaryCatalogName     : Wide_String;
                             PrimarySchemaName      : Wide_String;
                             PrimaryTableName       : Wide_String;
                             ForeignCatalogName     : Wide_String;
                             ForeignSchemaName      : Wide_String;
                             ForeignTableName       : Wide_String)
   is
      RC : constant SQLRETURN := SQLForeignKeys (StatementHandle,
                                                 PrimaryCatalogName,
                                                 PrimarySchemaName,
                                                 PrimaryTableName,
                                                 ForeignCatalogName,
                                                 ForeignSchemaName,
                                                 ForeignTableName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLForeignKeys",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLForeignKeys;


   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO;
                            ErrorCode       : access SQLRETURN)
                            return String is
      function DescribeCol (StatementHandle  : SQLHSTMT;
                            ColumnNumber     : SQL_Column_Number;
                            pColumnName      : PTR_SQLCHAR;
                            BufferLength     : SQLSMALLINT;
                            pNameLength      : access SQLSMALLINT;
                            pDataType        : access SQL_DATA_TYPE;
                            PColumnSize      : access SQLUINTEGER;
                            pDecimalDigits   : access SQLSMALLINT;
                            pNullable        : access SQL_NULLABLE_INFO)
                            return SQLRETURN;
      pragma Import (Stdcall, DescribeCol, "SQLDescribeCol");
      function Cvt_Type is
         new Ada.Unchecked_Conversion (SQL_DATA_TYPE, SQLSMALLINT);
      function Cvt_NI is
         new Ada.Unchecked_Conversion (SQL_NULLABLE_INFO, SQLSMALLINT);

      ProcName : constant String := "SQLDescribeCol";

      pragma Assert (MaxNameLength > 0);
      pragma Warnings (Off);
      Str : String (1 .. Positive (MaxNameLength));
      pragma Warnings (On);
      Len : aliased SQLSMALLINT := SQLSMALLINT (Str'Length);
      D_T : aliased SQL_DATA_TYPE;
      N_I : aliased SQL_NULLABLE_INFO;

      RC  : constant SQLRETURN := DescribeCol (StatementHandle,
                                               ColumnNumber,
                                               To_PTR_SQLCHAR (Str'Address),
                                               Len,
                                               Len'Access,
                                               D_T'Access,
                                               ColumnSize,
                                               DecimalDigits,
                                               N_I'Access);
   begin
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         if not D_T'Valid then
            Raise_Invalid_Enum (ProcedureName => ProcName,
                                EnumName      => "SQL_DATA_TYPE",
                                EnumValue     =>
                                  SQLSMALLINT'Image (Cvt_Type (D_T)));
         elsif not N_I'Valid then
            Raise_Invalid_Enum (ProcedureName => ProcName,
                                EnumName      => "SQL_NULLABLE_INFO",
                                EnumValue     =>
                                  SQLSMALLINT'Image (Cvt_NI (N_I)));
         else
            Nullable.all := N_I;
            DataType.all := D_T;
            if Len <= 0 then
               return "";
            else
               return Str (1 .. Positive (Len));
            end if;
         end if;
      else
         return "";
      end if;
   end SQLDescribeCol;

   function SQLDescribeCol (StatementHandle : in  SQLHSTMT;
                            ColumnNumber    : in  SQL_Column_Number;
                            MaxNameLength   : in  SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO)
                           return String is

      ProcName : constant String := "SQLDescribeCol";
      RC : aliased SQLRETURN;
      S  : constant String := SQLDescribeCol (StatementHandle,
                                              ColumnNumber,
                                              MaxNameLength,
                                              DataType,
                                              ColumnSize,
                                              DecimalDigits,
                                              Nullable,
                                              RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => ProcName,
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return S;
   end SQLDescribeCol;

   function SQLDescribeCol (StatementHandle : SQLHSTMT;
                            ColumnNumber    : SQL_Column_Number;
                            MaxNameLength   : SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO;
                            ErrorCode       : access SQLRETURN)
                            return Wide_String is
      ProcName : constant String := "SQLDescribeCol";
   begin
      ErrorCode.all := SQL_ADA95_NO_UNICODE;
      return Wide_String'("");
   end SQLDescribeCol;

   function SQLDescribeCol (StatementHandle : in  SQLHSTMT;
                            ColumnNumber    : in  SQL_Column_Number;
                            MaxNameLength   : in  SQLSMALLINT;
                            DataType        : access SQL_DATA_TYPE;
                            ColumnSize      : access SQLUINTEGER;
                            DecimalDigits   : access SQLSMALLINT;
                            Nullable        : access SQL_NULLABLE_INFO)
                           return Wide_String is

      ProcName : constant String := "SQLDescribeCol";
      RC : aliased SQLRETURN;
      S  : constant Wide_String := SQLDescribeCol (StatementHandle,
                                                   ColumnNumber,
                                                   MaxNameLength,
                                                   DataType,
                                                   ColumnSize,
                                                   DecimalDigits,
                                                   Nullable,
                                                   RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => ProcName,
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
      return S;
   end SQLDescribeCol;

   function SQLColumns (StatementHandle : in SQLHSTMT;
                        CatalogName     : in String;
                        SchemaName      : in String := SQL_ALL_SCHEMAS;
                        TableName       : in String := SQL_ALL_TABLES;
                        ColumnName      : in String := SQL_ALL_COLUMNS)
                       return SQLRETURN is
      function Columns (StatementHandle   : SQLHSTMT;
                        pCatalogName      : PTR_SQLCHAR;
                        NameLength1       : SQLSMALLINT;
                        pSchemaName       : PTR_SQLCHAR;
                        NameLength2       : SQLSMALLINT;
                        pTableName        : PTR_SQLCHAR;
                        NameLength3       : SQLSMALLINT;
                        pColumnName       : PTR_SQLCHAR;
                        NameLength4       : SQLSMALLINT)
                       return SQLRETURN;
      pragma Import (Stdcall, Columns, "SQLColumns");
   begin
      return Columns (StatementHandle,
                      To_PTR_SQLCHAR (CatalogName'Address),
                      SQLSMALLINT (CatalogName'Length),
                      To_PTR_SQLCHAR (SchemaName'Address),
                      SQLSMALLINT (SchemaName'Length),
                      To_PTR_SQLCHAR (TableName'Address),
                      SQLSMALLINT (TableName'Length),
                      To_PTR_SQLCHAR (ColumnName'Address),
                      SQLSMALLINT (ColumnName'Length));
   end SQLColumns;

   procedure SQLColumns (StatementHandle : in SQLHSTMT;
                         CatalogName     : in String;
                         SchemaName      : in String := SQL_ALL_SCHEMAS;
                         TableName       : in String := SQL_ALL_TABLES;
                         ColumnName      : in String := SQL_ALL_COLUMNS) is
      RC : constant SQLRETURN := SQLColumns (StatementHandle,
                                             CatalogName,
                                             SchemaName,
                                             TableName,
                                             ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColumns;

   function SQLColumns
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String;
      SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : in Wide_String := W_SQL_ALL_TABLES;
      ColumnName      : in Wide_String := W_SQL_ALL_COLUMNS) return SQLRETURN
   is
   begin
      return SQL_ADA95_NO_UNICODE;
   end SQLColumns;

   procedure SQLColumns
     (StatementHandle : in SQLHSTMT;
      CatalogName     : in Wide_String;
      SchemaName      : in Wide_String := W_SQL_ALL_SCHEMAS;
      TableName       : in Wide_String := W_SQL_ALL_TABLES;
      ColumnName      : in Wide_String := W_SQL_ALL_COLUMNS)
   is
      RC : constant SQLRETURN := SQLColumns (StatementHandle,
                                             CatalogName,
                                             SchemaName,
                                             TableName,
                                             ColumnName);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColumns",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColumns;

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : String;
                          MaxLength        : SQLINTEGER := 1024;
                          ErrorCode        : access SQLRETURN)
                          return String is
      function NativeSql (ConnectionHandle : SQLHDBC;
                          pszSqlStrIn      : PTR_SQLCHAR;
                          cbSqlStrIn       : SQLINTEGER;
                          pszSqlStr        : PTR_SQLCHAR;
                          cbSqlStrMax      : SQLINTEGER;
                          pcbSqlStr        : access SQLINTEGER)
                          return SQLRETURN;
      pragma Import (Stdcall, NativeSql, "SQLNativeSql");
      pragma Assert (MaxLength > 0);

      pragma Warnings (Off);
      Str : String (1 .. Positive (MaxLength));
      pragma Warnings (On);
      Len : aliased SQLINTEGER := MaxLength;
      RC  : constant SQLRETURN := NativeSql (ConnectionHandle,
                                             To_PTR_SQLCHAR (StatementText'Address),
                                             SQLINTEGER (StatementText'Length),
                                             To_PTR_SQLCHAR (Str'Address),
                                             Len,
                                             Len'Access);
   begin
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         if Len <= 0 then
            return "";
         else
            return Str (1 .. Positive (Len));
         end if;
      else
         return "";
      end if;
   end SQLNativeSql;

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : String;
                          MaxLength        : SQLINTEGER := 1024)
                         return String is

      RC : aliased SQLRETURN;
      S  : constant String := SQLNativeSql (ConnectionHandle,
                                            StatementText,
                                            MaxLength,
                                            RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLNativeSql",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      return S;
   end SQLNativeSql;

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : Wide_String;
                          MaxLength        : SQLINTEGER := 1024;
                          ErrorCode        : access SQLRETURN)
                          return Wide_String is
   begin
      ErrorCode.all := SQL_ADA95_NO_UNICODE;
      return Wide_String'("");
   end SQLNativeSql;

   function SQLNativeSql (ConnectionHandle : SQLHDBC;
                          StatementText    : Wide_String;
                          MaxLength        : SQLINTEGER := 1024)
                         return Wide_String is

      RC : aliased SQLRETURN;
      S  : constant Wide_String := SQLNativeSql (ConnectionHandle,
                                                 StatementText,
                                                 MaxLength,
                                                 RC'Access);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLNativeSql",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
      return S;
   end SQLNativeSql;

   function SQLBulkOperations (StatementHandle  : SQLHSTMT;
                               Operation        : BULK_OPERATION)
                              return SQLRETURN
   is
      function BulkOperations (StatementHandle : SQLHSTMT;
                               Operation       : BULK_OPERATION)
                              return SQLRETURN;
      pragma Import (Stdcall, BulkOperations, "SQLBulkOperations");
   begin
      return BulkOperations (StatementHandle, Operation);
   end SQLBulkOperations;

   procedure SQLBulkOperations (StatementHandle  : SQLHSTMT;
                                Operation        : BULK_OPERATION)
   is
      RC : constant SQLRETURN := SQLBulkOperations (StatementHandle,
                                                    Operation);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBulkOperations",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLBulkOperations;

   function SQLMoreResults (StatementHandle : SQLHSTMT) return SQLRETURN
   is
      function MoreResults (StatementHandle : SQLHSTMT) return SQLRETURN;
      pragma Import (Stdcall, MoreResults, "SQLMoreResults");
   begin
      return MoreResults (StatementHandle);
   end SQLMoreResults;

   function SQLSetPos (StatementHandle  : in SQLHSTMT;
                       RowNumber        : in SQLUSMALLINT;
                       Operation        : in POSITION_OPERATION;
                       LockType         : in POSITION_LOCKTYPE)
                      return SQLRETURN
   is
      function SetPos (StatementHandle  : in SQLHSTMT;
                       RowNumber        : in SQLUSMALLINT;
                       Operation        : in POSITION_OPERATION;
                       LockType         : in POSITION_LOCKTYPE)
                      return SQLRETURN;
      pragma Import (Stdcall, SetPos, "SQLSetPos");
   begin
      return SetPos (StatementHandle, RowNumber, Operation, LockType);
   end SQLSetPos;

   procedure SQLSetPos (StatementHandle  : in SQLHSTMT;
                        RowNumber        : in SQLUSMALLINT;
                        Operation        : in POSITION_OPERATION;
                        LockType         : in POSITION_LOCKTYPE)
   is
      RC : constant SQLRETURN := SQLSetPos (StatementHandle,
                                            RowNumber,
                                            Operation,
                                            LockType);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetPos",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLSetPos;

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN)
   is
      use Ada.Strings.Fixed;
      function BrowseConnect (ConnectionHandle    : SQLHDBC;
                              InConnectionString  : PTR_SQLCHAR;
                              Len1                : SQLSMALLINT;
                              OutConnectionString : PTR_SQLCHAR;
                              Len2                : SQLSMALLINT;
                              pOutLen             : access SQLSMALLINT)
                             return SQLRETURN;
      pragma Import (Stdcall, BrowseConnect, "SQLBrowseConnect");

      Len   : aliased SQLSMALLINT;
      IFill : Natural;
      EC    : constant SQLRETURN :=
        BrowseConnect (ConnectionHandle,
                       To_PTR_SQLCHAR (InConnectionString'Address),
                       SQLSMALLINT (InConnectionString'Length),
                       To_PTR_SQLCHAR (OutConnectionString'Address),
                       SQLSMALLINT (OutConnectionString'Length),
                       Len'Access);
   begin
      RC := EC;
      if Is_SQL_Ok (EC) then
         OutConnectionStrLen := Len;
         if Len < OutConnectionString'Length then
            IFill := OutConnectionString'Length - Natural (Len);
            OutConnectionString ((OutConnectionString'First + Natural (Len)) ..
                                 OutConnectionString'Last) :=
              IFill * ' ';
         end if;
      end if;
   end SQLBrowseConnect;

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT)
   is
      RC : SQLRETURN;
   begin
      SQLBrowseConnect (ConnectionHandle,
                        InConnectionString, OutConnectionString,
                        OutConnectionStrLen, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBrowseConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLBrowseConnect;

   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN)
   is
      use Ada.Strings.Wide_Fixed;
   begin
      RC := SQL_ADA95_NO_UNICODE;
      OutConnectionString := OutConnectionString'Length * Wide_Character'(' ');
      OutConnectionStrLen := 0;
   end SQLBrowseConnect;


   procedure SQLBrowseConnect
     (ConnectionHandle    : in  SQLHDBC;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT)
   is
      RC : SQLRETURN;
   begin
      SQLBrowseConnect (ConnectionHandle,
                        InConnectionString, OutConnectionString,
                        OutConnectionStrLen, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLBrowseConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLBrowseConnect;

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN)
   is
      use Ada.Strings.Fixed;
      function DriverConnect (ConnectionHandle    : SQLHDBC;
                              WindowHandle        : SQLHWND;
                              InConnectionString  : PTR_SQLCHAR;
                              Len1                : SQLSMALLINT;
                              OutConnectionString : PTR_SQLCHAR;
                              Len2                : SQLSMALLINT;
                              pOutLen             : access SQLSMALLINT;
                              Completion          : DRIVER_COMPLETION)
                             return SQLRETURN;
      pragma Import (Stdcall, DriverConnect, "SQLDriverConnect");

      Len   : aliased SQLSMALLINT;
      IFill : Natural;
      EC    : constant SQLRETURN :=
        DriverConnect (ConnectionHandle,
                       WindowHandle,
                       To_PTR_SQLCHAR (InConnectionString'Address),
                       SQLSMALLINT (InConnectionString'Length),
                       To_PTR_SQLCHAR (OutConnectionString'Address),
                       SQLSMALLINT (OutConnectionString'Length),
                       Len'Access,
                       Completion);
   begin
      RC := EC;
      if Is_SQL_Ok (EC) then
         OutConnectionStrLen := Len;
         if Len < OutConnectionString'Length then
            IFill := OutConnectionString'Length - Natural (Len);
            OutConnectionString ((OutConnectionString'First + Natural (Len)) ..
                                 OutConnectionString'Last) :=
              IFill * ' ';
         end if;
      end if;
   end SQLDriverConnect;

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  String;
      OutConnectionString : out String;
      OutConnectionStrLen : out SQLSMALLINT)
   is
      RC : SQLRETURN;
   begin
      SQLDriverConnect (ConnectionHandle, WindowHandle, Completion,
                        InConnectionString, OutConnectionString,
                        OutConnectionStrLen, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDriverConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLDriverConnect;

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT;
      RC                  : out SQLRETURN)
   is
      use Ada.Strings.Wide_Fixed;
   begin
      RC := SQL_ADA95_NO_UNICODE;
      OutConnectionString := OutConnectionString'Length * Wide_Character'(' ');
      OutConnectionStrLen := 0;
   end SQLDriverConnect;

   procedure SQLDriverConnect
     (ConnectionHandle    : in  SQLHDBC;
      WindowHandle        : in  SQLHWND := SQL_NULL_HWND;
      Completion          : in  DRIVER_COMPLETION := SQL_DRIVER_NOPROMPT;
      InConnectionString  : in  Wide_String;
      OutConnectionString : out Wide_String;
      OutConnectionStrLen : out SQLSMALLINT)
   is
      RC : SQLRETURN;
   begin
      SQLDriverConnect (ConnectionHandle, WindowHandle, Completion,
                        InConnectionString, OutConnectionString,
                        OutConnectionStrLen, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLDriverConnect",
                       HandleType    => SQL_HANDLE_DBC,
                       Handle        => ConnectionHandle);
   end SQLDriverConnect;

   procedure Raise_Invalid_Enum (ProcedureName : in String;
                                 EnumName      : in String;
                                 EnumValue     : in String) is
   begin
      Raise_SQL_Error (ProcedureName => ProcedureName,
                       ErrorMessage  =>
                         EnumName & " invalid value is " & EnumValue,
                       RC            => SQL_ADA95_INVALID_ENUM);
   end Raise_Invalid_Enum;


   function SQL_LEN_DATA_AT_EXEC (Length : SQLINTEGER) return SQLINTEGER is
   begin
      return SQL_LEN_DATA_AT_EXEC_OFFSET - Length;
   end SQL_LEN_DATA_AT_EXEC;

   function SQL_LEN_BINARY_ATTR (Length : SQLINTEGER) return SQLINTEGER is
   begin
      return SQL_LEN_BINARY_ATTR_OFFSET - Length;
   end SQL_LEN_BINARY_ATTR;

   procedure SQLFixNTS (Column  : in out String;
                        R_Index : SQLINTEGER := SQLINTEGER'Last;
                        Pad     : Character := ' ') is
      use Ada.Strings.Fixed;
      I : Integer := Integer (R_Index);
   begin
      if I = Integer (SQLINTEGER'Last) then
         I := Column'Last;
      elsif I <= 0 then
         Column := Column'Length * Pad;
         return;
      end if;
      if I in Column'First .. Column'Last then
         while I >= Column'First and then Column (I) = ASCII.NUL loop
            Column (I) := Pad;
            I := I - 1;
         end loop;
         I := I + 1;
         while I <= Column'Last loop
            Column (I) := Pad;
            I := I + 1;
         end loop;
      end if;
   end SQLFixNTS;

   procedure SQLFixNTS (Column  : in out Wide_String;
                        R_Index : SQLINTEGER := SQLINTEGER'Last;
                        Pad     : Wide_Character := Wide_Character'(' ')) is
      use Ada.Strings.Wide_Fixed;
      I : Integer := Integer (R_Index);
   begin
      if I = Integer (SQLINTEGER'Last) then
         I := Column'Last;
      elsif I <= 0 then
         Column := Column'Length * Pad;
         return;
      end if;
      if I in Column'First .. Column'Last then
         while I >= Column'First and then
           Wide_Character'Pos (Column (I)) = 0 loop
            Column (I) := Pad;
            I := I - 1;
         end loop;
         I := I + 1;
         while I <= Column'Last loop
            Column (I) := Pad;
            I := I + 1;
         end loop;
      end if;
   end SQLFixNTS;

   Attr_Init_Index : Natural := 0;
   type Attr_Initializer is array (1 .. 8) of Attr_Init;
   AI : Attr_Initializer;

   procedure Unicode_Attributes (Flag : in Boolean := True) is
   begin
      if Flag /= Unicode_Attr_Flag then
         Unicode_Attr_Flag := Flag;
         if Attr_Init_Index > 0 then
            for I in 1 .. Attr_Init_Index loop
               AI (I).all;
            end loop;
         end if;
      end if;
   end Unicode_Attributes;


   procedure Register_Initializer (Ini : in Attr_Init) is
   begin
      if Attr_Init_Index = Attr_Initializer'Last then
         raise Constraint_Error;
      else
         Attr_Init_Index := Attr_Init_Index + 1;
         AI (Attr_Init_Index) := Ini;
      end if;
   end Register_Initializer;

begin
   Generate_Detailed_Exceptions := True;
end GNU.DB.SQLCLI;
