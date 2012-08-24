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
with GNU.DB.SQLCLI.Generic_Attr;
with GNU.DB.SQLCLI.Dispatch;
with GNU.DB.SQLCLI.Dispatch.A_Integer;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Wide_String;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
package GNU.DB.SQLCLI.Diag is

   function SQLGetDiagRec (HandleType  : SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
                           Handle      : SQLHANDLE;
                           RecNumber   : SQLSMALLINT := 1;
                           State       : access SQLSTATE;
                           NativeError : access SQLINTEGER)
                          return String;

   function SQLGetDiagRec (HandleType  : SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
                           Handle      : SQLHANDLE;
                           RecNumber   : SQLSMALLINT := 1;
                           State       : access WIDE_SQLSTATE;
                           NativeError : access SQLINTEGER)
                          return Wide_String;

   --  defines for diagnostics fields
   type SQL_DIAGNOSTIC_IDENTIFIER is
     (SQL_DIAG_CURSOR_ROW_COUNT,
      SQL_DIAG_ROW_NUMBER,
      SQL_DIAG_COLUMN_NUMBER,
      SQL_DIAG_RETURNCODE,
      SQL_DIAG_NUMBER,
      SQL_DIAG_ROW_COUNT,
      SQL_DIAG_SQLSTATE,
      SQL_DIAG_NATIVE,
      SQL_DIAG_MESSAGE_TEXT,
      SQL_DIAG_DYNAMIC_FUNCTION,
      SQL_DIAG_CLASS_ORIGIN,
      SQL_DIAG_SUBCLASS_ORIGIN,
      SQL_DIAG_CONNECTION_NAME,
      SQL_DIAG_SERVER_NAME,
      SQL_DIAG_DYNAMIC_FUNCTION_CODE);

   for SQL_DIAGNOSTIC_IDENTIFIER use
      (SQL_DIAG_CURSOR_ROW_COUNT          => -1249,
       SQL_DIAG_ROW_NUMBER                => -1248,
       SQL_DIAG_COLUMN_NUMBER             => -1247,
       SQL_DIAG_RETURNCODE                => 1,
       SQL_DIAG_NUMBER                    => 2,
       SQL_DIAG_ROW_COUNT                 => 3,
       SQL_DIAG_SQLSTATE                  => 4,
       SQL_DIAG_NATIVE                    => 5,
       SQL_DIAG_MESSAGE_TEXT              => 6,
       SQL_DIAG_DYNAMIC_FUNCTION          => 7,
       SQL_DIAG_CLASS_ORIGIN              => 8,
       SQL_DIAG_SUBCLASS_ORIGIN           => 9,
       SQL_DIAG_CONNECTION_NAME           => 10,
       SQL_DIAG_SERVER_NAME               => 11,
       SQL_DIAG_DYNAMIC_FUNCTION_CODE     => 12);
   for SQL_DIAGNOSTIC_IDENTIFIER'Size use SQLSMALLINT'Size;

   type Diagnostic_Context_Record is private;
   type Diagnostic_Context is access all Diagnostic_Context_Record;
   function Default_Context return Diagnostic_Context;

   procedure Get_Diag_Field (Ctx       : Diagnostic_Context;
                             AttrType  : SQL_DIAGNOSTIC_IDENTIFIER;
                             Value     : SQLPOINTER;
                             Length    : in out SQLSMALLINT;
                             Data      : in SQLSMALLINT;
                             ErrorCode : access SQLRETURN);
   procedure Set_Diag_Field (Ctx       : Diagnostic_Context;
                             AttrType  : SQL_DIAGNOSTIC_IDENTIFIER;
                             Value     : SQLPOINTER;
                             Length    : in SQLSMALLINT;
                             Data      : in SQLSMALLINT;
                             ErrorCode : out SQLRETURN);

   package Diagnostic_Fields is new GNU.DB.SQLCLI.Generic_Attr
     (Context         => Diagnostic_Context,
      T               => SQL_DIAGNOSTIC_IDENTIFIER,
      Base            => SQLSMALLINT,
      Aux             => SQLSMALLINT,
      Get             => Get_Diag_Field,
      Set             => Set_Diag_Field,
      Default_Context => Default_Context);
   subtype Diagnostic_Field is Diagnostic_Fields.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Diagnostic_Fields);

   package DF_String is new Dispatch.A_String;
   subtype Diagnostic_Field_String is DF_String.Info;

   package DF_WString is new Dispatch.A_Wide_String;
   subtype Diagnostic_Field_Wide_String is DF_WString.Info;

   package DF_Integer is new Dispatch.A_Integer (SQLINTEGER);
   subtype Diagnostic_Field_Int is DF_Integer.Info;

   package DF_SmallInteger is new Dispatch.A_Integer (SQLSMALLINT);
   subtype Diagnostic_Field_SmallInt is DF_SmallInteger.Info;


   type SQL_DYNAMIC_FUNCTION_CODE is
     (SQL_DIAG_DROP_INDEX,
      SQL_DIAG_CREATE_INDEX,
      SQL_DIAG_UNKNOWN_STATEMENT,
      SQL_DIAG_ALTER_DOMAIN,
      SQL_DIAG_ALTER_TABLE,
      SQL_DIAG_CREATE_ASSERTION,
      SQL_DIAG_CALL,
      SQL_DIAG_CREATE_CHARACTER_SET,
      SQL_DIAG_CREATE_COLLATION,
      SQL_DIAG_DELETE_WHERE,
      SQL_DIAG_CREATE_DOMAIN,
      SQL_DIAG_DROP_ASSERTION,
      SQL_DIAG_DROP_CHARACTER_SET,
      SQL_DIAG_DROP_COLLATION,
      SQL_DIAG_DROP_DOMAIN,
      SQL_DIAG_DROP_SCHEMA,
      SQL_DIAG_DROP_TABLE,
      SQL_DIAG_DROP_TRANSLATION,
      SQL_DIAG_DROP_VIEW,
      SQL_DIAG_DYNAMIC_DELETE_CURSOR,
      SQL_DIAG_GRANT,
      SQL_DIAG_INSERT,
      SQL_DIAG_REVOKE,
      SQL_DIAG_CREATE_TABLE,
      SQL_DIAG_DYNAMIC_UPDATE_CURSOR,
      SQL_DIAG_UPDATE_WHERE,
      SQL_DIAG_CREATE_VIEW,
      SQL_DIAG_SELECT_CURSOR);

   for SQL_DYNAMIC_FUNCTION_CODE use
      (SQL_DIAG_DROP_INDEX                => -2,
       SQL_DIAG_CREATE_INDEX              => -1,
       SQL_DIAG_UNKNOWN_STATEMENT         => 0,
       SQL_DIAG_ALTER_DOMAIN              => 3,
       SQL_DIAG_ALTER_TABLE               => 4,
       SQL_DIAG_CREATE_ASSERTION          => 6,
       SQL_DIAG_CALL                      => 7,
       SQL_DIAG_CREATE_CHARACTER_SET      => 8,
       SQL_DIAG_CREATE_COLLATION          => 10,
       SQL_DIAG_DELETE_WHERE              => 19,
       SQL_DIAG_CREATE_DOMAIN             => 23,
       SQL_DIAG_DROP_ASSERTION            => 24,
       SQL_DIAG_DROP_CHARACTER_SET        => 25,
       SQL_DIAG_DROP_COLLATION            => 26,
       SQL_DIAG_DROP_DOMAIN               => 27,
       SQL_DIAG_DROP_SCHEMA               => 31,
       SQL_DIAG_DROP_TABLE                => 32,
       SQL_DIAG_DROP_TRANSLATION          => 33,
       SQL_DIAG_DROP_VIEW                 => 36,
       SQL_DIAG_DYNAMIC_DELETE_CURSOR     => 38,
       SQL_DIAG_GRANT                     => 48,
       SQL_DIAG_INSERT                    => 50,
       SQL_DIAG_REVOKE                    => 59,
       SQL_DIAG_CREATE_TABLE              => 77,
       SQL_DIAG_DYNAMIC_UPDATE_CURSOR     => 81,
       SQL_DIAG_UPDATE_WHERE              => 82,
       SQL_DIAG_CREATE_VIEW               => 84,
       SQL_DIAG_SELECT_CURSOR             => 85);
   for SQL_DYNAMIC_FUNCTION_CODE'Size use SQLINTEGER'Size;

   package Dsp_Dynamic_Function_Code is new
     Dispatch.A_Enumerated (SQL_DIAG_DYNAMIC_FUNCTION_CODE,
                            SQL_DYNAMIC_FUNCTION_CODE,
                            SQLINTEGER,
                            "SQL_DYNAMIC_FUNCTION_CODE");
   subtype Diagnostic_Field_Dynamic_Function_Code is
     Dsp_Dynamic_Function_Code.Info;


   function SQLGetDiagField (HandleType   : SQL_HANDLE_TYPE;
                             Handle       : SQLHANDLE;
                             RecordNumber : SQLSMALLINT;
                             Field        : SQL_DIAGNOSTIC_IDENTIFIER;
                             MaxLength    : SQLSMALLINT := 1024)
                            return Diagnostic_Field'Class;

private
   pragma Inline (Default_Context);

   type Diagnostic_Context_Record is
      record
         Handle       : SQLHANDLE;
         HandleType   : SQL_HANDLE_TYPE;
         RecordNumber : SQLSMALLINT;
      end record;

end GNU.DB.SQLCLI.Diag;
