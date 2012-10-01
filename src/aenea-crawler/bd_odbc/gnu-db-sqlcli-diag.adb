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
with Ada.Unchecked_Deallocation;
with GNU.DB.SQLCLI.Dispatch.Debug;
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.Debug);

package body GNU.DB.SQLCLI.Diag is
   package DD is new Dispatch.Debug;
   procedure Register_String_Attributes;

   function SQLGetDiagRec (HandleType  : SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
                           Handle      : SQLHANDLE;
                           RecNumber   : SQLSMALLINT := 1;
                           State       : access SQLSTATE;
                           NativeError : access SQLINTEGER)
                           return String is
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
      Len : aliased SQLSMALLINT := SQLSMALLINT (SQL_MAX_MESSAGE_SIZE);

      pragma Warnings (Off);
      Str : String (1 .. Positive (SQL_MAX_MESSAGE_SIZE));
      pragma Warnings (On);

      C_State : aliased C_SQLSTATE;
      RC  : constant SQLRETURN := GetDiagRec (HandleType,
                                              Handle,
                                              RecNumber,
                                              C_State'Access,
                                              NativeError,
                                              To_PTR_SQLCHAR (Str'Address),
                                              Len,
                                              Len'Access);
   begin
      State.all := GNU.DB.SQLCLI.To_Ada (C_State);
      if Is_SQL_Ok (RC) then
         if Len > 0 then
            return Str (1 .. Positive (Len));
         else
            return "";
         end if;
      else
         if RC = SQL_ERROR then
            return "Invalid Record Number";
         elsif RC = SQL_INVALID_HANDLE then
            return "Invalid Handle";
         elsif RC = SQL_NO_DATA then
            return "No diagnostic data available";
         else
            return "Unknown error return from SQLGetDiagRec";
         end if;
      end if;
   end SQLGetDiagRec;

   function SQLGetDiagRec (HandleType  : SQL_HANDLE_TYPE := SQL_HANDLE_STMT;
                           Handle      : SQLHANDLE;
                           RecNumber   : SQLSMALLINT := 1;
                           State       : access WIDE_SQLSTATE;
                           NativeError : access SQLINTEGER)
                           return Wide_String is
   begin
      Raise_SQL_Error ("SQLGetDiagRec",
                       ErrorMessage  =>
                         "The binding has'nt been built with UNICODE Support.",
                       RC            => SQL_ADA95_NO_UNICODE);
      return Wide_String'("");
   end SQLGetDiagRec;

   procedure Get_Diag_Field (Ctx       : Diagnostic_Context;
                             AttrType  : SQL_DIAGNOSTIC_IDENTIFIER;
                             Value     : SQLPOINTER;
                             Length    : in out SQLSMALLINT;
                             Data      : in SQLSMALLINT;
                             ErrorCode : access SQLRETURN)
   is
      pragma Unreferenced (Data);
      function GetDiagField (HandleType     : SQL_HANDLE_TYPE;
                             Handle         : SQLHANDLE;
                             RecNumber      : SQLSMALLINT;
                             DiagIdentifier : SQL_DIAGNOSTIC_IDENTIFIER;
                             DiagInfo       : SQLPOINTER;
                             BufferLength   : SQLSMALLINT;
                             pStringLength  : access SQLSMALLINT)
                             return SQLRETURN;
      pragma Import (Stdcall, GetDiagField, "SQLGetDiagField");
      pragma Assert (Ctx /= null);
      Name : constant String := "SQLGetDiagField";
      Len  : aliased SQLSMALLINT := Length;
      RC   : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
         Length := 0;
      else
         RC := GetDiagField (Ctx.HandleType,
                             Ctx.Handle,
                             Ctx.RecordNumber,
                             AttrType,
                             Value,
                             Len,
                             Len'Access);
      end if;
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      else
         if RC = SQL_ERROR then
            Raise_SQL_Error (ProcedureName => Name,
                             ErrorMessage  => "Invalid Field request");
         elsif RC = SQL_INVALID_HANDLE then
            Raise_SQL_Error (ProcedureName => Name,
                             ErrorMessage  => "Invalid Handle");
         elsif RC = SQL_NO_DATA then
            Raise_SQL_Error (ProcedureName => Name,
                             ErrorMessage  => "No diagnostic data available");
         else
            Raise_SQL_Error (ProcedureName => Name,
                             ErrorMessage  => "Unknown error return");
         end if;
      end if;
   end Get_Diag_Field;

   procedure Set_Diag_Field (Ctx       : Diagnostic_Context;
                             AttrType  : SQL_DIAGNOSTIC_IDENTIFIER;
                             Value     : SQLPOINTER;
                             Length    : in SQLSMALLINT;
                             Data      : in SQLSMALLINT;
                             ErrorCode : out SQLRETURN) is
      pragma Unreferenced (Data);
      pragma Unreferenced (Length);
      pragma Unreferenced (Value);
      pragma Unreferenced (AttrType);
      pragma Unreferenced (Ctx);
   begin
      ErrorCode := -1;
      Raise_SQL_Error (ProcedureName => "Set_Diag_Field",
                       ErrorMessage  => "Set Value not allowed");
   end Set_Diag_Field;

   function Default_Context return Diagnostic_Context is
   begin
      return null;
   end Default_Context;

   function SQLGetDiagField (HandleType   : SQL_HANDLE_TYPE;
                             Handle       : SQLHANDLE;
                             RecordNumber : SQLSMALLINT;
                             Field        : SQL_DIAGNOSTIC_IDENTIFIER;
                             MaxLength    : SQLSMALLINT := 1024)
                             return Diagnostic_Field'Class is
      use type Dispatch.Attr_Get_Func;
      procedure Free is new
        Ada.Unchecked_Deallocation (Diagnostic_Context_Record,
                                    Diagnostic_Context);

      Ctx : Diagnostic_Context := new Diagnostic_Context_Record'
        (Handle       => Handle,
         HandleType   => HandleType,
         RecordNumber => RecordNumber);
      ErrorCode : aliased SQLRETURN;
      F : constant Dispatch.Attr_Get_Func := Dispatch.Get_Func (Field);
   begin
      if F = null then
         Free (Ctx);
         Raise_SQL_Error ("SQLGetDiagField",
                          SQL_DIAGNOSTIC_IDENTIFIER'Image (Field) &
                          Attr_Not_Supported_Msg);
      else
         declare
            Res : constant Diagnostic_Field'Class :=
              F.all (Ctx, Field, MaxLength, 0, ErrorCode'Access);
         begin
            Free (Ctx);
            return Res;
         end;
      end if;
   end SQLGetDiagField;

   procedure Register_String_Attributes is
   begin
      if Unicode_Attr_Flag then
         DF_WString.Register (SQL_DIAG_SUBCLASS_ORIGIN);
         DF_WString.Register (SQL_DIAG_SQLSTATE);
         DF_WString.Register (SQL_DIAG_SERVER_NAME);
         DF_WString.Register (SQL_DIAG_MESSAGE_TEXT);
         DF_WString.Register (SQL_DIAG_CONNECTION_NAME);
         DF_WString.Register (SQL_DIAG_CLASS_ORIGIN);
         DF_WString.Register (SQL_DIAG_DYNAMIC_FUNCTION);
      else
         DF_String.Register (SQL_DIAG_SUBCLASS_ORIGIN);
         DF_String.Register (SQL_DIAG_SQLSTATE);
         DF_String.Register (SQL_DIAG_SERVER_NAME);
         DF_String.Register (SQL_DIAG_MESSAGE_TEXT);
         DF_String.Register (SQL_DIAG_CONNECTION_NAME);
         DF_String.Register (SQL_DIAG_CLASS_ORIGIN);
         DF_String.Register (SQL_DIAG_DYNAMIC_FUNCTION);
      end if;
   end Register_String_Attributes;

begin
   DF_Integer.Register (SQL_DIAG_NATIVE);
   DF_Integer.Register (SQL_DIAG_ROW_NUMBER);
   DF_Integer.Register (SQL_DIAG_COLUMN_NUMBER);
   DF_Integer.Register (SQL_DIAG_ROW_COUNT);
   DF_Integer.Register (SQL_DIAG_NUMBER);
   DF_Integer.Register (SQL_DIAG_CURSOR_ROW_COUNT);

   DF_SmallInteger.Register (SQL_DIAG_RETURNCODE);

   Register_String_Attributes;
   Register_Initializer (Register_String_Attributes'Access);
   DD.Verify;
end GNU.DB.SQLCLI.Diag;
