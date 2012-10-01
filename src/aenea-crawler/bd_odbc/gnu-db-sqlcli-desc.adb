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
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with GNU.DB.SQLCLI.Dispatch.Debug;
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.Debug);

package body GNU.DB.SQLCLI.Desc is

   package DD is new Dispatch.Debug;
   procedure Register_String_Attributes;

   procedure SQLSetDescRec (DescriptorHandle : in     SQLHDESC;
                            RecNumber        : in     SQLSMALLINT;
                            SQLType          : in     SQL_DATA_TYPE;
                            DateTime_SubType : in     SQL_DATETIME_SUBCODE;
                            Length           : in     SQLINTEGER;
                            Precision        : in     SQLSMALLINT;
                            Scale            : in     SQLSMALLINT;
                            Data             : in     SQLPOINTER;
                            StringLength     : out    SQLINTEGER;
                            Indicator        : out    SQLINTEGER;
                            RC               : out    SQLRETURN) is

      function SetDescRec (DescriptorHandle : SQLHDESC;
                           RecNumber        : SQLSMALLINT;
                           SQLType          : SQL_DATA_TYPE;
                           DateTime_SubType : SQL_DATETIME_SUBCODE;
                           Length           : SQLINTEGER;
                           Precision        : SQLSMALLINT;
                           Scale            : SQLSMALLINT;
                           Data             : SQLPOINTER;
                           StringLength     : access SQLINTEGER;
                           Indicator        : access SQLINTEGER)
                          return SQLRETURN;
      pragma Import (Stdcall, SetDescRec, "SQLSetDescRec");

      SL : aliased SQLINTEGER;
      IC : aliased SQLINTEGER;
      ER : SQLRETURN;
   begin
      ER := SetDescRec (DescriptorHandle,
                        RecNumber,
                        SQLType,
                        DateTime_SubType,
                        Length,
                        Precision,
                        Scale,
                        Data,
                        SL'Access,
                        IC'Access);
      RC := ER;
      if Is_SQL_Ok (ER) then
         StringLength := SL;
         Indicator := IC;
      end if;
   end SQLSetDescRec;

   procedure SQLSetDescRec (DescriptorHandle : in     SQLHDESC;
                            RecNumber        : in     SQLSMALLINT;
                            SQLType          : in     SQL_DATA_TYPE;
                            DateTime_SubType : in     SQL_DATETIME_SUBCODE;
                            Length           : in     SQLINTEGER;
                            Precision        : in     SQLSMALLINT;
                            Scale            : in     SQLSMALLINT;
                            Data             : in     SQLPOINTER;
                            StringLength     : out    SQLINTEGER;
                            Indicator        : out    SQLINTEGER)
   is
      RC : SQLRETURN;
   begin
      SQLSetDescRec (DescriptorHandle,
                     RecNumber,
                     SQLType,
                     DateTime_SubType,
                     Length,
                     Precision,
                     Scale,
                     Data,
                     StringLength,
                     Indicator,
                     RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetDescRec",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => DescriptorHandle);
   end SQLSetDescRec;

   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO;
                            RC               : out SQLRETURN) is
      use Ada.Strings.Fixed;
      function GetDescRec (DescriptorHandle : SQLHDESC;
                           RecNumber        : SQLSMALLINT;
                           pName            : PTR_SQLCHAR;
                           BufferLength     : SQLSMALLINT;
                           pStringLength    : access SQLSMALLINT;
                           pType            : access SQL_DATA_TYPE;
                           pSubType         : access SQL_DATETIME_SUBCODE;
                           pLength          : access SQLINTEGER;
                           pPrecision       : access SQLSMALLINT;
                           pScale           : access SQLSMALLINT;
                           pNullable        : access SQL_NULLABLE_INFO)
                          return SQLRETURN;
      pragma Import (Stdcall, GetDescRec, "SQLGetDescRec");

      function Cvt_Type is new
        Ada.Unchecked_Conversion (SQL_DATA_TYPE, SQLSMALLINT);
      function Cvt_SubType is new
        Ada.Unchecked_Conversion (SQL_DATETIME_SUBCODE, SQLSMALLINT);
      function Cvt_NI is new
        Ada.Unchecked_Conversion (SQL_NULLABLE_INFO, SQLSMALLINT);

      StrLen     : aliased SQLSMALLINT := SQLSMALLINT (Name'Length);
      pType      : aliased SQL_DATA_TYPE := SQL_DATA_TYPE'First;
      pSubType   : aliased SQL_DATETIME_SUBCODE := SQL_DATETIME_SUBCODE'First;
      pLength    : aliased SQLINTEGER  := 0;
      pPrecision : aliased SQLSMALLINT := 0;
      pScale     : aliased SQLSMALLINT := 0;
      pNullable  : aliased SQL_NULLABLE_INFO := SQL_NULLABLE_INFO'First;
      ProcName   : constant String := "SQLGetDescRec";
      IFill      : Positive;
      EC : constant SQLRETURN := GetDescRec (DescriptorHandle,
                                             RecNumber,
                                             To_PTR_SQLCHAR (Name'Address),
                                             StrLen,
                                             StrLen'Access,
                                             pType'Access,
                                             pSubType'Access,
                                             pLength'Access,
                                             pPrecision'Access,
                                             pScale'Access,
                                             pNullable'Access);
   begin
      RC := EC;
      if Is_SQL_Ok (EC) then
         if not pType'Valid then
            Raise_Invalid_Enum (ProcedureName => ProcName,
                                EnumName      => "SQL_DATA_TYPE",
                                EnumValue     =>
                                  SQLSMALLINT'Image (Cvt_Type (pType)));
         elsif not pSubType'Valid then
            Raise_Invalid_Enum (ProcedureName => ProcName,
                                EnumName      => "SQL_DATETIME_SUBCODE",
                                EnumValue     =>
                                  SQLSMALLINT'Image (Cvt_SubType (pSubType)));
         elsif not pNullable'Valid then
            Raise_Invalid_Enum (ProcedureName => ProcName,
                                EnumName      => "SQL_NULLABLE_INFO",
                                EnumValue     =>
                                  SQLSMALLINT'Image (Cvt_NI (pNullable)));
         else
            StringLength     := StrLen;
            SQLType          := pType;
            DateTime_SubType := pSubType;
            Length           := pLength;
            Precision        := pPrecision;
            Scale            := pScale;
            Nullable         := pNullable;
            if StrLen > 0 and then StrLen < Name'Length then
               IFill := Name'Length - Positive (StrLen);
               Name ((Name'First + Positive (StrLen)) .. Name'Last) :=
                 IFill * ' ';
            end if;
         end if;
      end if;
   end SQLGetDescRec;

   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO)
   is
      RC : SQLRETURN;
   begin
      SQLGetDescRec (DescriptorHandle, RecNumber, Name, StringLength,
                     SQLType, DateTime_SubType, Length,
                     Precision, Scale, Nullable, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetDescRec",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => DescriptorHandle);
   end SQLGetDescRec;

   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out Wide_String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO;
                            RC               : out SQLRETURN) is
      use Ada.Strings.Wide_Fixed;
   begin
      RC := SQL_ADA95_NO_UNICODE;
      Name := Name'Length * Wide_Character'(' ');
      StringLength := 0;
      SQLType := SQL_DATA_TYPE'First;  --  Just to assign something
      DateTime_SubType := SQL_DATETIME_SUBCODE'First;
      Length := 0;
      Precision := 0;
      Scale := 0;
      Nullable := SQL_NULLABLE_INFO'First;
   end SQLGetDescRec;

   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out Wide_String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO)
   is
      RC : SQLRETURN;
   begin
      SQLGetDescRec (DescriptorHandle, RecNumber, Name, StringLength,
                     SQLType, DateTime_SubType, Length,
                     Precision, Scale, Nullable, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetDescRec",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => DescriptorHandle);
   end SQLGetDescRec;

   procedure Get_Desc_Field (DescriptorHandle : in SQLHDESC;
                             Attribute        : in SQL_DESC_FIELD_TYPE;
                             Value            : in SQLPOINTER;
                             Length           : in out SQLINTEGER;
                             Data             : in SQLSMALLINT;
                             ErrorCode        : access SQLRETURN)
   is
      function GetDescField (DescriptorHandle : SQLHDESC;
                             RecNumber        : SQLSMALLINT;
                             FieldIdentifier  : SQL_DESC_FIELD_TYPE;
                             Value            : SQLPOINTER;
                             BufferLength     : SQLINTEGER;
                             pStringLength    : access SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, GetDescField, "SQLGetDescField");
      RC  : SQLRETURN;
      Len : aliased SQLINTEGER := Length;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
         Length := 0;
      else
         RC := GetDescField (DescriptorHandle, Data, Attribute, Value, Len,
                             Len'Access);
      end if;
      ErrorCode.all := RC;
      if Is_SQL_Ok (RC) then
         Length := Len;
      end if;
   end Get_Desc_Field;

   function SetDescField (DescriptorHandle : SQLHDESC;
                          RecNumber        : SQLSMALLINT;
                          FieldIdentifier  : SQL_DESC_FIELD_TYPE;
                          Value            : SQLPOINTER;
                          BufferLength     : SQLINTEGER)
                         return SQLRETURN;
   pragma Import (Stdcall, SetDescField, "SQLSetDescField");

   procedure Set_Desc_Field (DescriptorHandle : in  SQLHDESC;
                             Attribute        : in  SQL_DESC_FIELD_TYPE;
                             Value            : in  SQLPOINTER;
                             Length           : in  SQLINTEGER;
                             Data             : in  SQLSMALLINT;
                             ErrorCode        : out SQLRETURN)
   is
      RC : SQLRETURN;
   begin
      if Unicode_Attr_Flag then
         RC := SQL_ADA95_NO_UNICODE;
      else
         RC := SetDescField (DescriptorHandle,
                             Data, Attribute, Value, Length);
      end if;
      ErrorCode := RC;
   end Set_Desc_Field;

   procedure SQLGetDescField (DescriptorHandle : in  SQLHDESC;
                              RecNumber        : in  SQLSMALLINT;
                              FieldIdentifier  : in  SQL_DESC_FIELD_TYPE;
                              Data             : in  SQLPOINTER;
                              Length           : in  SQLINTEGER;
                              StringLength     : out SQLINTEGER;
                              RC               : out SQLRETURN) is

      function GetDescField (DescriptorHandle : SQLHDESC;
                             RecNumber        : SQLSMALLINT;
                             FieldIdentifier  : SQL_DESC_FIELD_TYPE;
                             Value            : SQLPOINTER;
                             BufferLength     : SQLINTEGER;
                             pStringLength    : access SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, GetDescField, "SQLGetDescField");

      Len : aliased SQLINTEGER;
      EC  : constant SQLRETURN := GetDescField (DescriptorHandle,
                                                RecNumber,
                                                FieldIdentifier,
                                                Data,
                                                Length,
                                                Len'Access);
   begin
      RC := EC;
      if Is_SQL_Ok (EC) then
         StringLength := Len;
      end if;
   end SQLGetDescField;

   procedure SQLGetDescField (DescriptorHandle : in  SQLHDESC;
                              RecNumber        : in  SQLSMALLINT;
                              FieldIdentifier  : in  SQL_DESC_FIELD_TYPE;
                              Data             : in  SQLPOINTER;
                              Length           : in  SQLINTEGER;
                              StringLength     : out SQLINTEGER)
   is
      RC : SQLRETURN;
   begin
      SQLGetDescField (DescriptorHandle, RecNumber, FieldIdentifier,
                       Data, Length, StringLength, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLGetDescField",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => DescriptorHandle);
   end SQLGetDescField;

   function SQLSetDescField (DescriptorHandle : SQLHDESC;
                             RecNumber        : SQLSMALLINT;
                             FieldIdentifier  : SQL_DESC_FIELD_TYPE;
                             Data             : SQLPOINTER;
                             Length           : SQLINTEGER)
                            return SQLRETURN
   is

      RC : constant SQLRETURN := SetDescField (DescriptorHandle,
                                               RecNumber,
                                               FieldIdentifier,
                                               Data,
                                               Length);
   begin
      return RC;
   end SQLSetDescField;

   procedure SQLSetDescField (DescriptorHandle : SQLHDESC;
                              RecNumber        : SQLSMALLINT;
                              FieldIdentifier  : SQL_DESC_FIELD_TYPE;
                              Data             : SQLPOINTER;
                              Length           : SQLINTEGER)
   is
      RC : constant SQLRETURN := SQLSetDescField (DescriptorHandle,
                                                  RecNumber,
                                                  FieldIdentifier,
                                                  Data,
                                                  Length);
   begin
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLSetDescField",
                       HandleType    => SQL_HANDLE_DESC,
                       Handle        => DescriptorHandle);
   end SQLSetDescField;

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out String;
                              NumericAttribute   : out SQLINTEGER;
                              RC                 : out SQLRETURN) is
      --  This version doesn't raise an exception but returns a SQLRETURN
      use Ada.Strings.Fixed;

      function ColAttribute (StatementHandle    : SQLHSTMT;
                             ColumnNumber       : SQLUSMALLINT;
                             FieldIdentifier    : SQL_DESC_FIELD_TYPE;
                             CharacterAttribute : SQLPOINTER;
                             BufferLength       : SQLSMALLINT;
                             pStringLength      : access SQLSMALLINT;
                             NumericAttribute   : access SQLINTEGER)
                            return SQLRETURN;
      pragma Import (Stdcall, ColAttribute, "SQLColAttribute");

      StringLength : aliased SQLSMALLINT;
      IFill : Integer;
      NA  : aliased SQLINTEGER := 0;
   begin
      RC := ColAttribute (StatementHandle,
                          ColumnNumber,
                          FieldIdentifier,
                          To_SQLPOINTER (CharacterAttribute'Address),
                          SQLSMALLINT (CharacterAttribute'Length),
                          StringLength'Access,
                          NA'Access);
      if Is_SQL_Ok (RC) then
         NumericAttribute := NA;
         IFill := CharacterAttribute'Length - Integer (StringLength);
         if IFill > 0 then
            CharacterAttribute ((CharacterAttribute'First +
                                 Integer(StringLength))
                                .. CharacterAttribute'Last) :=
              IFill * ' ';
         end if;
      end if;
   end SQLColAttribute;

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out String;
                              NumericAttribute   : out SQLINTEGER) is
      RC : SQLRETURN;
   begin
      SQLColAttribute (StatementHandle, ColumnNumber, FieldIdentifier,
                       CharacterAttribute, NumericAttribute, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColAttribute",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColAttribute;

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out Wide_String;
                              NumericAttribute   : out SQLINTEGER;
                              RC                 : out SQLRETURN) is

      use Ada.Strings.Wide_Fixed;
   begin
      RC := SQL_ADA95_NO_UNICODE;
      NumericAttribute := 0;
      CharacterAttribute := CharacterAttribute'Length * Wide_Character'(' ');
   end SQLColAttribute;

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out Wide_String;
                              NumericAttribute   : out SQLINTEGER) is
      RC : SQLRETURN;
   begin
      SQLColAttribute (StatementHandle, ColumnNumber, FieldIdentifier,
                       CharacterAttribute, NumericAttribute, RC);
      Check_SQL_Error (RC            => RC,
                       ProcedureName => "SQLColAttribute",
                       HandleType    => SQL_HANDLE_STMT,
                       Handle        => StatementHandle);
   end SQLColAttribute;

   procedure Register_String_Attributes
   is
   begin
      if Unicode_Attr_Flag then
         DF_WString.Register (SQL_DESC_BASE_COLUMN_NAME);
         DF_WString.Register (SQL_DESC_BASE_TABLE_NAME);
         DF_WString.Register (SQL_DESC_CATALOG_NAME);
         DF_WString.Register (SQL_DESC_LABEL);
         DF_WString.Register (SQL_DESC_LITERAL_PREFIX);
         DF_WString.Register (SQL_DESC_LITERAL_SUFFIX);
         DF_WString.Register (SQL_DESC_LOCAL_TYPE_NAME);
         DF_WString.Register (SQL_DESC_NAME);
         DF_WString.Register (SQL_DESC_SCHEMA_NAME);
         DF_WString.Register (SQL_DESC_TABLE_NAME);
         DF_WString.Register (SQL_DESC_TYPE_NAME);
      else
         DF_String.Register (SQL_DESC_BASE_COLUMN_NAME);
         DF_String.Register (SQL_DESC_BASE_TABLE_NAME);
         DF_String.Register (SQL_DESC_CATALOG_NAME);
         DF_String.Register (SQL_DESC_LABEL);
         DF_String.Register (SQL_DESC_LITERAL_PREFIX);
         DF_String.Register (SQL_DESC_LITERAL_SUFFIX);
         DF_String.Register (SQL_DESC_LOCAL_TYPE_NAME);
         DF_String.Register (SQL_DESC_NAME);
         DF_String.Register (SQL_DESC_SCHEMA_NAME);
         DF_String.Register (SQL_DESC_TABLE_NAME);
         DF_String.Register (SQL_DESC_TYPE_NAME);
      end if;
   end Register_String_Attributes;

begin
   DF_SmallInt.Register (SQL_DESC_TYPE);
   DF_SmallInt.Register (SQL_DESC_COUNT);
   DF_SmallInt.Register (SQL_DESC_CONCISE_TYPE);
   DF_SmallInt.Register (SQL_DESC_FIXED_PREC_SCALE);
   DF_SmallInt.Register (SQL_DESC_PRECISION);
   DF_SmallInt.Register (SQL_DESC_SCALE);
   DF_SmallInt.Register (SQL_DESC_MAXIMUM_SCALE);
   DF_SmallInt.Register (SQL_DESC_MINIMUM_SCALE);

   DF_Int.Register (SQL_DESC_BIND_TYPE);
   DF_Int.Register (SQL_DESC_DATETIME_INTERVAL_PRECISION);
   DF_Int.Register (SQL_DESC_DISPLAY_SIZE);
   DF_Int.Register (SQL_DESC_NUM_PREC_RADIX);
   DF_Int.Register (SQL_DESC_OCTET_LENGTH);

   DF_Unsigned.Register (SQL_DESC_ARRAY_SIZE);
   DF_Unsigned.Register (SQL_DESC_LENGTH);

   DF_Boolean.Register (SQL_DESC_AUTO_UNIQUE_VALUE);
   DF_Boolean.Register (SQL_DESC_CASE_SENSITIVE);
   DF_SBoolean.Register (SQL_DESC_ROWVER);
   DF_SBoolean.Register (SQL_DESC_UNSIGNED);

   --  Array attributes
   Dsp_Desc_Array_Status.Register (SQL_DESC_ARRAY_STATUS_PTR);
   Dsp_Bind_Offsets.Register (SQL_DESC_BIND_OFFSET_PTR);

   --  Pointer attributes
   Dsp_Rows_Processed.Register (SQL_DESC_ROWS_PROCESSED_PTR);
   Dsp_Data_Ptr.Register (SQL_DESC_DATA_PTR);
   Dsp_Int_Pointer.Register (SQL_DESC_INDICATOR_PTR);
   Dsp_Int_Pointer.Register (SQL_DESC_OCTET_LENGTH_PTR);

   Register_String_Attributes;
   Register_Initializer (Register_String_Attributes'Access);

   DD.Verify;
end GNU.DB.SQLCLI.Desc;
