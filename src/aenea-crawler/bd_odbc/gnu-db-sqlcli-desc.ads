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
with GNU.DB.SQLCLI.Dispatch.A_Unsigned;
with GNU.DB.SQLCLI.Dispatch.A_Boolean;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
with GNU.DB.SQLCLI.Dispatch.A_Array;
with GNU.DB.SQLCLI.Dispatch.A_Pointer;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Wide_String;
package GNU.DB.SQLCLI.Desc is

   type SQL_DESC_FIELD_TYPE is
     (SQL_DESC_CONCISE_TYPE,
      SQL_DESC_DISPLAY_SIZE,
      SQL_DESC_UNSIGNED,
      SQL_DESC_FIXED_PREC_SCALE,
      SQL_DESC_UPDATABLE,
      SQL_DESC_AUTO_UNIQUE_VALUE,
      SQL_DESC_CASE_SENSITIVE,
      SQL_DESC_SEARCHABLE,
      SQL_DESC_TYPE_NAME,
      SQL_DESC_TABLE_NAME,
      SQL_DESC_SCHEMA_NAME,
      SQL_DESC_CATALOG_NAME,
      SQL_DESC_LABEL,
      SQL_DESC_ARRAY_SIZE,
      SQL_DESC_ARRAY_STATUS_PTR,
      SQL_DESC_BASE_COLUMN_NAME,
      SQL_DESC_BASE_TABLE_NAME,
      SQL_DESC_BIND_OFFSET_PTR,
      SQL_DESC_BIND_TYPE,
      SQL_DESC_DATETIME_INTERVAL_PRECISION,
      SQL_DESC_LITERAL_PREFIX,
      SQL_DESC_LITERAL_SUFFIX,
      SQL_DESC_LOCAL_TYPE_NAME,
      SQL_DESC_MAXIMUM_SCALE,
      SQL_DESC_MINIMUM_SCALE,
      SQL_DESC_NUM_PREC_RADIX,
      SQL_DESC_PARAMETER_TYPE,
      SQL_DESC_ROWS_PROCESSED_PTR,
      SQL_DESC_ROWVER,
      SQL_DESC_COUNT,
      SQL_DESC_TYPE,
      SQL_DESC_LENGTH,
      SQL_DESC_OCTET_LENGTH_PTR,
      SQL_DESC_PRECISION,
      SQL_DESC_SCALE,
      SQL_DESC_DATETIME_INTERVAL_CODE,
      SQL_DESC_NULLABLE,
      SQL_DESC_INDICATOR_PTR,
      SQL_DESC_DATA_PTR,
      SQL_DESC_NAME,
      SQL_DESC_UNNAMED,
      SQL_DESC_OCTET_LENGTH,
      SQL_DESC_ALLOC_TYPE);
   for SQL_DESC_FIELD_TYPE use
      (SQL_DESC_CONCISE_TYPE                   => 2,      --
       SQL_DESC_DISPLAY_SIZE                   => 6,      --
       SQL_DESC_UNSIGNED                       => 8,      --
       SQL_DESC_FIXED_PREC_SCALE               => 9,      --
       SQL_DESC_UPDATABLE                      => 10,     --
       SQL_DESC_AUTO_UNIQUE_VALUE              => 11,     --
       SQL_DESC_CASE_SENSITIVE                 => 12,     --
       SQL_DESC_SEARCHABLE                     => 13,     --
       SQL_DESC_TYPE_NAME                      => 14,     --
       SQL_DESC_TABLE_NAME                     => 15,     --
       SQL_DESC_SCHEMA_NAME                    => 16,     --
       SQL_DESC_CATALOG_NAME                   => 17,     --
       SQL_DESC_LABEL                          => 18,     --
       SQL_DESC_ARRAY_SIZE                     => 20,     --
       SQL_DESC_ARRAY_STATUS_PTR               => 21,     --
       SQL_DESC_BASE_COLUMN_NAME               => 22,     --
       SQL_DESC_BASE_TABLE_NAME                => 23,     --
       SQL_DESC_BIND_OFFSET_PTR                => 24,     --
       SQL_DESC_BIND_TYPE                      => 25,     --
       SQL_DESC_DATETIME_INTERVAL_PRECISION    => 26,     --
       SQL_DESC_LITERAL_PREFIX                 => 27,     --
       SQL_DESC_LITERAL_SUFFIX                 => 28,     --
       SQL_DESC_LOCAL_TYPE_NAME                => 29,     --
       SQL_DESC_MAXIMUM_SCALE                  => 30,
       SQL_DESC_MINIMUM_SCALE                  => 31,
       SQL_DESC_NUM_PREC_RADIX                 => 32,     --
       SQL_DESC_PARAMETER_TYPE                 => 33,     --
       SQL_DESC_ROWS_PROCESSED_PTR             => 34,     --
       SQL_DESC_ROWVER                         => 35,     --
       SQL_DESC_COUNT                          => 1001,   --
       SQL_DESC_TYPE                           => 1002,   --
       SQL_DESC_LENGTH                         => 1003,   --
       SQL_DESC_OCTET_LENGTH_PTR               => 1004,   --
       SQL_DESC_PRECISION                      => 1005,   --
       SQL_DESC_SCALE                          => 1006,   --
       SQL_DESC_DATETIME_INTERVAL_CODE         => 1007,   --
       SQL_DESC_NULLABLE                       => 1008,   --
       SQL_DESC_INDICATOR_PTR                  => 1009,   --
       SQL_DESC_DATA_PTR                       => 1010,   --
       SQL_DESC_NAME                           => 1011,   --
       SQL_DESC_UNNAMED                        => 1012,   --
       SQL_DESC_OCTET_LENGTH                   => 1013,   --
       SQL_DESC_ALLOC_TYPE                     => 1099);  --
   for SQL_DESC_FIELD_TYPE'Size use SQLSMALLINT'Size;

   procedure Get_Desc_Field (DescriptorHandle : in SQLHDESC;
                             Attribute        : in SQL_DESC_FIELD_TYPE;
                             Value            : in SQLPOINTER;
                             Length           : in out SQLINTEGER;
                             Data             : in SQLSMALLINT;
                             ErrorCode        : access SQLRETURN);
   pragma Inline (Get_Desc_Field);

   procedure Set_Desc_Field (DescriptorHandle : in  SQLHDESC;
                             Attribute        : in  SQL_DESC_FIELD_TYPE;
                             Value            : in  SQLPOINTER;
                             Length           : in  SQLINTEGER;
                             Data             : in  SQLSMALLINT;
                             ErrorCode        : out SQLRETURN);
   pragma Inline (Set_Desc_Field);

   package Descriptor_Fields is new GNU.DB.SQLCLI.Generic_Attr
     (Context         => SQLHDESC,
      T               => SQL_DESC_FIELD_TYPE,
      Base            => SQLINTEGER,
      Aux             => SQLSMALLINT,
      Get             => Get_Desc_Field,
      Set             => Set_Desc_Field,
      Default_Context => Null_Handle);
   subtype Descriptor_Field is Descriptor_Fields.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Descriptor_Fields);

   package DF_SmallInt is new Dispatch.A_Integer (SQLSMALLINT);
   subtype Desc_Field_SmallInt is DF_SmallInt.Info;

   package DF_Int is new Dispatch.A_Integer (SQLINTEGER);
   subtype Desc_Field_Int is DF_Int.Info;

   package DF_Unsigned is new Dispatch.A_Unsigned (SQLUINTEGER);
   subtype Desc_Field_UInt is DF_Unsigned.Info;

   package DF_Boolean is new Dispatch.A_Boolean (SQLUINTEGER);
   subtype Desc_Field_Boolean is DF_Boolean.Info;

   package DF_SBoolean is new Dispatch.A_Boolean (SQLUSMALLINT);
   subtype Desc_Field_SBoolean is DF_SBoolean.Info;

   package DF_String is new Dispatch.A_String;
   subtype Desc_Field_String is DF_String.Info;

   package DF_WString is new Dispatch.A_Wide_String;
   subtype Desc_Field_WString is DF_WString.Info;

   function SQLSetDescField (DescriptorHandle : in SQLHDESC;
                             RecNumber        : in SQLSMALLINT;
                             FieldIdentifier  : in SQL_DESC_FIELD_TYPE;
                             Data             : in SQLPOINTER;
                             Length           : in SQLINTEGER)
                            return SQLRETURN;
   procedure SQLSetDescField (DescriptorHandle : in SQLHDESC;
                              RecNumber        : in SQLSMALLINT;
                              FieldIdentifier  : in SQL_DESC_FIELD_TYPE;
                              Data             : in SQLPOINTER;
                              Length           : in SQLINTEGER);
   pragma Inline (SQLSetDescField);

   procedure SQLGetDescField (DescriptorHandle : in  SQLHDESC;
                              RecNumber        : in  SQLSMALLINT;
                              FieldIdentifier  : in  SQL_DESC_FIELD_TYPE;
                              Data             : in  SQLPOINTER;
                              Length           : in  SQLINTEGER;
                              StringLength     : out SQLINTEGER;
                              RC               : out SQLRETURN);

   procedure SQLGetDescField (DescriptorHandle : in  SQLHDESC;
                              RecNumber        : in  SQLSMALLINT;
                              FieldIdentifier  : in  SQL_DESC_FIELD_TYPE;
                              Data             : in  SQLPOINTER;
                              Length           : in  SQLINTEGER;
                              StringLength     : out SQLINTEGER);
   pragma Inline (SQLGetDescField);


   type SQL_DATETIME_SUBCODE is (SQL_CODE_YEAR,
                                 SQL_CODE_MONTH,
                                 SQL_CODE_DAY,
                                 SQL_CODE_HOUR,
                                 SQL_CODE_MINUTE,
                                 SQL_CODE_SECOND,
                                 SQL_CODE_YEAR_TO_MONTH,
                                 SQL_CODE_DAY_TO_HOUR,
                                 SQL_CODE_DAY_TO_MINUTE,
                                 SQL_CODE_DAY_TO_SECOND,
                                 SQL_CODE_HOUR_TO_MINUTE,
                                 SQL_CODE_HOUR_TO_SECOND,
                                 SQL_CODE_MINUTE_TO_SECOND);
   for SQL_DATETIME_SUBCODE use (SQL_CODE_YEAR                 => 1,
                                 SQL_CODE_MONTH                => 2,
                                 SQL_CODE_DAY                  => 3,
                                 SQL_CODE_HOUR                 => 4,
                                 SQL_CODE_MINUTE               => 5,
                                 SQL_CODE_SECOND               => 6,
                                 SQL_CODE_YEAR_TO_MONTH        => 7,
                                 SQL_CODE_DAY_TO_HOUR          => 8,
                                 SQL_CODE_DAY_TO_MINUTE        => 9,
                                 SQL_CODE_DAY_TO_SECOND        => 10,
                                 SQL_CODE_HOUR_TO_MINUTE       => 11,
                                 SQL_CODE_HOUR_TO_SECOND       => 12,
                                 SQL_CODE_MINUTE_TO_SECOND     => 13);
   for SQL_DATETIME_SUBCODE'Size use SQLSMALLINT'Size;

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
                            RC               : out    SQLRETURN);
   procedure SQLSetDescRec (DescriptorHandle : in     SQLHDESC;
                            RecNumber        : in     SQLSMALLINT;
                            SQLType          : in     SQL_DATA_TYPE;
                            DateTime_SubType : in     SQL_DATETIME_SUBCODE;
                            Length           : in     SQLINTEGER;
                            Precision        : in     SQLSMALLINT;
                            Scale            : in     SQLSMALLINT;
                            Data             : in     SQLPOINTER;
                            StringLength     : out    SQLINTEGER;
                            Indicator        : out    SQLINTEGER);
   pragma Inline (SQLSetDescRec);

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
                            RC               : out SQLRETURN);
   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO);

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
                            RC               : out SQLRETURN);
   procedure SQLGetDescRec (DescriptorHandle : in  SQLHDESC;
                            RecNumber        : in  SQLSMALLINT;
                            Name             : out Wide_String;
                            StringLength     : out SQLSMALLINT;
                            SQLType          : out SQL_DATA_TYPE;
                            DateTime_SubType : out SQL_DATETIME_SUBCODE;
                            Length           : out SQLINTEGER;
                            Precision        : out SQLSMALLINT;
                            Scale            : out SQLSMALLINT;
                            Nullable         : out SQL_NULLABLE_INFO);

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out String;
                              NumericAttribute   : out SQLINTEGER;
                              RC                 : out SQLRETURN);
   --  This version doesn't raise an exception but returns a SQLRETURN

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out String;
                              NumericAttribute   : out SQLINTEGER);
   --  This version raises an exception if the SQLRETURN indicates an error

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out Wide_String;
                              NumericAttribute   : out SQLINTEGER;
                              RC                 : out SQLRETURN);
   --  This version doesn't raise an exception but returns a SQLRETURN

   procedure SQLColAttribute (StatementHandle    : in  SQLHSTMT;
                              ColumnNumber       : in  SQLUSMALLINT;
                              FieldIdentifier    : in  SQL_DESC_FIELD_TYPE;
                              CharacterAttribute : out Wide_String;
                              NumericAttribute   : out SQLINTEGER);
   --  This version raises an exception if the SQLRETURN indicates an error
   pragma Inline (SQLColAttribute);

   --  ----------------------------------------------------------------------

   type DESC_ALLOC is (SQL_DESC_ALLOC_AUTO,
                       SQL_DESC_ALLOC_USER);
   for DESC_ALLOC use (SQL_DESC_ALLOC_AUTO  => 1,
                       SQL_DESC_ALLOC_USER  => 2);
   for DESC_ALLOC'Size use SQLSMALLINT'Size;

   package Dsp_DescAlloc is new
     Dispatch.A_Enumerated (SQL_DESC_ALLOC_TYPE,
                            DESC_ALLOC,
                            SQLSMALLINT,
                            "DESC_ALLOC");
   subtype Desc_Alloc_Type is Dsp_DescAlloc.Info;

   --  ----------------------------------------------------------------------

   package Dsp_Desc_Array_Status is new
     Dispatch.A_Array (ROW_STATUS,
                       Natural,
                       RowStatusArray,
                       RowStatusArrayPtr);
   subtype Desc_Array_Status_Info is Dsp_Desc_Array_Status.Info;

   --  ----------------------------------------------------------------------

   type Desc_Bind_Offsets is array (Natural) of aliased SQLINTEGER;
   pragma Convention (C, Desc_Bind_Offsets);
   type Desc_Bind_Offset_Ptr is access all Desc_Bind_Offsets;
   --  this is a non-fat pointer

   package Dsp_Bind_Offsets is new
     Dispatch.A_Array (SQLINTEGER,
                       Natural,
                       Desc_Bind_Offsets,
                       Desc_Bind_Offset_Ptr);
   subtype Desc_Bind_Offset_Info is Dsp_Bind_Offsets.Info;

   --  ----------------------------------------------------------------------

   package Dsp_Rows_Processed is new
     Dispatch.A_Pointer (SQLUINTEGER, PTR_SQLUINTEGER);
   subtype Desc_Rows_Processed is Dsp_Rows_Processed.Info;

   --  ----------------------------------------------------------------------
   type DataPtr is access all SQLPOINTER;

   package Dsp_Data_Ptr is new
     Dispatch.A_Pointer (SQLPOINTER, DataPtr);
   subtype Desc_Data_Ptr is Dsp_Data_Ptr.Info;

   --  ----------------------------------------------------------------------

   package Dsp_DateTime_Interval_Code is new
     Dispatch.A_Enumerated (SQL_DESC_DATETIME_INTERVAL_CODE,
                            SQL_DATETIME_SUBCODE,
                            SQLSMALLINT,
                            "SQL_DATETIME_SUBCODE");
   subtype Desc_DateTime_Interval_Code is Dsp_DateTime_Interval_Code.Info;

   --  ----------------------------------------------------------------------

   package Dsp_Int_Pointer is new
     Dispatch.A_Pointer (SQLINTEGER, PTR_SQLINTEGER);
   subtype Desc_Int_Pointer is Dsp_Int_Pointer.Info;

   --  ----------------------------------------------------------------------

   package Dsp_Nullable is new
     Dispatch.A_Enumerated (SQL_DESC_NULLABLE,
                            SQL_NULLABLE_INFO,
                            SQLSMALLINT,
                            "SQL_NULLABLE_INFO");
   subtype Desc_Nullable is Dsp_Nullable.Info;

   --  ----------------------------------------------------------------------

   package Dsp_Param_Type is new
     Dispatch.A_Enumerated (SQL_DESC_PARAMETER_TYPE,
                            SQL_Parameter_Type,
                            SQLSMALLINT,
                            "SQL_Parameter_Type");
   subtype Desc_Parameter_Type is Dsp_Param_Type.Info;


   --  ----------------------------------------------------------------------

   type SEARCHABLE_ATTRIBUTE is (SQL_UNSEARCHABLE,
                                 SQL_LIKE_ONLY,
                                 SQL_ALL_EXCEPT_LIKE,
                                 SQL_SEARCHABLE);
   for SEARCHABLE_ATTRIBUTE'Size use SQLSMALLINT'Size;

   SQL_PRED_SEARCHABLE : constant SEARCHABLE_ATTRIBUTE := SQL_SEARCHABLE;
   SQL_PRED_CHAR       : constant SEARCHABLE_ATTRIBUTE := SQL_LIKE_ONLY;
   SQL_PRED_BASIC      : constant SEARCHABLE_ATTRIBUTE := SQL_ALL_EXCEPT_LIKE;

   package Dsp_Searchable is new
     Dispatch.A_Enumerated (SQL_DESC_SEARCHABLE,
                            SEARCHABLE_ATTRIBUTE,
                            SQLSMALLINT,
                            "SEARCHABLE_ATTRIBUTE");
   subtype Desc_Searchable is Dsp_Searchable.Info;


   --  ----------------------------------------------------------------------

   type NAMED_ATTRIBUTE is (SQL_NAMED,
                            SQL_UNNAMED);
   for NAMED_ATTRIBUTE'Size use SQLSMALLINT'Size;

   package Dsp_UnNamed is new
     Dispatch.A_Enumerated (SQL_DESC_UNNAMED,
                            NAMED_ATTRIBUTE,
                            SQLSMALLINT,
                            "NAMED_ATTRIBUTE");
   subtype Desc_UnNamed is Dsp_UnNamed.Info;


   --  ----------------------------------------------------------------------

   type UPDATABLE_ATTRIBUTE is (SQL_ATTR_READONLY,
                                SQL_ATTR_WRITE,
                                SQL_ATTR_READWRITE_UNKNOWN);
   for UPDATABLE_ATTRIBUTE'Size use SQLSMALLINT'Size;

   package Dsp_Updatable is new
     Dispatch.A_Enumerated (SQL_DESC_UPDATABLE,
                            UPDATABLE_ATTRIBUTE,
                            SQLSMALLINT,
                            "UPDATABLE_ATTRIBUTE");
   subtype Desc_Updatable is Dsp_Updatable.Info;


   --  ----------------------------------------------------------------------
   --  SQL_BIND_TYPE options
   SQL_BIND_BY_COLUMN    : constant Desc_Field_Int :=
     (Attribute => SQL_DESC_BIND_TYPE,
      Value => 0);

   SQL_BIND_TYPE_DEFAULT : constant Desc_Field_Int :=
     SQL_BIND_BY_COLUMN;


end GNU.DB.SQLCLI.Desc;
