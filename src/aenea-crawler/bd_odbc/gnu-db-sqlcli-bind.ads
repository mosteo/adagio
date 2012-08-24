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
generic
   type Variable_Type is limited private;
   type Variable_Type_Pointer is access all Variable_Type;
package GNU.DB.SQLCLI.Bind is
   function SQLBindCol (StatementHandle : SQLHSTMT;
                        ColumnNumber    : SQL_Column_Number;
                        TargetType      : SQL_C_DATA_TYPE;
                        TargetValue     : Variable_Type_Pointer;
                        BufferLength    : SQLINTEGER;
                        pStrLen_or_Ind  : access SQLINTEGER) return SQLRETURN;

   procedure SQLBindCol (StatementHandle  : in SQLHSTMT;
                         ColumnNumber     : in SQL_Column_Number;
                         TargetType       : in SQL_C_DATA_TYPE;
                         TargetValuePtr   : in Variable_Type_Pointer;
                         BufferLength     : in SQLINTEGER;
                         StrLen_Or_IndPtr : access SQLINTEGER);
   pragma Inline_Always (SQLBindCol);

   function SQLBindParameter (StatementHandle  : in SQLHSTMT;
                              ParameterNumber  : in SQL_Parameter_Number;
                              InputOutputType  : in SQL_Parameter_Type;
                              ValueType        : in SQL_C_DATA_TYPE;
                              ParameterType    : in SQL_DATA_TYPE;
                              ColumnSize       : in SQLUINTEGER;
                              DecimalDigits    : in SQLSMALLINT;
                              Value            : in Variable_Type_Pointer;
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
                               Value            : in Variable_Type_Pointer;
                               BufferLength     : in SQLINTEGER;
                               StrLen_Or_IndPtr : access SQLINTEGER);
   pragma Inline_Always (SQLBindParameter);

end GNU.DB.SQLCLI.Bind;
