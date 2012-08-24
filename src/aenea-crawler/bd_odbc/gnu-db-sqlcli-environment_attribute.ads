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
with GNU.DB.SQLCLI;
with GNU.DB.SQLCLI.Generic_Attr;
with GNU.DB.SQLCLI.Dispatch;
with GNU.DB.SQLCLI.Dispatch.A_Unsigned;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
with GNU.DB.SQLCLI.Dispatch.A_Boolean;

pragma Elaborate_All (GNU.DB.SQLCLI.Generic_Attr);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Unsigned);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Enumerated);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Boolean);

package GNU.DB.SQLCLI.Environment_Attribute is

   type SQL_ENVIRONMENT_ATTRIBUTE is (SQL_ATTR_ODBC_VERSION,
                                      SQL_ATTR_CONNECTION_POOLING,
                                      SQL_ATTR_CP_MATCH,
                                      SQL_ATTR_OUTPUT_NTS);
   for SQL_ENVIRONMENT_ATTRIBUTE use (SQL_ATTR_ODBC_VERSION        => 200,
                                      SQL_ATTR_CONNECTION_POOLING  => 201,
                                      SQL_ATTR_CP_MATCH            => 202,
                                      SQL_ATTR_OUTPUT_NTS          => 10001);
   for SQL_ENVIRONMENT_ATTRIBUTE'Size use SQLINTEGER'Size;

   procedure Get_Env_Attr (EnvironmentHandle : in SQLHENV;
                           Attribute         : in SQL_ENVIRONMENT_ATTRIBUTE;
                           Value             : in SQLPOINTER;
                           Length            : in out SQLINTEGER;
                           Data              : in SQLSMALLINT;
                           ErrorCode         : access SQLRETURN);
   pragma Inline (Get_Env_Attr);

   procedure Set_Env_Attr (EnvironmentHandle : in  SQLHENV;
                           Attribute         : in  SQL_ENVIRONMENT_ATTRIBUTE;
                           Value             : in  SQLPOINTER;
                           Length            : in  SQLINTEGER;
                           Data              : in  SQLSMALLINT;
                           ErrorCode         : out SQLRETURN);
   pragma Inline (Set_Env_Attr);
   --  Despite what the ODBC API says, apparently 'Value' must be the
   --  _value_ of the attribute, not a pointer to the value! However,
   --  we must use SQLPOINTER here to match the profile for
   --  GNU.DB.SQLCLI.Generic_Attr. There's a hack in
   --  GNU.DB.SQLCLI.Generic_Attr.Enumerated_Attribute.SetAttr to pass
   --  the value as SQLPOINTER (FIXME!).

   package Environment_Attributes is new GNU.DB.SQLCLI.Generic_Attr
     (Context         => SQLHENV,
      T               => SQL_ENVIRONMENT_ATTRIBUTE,
      Base            => SQLINTEGER,
      Aux             => SQLSMALLINT,
      Get             => Get_Env_Attr,
      Set             => Set_Env_Attr,
      Default_Context => Null_Handle);
   subtype
     Environment_Attribute is Environment_Attributes.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Environment_Attributes);

   package EA_String is new Dispatch.A_String;
   subtype Environment_Attribute_String is EA_String.Info;

   package EA_Unsigned is new Dispatch.A_Unsigned (SQLUINTEGER);
   subtype Environment_Attribute_Unsigned is EA_Unsigned.Info;

   package EA_Boolean is new Dispatch.A_Boolean (SQLUINTEGER);
   subtype Environment_Attribute_Boolean is EA_Boolean.Info;


   --  ----------------------------------------------------------------------


   type CONNECTION_POOLING is (SQL_CP_OFF,
                               SQL_CP_ONE_PER_DRIVER,
                               SQL_CP_ONE_PER_HENV);
   for CONNECTION_POOLING'Size use SQLINTEGER'Size;

   SQL_CP_DEFAULT : constant CONNECTION_POOLING := SQL_CP_OFF;

   package Dsp_Connection_Pooling is new
     Dispatch.A_Enumerated (SQL_ATTR_CONNECTION_POOLING,
                            CONNECTION_POOLING,
                            SQLINTEGER,
                            "CONNECTION_POOLING");
   subtype Environment_Attribute_Pooling is
     Dsp_Connection_Pooling.Info;


   --  ----------------------------------------------------------------------


   type CONNECTION_POOLING_MATCH is (SQL_CP_STRICT_MATCH,
                                     SQL_CP_RELAXED_MATCH);
   for CONNECTION_POOLING_MATCH'Size use SQLINTEGER'Size;

   SQL_CP_MATCH_DEFAULT : constant CONNECTION_POOLING_MATCH
     := SQL_CP_STRICT_MATCH;

   package Dsp_Connection_Pooling_Match is new
     Dispatch.A_Enumerated (SQL_ATTR_CP_MATCH,
                            CONNECTION_POOLING_MATCH,
                            SQLINTEGER,
                            "CONNECTION_POOLING_MATCH");
   subtype Environment_Attribute_Pooling_Match is
     Dsp_Connection_Pooling_Match.Info;


   --  ----------------------------------------------------------------------


   type SQL_ODBC_VERSION is (SQL_OV_ODBC2,
                             SQL_OV_ODBC3);
   for SQL_ODBC_VERSION use (SQL_OV_ODBC2   => 2,
                             SQL_OV_ODBC3   => 3);
   for SQL_ODBC_VERSION'Size use SQLINTEGER'Size;

   package Dsp_SQL_ODBC_Version is new
     Dispatch.A_Enumerated (SQL_ATTR_ODBC_VERSION,
                            SQL_ODBC_VERSION,
                            SQLINTEGER,
                            "SQL_ODBC_VERSION");
   subtype Environment_Attribute_ODBC_Version is
     Dsp_SQL_ODBC_Version.Info;


   --  ----------------------------------------------------------------------


   function SQLSetEnvAttr
     (EnvironmentHandle : SQLHENV;
      AttrRec           : Environment_Attribute'Class) return SQLRETURN;

   procedure SQLSetEnvAttr
     (EnvironmentHandle : in SQLHENV;
      AttrRec           : in Environment_Attribute'Class);
   --  Sets an environment attribute.

   function SQLGetEnvAttr
     (EnvironmentHandle : SQLHENV;
      Attribute         : SQL_ENVIRONMENT_ATTRIBUTE;
      MaxLength         : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode         : access SQLRETURN)
      return Environment_Attribute'Class;
   --  This version returns an ErrorCode when an ODBC error occurs

   function SQLGetEnvAttr
     (EnvironmentHandle : SQLHENV;
      Attribute         : SQL_ENVIRONMENT_ATTRIBUTE;
      MaxLength         : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Environment_Attribute'Class;
   --  This version raises an exception when an ODBC error occurs

end GNU.DB.SQLCLI.Environment_Attribute;
