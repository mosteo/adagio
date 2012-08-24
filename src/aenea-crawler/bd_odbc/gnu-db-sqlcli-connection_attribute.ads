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
with GNU.DB.SQLCLI.Dispatch.A_Unsigned;
with GNU.DB.SQLCLI.Dispatch.A_String;
with GNU.DB.SQLCLI.Dispatch.A_Wide_String;
with GNU.DB.SQLCLI.Dispatch.A_Enumerated;
with GNU.DB.SQLCLI.Dispatch.A_Boolean;

pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Unsigned);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Wide_String);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Enumerated);
pragma Elaborate_All (GNU.DB.SQLCLI.Dispatch.A_Boolean);

package GNU.DB.SQLCLI.Connection_Attribute is

   type SQL_CONNECTION_ATTRIBUTE is (SQL_ATTR_ASYNC_ENABLE,
                                     SQL_ATTR_ACCESS_MODE,
                                     SQL_ATTR_AUTOCOMMIT,
                                     SQL_ATTR_LOGIN_TIMEOUT,
                                     SQL_ATTR_TRACE,
                                     SQL_ATTR_TRACEFILE,
                                     SQL_ATTR_TRANSLATE_LIB,
                                     SQL_ATTR_TRANSLATE_OPTION,
                                     SQL_ATTR_TXN_ISOLATION,
                                     SQL_ATTR_CURRENT_CATALOG,
                                     SQL_ATTR_ODBC_CURSORS,
                                     SQL_ATTR_QUIET_MODE,
                                     SQL_ATTR_PACKET_SIZE,
                                     SQL_ATTR_CONNECTION_TIMEOUT,
                                     SQL_ATTR_DISCONNECT_BEHAVIOR,
                                     SQL_ATTR_ENLIST_IN_DTC,
                                     SQL_ATTR_ENLIST_IN_XA,
                                     SQL_ATTR_CONNECTION_DEAD,
                                     SQL_ATTR_AUTO_IPD,
                                     SQL_ATTR_METADATA_ID);
   for SQL_CONNECTION_ATTRIBUTE use (SQL_ATTR_ASYNC_ENABLE         => 4,
                                     SQL_ATTR_ACCESS_MODE          => 101,
                                     SQL_ATTR_AUTOCOMMIT           => 102,
                                     SQL_ATTR_LOGIN_TIMEOUT        => 103,
                                     SQL_ATTR_TRACE                => 104,
                                     SQL_ATTR_TRACEFILE            => 105,
                                     SQL_ATTR_TRANSLATE_LIB        => 106,
                                     SQL_ATTR_TRANSLATE_OPTION     => 107,
                                     SQL_ATTR_TXN_ISOLATION        => 108,
                                     SQL_ATTR_CURRENT_CATALOG      => 109,
                                     SQL_ATTR_ODBC_CURSORS         => 110,
                                     SQL_ATTR_QUIET_MODE           => 111,
                                     SQL_ATTR_PACKET_SIZE          => 112,
                                     SQL_ATTR_CONNECTION_TIMEOUT   => 113,
                                     SQL_ATTR_DISCONNECT_BEHAVIOR  => 114,
                                     SQL_ATTR_ENLIST_IN_DTC        => 1207,
                                     SQL_ATTR_ENLIST_IN_XA         => 1208,
                                     SQL_ATTR_CONNECTION_DEAD      => 1209,
                                     SQL_ATTR_AUTO_IPD             => 10001,
                                     SQL_ATTR_METADATA_ID          => 10014);
   for SQL_CONNECTION_ATTRIBUTE'Size use SQLINTEGER'Size;

   --  Some old names
   SQL_ACCESS_MODE           : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_ACCESS_MODE;
   SQL_AUTOCOMMIT            : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_AUTOCOMMIT;
   SQL_CURRENT_QUALIFIER     : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_CURRENT_CATALOG;
   SQL_LOGIN_TIMEOUT         : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_LOGIN_TIMEOUT;
   SQL_ODBC_CURSORS          : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_ODBC_CURSORS;
   SQL_PACKET_SIZE           : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_PACKET_SIZE;
   SQL_QUIET_MODE            : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_QUIET_MODE;
   SQL_OPT_TRACE             : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_TRACE;
   SQL_OPT_TRACEFILE         : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_TRACEFILE;
   SQL_TRANSLATE_DLL         : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_TRANSLATE_LIB;
   SQL_TRANSLATE_OPTION      : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_TRANSLATE_OPTION;
   SQL_TXN_ISOLATION         : constant SQL_CONNECTION_ATTRIBUTE
     := SQL_ATTR_TXN_ISOLATION;

   procedure Get_Connect_Attr (ConnectionHandle : in SQLHDBC;
                               Attribute        : in SQL_CONNECTION_ATTRIBUTE;
                               Value            : in SQLPOINTER;
                               Length           : in out SQLINTEGER;
                               Data             : in SQLSMALLINT;
                               ErrorCode        : access SQLRETURN);
   pragma Inline (Get_Connect_Attr);

   procedure Set_Connect_Attr (ConnectionHandle : in SQLHDBC;
                               Attribute        : in SQL_CONNECTION_ATTRIBUTE;
                               Value            : in SQLPOINTER;
                               Length           : in SQLINTEGER;
                               Data             : in SQLSMALLINT;
                               ErrorCode        : out SQLRETURN);
   pragma Inline (Set_Connect_Attr);

   package Connection_Attributes is new GNU.DB.SQLCLI.Generic_Attr
     (Context         => SQLHDBC,
      T               => SQL_CONNECTION_ATTRIBUTE,
      Base            => SQLINTEGER,
      Aux             => SQLSMALLINT,
      Get             => Get_Connect_Attr,
      Set             => Set_Connect_Attr,
      Default_Context => Null_Handle);
   subtype Connection_Attribute is Connection_Attributes.Attribute_Value_Pair;

   package Dispatch is new GNU.DB.SQLCLI.Dispatch (Connection_Attributes);

   package CA_String is new Dispatch.A_String;
   subtype Connection_Attribute_String is CA_String.Info;

   package CA_WString is new Dispatch.A_Wide_String;
   subtype Connection_Attribute_Wide_String is CA_WString.Info;

   function SQLGetConnectAttr
     (ConnectionHandle : SQLHDBC;
      Attribute        : SQL_CONNECTION_ATTRIBUTE;
      MaxLength        : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH;
      ErrorCode        : access SQLRETURN)
     return Connection_Attribute'Class;

   function SQLGetConnectAttr
     (ConnectionHandle : SQLHDBC;
      Attribute        : SQL_CONNECTION_ATTRIBUTE;
      MaxLength        : SQLSMALLINT := SQL_MAX_OPTION_STRING_LENGTH)
     return Connection_Attribute'Class;

   function SQLSetConnectAttr
     (ConnectionHandle : in SQLHDBC;
      AttrRec          : in Connection_Attribute'Class) return SQLRETURN;

   procedure SQLSetConnectAttr
     (ConnectionHandle : in SQLHDBC;
      AttrRec          : in Connection_Attribute'Class);

   package CA_Unsigned is new Dispatch.A_Unsigned (SQLUINTEGER);
   subtype Connection_Attribute_Unsigned is CA_Unsigned.Info;

   package CA_Boolean is new Dispatch.A_Boolean (SQLUINTEGER);
   subtype Connection_Attribute_Boolean is CA_Boolean.Info;

   --  ---------------------------------------------------------------------
   --  SQL_ATTR_CONNECTION_DEAD Values
   SQL_CD_TRUE  : constant Boolean := True;
   SQL_CD_FALSE : constant Boolean := False;

   --  SQL_LOGIN_TIMEOUT options
   SQL_LOGIN_TIMEOUT_DEFAULT : constant SQLUINTEGER := 15;

   --  ----------------------------------------------------------------------


   --  SQL_ACCESS_MODE values
   type ACCESS_MODE is (SQL_MODE_READ_WRITE,
                        SQL_MODE_READ_ONLY);
   for ACCESS_MODE'Size use SQLINTEGER'Size;

   SQL_MODE_DEFAULT : constant ACCESS_MODE := SQL_MODE_READ_WRITE;

   package Dsp_Access_Mode is new
     Dispatch.A_Enumerated (SQL_ATTR_ACCESS_MODE,
                            ACCESS_MODE,
                            SQLINTEGER,
                            "ACCESS_MODE");
   subtype Connection_Attribute_Mode is Dsp_Access_Mode.Info;


   --  ----------------------------------------------------------------------


   type ASYNC_ENABLE is (SQL_ASYNC_ENABLE_OFF,
                         SQL_ASYNC_ENABLE_ON);
   --  |
   --  | values for SQL_ATTR_ASYNC_ENABLE
   --  |
   for ASYNC_ENABLE'Size use SQLINTEGER'Size;
   SQL_ASYNC_ENABLE_DEFAULT : constant ASYNC_ENABLE := SQL_ASYNC_ENABLE_OFF;

   package Dsp_Async_Enable is new
     Dispatch.A_Enumerated (SQL_ATTR_ASYNC_ENABLE,
                            ASYNC_ENABLE,
                            SQLINTEGER,
                            "ASYNC_ENABLE");
   subtype Connection_Attribute_Async is Dsp_Async_Enable.Info;


   --  ----------------------------------------------------------------------


   type AUTOCOMMIT is (SQL_AUTOCOMMIT_OFF,
                       SQL_AUTOCOMMIT_ON);
   for AUTOCOMMIT'Size use SQLINTEGER'Size;

   SQL_AUTOCOMMIT_DEFAULT : constant AUTOCOMMIT := SQL_AUTOCOMMIT_ON;

   package Dsp_AutoCommit is new
     Dispatch.A_Enumerated (SQL_ATTR_AUTOCOMMIT,
                            AUTOCOMMIT,
                            SQLINTEGER,
                            "AUTOCOMMIT");
   subtype Connection_Attribute_AutoCommit is Dsp_AutoCommit.Info;


   --  ----------------------------------------------------------------------


   type CURSOR_USE is (SQL_CUR_USE_IF_NEEDED,
                       SQL_CUR_USE_ODBC,
                       SQL_CUR_USE_DRIVER);
   for CURSOR_USE'Size use SQLINTEGER'Size;
   SQL_CUR_DEFAULT : constant CURSOR_USE := SQL_CUR_USE_DRIVER;

   package Dsp_Cursors is new
     Dispatch.A_Enumerated (SQL_ATTR_ODBC_CURSORS,
                            CURSOR_USE,
                            SQLINTEGER,
                            "CURSOR_USE");
   subtype Connection_Attribute_ODBC_Cursors is Dsp_Cursors.Info;


   --  ----------------------------------------------------------------------


   type DISCONNECT_BEHAVIOR is (SQL_DB_RETURN_TO_POOL,
                                SQL_DB_DISCONNECT);
   for DISCONNECT_BEHAVIOR'Size use SQLINTEGER'Size;

   SQL_DB_DEFAULT : constant DISCONNECT_BEHAVIOR := SQL_DB_RETURN_TO_POOL;

   package Dsp_Disconnect_Behavior is new
     Dispatch.A_Enumerated (SQL_ATTR_DISCONNECT_BEHAVIOR,
                            DISCONNECT_BEHAVIOR,
                            SQLINTEGER,
                            "DISCONNECT_BEHAVIOR");
   subtype Connection_Attribute_Disconnect_Behavior is
     Dsp_Disconnect_Behavior.Info;


   --  ----------------------------------------------------------------------


   type OPT_TRACE is (SQL_OPT_TRACE_OFF,
                      SQL_OPT_TRACE_ON);
   for OPT_TRACE'Size use SQLINTEGER'Size;

   SQL_OPT_TRACE_DEFAULT : constant OPT_TRACE := SQL_OPT_TRACE_OFF;

   package Dsp_Trace is new
     Dispatch.A_Enumerated (SQL_ATTR_TRACE,
                            OPT_TRACE,
                            SQLINTEGER,
                            "OPT_TRACE");
   subtype Connection_Attribute_Trace is Dsp_Trace.Info;


   --  ----------------------------------------------------------------------


   type TXN_ISOLATION_OPTION is (SQL_TRANSACTION_READ_UNCOMMITTED,
                                 SQL_TRANSACTION_READ_COMMITTED,
                                 SQL_TRANSACTION_REPEATABLE_READ,
                                 SQL_TRANSACTION_SERIALIZABLE);
   for TXN_ISOLATION_OPTION use (SQL_TRANSACTION_READ_UNCOMMITTED => 1,
                                 SQL_TRANSACTION_READ_COMMITTED   => 2,
                                 SQL_TRANSACTION_REPEATABLE_READ  => 4,
                                 SQL_TRANSACTION_SERIALIZABLE     => 8);
   for TXN_ISOLATION_OPTION'Size use SQLUINTEGER'Size;

   SQL_TXN_READ_UNCOMMITTED : constant TXN_ISOLATION_OPTION :=
     SQL_TRANSACTION_READ_UNCOMMITTED;

   SQL_TXN_READ_COMMITTED   : constant TXN_ISOLATION_OPTION :=
     SQL_TRANSACTION_READ_COMMITTED;

   SQL_TXN_REPEATABLE_READ  : constant TXN_ISOLATION_OPTION :=
     SQL_TRANSACTION_REPEATABLE_READ;

   SQL_TXN_SERIALIZABLE     : constant TXN_ISOLATION_OPTION :=
     SQL_TRANSACTION_SERIALIZABLE;

   package Dsp_TXN_Isolation is new
     Dispatch.A_Enumerated (SQL_ATTR_TXN_ISOLATION,
                            TXN_ISOLATION_OPTION,
                            SQLINTEGER,
                            "Info.TXN_ISOLATION_OPTION");
   subtype Connection_Attribute_TXN_Isolation is Dsp_TXN_Isolation.Info;


   --  ----------------------------------------------------------------------

end GNU.DB.SQLCLI.Connection_Attribute;

