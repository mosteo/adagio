--|
--| Filename        : $Source: /cvsroot/gnade/gnade/esql/sql_standard.ads,v $
--| Description     : Scanner for the embedded SQL translator
--| Author          : Michael Erdmann
--| Created On      : 22.12.2000
--| Last Modified By: $Author: stephen_leake $
--| Last Modified On: $Date: 2003/09/28 17:36:42 $
--| Status          : $State: Exp $
--|
--| Copyright (C) 2000, 2002 Michael Erdmann
--|
--| This program is free software; you can redistribute it and/or
--| modify it under the terms of the GNU General Public License
--| as published by the Free Software Foundation; either version 2
--| of the License, or (at your option) any later version.
--|
--| This program/code is distributed in the hope that it will be useful,
--| but WITHOUT ANY WARRANTY; without even the implied warranty of
--| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--| GNU General Public License for more details.
--|
--| You should have received a copy of the GNU General Public License along
--| with this program; if not, write to the Free Software Foundation, Inc.,
--| 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--|
--| As a special exception,  if other files  instantiate  generics from this
--| unit, or you link  this unit with other files  to produce an executable,
--| this  unit  does not  by itself cause  the resulting  executable  to  be
--| covered  by the  GNU  General  Public  License.  This exception does not
--| however invalidate  any other reasons why  the executable file  might be
--| covered by the  GNU Public License.
--|
--| Functional Description
--| ======================
--| This package contains the SQL types as they are required
--| by the ISO/92 standart. it links the ODBC data types with the
--| SQL/92 data types.
--| This file exists only for compatibilty with the ISO/92
--| document.
--|
--| Restrictions
--| ============
--|
--| References
--| ==========
--|
--| /1/ - (Second Informal Review Draft) ISO/IEC 9075:1992, Database
--|       Language SQL- July 30, 1992
--|
--| History
--| =======
--|
--|
with GNU.DB.SQLCLI; use GNU.DB.SQLCLI;

package SQL_STANDARD is

   package SQL renames GNU.DB.SQLCLI;

   type    CHAR             is array( Positive range <>) of SQL.SQLCHAR;
   type    BIT              is array (Natural range <>) of SQL.SQL_BOOLEAN;

   subtype SMALLINT         is SQL.SQLSMALLINT;
   subtype INT              is SQL.SQLINTEGER;

   subtype REAL             is SQL.SQLREAL;
   subtype DOUBLE_PRECISION is SQL.SQLDOUBLE;
   subtype INDICATOR_TYPE   is SQL.SQLINTEGER;

   subtype SQLCODE_TYPE     is SQL.SQLRETURN;
   subtype SQL_ERROR        is SQLCODE_TYPE range SQLCODE_TYPE'First .. -1;
   subtype NOT_FOUND        is SQLCODE_TYPE range 100 .. 100;
   subtype SQLSTATE_TYPE    is SQL.SQLSTATE;

   package SQLSTATE_CODES is

      AMBIGUOUS_CURSOR_NAME_NO_SUBCLASS :
         constant SQLSTATE_TYPE :="3C000";
      CARDINALITY_VIOLATION_NO_SUBCLASS :
         constant SQLSTATE_TYPE :="21000";
      CONNECTION_EXCEPTION_NO_SUBCLASS :
         constant SQLSTATE_TYPE :="08000";
      CONNECTION_EXCEPTION_CONNECTION_DOES_NOT_EXIST :
         constant SQLSTATE_TYPE :="08003";

      CONNECTION_EXCEPTION_CONNECTION_FAILURE :
         constant SQLSTATE_TYPE :="08006";
      CONNECTION_EXCEPTION_CONNECTION_NAME_IN_USE :
         constant SQLSTATE_TYPE :="08002";
      CONNECTION_EXCEPTION_SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION:
         constant SQLSTATE_TYPE :="08001";
      CONNECTION_EXCEPTION_SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION:
         constant SQLSTATE_TYPE :="08004";
      CONNECTION_EXCEPTION_TRANSACTION_RESOLUTION_UNKNOWN :
         constant SQLSTATE_TYPE :="08007";
      DATA_EXCEPTION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="22000";
      DATA_EXCEPTION_CHARACTER_NOT_IN_REPERTOIRE:
         constant SQLSTATE_TYPE :="22021";

      DATA_EXCEPTION_DATETIME_FIELD_OVERFLOW:
         constant SQLSTATE_TYPE :="22008";
      DATA_EXCEPTION_DIVISION_BY_ZERO:
         constant SQLSTATE_TYPE :="22012";
      DATA_EXCEPTION_ERROR_IN_ASSIGNMENT:
         constant SQLSTATE_TYPE :="22005";
      DATA_EXCEPTION_INDICATOR_OVERFLOW:
         constant SQLSTATE_TYPE :="22022";
      DATA_EXCEPTION_INTERVAL_FIELD_OVERFLOW:
         constant SQLSTATE_TYPE :="22015";
      DATA_EXCEPTION_INVALID_CHARACTER_VALUE_FOR_CAST:
         constant SQLSTATE_TYPE :="22018";
      DATA_EXCEPTION_INVALID_DATETIME_FORMAT:
         constant SQLSTATE_TYPE :="22007";
      DATA_EXCEPTION_INVALID_ESCAPE_CHARACTER:
         constant SQLSTATE_TYPE :="22019";
      DATA_EXCEPTION_INVALID_ESCAPE_SEQUENCE:
         constant SQLSTATE_TYPE :="22025";
      DATA_EXCEPTION_INVALID_PARAMETER_VALUE:
         constant SQLSTATE_TYPE :="22023";
      DATA_EXCEPTION_INVALID_TIME_ZONE_DISPLACEMENT_VALUE:
         constant SQLSTATE_TYPE :="22009";
      DATA_EXCEPTION_NULL_VALUE_NO_INDICATOR_PARAMETER:
         constant SQLSTATE_TYPE :="22002";
      DATA_EXCEPTION_NUMERIC_VALUE_OUT_OF_RANGE:
         constant SQLSTATE_TYPE :="22003";
      DATA_EXCEPTION_STRING_DATA_LENGTH_MISMATCH:
         constant SQLSTATE_TYPE :="22026";
      DATA_EXCEPTION_STRING_DATA_RIGHT_TRUNCATION:
         constant SQLSTATE_TYPE :="22001";
      DATA_EXCEPTION_SUBSTRING_ERROR:
         constant SQLSTATE_TYPE :="22011";
      DATA_EXCEPTION_TRIM_ERROR:
         constant SQLSTATE_TYPE :="22027";
      DATA_EXCEPTION_UNTERMINATED_C_STRING:
         constant SQLSTATE_TYPE :="22024";
      DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="2B000";
      DYNAMIC_SQL_ERROR_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="07000";
      DYNAMIC_SQL_ERROR_CURSOR_SPECIFICATION_CANNOT_BE_EXECUTED:
         constant SQLSTATE_TYPE :="07003";
      DYNAMIC_SQL_ERROR_INVALID_DESCRIPTOR_COUNT:
         constant SQLSTATE_TYPE :="07008";
      DYNAMIC_SQL_ERROR_INVALID_DESCRIPTOR_INDEX:
         constant SQLSTATE_TYPE :="07009";
      DYNAMIC_SQL_ERROR_PREPARED_STATEMENT_NOT_A_CURSOR_SPECIFICATION:
         constant SQLSTATE_TYPE :="07005";
      DYNAMIC_SQL_ERROR_RESTRICTED_DATA_TYPE_ATTRIBUTE_VIOLATION:
         constant SQLSTATE_TYPE :="07006";
      DYNAMIC_SQL_ERROR_USING_CLAUSE_DOES_NOT_MATCH_DYNAMIC_PARAMETER_SPEC:
         constant SQLSTATE_TYPE :="07001";
      DYNAMIC_SQL_ERROR_USING_CLAUSE_DOES_NOT_MATCH_TARGET_SPEC:
         constant SQLSTATE_TYPE :="07002";
      DYNAMIC_SQL_ERROR_USING_CLAUSE_REQUIRED_FOR_DYNAMIC_PARAMETERS:
         constant SQLSTATE_TYPE :="07004";
      DYNAMIC_SQL_ERROR_USING_CLAUSE_REQUIRED_FOR_RESULT_FIELDS:
         constant SQLSTATE_TYPE :="07007";
      FEATURE_NOT_SUPPORTED_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="0A000";
      FEATURE_NOT_SUPPORTED_MULTIPLE_ENVIRONMENT_TRANSACTIONS:
         constant SQLSTATE_TYPE :="0A001";
      INTEGRITY_CONSTRAINT_VIOLATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="23000";
      INVALID_AUTHORIZATION_SPECIFICATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="28000";
      INVALID_CATALOG_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="3D000";
      INVALID_CHARACTER_SET_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="2C000";
      INVALID_CONDITION_NUMBER_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="35000";
      INVALID_CONNECTION_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="2E000";
      INVALID_CURSOR_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="34000";
      INVALID_CURSOR_STATE_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="24000";
      INVALID_SCHEMA_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="3F000";
      INVALID_SQL_DESCRIPTOR_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="33000";
      INVALID_SQL_STATEMENT_NAME_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="26000";
      INVALID_TRANSACTION_STATE_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="25000";
      INVALID_TRANSACTION_TERMINATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="2D000";
      NO_DATA_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="02000";
      REMOTE_DATABASE_ACCESS_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="HZ000";
      SUCCESSFUL_COMPLETION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="00000";
      SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="42000";
      SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION_IN_DIRECT_STATEMENT_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="2A000";
      SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION_IN_DYNAMIC_STATEMENT_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="37000";
      TRANSACTION_ROLLBACK_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="40000";
      TRANSACTION_ROLLBACK_INTEGRITY_CONSTRAINT_VIOLATION:
         constant SQLSTATE_TYPE :="40002";
      TRANSACTION_ROLLBACK_SERIALIZATION_FAILURE:
         constant SQLSTATE_TYPE :="40001";
      TRANSACTION_ROLLBACK_STATEMENT_COMPLETION_UNKNOWN:
         constant SQLSTATE_TYPE :="40003";
      TRIGGERED_DATA_CHANGE_VIOLATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="27000";
      WARNING_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="01000";
      WARNING_CURSOR_OPERATION_CONFLICT:
         constant SQLSTATE_TYPE :="01001";
      WARNING_DISCONNECT_ERROR:
         constant SQLSTATE_TYPE :="01002";
      WARNING_IMPLICIT_ZERO_BIT_PADDING:
         constant SQLSTATE_TYPE :="01008";
      WARNING_INSUFFICIENT_ITEM_DESCRIPTOR_AREAS:
         constant SQLSTATE_TYPE :="01005";
      WARNING_NULL_VALUE_ELIMINATED_IN_SET_FUNCTION:
         constant SQLSTATE_TYPE :="01003";
      WARNING_PRIVILEGE_NOT_GRANTED:
         constant SQLSTATE_TYPE :="01007";
      WARNING_PRIVILEGE_NOT_REVOKED:
         constant SQLSTATE_TYPE :="01006";
      WARNING_QUERY_EXPRESSION_TOO_LONG_FOR_INFORMATION_SCHEMA:
         constant SQLSTATE_TYPE :="0100A";
      WARNING_SEARCH_CONDITION_TOO_LONG_FOR_INFORMATION_SCHEMA:
         constant SQLSTATE_TYPE :="01009";
      WARNING_STRING_DATA_RIGHT_TRUNCATION_WARNING:
         constant SQLSTATE_TYPE :="01004";
      WITH_CHECK_OPTION_VIOLATION_NO_SUBCLASS:
         constant SQLSTATE_TYPE :="44000";
   end SQLSTATE_CODES;

   -- GNADE SPECIFIC DATA TYPES
   package GNADE is

      type VARCHAR( Max : Positive )  is record
            Value  : aliased CHAR( 1..Max ) := (others => 32);
            Length : aliased INDICATOR_TYPE := 0;
      end record;

      type BINARY is array( Positive range <>) of SQL.SQLCHAR;

      type VARBINARY( Max : Positive ) is record
            Value  : aliased GNADE.BINARY( 1..Max ) ;
            Length : aliased INDICATOR_TYPE := 0;
      end record;

   end GNADE;
end SQL_STANDARD;














