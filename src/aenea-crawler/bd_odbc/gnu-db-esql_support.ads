-------------------------------------------------------------------------------
--  Filename        : $Source: /cvsroot/gnade/gnade/esql/gnu-db-esql_support.ads,v $
--  Description     : Support package for ISO92 and GNADE ESQL types         --
--  Author          : Michael Erdmann                                        --
--  Created         : 22.12.2000                                             --
--  Last Modified By: $Author: stephen_leake $
--  Last Modified On: $Date: 2003/09/28 17:36:16 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2000, 2002 Michael Erdmann                                       --
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains support procedures for ISO92 and GNADE SQL types   --
--  It is intended to simplify the development of ESQL applications.         --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
--  Author                                                                   --
--  ======                                                                   --
--                                                                           --
--  Author: Michael Erdmann <michael.erdmann@snafu.de>                       --
--                                                                           --
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
-------------------------------------------------------------------------------

with SQL_STANDARD;                      use SQL_STANDARD;
with System;                            use System;

package GNU.DB.ESQL_Support is

   -- ******************************************************************** --
   -- *                    SQL Communication Area                        * --
   -- *                    ======================                        * --
   -- *                                                                  * --
   -- * This type is only provided for Oracle compatibilty. In the moment* --
   -- * it is not clear what kind of information will be available with  * --
   -- * ODBC.                                                            * --
   -- ******************************************************************** --

   type SQLCA_Type is record
         Message       : aliased String(1..255 );
         State         : aliased SQLSTATE_TYPE;
         SqlCode       : aliased SQLCODE_TYPE;
         Affected_Rows : aliased Integer := 0;
   end record;

   type SQLCA_Access is access all SQLCA_Type ;


   SUCCESS_CLASS   : constant String := "00" ;
   WARNING_CLASS   : constant String := "01" ;
   NOTFOUND_CLASS  : constant String := "02" ;
   ERROR_CLASS     : constant String := "HY" ;

   --|
   --| This procedure is used by the esql code generator internally
   --| to print out the result of a query after an ODBC command
   --| has been issued.
   --|
   procedure DEBUG(
      Where : in String;
      Query : in String;
      RC    : in System.Address);

   function Is_Null (
      s : SQL_STANDARD.INDICATOR_TYPE ) return Boolean;

   -- ******************************************************************** --
   -- *                    S U P P O R T    F O R                        * --
   -- *                        SQL_STANDARD.CHAR                         * --
   -- ******************************************************************** --

   function To_String(
      Item   : in SQL_STANDARD.CHAR ) return String;

   procedure To_String(
      Item   : in SQL_STANDARD.CHAR;
      Target : out String );

   procedure Move(
      S      : in String ;
      C      : out SQL_STANDARD.CHAR );

   --- ******************************************************************* ---
   --- *                    S U P P O R T    F O R                       * ---
   --- *                        GNADE.VARCHAR                            * ---
   --- ******************************************************************* ---

   Is_Empty : exception;

   function To_String(
      This   : in GNADE.VARCHAR ) return String;

   procedure To_VARCHAR(
      Value  : in String;
      Result : out GNADE.VARCHAR);

   function Is_Null(
      This   : in GNADE.VARCHAR ) return Boolean;

   function Length(
      This   : in GNADE.VARCHAR ) return Natural;

   function To_Binary(
      Src    : in String ) return GNADE.BINARY;

   function To_Binary(
      This   : in GNADE.VARBINARY ) return GNADE.BINARY;

   procedure To_Binary(
      This   : in GNADE.VARBINARY;
      Dest   : out GNADE.BINARY;
      Len    : out Positive );

   procedure To_VARBINARY(
      Value  : in GNADE.BINARY;
      Result : out GNADE.VARBINARY );

   function Length(
      This   : in GNADE.VARBINARY ) return Natural;

   Out_Of_Resources     : exception ;
   No_Reopenable_Cursor : exception ;
   General_Error        : exception ;

end GNU.DB.ESQL_Support;


