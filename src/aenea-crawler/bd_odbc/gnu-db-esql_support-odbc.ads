-------------------------------------------------------------------------------
--                                                                           --
--                     GNADE  : GNat Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/esql/gnu-db-esql_support-odbc.ads,v $
--  Description     : Small interface to ODBC for the ESQL translator        --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 22-Dec-2000                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2004/02/24 08:37:23 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2003 Michael Erdmann                                  --
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
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--  This package is a wrapper around the ODBC interface in order             --
--  to make the code generation easier.                                      --
--  The package is generaly not intended as an programming API for the       --
--  progrmmer.                                                               --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - The number of reopens per reopenable cursor is limited.            --
--  R.2 - No Tasking support                                                 --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  //1// - (Second Informal Review Draft) ISO/IEC 9075:1992, Database       --
--  Language SQL- July 30, 1992                                              --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Finalization;                  use Ada.Finalization;
with System;                            use System;

with SQL_STANDARD;                      use SQL_STANDARD;
with GNU.DB.SQLCLI;                     use GNU.DB.SQLCLI;
with GNU.DB.ESQL_ISO92_Types;           use GNU.DB.ESQL_ISO92_Types;

package GNU.DB.ESQL_Support.ODBC is

   type Connection_Type is tagged private;
   type Connection_Handle is access Connection_Type'Class;

   type Context_Type is private;
   type Context_Access is access Context_Type;

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Connect to a data base
   --| Preconditions  :
   --|    P.1 - Not already connected.
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   function Connect(
      Source     : in String;
      UserName   : in String;
      Password   : in String  ) return Connection_Handle;

   procedure Connect(
      Connection_Obj  : in out Connection_Type'Class;
      Source     : in String;
      UserName   : in String;
      Password   : in String  );

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Disconnect from a data base
   --| Preconditions  :
   --|
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   procedure  Disconnect(
      H          : in out Connection_Handle;
      Global_CTX : in out Context_Type;
      Commit     : in Boolean := False );


   --- ================================================================= ---
   ---       Q U E R Y    AND  C U R S O R     H A N D L I N G           ---
   ---                                                                   ---
   --- The following procedures are only for use by the esql code        ---
   --- generator.
   --- ================================================================= ---

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Prepare a query. This is used by the ESQL code generator
   --|    when the query contains a host viariable as parameter or
   --|    in the INTO clause.
   --|
   --| Preconditions  :
   --|    P.1 - Database is connected
   --|
   --| Postconditions :
   --|    C.1 - Cursor is created if the Cursor paramter has been set.
   --|          (see Note 1 and Note 2 below).
   --|    C.2 - The query has been processed by the underlying DBCS.
   --|    C.3 - The resul codes are stored in the SQLCA.
   --|
   --| Exceptions     :
   --|    Any
   --|
   --| Note           :
   --|    N.1 - Is_Reopenable allows to stack the cursors,  which means
   --|          at the same time you may have several cursors with the
   --|          same name  acitve but only the last defined cursor of the
   --|          name is active.
   --|
   --|    N.2 - Is_local indicates, that the cursor has to be allocate
   --|          locally in the local context. If the local context is left,
   --|          the cursor is closed.
   --|
   ---------------------------------------------------------------------------
   procedure Prepare(
      H             : in Connection_Handle;
      Context       : in out Context_Type;
      S             : in String;
      Result        : in Address;
      Cursor        : in String := "";
      Is_Reopenable : in Boolean := False;
      Is_Local      : in Boolean := False );

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Execute a prepared statement or the statement as it is given in the
   --|    statement parameter.
   --|
   --| Preconditions  :
   --|    P.1 - Database is connected
   --| Postconditions :
   --|    C.1 - The result set is created the underlying daatabase using
   --|          the specified cusor.
   --|    C.2 - The resul codes are stored in the SQLCA.
   --| Exceptions     :
   --|    Any
   --| Note           :
   --|    None
   ---------------------------------------------------------------------------
   procedure Execute(
      H         : in Connection_Handle;
      Result    : in Address ;
      Statement : in String := "";
      Cursor    : String := "" );

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Close the named cusor.
   --| Preconditions  :
   --|    P.1 - Database is connected
   --|    P.2 - Cursor does exist.
   --| Postconditions :
   --|    C.1 - The current cursor is closed and the next active
   --|          cursor is made active if the cursor is reopenable.
   --|    C.2 - All cursors of this name are deallocated if the
   --|          Finalize indicator is set.
   --| Exceptions     :
   --|    Any
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Close_Cursor(
      H         : in Connection_Handle;
      Cursor    : in String;
      Finalize  : in Boolean := False );

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Fetch the result using the specified cursor.
   --| Preconditions  :
   --|
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Fetch(
      H         : in Connection_Handle;
      Result    : in Address;
      Cursor    : String := "" );


   ---------------------------------------------------------------------------
   --| Description    :
   --|
   --| Preconditions  :
   --|
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   function Columns(
      H         : in Connection_Handle;
      Cursor    : in String ) return Natural;

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Return the number of rows found by the query.
   --| Preconditions  :
   --|
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   function Count (
      H         : Connection_Handle;
      Cursor    : String := "" ) return Integer;

   ---------------------------------------------------------------------------
   --| Description    :
   --|
   --| Preconditions  :
   --|
   --| Postconditions :
   --|
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   --function Is_Null (
   --   s         : in SQL_STANDARD.INDICATOR_TYPE ) return Boolean;

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Get the current statement handle
   --| Preconditions  :
   --|    P.1 - Connected to a data base
   --| Postconditions :
   --|    C.1 - If no cursor name has been given, the default statement
   --|          handle of the last query is returned.
   --|    C.2 - If a cursor is named, the statement handle of the
   --|          cursor is returned.
   --| Exceptions     :
   --|    Any.
   --| Note           :
   ---------------------------------------------------------------------------
   function ODBC_Stmt_Handle(
      H        : in  Connection_Handle;
      Name     : in String ) return SQLHSTMT;

   procedure Warning(
      Object : Connection_Type;
      Text   : String );

   procedure Error(
      Object : Connection_Type;
      Text   : String );

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Return the ODBC connection handle
   --| Preconditions  :
   --|    P.1 - Connected to a data base
   --| Postconditions :
   --|    C.1 - Function returns the ODBC connection handle
   --| Exceptions     :
   --|
   --| Note           :
   ---------------------------------------------------------------------------
   function ODBC_Con_Handle(
      H        : in  Connection_Handle ) return SQLHDBC;

   --- ================================================================= ---
   ---            BIND ALL ISO/92 Types to columns                       ---
   ---                                                                   ---
   --- The following procedures are only for use by the code generator.  ---
   --- Each of the procedures below takes the following arguments:       ---
   ---    Connection Handle                                              ---
   ---    Column_Number      - Indicating the position of the columns in ---
   ---                         the range 1..                             ---
   ---    Cursor             - If no cursor is specified, the default    ---
   ---                         statement handle will be used.            ---
   ---    Column_Data        - Pointer to a variable where the result    ---
   ---                         is stored later.                          ---
   ---    Result             - Indicator variable indcating the result   ---
   ---                         The funct. Is_Null may be used to check   ---
   ---                         for a Null value.                         ---
   --- ================================================================= ---
   procedure BindColumn(
      H             : Connection_Handle;
      Column_Number : Natural;
      Column_Data   : Address;
      Length        : Natural;
      Column_Type   : ISO92_Host_Var_Type;
      Result        : access INDICATOR_TYPE;
      Cursor        : in String := "" );

  procedure BindParameter(
      H             : Connection_Handle;
      Number        : Natural;
      Data          : Address;
      Length        : Natural;
      Column_Type   : ISO92_Host_Var_Type;
      Indicator     : access INDICATOR_TYPE;
      Cursor        : String := "" ) ;


private

   -- Connection data
   type Connection_Data;
   type Connection_Data_Access is access Connection_Data;

   type Connection_Type is tagged record
      Data : Connection_Data_Access := null;
   end record;

   -- Context data
   type Context_Data_Type ;
   type Context_Data_Access is access Context_Data_Type;

   type Context_Type is new Controlled with record
         Data : Context_Data_Access := null;
   end record;

   procedure Initialize( Item : in out Context_Type );
   procedure Finalize( Item : in out Context_Type );

end GNU.DB.ESQL_Support.ODBC;
