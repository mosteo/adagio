-------------------------------------------------------------------------------
--                                                                           --
--                     GNADE  : GNat Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/esql/gnu-db-esql_support-odbc.adb,v $
--  Description     : Small interface to ODBC for the ESQL translator        --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 22-Dec-2000                                            --
--  Last Modified By: $Author: stephen_leake $
--  Last Modified On: $Date: 2004/03/17 02:30:34 $                           --
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
--                                                                           --
--  It contains the following sections:                                      --
--                                                                           --
--  Section 1. General support procedures                                    --
--  Section 2. Cursor Management                                             --
--  Section 3. Type Conversion                                               --
--  Section 4. Database access procedures                                    --
--  Section 5. Column Access procedures                                      --
--  Section 6. Paramter Access procedures.                                   --
--                                                                           --
--  The package maintains the list of cursors dynamicaly.                    --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  No Tasking support                                                       --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  //1// - (Second Informal Review Draft) ISO/IEC 9075:1992, Database       --
--  Language SQL- July 30, 1992                                              --
--                                                                           --
-------------------------------------------------------------------------------

--* Ada
with Ada.Finalization;                  use Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Command_Line;                  use Ada.Command_Line;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Interfaces.C;
use  System;
use  Ada;

with GNU.DB.SQLCLI;                     use GNU.DB.SQLCLI;
with GNU.DB.SQLCLI.Environment_Attribute;
use  GNU.DB.SQLCLI.Environment_Attribute;
with GNU.DB.SQLCLI.Connection_Attribute;
use  GNU.DB.SQLCLI.Connection_Attribute;


with Unchecked_Deallocation;

use  Ada.Strings;
use  System;
use  Ada;

with SQL_STANDARD;                      use SQL_STANDARD;
with GNU.DB.Support.Tables;
use  GNU.DB.Support;
package body GNU.DB.ESQL_Support.ODBC is

   Version : constant String := "$Id: gnu-db-esql_support-odbc.adb,v 1.26 2004/03/17 02:30:34 stephen_leake Exp $";
   pragma Unreferenced (Version);

   use type Interfaces.C.int;

   package CLI renames GNU.DB.SQLCLI;

   package SQLCA is new Address_To_Access_Conversions(
         Object => SQLCA_Type );
   use SQLCA;

   -- this a a mapping table which maps the ESQL data types to the
   -- ODBC data type.
   type ODBC_Type_Mapping is
      array ( ISO92_Host_Var_Type ) of SQL_C_DATA_TYPE;

   To_C_DATA_TYPE : constant ODBC_Type_Mapping := (
      ISO92_CHAR_TYPE             => SQL_C_CHAR,
      ISO92_BIT_TYPE              => SQL_C_BIT,
      ISO92_SMALLINT_TYPE         => SQL_C_SHORT,
      ISO92_INT_TYPE              => SQL_C_SLONG,
      ISO92_REAL_TYPE             => SQL_C_FLOAT,
      ISO92_DOUBLE_PRECISION_TYPE => SQL_C_DOUBLE,
      ISO92_SQLCODE_TYPE          => SQL_C_DEFAULT,
      ISO92_SQLSTATE_TYPE         => SQL_C_DEFAULT,
      ISO92_INDICATOR_TYPE        => SQL_C_DEFAULT,
      ISO92_Unknown_Type          => SQL_C_DEFAULT,

      GNADE_VARCHAR_TYPE          => SQL_C_CHAR,
      GNADE_BINARY_TYPE           => SQL_C_BINARY,
      GNADE_VARBINARY_TYPE        => SQL_C_BINARY
   );

   --
   -- this is used to maintain a list of cursors per connection. each
   -- cursor is simply a statement handle and the name of the cusor.
   --
   type Handle_Array is array( Positive range <> ) of SQLHSTMT;
   subtype Handle_Number is Positive range 1..50;

   -----------------
   -- Cursor_Type --
   -----------------
   type Cursor_Type is record
         Name            : Unbounded_String            := Null_Unbounded_String;
         Current         : Handle_Number               := Handle_Number'First;
         Handle          : Handle_Array(Handle_Number) := (others => SQL_NULL_HANDLE);
         Nbr_Of_Columns  : Natural                     := 0;   -- if a column is bound
         Is_Reopenable   : Boolean                     := False;
   end record;

   Null_Cursor : constant Cursor_Type := (
         Name           => Null_Unbounded_String,
         Current        => Handle_Number'First,
         Handle         => (others => SQL_NULL_HANDLE ),
         Nbr_Of_Columns => 0,
         Is_Reopenable  => False );

   package Cursor_Table is new Tables( Cursor_Type );
   use Cursor_Table;

   ---------------------
   -- Connection_Data --
   ---------------------
   type Connection_Data is record
         -- this is the data as it is maintained per data base connection
         EnvironmentHandle : SQLHENV;
         ConnectionHandle  : SQLHDBC;
         Default_Statement : SQLHSTMT := SQL_NULL_HANDLE;
         Cursors           : Cursor_Table.Tree_Node_Access := null;
   end record;

   ---------------------
   -- Cursor_CTX_Type --
   ---------------------
   type Cursor_CTX_Type is record
         Name : Unbounded_String  := Null_Unbounded_String;
         H    : Connection_Handle := null;
   end record;

   type Cursor_CTX_Array is array ( Positive range <> ) of Cursor_CTX_Type;

   type Context_Data_Type is record
         Cursor : Cursor_CTX_Array(1..50);
   end record;

   Option_Show_RT_Errors   : Boolean := False;
   Option_Show_RT_Warnings : Boolean := False;

   --- ********************************************************************* ---
   --- ** SECTION 1.       S U P P O R T    P R O C E D U R E S          *** ---
   --- **                  ====================================          *** ---
   --- **                                                                *** ---
   --- ********************************************************************* ---

   ---------------------
   -- Get_Cursor_Data --
   ---------------------
   function Get_Cursor_Data(
      H     : in Connection_Handle;
      Name  : in String;
      Sense : in Boolean := False ) return Cursor_Type is
      -- This function returns the reference to the cursor data stored in
      -- the connection.
      -- The parameter sense controls wether a exception or a null pointer
      -- should be given back in case, that the cursor is not found.
      Connection  : Connection_Data_Access := H.Data;
      Cursors     : Cursor_Table.Tree_Node_Access renames Connection.Cursors;
      Cursor_Data : Cursor_Type;
   begin
      Cursor_Table.Fetch( Cursors, Name, Cursor_Data );
      return Cursor_Data;

   exception
      when others =>
         if Sense then
            return Null_Cursor;
         else
            Error( H.all, "cursor " & Name & " not found " );
            raise General_Error;
         end if;
   end Get_Cursor_Data;

   -------------
   -- Columns --
   -------------
   function Columns(
      H         : in Connection_Handle;
      Cursor    : in String ) return Natural is
      --
      -- Indicate if there have been columns bound to the
      -- cursor.
      --
      Cursor_Data : Cursor_Type := Get_Cursor_Data(H, Cursor );
   begin
      return Cursor_Data.Nbr_Of_Columns;
   end Columns;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
      Item : in out Context_Type ) is
      -- Initialize the local sql context
      Data : constant Context_Data_Access := new Context_Data_Type;
   begin
      for I in 1..Argument_Count loop
         if Argument(I) = "--esql-errors" then
            Option_Show_RT_Errors := True;
         elsif Argument(I) = "--esql-warnings" then
            Option_Show_RT_Warnings := True;
         end if;
      end loop;

      Item.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      Item        : in out Context_Type ) is
      -- delete all resources as they are registered with the context. For
      -- odbc this is the statement handles.
      Data        : Context_Data_Access    := Item.Data;

      procedure Free is
         new Unchecked_Deallocation( Context_Data_Type, Context_Data_Access);

   begin
      for I in Data.Cursor'Range loop
         if Data.Cursor(I).Name /= Null_Unbounded_String then
            declare
               Name : Unbounded_String  renames Data.Cursor(I).Name;
               H    : Connection_Handle renames Data.Cursor(I).H;
               Cursor_Data : Cursor_Type;
               Connection  : Connection_Data_Access := H.Data;
            begin
               Cursor_Data := Get_Cursor_Data( H, To_String(Name), Sense => True);
               if Cursor_Data /= Null_Cursor then
                  for J in Handle_Number'Range loop
                     if Cursor_Data.Handle(J) /= SQL_NULL_HANDLE then
                        SQLFreeStmt( Cursor_Data.Handle(J), SQL_CLOSE );
                        SQLFreeHandle (SQL_HANDLE_STMT, Cursor_Data.Handle(J));
                     end if;
                  end loop;
                  Delete( Connection.Cursors, To_String(Name) );
               end if;
            exception
               when General_Error =>
                  null;
            end;
         end if;
      end loop;

      Free( Data );

   end Finalize;

   ----------------
   -- Add_Cursor --
   ----------------
   procedure Add_Cursor(
      Ctx   : in out Context_Type;
      H     : in Connection_Handle;
      C     : in String ) is
      -- Add a cursor to the contect the following rules do apply:
      -- R.1 If the cursor alrady exisit, dont enter the same name
      --     twice.
      -- R.2 If a free entry in the cursor table is found, insert
      --     the name and the connection handle.
      --
      Data  : Context_Data_Access := Ctx.Data;
      Name  : Unbounded_String := To_Unbounded_String(C);
   begin
      for I in Data.Cursor'Range loop
         if Data.Cursor(I).Name /= Null_Unbounded_String       -- R.1
            and then  Data.Cursor(I) = ( Name, H )
         then
            return;
         end if;

         if Data.Cursor(I).Name = Null_Unbounded_String then   -- R.2
            Data.Cursor(I) := ( Name, H );
            return;
         end if;
      end loop;
   end Add_Cursor;

   --- ********************************************************************* ---
   --- ** SECTION 2.      C U R S O R   M A N A G E M E N T              *** ---
   --- ********************************************************************* ---

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor(
      H           : in Connection_Handle;
      Cursor      : in String ) return SQLHSTMT is
      --
      -- Try to locate the handle of the given cursor. if no cursor
      -- is given, the default handle is used. In case no cursor
      -- is found an exception will be raised.
      --
      Connection  : constant Connection_Data_Access := H.Data;
      Cursor_Data : Cursor_Type;
      --
   begin
      if Cursor = "" then
         return Connection.Default_Statement;
      end if;
      Cursor_Data := Get_Cursor_Data( H, Cursor );
      -- Put_Line("Get_Cursor " & Natural'Image(Cursor_Data.Current));
      return Cursor_Data.Handle(Cursor_Data.Current);

   exception
      when Cursor_Table.Entry_Not_Found =>
         Warning(
            H.all,
            "*** Cursor '" & Cursor & " not found, using default cursor ****" );
         return Connection.Default_Statement;
   end Get_Cursor;

   --- ********************************************************************* ---
   --- ** SECTION 3.      T Y P E     C O N V E R S I O N                *** ---
   --- ********************************************************************* ---



   --- ********************************************************************* ---
   --- ** SECTION 4.    O D B C   I n t e r f a c e                      *** ---
   --- ********************************************************************* ---

   -------------
   -- Warning --
   -------------
   procedure Warning(
      Object : in Connection_Type;
      Text   : in String ) is
      pragma Unreferenced (Object);
   begin
      if not Option_Show_RT_Warnings then
         return;
      end if;
      Put_Line( "*** Warning *** " & Text );
   end Warning;

   ------------
   -- Error  --
   ------------
   procedure Error(
      Object : in Connection_Type;
      Text   : in String ) is
      pragma Unreferenced (Object);
   begin
      if not Option_Show_RT_Errors then
         return;
      end if;

      Put_Line( "*** Error *** " & Text );
   end Error;

   -------------
   -- Connect --
   -------------
   function Connect(
      Source            : in String;
      UserName          : in String;
      Password          : in String
        ) return Connection_Handle is
      -- Connect to the data base.
      Result            : constant Connection_Handle      := new Connection_Type;
   begin
      Connect( Result.all, Source, UserName, Password );
      return Result;
   end Connect;

   -------------
   -- Connect --
   -------------
   procedure Connect(
      Connection_Obj    : in out Connection_Type'Class;
      Source            : in String;
      UserName          : in String;
      Password          : in String ) is
      -- Connect to the data base.
      Data              : Connection_Data_Access := new Connection_Data;
      EnvironmentHandle : SQLHENV renames Data.EnvironmentHandle ;
      ConnectionHandle  : SQLHDBC renames Data.ConnectionHandle ;
      ---
   begin
      Connection_Obj.Data := Data;

      Data.Cursors := Cursor_Table.Create_Tree;

      SQLAllocHandle (
         SQL_HANDLE_ENV,
         SQL_NULL_HANDLE,
         EnvironmentHandle);

      SQLSetEnvAttr (
         EnvironmentHandle,
         Environment_Attribute_ODBC_Version'
            (Attribute => SQL_ATTR_ODBC_VERSION,
             Value     => SQL_OV_ODBC3)
         );

      SQLAllocHandle (
         SQL_HANDLE_DBC,
         EnvironmentHandle,
         ConnectionHandle);

      begin
         SQLSetConnectAttr(
            ConnectionHandle,
            Connection_Attribute_Unsigned'(Attribute => SQL_ATTR_LOGIN_TIMEOUT,
                                           Value     => 30));
      exception
         when Not_Implemented => null;
      end;

      SQLConnect (
         ConnectionHandle => ConnectionHandle,
         ServerName       => Source,
         UserName         => UserName,
         Authentication   => Password );

   end Connect;

   ------------------
   --- Disconnect ---
   ------------------
   procedure  Disconnect(
      H          : in out Connection_Handle;
      Global_CTX : in out Context_Type;
      Commit     : in Boolean := False ) is
      -- Disconnect from the data base
      --
      -- R.1  - Check in the global context table for all cursors
      --        which have been created on this data base connection.
      -- R.2  - Delete every resource held by this data base in the
      --        context table.
      -- R.3  - Drop the data base connection
      --
      Connection        : Connection_Data_Access := H.Data;
      EnvironmentHandle : SQLHENV  renames Connection.EnvironmentHandle ;
      ConnectionHandle  : SQLHDBC  renames Connection.ConnectionHandle  ;
      StatementHandle   : SQLHSTMT renames Connection.Default_Statement ;

      CTX               : Context_Data_Access    := Global_CTX.Data;
      ---
   begin

      if StatementHandle /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, StatementHandle);
      end if;

      if Commit then
         SQLCommit (ConnectionHandle);
      end if;

      for I in CTX.Cursor'Range loop
         if CTX.Cursor(I).H = H and then                               -- R.1
            CTX.Cursor(I).Name /= Null_Unbounded_String
         then
            declare
               Name        : Unbounded_String  renames CTX.Cursor(I).Name;
               Cursor_Data : Cursor_Type;
            begin                                                      -- R.2
               Cursor_Data := Get_Cursor_Data( H, To_String(Name));

               for J in Handle_Number'Range loop
                  if Cursor_Data.Handle(J) /= SQL_NULL_HANDLE then
                     SQLFreeStmt( Cursor_Data.Handle(J), SQL_CLOSE );
                     SQLFreeHandle (SQL_HANDLE_STMT, Cursor_Data.Handle(J));
                  end if;
               end loop;

               Delete( Connection.Cursors, To_String(Name) );
               Name := Null_Unbounded_String;
            end;
         end if;
      end loop;
                                                                      -- R.3
      SQLDisconnect (ConnectionHandle);
      SQLFreeHandle (SQL_HANDLE_DBC, ConnectionHandle);
      SQLFreeHandle (SQL_HANDLE_ENV, EnvironmentHandle);
   end Disconnect;

   -------------------
   -- Get_SQL_State --
   -------------------
   procedure Get_SQL_State(
      Handle      : SQLHSTMT;
      SCA         : SQLCA_Access ) is
   begin
      SCA.Message := ( others => ' ' );
      SCA.State   := EMPTY_SQLSTATE;

      Move( Source => SQL_Error_Message(
                         SQL_HANDLE_STMT,
                         Handle,
                         SCA.State'Access),
            Target => SCA.Message );
   end Get_SQL_State;

   -------------
   -- Prepare --
   -------------
   procedure Prepare(
      H               : in Connection_Handle;
      Context         : in out Context_Type;
      S               : in String;
      Result          : in Address;
      Cursor          : in String  := "";
      Is_Reopenable   : in Boolean := False;
      Is_Local        : in Boolean := False ) is
      pragma Unreferenced (Is_Local);
      --
      --  R.1 If a cursor name is given, we check if it is already
      --      exisiting. If so, a exception is raised if the cursor
      --      is not reopenable.
      --  R.2 If no cursor name is passed we release the the prev.
      --      default statement handle and allocate a new one.
      --  R.3 After excution of the prepare, the sql state is fetched.
      --
      RC              : SQLRETURN := 0;
      Connection      : Connection_Data_Access := H.Data;
      StatementHandle : SQLHSTMT;
      Cursor_Data     : Cursor_Type;
      CA              : SQLCA.Object_Pointer := To_Pointer( Result );
   begin
      SQLAllocHandle (
         SQL_HANDLE_STMT,
         Connection.ConnectionHandle,
         StatementHandle);

      if Cursor /= "" then
         begin                                                          -- R.1
            Insert(Connection.Cursors, Cursor, Cursor_Data );
            Cursor_Data.Current         := 1;

         exception
            when Cursor_Table.Already_Stored =>
               if not Is_Reopenable then
                  raise No_Reopenable_Cursor;
               else
                  Cursor_Data.Current := Cursor_Data.Current + 1;
               end if;
         end;

         -- SQLSetCursorName( StatementHandle, Cursor );
         Cursor_Data.Handle(Cursor_Data.Current) := StatementHandle;

         Cursor_Data.Name            := To_Unbounded_String(Cursor);
         Cursor_Data.Nbr_Of_Columns  := 0;
         Cursor_Data.Is_Reopenable   := Is_Reopenable;

         Update(Connection.Cursors, Cursor, Cursor_Data );

         Add_Cursor( Context, H, Cursor );
      else                                                              -- R.2
         if Connection.Default_Statement /= SQL_NULL_HANDLE then
            SQLCloseCursor( Connection.Default_Statement );
            SQLFreeStmt( Connection.Default_Statement, SQL_CLOSE );
         end if;
         Connection.Default_Statement := StatementHandle;
      end if;

      RC := SQLPrepare ( StatementHandle, S);
      CA.SqlCode := RC;

      if RC = CLI.SQL_ERROR then                                        -- R.3
         Get_SQL_State( StatementHandle, SQLCA_Access(CA) );
      else
         CA.State   := EMPTY_SQLSTATE;
      end if;

   end Prepare;

   -------------
   -- Execute --
   -------------
   procedure Execute(
      H               : in Connection_Handle;
      Result          : in Address;
      Statement       : in String := "";
      Cursor          : in String := "" ) is
      --
      -- R.1  If a statement for execution is passed, assume that not the
      --      default statement handle has to be used.
      -- R.2  If a no statement has been given, we may assume cursor
      --      handling, which means, the cursors has been defined either
      --      by a prepare or a decalre cursor.
      --
      RC              : CLI.SQLRETURN;
      Connection      : constant Connection_Data_Access := H.Data;
      StatementHandle : SQLHSTMT;
      CA              : SQLCA.Object_Pointer := To_Pointer( Result );
      --
   begin
      if Statement /= "" then                                   -- R.1
         SQLAllocHandle (
            SQL_HANDLE_STMT,
            Connection.ConnectionHandle,
            StatementHandle);

         RC := SQLExecDirect ( StatementHandle, Statement );

         CA.SqlCode := RC;
         if RC = CLI.SQL_ERROR then
            Get_SQL_State( StatementHandle, SQLCA_Access(CA) );
         else
            CA.State  := EMPTY_SQLSTATE;
            CA.Affected_Rows := Integer( SQLRowCount(StatementHandle) );
         end if;

         SQLFreeStmt( StatementHandle, SQL_CLOSE );
      else                                                      -- R.2
         StatementHandle := Get_Cursor(H, Cursor);
         RC := SQLExecute( StatementHandle );

         if RC = CLI.SQL_ERROR then
            Get_SQL_State( StatementHandle, SQLCA_Access(CA) );
         else
            CA.State  := EMPTY_SQLSTATE;
            CA.Affected_Rows := Integer( SQLRowCount(StatementHandle) );
         end if;
      end if;
   end Execute;

   ------------------
   -- Close_Cursor --
   ------------------
   procedure Close_Cursor(
      H               : in Connection_Handle;
      Cursor          : in String;
      Finalize        : in Boolean := False ) is
      --
      -- Close the named cursor. This will deallocate all
      -- resources held by this cursor.
      --
      Connection      : Connection_Data_Access := H.Data;
      Cursors         : Cursor_Table.Tree_Node_Access renames Connection.Cursors;
      Cursor_Data     : Cursor_Type := Get_Cursor_Data( H, Cursor );
   begin
      SQLCloseCursor( Cursor_Data.Handle(Cursor_Data.Current) );
      -- Cursor_Data.Handle(Cursor_Data.Current) := SQL_NULL_HANDLE;

      if Cursor_Data.Current > 1 then
         Cursor_Data.Current := Cursor_Data.Current - 1;
      end if;

      if Finalize and Cursor_Data.Current = 1 then
         Delete( Cursors, Cursor );
         Cursor_Data.Name := Null_Unbounded_String;
      else
         Update( Cursors, Cursor, Cursor_Data );
      end if;

   end Close_Cursor;

   -----------
   -- Fetch --
   -----------
   procedure Fetch(
      H               : in Connection_Handle;
      Result          : in Address;
      Cursor          : in String := "" ) is
      -- Execute a fetch operation. A fetch operation is either
      -- used with DECALRE cursor or used with a simple
      -- direct query (select ... into,,).
      RC              : SQLRETURN := 0;
      StatementHandle : constant SQLHSTMT := Get_Cursor(H, Cursor);

      CA              : SQLCA.Object_Pointer := To_Pointer( Result );
   begin
      RC := SQLFetch (StatementHandle);
      CA.SqlCode := RC;
      CA.State   := EMPTY_SQLSTATE;

      if RC /= CLI.SQL_SUCCESS then
         Get_SQL_State(StatementHandle, SQLCA_Access(CA));
      end if;
   end Fetch;

   -----------
   -- Count --
   -----------
   function Count (
      H               : Connection_Handle;
      Cursor          : in String := "" ) return Integer is
      -- return the number of rows in the result set
      Count           : aliased SQLINTEGER;
      RC              : SQLRETURN := 0;
      StatementHandle : constant SQLHSTMT := Get_Cursor(H, Cursor);
      ---
   begin
      RC := SQLRowCount (StatementHandle, Count'Access);
      if RC = SQL_SUCCESS then
         return Integer(Count);
      end if;
      return 0;
   end Count;

   ----------------------
   -- ODBC_Stmt_Handle --
   ----------------------
   function ODBC_Stmt_Handle(
      H        : in  Connection_Handle;
      Name     : in String ) return SQLHSTMT is
   begin
      return Get_Cursor( H, Name );
   end ODBC_Stmt_Handle;

   ---------------------
   -- ODBC_Con_Handle --
   ---------------------
   function ODBC_Con_Handle(
      H          : in  Connection_Handle ) return SQLHDBC is
      Connection : constant Connection_Data_Access := H.Data;
   begin
      return Connection.ConnectionHandle;
   end ODBC_Con_Handle;

   --- ********************************************************************* ---
   --- ** SECTION 4.            C O L U M N   B I N D I N G              *** ---
   --- ********************************************************************* ---

   function To_SQLPOINTER is
      new Ada.Unchecked_Conversion ( Source => Address, Target => SQLPOINTER );

   -------------------
   -- Mark_As_Bound --
   -------------------
   procedure Mark_As_Bound(
      H          : Connection_Handle;
      Cursor     : String ) is
      --
      -- This procedure increment the number of bound columns. This
      -- information is used by the esql generated code to see if
      -- columns already has bound.
      --
      Data       : Cursor_Type;
      Connection : Connection_Data_Access := H.Data;
      Cursors    : Cursor_Table.Tree_Node_Access renames Connection.Cursors;
   begin
      if Cursor = "" then
         return;
      end if;

      Data := Get_Cursor_Data( H, Cursor );
      Data.Nbr_Of_Columns := Data.Nbr_Of_Columns + 1;

      Cursor_Table.Update(Cursors, Cursor, Data );
   end Mark_As_Bound;

   ----------------
   -- BindColumn --
   ----------------
   procedure BindColumn(
      H               : in Connection_Handle;
      Column_Number   : in Natural;
      Column_Data     : in Address ;
      Length          : in Natural ;
      Column_Type     : in ISO92_Host_Var_Type ;
      Result          : access INDICATOR_TYPE;
      Cursor          : in String := "" ) is
      -- bind a column to the given data field.
      RC : SQLRETURN;
      pragma Warnings (Off, RC);
      StatementHandle : constant SQLHSTMT := Get_Cursor(H, Cursor);

      Size : constant SQLINTEGER := SQLINTEGER( Length / 8 );
      ---
   begin
      RC := SQLBindCol (
         StatementHandle,
         SQL_Column_Number(Column_Number),
         To_C_DATA_TYPE( Column_Type ),
         To_SQLPOINTER( Column_Data ),
         Size,
         Result
      );
      Mark_As_Bound(H, Cursor );
   end BindColumn;

  -------------------
  -- BindParameter --
  -------------------

  procedure BindParameter(
      H             : in Connection_Handle;
      Number        : in Natural;
      Data          : in Address;
      Length        : in Natural;
      Column_Type   : in ISO92_Host_Var_Type;
      Indicator     : access INDICATOR_TYPE;
      Cursor        : in String := "" ) is
      ---
      RC : SQLRETURN;
      pragma Warnings (Off, RC);
      StatementHandle : constant SQLHSTMT := Get_Cursor(H, Cursor);

      Size          : constant SQLINTEGER := SQLINTEGER( Length / 8 );
      SQL_Type      : SQL_DATA_TYPE := SQL_INTEGER;
      ---
  begin
      if Column_Type = ISO92_CHAR_TYPE then
         SQL_Type := SQL_VARCHAR;
      end if;

      RC := SQLBindParameter (
         StatementHandle,
         SQL_Parameter_Number(Number),
         SQL_PARAM_INPUT,
         To_C_DATA_TYPE( Column_Type ),
         SQL_Type,
         SQLUINTEGER(Size),
         0,
         To_SQLPOINTER(Data),
         Size,
         Indicator);
  end BindParameter;

end GNU.DB.ESQL_Support.ODBC;
