-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/dbi/mysql/gnu-db-mysql.gpb,v $
--  Description     : Ada Database Objects - Test client                     --
--  Author          : Michael Erdmann                                        --
--  Created         : 18.1.2002                                              --
--  Last Modified By: $Author: merdmann $                                     --
--  Last Modified On: $Date: 2004/05/29 13:29:19 $                           --
--  Version         : $Revision: 1.15 $                                      --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Stephen Leake                                         --
--  Copyright (C) 2001 Michael Erdmann                                       --
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
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--                                                                           --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This components implements a thick binding to the MySQL product          --
--  which is available for the Linux Plattform.                              --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  Only 100 fields per query                                                --
--  Number of Queries per connection is 10.                                  --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  R1. The into clause in case of FETCH seems not to work with ODBC. The    --
--      translator will raise an syntax error if such an clause has been     --
--      found.                                                               --
--
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--               Bertrand Carlier <bc@adalog.fr>                             --
--                                                                           --
-------------------------------------------------------------------------------
-- Generation for MySQL4

--* Ada
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Unchecked_Deallocation;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Latin_1;  use Ada.Characters;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Interfaces.C;            use Interfaces.C;
use  Interfaces;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Interfaces.C.Pointers;

with Unchecked_Deallocation;

package body GNU.DB.MySQL is

   Version : constant String := "$Id: gnu-db-mysql.gpb,v 1.15 2004/05/29 13:29:19 merdmann Exp $";
   pragma Unreferenced (Version);

   --|
   --| Interface to the MYSQL Library. Some of the data types are simply
   --| Addresses because the internals of these strictured do not have
   --| to be known here.
   --|
   subtype MYSQL_SERVER is  System.Address;
   subtype MYSQL_ROW    is  System.Address;
   subtype MYSQL_RES    is  System.Address;

   type MYSQL_RES_Record is                                     -- mysql.h:89
        record
--            row_count    : c.unsigned;                         --  mysql.h:90
            row_count    : long_long_integer;                 -- mysql.h:90
            field_count  : c.unsigned;                          -- mysql.h:91
            current_field: c.unsigned;                          -- mysql.h:91
            fields       : Address;                             -- mysql.h:92
            data         : Address;                             -- mysql.h:93
            data_cursor  : Address;                             -- mysql.h:94
            field_alloc  : Address;                             -- mysql.h:95
            row          : MYSQL_ROW;                           -- mysql.h:96
            current_row  : MYSQL_ROW;                           -- mysql.h:97
            lengths      : Address;                             -- mysql.h:98
            handle       : Address;                             -- mysql.h:99
            eof          : C.char;                              -- mysql.h:100
        end record;

   pragma Convention(C, MYSQL_RES_Record);

   type Unsigned_Long_Array is array(Natural range <> )
     of aliased C.Unsigned_Long;

   Unsigned_Long_Null : aliased C.Unsigned_Long := 0;

   package Unsigned_Long_Ptr is
      new Interfaces.C.Pointers(Index => Natural,
                                Element => C.Unsigned_Long,
                                Element_Array=> Unsigned_Long_Array,
                                Default_Terminator => Unsigned_Long_Null);


   function mysql_real_connect(mysql       : MYSQL_SERVER;
                               Host        : Strings.Chars_Ptr;
                               User        : Strings.Chars_Ptr;
                               Passwd      : Strings.Chars_Ptr;
                               Db          : Strings.Chars_Ptr;
                               Port        : Unsigned;
                               Unix_Socket : Strings.Chars_Ptr;
                               Clientflag  : Unsigned)
                              return MYSQL_SERVER;
   pragma Import(C, mysql_real_connect, "mysql_real_connect");


   procedure mysql_close(sock: MYSQL_SERVER);
   pragma Import (C, mysql_close, "mysql_close");

   function mysql_select_db(mysql: MYSQL_SERVER; db : Strings.chars_ptr) return c.int;
   pragma Import (C, mysql_select_db, "mysql_select_db");

   function mysql_query(mysql: MYSQL_SERVER; q : Strings.chars_ptr) return c.int;
   pragma Import (C, mysql_query, "mysql_query");

   function mysql_fetch_row(mysql: MYSQL_RES) return MYSQL_ROW;
   pragma Import (C, mysql_fetch_row, "mysql_fetch_row");

   -- For retrieving blob lengths
   function Mysql_Fetch_Lengths
     (Mysql : MYSQL_RES) return Unsigned_Long_Ptr.pointer;
   pragma Import(C, Mysql_Fetch_Lengths, "mysql_fetch_lengths");


   type MYSQL_field_types is (                                  -- mysql_com.h:64
        FIELD_TYPE_DECIMAL,                                     -- mysql_com.h:64
        FIELD_TYPE_TINY,                                        -- mysql_com.h:64
        FIELD_TYPE_SHORT,                                       -- mysql_com.h:65
        FIELD_TYPE_LONG,                                        -- mysql_com.h:65
        FIELD_TYPE_FLOAT,                                       -- mysql_com.h:66
        FIELD_TYPE_DOUBLE,                                      -- mysql_com.h:66
        FIELD_TYPE_NULL,                                        -- mysql_com.h:67
        FIELD_TYPE_TIMESTAMP,                                   -- mysql_com.h:67
        FIELD_TYPE_LONGLONG,                                    -- mysql_com.h:68
        FIELD_TYPE_INT24,                                       -- mysql_com.h:68
        FIELD_TYPE_DATE,                                        -- mysql_com.h:69
        FIELD_TYPE_TIME,                                        -- mysql_com.h:69
        FIELD_TYPE_DATETIME,                                    -- mysql_com.h:70
        FIELD_TYPE_YEAR,                                        -- mysql_com.h:70
        FIELD_TYPE_NEWDATE,                                     -- mysql_com.h:71
        FIELD_TYPE_ENUM,                                        -- mysql_com.h:72
        FIELD_TYPE_SET,                                         -- mysql_com.h:73
        FIELD_TYPE_TINY_BLOB,                                   -- mysql_com.h:74
        FIELD_TYPE_MEDIUM_BLOB,                                 -- mysql_com.h:75
        FIELD_TYPE_LONG_BLOB,                                   -- mysql_com.h:76
        FIELD_TYPE_BLOB,                                        -- mysql_com.h:77
        FIELD_TYPE_VAR_STRING,                                  -- mysql_com.h:78
        FIELD_TYPE_STRING                                       -- mysql_com.h:80
    );
    for MYSQL_field_types use (                             -- mysql_com.h:64
        FIELD_TYPE_DECIMAL => 0,                                -- mysql_com.h:64
        FIELD_TYPE_TINY => 1,                                   -- mysql_com.h:64
        FIELD_TYPE_SHORT => 2,                                  -- mysql_com.h:65
        FIELD_TYPE_LONG => 3,                                   -- mysql_com.h:65
        FIELD_TYPE_FLOAT => 4,                                  -- mysql_com.h:66
        FIELD_TYPE_DOUBLE => 5,                                 -- mysql_com.h:66
        FIELD_TYPE_NULL => 6,                                   -- mysql_com.h:67
        FIELD_TYPE_TIMESTAMP => 7,                              -- mysql_com.h:67
        FIELD_TYPE_LONGLONG => 8,                               -- mysql_com.h:68
        FIELD_TYPE_INT24 => 9,                                  -- mysql_com.h:68
        FIELD_TYPE_DATE => 10,                                  -- mysql_com.h:69
        FIELD_TYPE_TIME => 11,                                  -- mysql_com.h:69
        FIELD_TYPE_DATETIME => 12,                              -- mysql_com.h:70
        FIELD_TYPE_YEAR => 13,                                  -- mysql_com.h:70
        FIELD_TYPE_NEWDATE => 14,                               -- mysql_com.h:71
        FIELD_TYPE_ENUM => 247,                                 -- mysql_com.h:72
        FIELD_TYPE_SET => 248,                                  -- mysql_com.h:73
        FIELD_TYPE_TINY_BLOB => 249,                            -- mysql_com.h:74
        FIELD_TYPE_MEDIUM_BLOB => 250,                          -- mysql_com.h:75
        FIELD_TYPE_LONG_BLOB => 251,                            -- mysql_com.h:76
        FIELD_TYPE_BLOB => 252,                                 -- mysql_com.h:77
        FIELD_TYPE_VAR_STRING => 253,                           -- mysql_com.h:78
        FIELD_TYPE_STRING => 254                                -- mysql_com.h:80
    );
   for MYSQL_field_types'size use 32;

   type st_mysql_field is                                       -- mysql.h:39
        record
            name      : Strings.chars_ptr;                      -- mysql.h:40
            table     : Strings.chars_ptr;                      -- mysql.h:41
            def       : Strings.chars_ptr;                      -- mysql.h:42
            c_type    : MYSQL_field_types;                      -- mysql.h:43
            length    : c.unsigned;                             -- mysql.h:44
            max_length: c.unsigned;                             -- mysql.h:45
            flags     : c.unsigned;                             -- mysql.h:46
            decimals  : c.unsigned;                             -- mysql.h:47
        end record;

   pragma Convention(C, st_mysql_field);          -- mysql.h:39
   type MYSQL_Field is access all st_mysql_field;

   type enum_mysql_status is  -- mysql.h:67
     (
      MYSQL_STATUS_READY,                                 -- mysql.h:67
      MYSQL_STATUS_GET_RESULT,                            -- mysql.h:67
      MYSQL_STATUS_USE_RESULT                             -- mysql.h:68
      );
   for enum_mysql_status'size use 32;                      -- mysql.h:67

   type struct_st_net is record
      fd          : c.int;                            -- mysql_com.h:54
      fcntl       : c.int;                            -- mysql_com.h:55
      buff        : Strings.chars_ptr;                -- mysql_com.h:56
      buff_end    : Strings.chars_ptr;                -- mysql_com.h:56
      write_pos   : Strings.chars_ptr;                -- mysql_com.h:56
      last_error  : C.char_Array(0..199);             -- mysql_com.h:57
      last_errno  : c.unsigned;                       -- mysql_com.h:58
      max_packet  : c.unsigned;                       -- mysql_com.h:58
      timeout     : c.unsigned;                       -- mysql_com.h:58
      pkt_nr      : c.unsigned;                       -- mysql_com.h:58
      error       : c.signed_char;                    -- mysql_com.h:59
      return_errno: c.signed_char;                    -- mysql_com.h:59
   end record;

   pragma Convention(C,  struct_st_net);     -- mysql_com.h:53
   subtype NET is struct_st_net;

   type A_MYSQL_FIELD_T is access all st_mysql_field;         -- mysql.h:82

    type st_mem_root is                                  -- mysql.h:18
        record
            free         : Address;                            -- mysql.h:19
            used         : Address;                             -- mysql.h:20
            min_malloc   : c.unsigned;                          -- mysql.h:21
            block_size   : c.unsigned;                          -- mysql.h:22
            error_handler: Address     ;                        -- mysql.h:23
        end record;

    pragma Convention(C, st_mem_root);            -- mysql.h:18
    subtype MEM_ROOT is st_mem_root;                            -- mysql.h:24


    type st_mysql is  record
            The_net            : NET;                           -- mysql.h:71
            host               : Strings.chars_ptr;             -- mysql.h:72
            user               : Strings.chars_ptr;             -- mysql.h:72
            passwd             : Strings.chars_ptr;             -- mysql.h:72
            unix_socket        : Strings.chars_ptr;             -- mysql.h:72
            server_version     : Strings.chars_ptr;             -- mysql.h:72
            host_info          : Strings.chars_ptr;             -- mysql.h:72
            info               : Strings.chars_ptr;             -- mysql.h:73
            db                 : Strings.chars_ptr;             -- mysql.h:73
            port               : c.unsigned;                -- mysql.h:74
            client_flag        : c.unsigned;                -- mysql.h:74
            server_capabilities: c.unsigned;                -- mysql.h:74
            protocol_version   : c.unsigned;                -- mysql.h:75
            field_count        : c.unsigned;                -- mysql.h:76
            thread_id          : c.unsigned;                -- mysql.h:77
            affected_rows      : c.unsigned;                -- mysql.h:78
            insert_id          : c.unsigned;                -- mysql.h:79
            extra_info         : c.unsigned;                -- mysql.h:80
            status             : enum_mysql_status;             -- mysql.h:81
            fields             : A_MYSQL_FIELD_T;               -- mysql.h:82
            field_alloc        : MEM_ROOT;                      -- mysql.h:83
            free_me            : c.signed_char;                 -- mysql.h:84
            reconnect          : c.signed_char;                       -- mysql.h:85
        end record;

               -- mysql.h:89
   function mysql_init(mysql :  MYSQL_SERVER ) return MYSQL_SERVER;
   pragma Import (C, mysql_init, "mysql_init");

               -- mysql.h:89
   function mysql_fetch_field(handle: MYSQL_RES) return MYSQL_FIELD;
   pragma Import (C, mysql_fetch_field, "mysql_fetch_field");

   function mysql_store_result(mysql: MYSQL_SERVER) return MYSQL_RES; -- mysql.h:161
   pragma Import (C, mysql_store_result, "mysql_store_result");

   procedure mysql_free_result(result: MYSQL_RES);                                      -- mysql.h:163
   pragma Import (C, mysql_free_result, "mysql_free_result");


   -- Added PF for blob support
   function Mysql_Real_Escape_String( Mysql: MYSQL_SERVER;
                                      To : Strings.Chars_Ptr;
                                      From : System.Address ;
                                      Length : C.unsigned) return C.Unsigned;
   pragma Import(C, Mysql_Real_Escape_String, "mysql_real_escape_string");


   type Ulonglong is new natural;
   for Ulonglong'Size use 64;


   function Mysql_Insert_Id(Mysql: MYSQL_SERVER) return ulonglong;
   pragma Import(C, Mysql_Insert_Id , "mysql_insert_id");

   ---====================================================================---
   ---===                O B J  E C T     D A T A                      ===---
   ---====================================================================---

   type Field_Info_Array is array( Field_Number ) of St_MySql_Field;

   --|
   --| This is the instance of the component internal data.
   --|
   type Query_Context is record
         In_Use        : Boolean   := False;
         Result        : MYSQL_RES := Null_Address;
         Row           : MYSQL_ROW := Null_Address;
         Field         : Field_Info_Array;
         Affected_Rows : Integer   := 0;
         Lengths       : Unsigned_Long_Ptr.Pointer;
         Next          : Query_ID  := Query_ID'First;
      end record;

   type Query_Table is array( 1..Query_ID'Last ) of Query_Context;

   type Object_Data is record
         Server_Name : Unbounded_String := Null_Unbounded_String;
         Login_Name  : Unbounded_String := Null_Unbounded_String;
         Password    : Unbounded_String := Null_Unbounded_String;

         Connected   : Boolean          := False;
         Server      : MYSQL_Server     := Null_Address;

         Next_Free   : Query_ID         := Query_Table'First;
         Query       : Query_Table;
      end record;

   ---=====================================================================---
   ---===         L O C A L   S U P P O R T   P R O C E D U R E S       ===---
   ---=====================================================================---

   --------------
   -- Instance --
   --------------
   function Instance
     (this : in Object;
      info : in String)
     return  Object_Data_Access
   is
      pragma Unreferenced (info);
      -- Get pointer to object data
   begin
      if this.Data = null then
         raise Not_Initialized;
      end if;

      return this.Data;
   end Instance;

   -----------------
   -- Get_Address --
   -----------------
   function Get_Address(
      Row : in MYSQL_ROW;
      Pos : in Field_Number ) return Address is
      -- Retrieve the address from an array fo addresses

      Result : array( Field_Number ) of Address ;
      for Result'Address use Row;
   begin
      return Result( Pos );
   end Get_Address;

   -----------------
   -- Copy_String --
   -----------------
   function Copy_String (
      Addr   : in Address;
      Len    : in Integer ) return String is
      -- Copy a string from the memory address. This code is not nice but
      -- is works.
      Result : String( 1..len ) := ( others => ' ' );
      byte   : Character;

      Src    : String( 1..Len );
      for Src'Address use Addr;

   begin
      for i in 1..Len loop
         Byte := Src(I);
         exit when Byte = Latin_1.NUL;

         Result(I) := Byte;
      end loop;

      return Result;
   end Copy_String;


   ------------
   -- Escape --
   ------------
   --
   -- Escape a string for setting a blob value in an insert or update statement
   --
   function Escape( Obj : Object ; Str : in String) return Unbounded_String is

      procedure Free is new Ada.Unchecked_Deallocation(Char_Array,Char_Array_Access);

      Data       : Object_Data_Access := Instance( Obj , "escape");

      -- allocating buffer for encoding the buffer
      Buffer : Char_Array_access := new Char_Array(1..Str'Length*2+1);
   begin
      declare

         Buffer_Ptr : Strings.Chars_Ptr := Strings.To_Chars_ptr(Buffer) ;

         olen : C.unsigned := Mysql_Real_Escape_String( Data.Server ,
                                                        Buffer_Ptr,
                                                        Str'address ,
                                                        C.Unsigned(Str'Length) );
         S : Unbounded_string := To_Unbounded_string(Strings.Value(Buffer_Ptr));
      begin
         Free(Buffer);
         return S;
      end;
   exception
      when others =>
         Free(Buffer);
         raise;
   end;


   --------------
   -- Validate --
   --------------
   procedure Validate(
      data   : in Object_Data_Access;
      id     : in Query_ID ) is
      -- Verify if data and query are valid informations.
   begin
      if not Data.Query(id).In_Use then
         raise Usage_Error;
      end if;
   end Validate;

   --------------
   -- Num_Rows --
   --------------
   function NumRows(
      result : in Address  ) return Integer is
      --  Return the number of rows found during a selection
      Res    : MYSQL_RES_Record;
      for Res'Address use Result;
   begin
      if result = Null_Address then
         return 0;
      end if;

      return Integer(Res.Row_Count);
   end NumRows;

   ------------------
   -- Num_Affected --
   ------------------
   function Num_Affected(
      Server  : in Address  ) return Integer is
      -- return the number of affected rows
      S : st_mysql;
      pragma Import (Ada, S);
      for S'Address use Server;
   begin
      return Integer(S.Affected_Rows);
   end Num_Affected;

   -----------------------
   -- Allocate_Query_ID --
   -----------------------
   function Allocate_Query_Id(
      Data   : in Object_Data_Access ) return Query_ID is
      Result : Query_ID := Query_ID'First;
   begin
      if Data.Next_Free = Null_Query_ID then
         raise Out_Of_Resources;
      end if;

      Result := Data.Next_Free;
      Data.Next_Free := Data.Query( Result ).Next;
      Data.Query( Result ).Next   := Null_Query_ID;
      Data.Query( Result ).In_Use := True;

      return Result;
   end Allocate_Query_Id;

   -------------------
   -- Free_Query_ID --
   -------------------
   procedure Free_Query_ID(
      Data   : in Object_Data_Access;
      Id     : in Query_ID ) is
   begin
      Data.Query( Id ).Next   := Data.Next_Free;
      Data.Query( Id ).In_Use := False;

      Data.Next_Free := Id;
   end Free_Query_ID;

   ---======================================================================---
   ---===             C O M P O  N E N T    I N T E R F A C E            ===---
   ---======================================================================---

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
      this  : in out Object ) is
      -- Intialize the Object data and call the Initalization
      -- procedure of the extention.
   begin
      if This.Data /= null then
         raise Usage_Error;
      end if;

      This.Data := new Object_Data;

      for I in Query_Table'Range loop
         if I < Query_Table'Last then
            This.Data.Query(I).Next := I+1;
         end if;
      end loop;

   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      this : in out Object ) is
      -- Finalize the instance by releasing the object resources
      Data : Object_Data_Access renames this.data;

      procedure Free is
            new Unchecked_Deallocation( Object_Data, Object_Data_Access);
   begin
      if Data = null then
         raise Not_Initialized;
      end if;

      if Data.Connected = True then
         Disconnect( This );
      end if;

      for I in Query_Table'Range loop
         if Data.Query(I).Result /= Null_Address then
            Drop_Query( This, I );
         end if;
      end loop;

      Free( Data );
      This.Data := null;
   end Finalize;

   ---=====================================================================---
   ---===           A T T R I B U T E    F U N C T I O N S              ===---
   ---=====================================================================---

   ----------
   -- User --
   ----------
   procedure User(
      This : in out Object;
      Name : in String ) is
      -- Set the user Name for a server connection
      Data   : Object_Data_Access := Instance( this, "User");
   begin
      Data.Login_Name := To_Unbounded_String( Name );
   end User;

   --------------
   -- Password --
   --------------
   procedure Password(
      This  : in out Object;
      pwd   : in String ) is
      Data  : Object_Data_Access := Instance( this, "Password" );
   begin
      Data.Password := To_Unbounded_String( pwd );
   end Password;

   ---=====================================================================---
   ---===                        M E T H O D S                          ===---
   ---=====================================================================---

   -------------
   -- Connect --
   -------------
   procedure Connect(
      this   : in out Object;
      Server : in String;
      DB     : in String := "";
      Port   : in Natural := 0;
      Socket : in String := "";
      Flag   : in Natural := 0) is
      -- Connect to the server specified by the Object.
      data       : Object_Data_Access := Instance( this, "Connect" );
      DB_Ptr     : Strings.chars_ptr;
      Socket_Ptr : Strings.chars_ptr;
   begin
      Data.Server_Name := To_Unbounded_String( Server );

      if DB = "" then
         DB_Ptr := Null_Ptr;
      else
         DB_Ptr := New_String(DB);
      end if;
      if Socket = "" then
         Socket_Ptr := Null_Ptr;
      else
         Socket_Ptr := New_String(Socket);
      end if;

      Data.Server := MYSQL_Init( Null_Address);
      Data.Server := MYSQL_Real_Connect
        ( Data.Server,
          New_String( To_String( data.Server_Name) ),
          New_String( To_String(data.Login_Name) ),
          New_String( To_String(data.Password) ),
          DB_Ptr,
          Unsigned(Port),
          Socket_Ptr,
          Unsigned(Flag) );
      if Data.Server = Null_Address then
         raise Connect_Failure;
      end if;

      Data.Connected := True;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect(
      this   : in out Object ) is
      -- Disconnect from a server.
      data   : Object_Data_Access := Instance( this, "Disconnect");
   begin
      mysql_close( data.Server );
      data.Connected := False;
   end Disconnect;

   ---------------
   -- Select_DB --
   ---------------
   procedure Select_DB(
      this   : in out Object;
      name   : in String ) is
      -- Select a data base from the server.
      data   : constant Object_Data_Access := Instance( this, "Select_DB" );
      RC     : C.Int := 0;
   begin
      RC :=  mysql_select_db( data.Server, New_String( name ));
      if not ( RC = 0 ) then
         raise DB_Name_Failure;
      end if;
   end Select_DB;

   --------------------
   -- Number_Of_Rows --
   --------------------
   function Nbr_Of_Rows(
      this       : in Object;
      id         : in Query_ID ) return Integer is
      --  Return the number of rows found during a selection
      Data : constant Object_Data_Access := Instance( this, "Nbr_Of_Rows");
   begin
      return NumRows(Data.Query(id).Result );
   end Nbr_Of_Rows;

   -----------
   -- Query --
   -----------
   function Query(
      This       : in Object;
      Query      : in String ) return Query_ID
   is
      -- Perform a query on the server and retrieve the result
      -- context. The function returns the id of the query context.
      Data       : Object_Data_Access := Instance( this, "Query");
      Result     : Query_ID := 1;
      RC         : C.int    := 0;
      RES_Address: MYSQL_RES;
   begin
      -- perform the query
      declare
         Query_Str : Chars_ptr := New_String( Query);
      begin
         RC := mysql_query( data.Server, Query_Str);
         Free(Query_Str);
      end;

      if RC /= 0  then
         raise Error_During_Query;
      else
         Result := Allocate_Query_ID(Data);
      end if;

      -- get the information about the result set.
      declare
         Q  : Query_Context renames  Data.Query(Result);
      begin
         RES_Address := mysql_store_result( data.Server );
         if RES_Address /= Null_Address then
            if NumRows( RES_Address ) < 1 then
               -- Free the reource, and raise error
               Q.In_Use := False;
               raise No_Query_Result;
            end if;

            if NumRows( RES_Address ) > 0 then
               Q.Result := RES_Address;
               Q.Row    := mysql_fetch_row(Q.Result);

               -- Get All the Columns Lengths
               Q.Lengths := Mysql_Fetch_Lengths(Q.Result);

               --for I in Field_Number loop
               --   declare
               --      Field : constant MYSQL_Field := mysql_fetch_field( Q.Result  );
               --   begin
               --      exit when Field = null;
               --   Q.Field(i) := Field.all;
               --   end;
               --end loop;
            else
               Free_Query_Id(Data,Result);
               raise No_Query_result;
            end if;
         else
            Q.Affected_Rows := Num_Affected( Data.Server );
         end if;
      end;

      return Result;
   end Query;

   ----------------
   -- Drop_Query --
   ----------------
   procedure Drop_Query(
      this    : in out Object;
      id      : in Query_ID ) is
      -- Release the query context
      Data    : Object_Data_Access := Instance( this, "Drop_Query" );
      Q       : Query_Context renames Data.Query(id);

      procedure C_Free(P : Unsigned_Long_Ptr.Pointer);
      pragma Import(C,C_Free, "free");

      use Unsigned_Long_Ptr;

   begin
      if Q.In_Use then
         if Q.Result /= Null_Address then
            mysql_free_result( Q.Result);
         end if;
         Q.Result := Null_Address;
         Q.In_Use := False;

         -- Free Lengths
         if Q.Lengths /= null then
            --            C_Free(Q.Lengths);
            -- got RTLRuntimeException in the debugger ??? on win32 platform
            Q.Lengths := null;
         end if;
      end if;
      Free_Query_ID( Data, Id );
   end Drop_Query;

   ----------
   -- Next --
   ----------
   procedure Next(
      this    : in out Object;
      query   : in Query_ID ) is
      -- Get the next matching tuple for the given query
      Data    : Object_Data_Access := Instance( this, ".Next");
      Q       : Query_Context    renames Data.Query(query);
   begin
      Validate( data, query );

      if Q.Result /= Null_Address then
         Q.Row := mysql_fetch_row( Q.Result );

         -- Get All the Columns Lengths
         Q.Lengths := Mysql_Fetch_Lengths(Q.Result);

         if Q.Row = Null_Address then
            raise No_More_Rows;
         end if;
      else
         raise Usage_Error;
      end if;
   end Next;

   --------------------
   -- Get_Field_Type --
   --------------------
   function Get_Field_Type(
      This   : in Object;
      Id     : in Query_ID;
      FNumber: in Field_Number ) return Field_Type
   is
      Data : constant Object_Data_Access := Instance( this, "Get_Field_Type");
      Q    : Query_Context renames  data.Query(id);
   begin
      Validate( data, id );
      case Q.Field(FNumber).C_Type is
         when FIELD_TYPE_VAR_STRING
           |  FIELD_TYPE_STRING =>
           return String_Type;
         when FIELD_TYPE_DATE =>
            return Date_Type;
         when FIELD_TYPE_TINY_BLOB
           | FIELD_TYPE_MEDIUM_BLOB
           | FIELD_TYPE_LONG_BLOB
           | FIELD_TYPE_BLOB =>
            return Blob_Type;
         when FIELD_TYPE_TINY
           | FIELD_TYPE_SHORT
           | FIELD_TYPE_LONG =>
            return Integer_Type;
         when FIELD_TYPE_FLOAT
           | FIELD_TYPE_DOUBLE =>
            return Float_Type;
         when others =>
            return Unsupported_Type;
      end case;
      --FIELD_TYPE_NULL,                                        -- mysql_com.h:67
      --FIELD_TYPE_TIMESTAMP,                                   -- mysql_com.h:67
      --FIELD_TYPE_LONGLONG,                                    -- mysql_com.h:68
      --FIELD_TYPE_INT24,                                       -- mysql_com.h:68
      --FIELD_TYPE_TIME,                                        -- mysql_com.h:69
      --FIELD_TYPE_DATETIME,                                    -- mysql_com.h:70
      --FIELD_TYPE_YEAR,                                        -- mysql_com.h:70
      --FIELD_TYPE_NEWDATE,                                     -- mysql_com.h:71
      --FIELD_TYPE_ENUM,                                        -- mysql_com.h:72
      --FIELD_TYPE_SET,                                         -- mysql_com.h:73
   end Get_Field_Type;

   function Get_Field_Type(
      This : in Object;
      Id   : in Query_ID;
      Name : in String )   return Field_Type is
   begin
      return Get_Field_Type(This, Id, Get_Field_Number( this, id, name ));
   end Get_Field_Type;

   ----------------------
   -- Get_Field_Number --
   ----------------------
   function Get_Field_Number(
      this       : in Object;
      id         : in Query_ID;
      name       : in String) return Field_Number
   is
      -- Translate the name into a field number.
      data : constant Object_Data_Access := Instance( this, "Get_Field_Number" );
   begin
      Validate( data, id );

      for i in Field_Number loop
         if Value( Data.Query(id).Field(i).Name ) = name then
            return i;
         end if;
      end loop;

      raise Unknown_Field;
   end Get_Field_Number;

   --------------------
   -- Get_Field_Name --
   --------------------
   function Get_Field_Name
     ( This      : in Object;
       Id        : in Query_Id;
       FNumber   : in Field_Number ) return String
   is
      data : constant Object_Data_Access := Instance( this, "Get_Field_Number" );
   begin
      Validate( data, id );
      return Value (Data.Query(id).Field(FNumber).Name);
   end Get_Field_Name;

   ----------------------
   -- Get_Field_Length --
   ----------------------
   function Get_Field_Length
     ( This      : in Object;
       Id        : in Query_Id;
       FNumber   : in Field_Number ) return Natural
   is
      Data   : constant Object_Data_Access := Instance( this, "Get_Field_Length" );
      Q      : Query_Context renames Data.Query(id);
      Length : Natural;
   begin
      Validate( data, id );
      declare
         Lengths : Unsigned_Long_Array :=
           Unsigned_Long_Ptr.Value(Q.Lengths, C.Ptrdiff_T( Q.Field'Length ));
      begin
         length  := Positive( lengths(Fnumber-Field_Number'first) );
         return Length;
      end;
   end Get_Field_Length;

   ----------------------
   -- Get_Field_Length --
   ----------------------
   function Get_Field_Length
     ( This      : in Object;
       Id        : in Query_Id;
       Name      : in String ) return Natural
   is
   begin
      return Get_Field_Length( this, id, Get_Field_Number( this, id, name ) );
   end Get_Field_Length;

   ------------------
   -- String_Field --
   ------------------
   function String_Field(
      This       : in Object;
      Id         : in Query_ID;
      FNumber    : in Field_Number ) return Unbounded_String
   is
      -- Get String field identified by the field number
      Data   : constant Object_Data_Access := Instance( this, "String_Field");
      Length : Integer := 0;
      Q      : Query_Context renames  data.Query(id);
   begin
      Validate( data, id );

      Length := Integer( Q.Field(fNumber).Length );
      if Length = -1 then
         return To_Unbounded_String("");
      end if;

      if Get_Field_Type(This, Id, FNumber) = String_Type then
         return
            To_Unbounded_String(
               Copy_String( Get_Address( Q.Row, FNumber ), Length ));
      else
         Put_Line(Q.Field(fNumber).c_type'Img &
                  " is not a supported string type.");
         raise Field_Type_Error;
      end if ;
   end String_Field;

   ------------------
   -- String_Field --
   ------------------
   function String_Field(
      this      : in Object;
      id        : in Query_ID;
      name      : in String ) return Unbounded_String is
      -- Get the named string field
   begin
      return String_Field( this, id, Get_Field_Number( this, id, name ) );
   end String_Field;

   ----------------
   -- Date_Field --
   ----------------
   function Date_Field(
      this    : in Object;
      id      : in Query_ID;
      fNumber : in Field_Number ) return Time is
      -- Get String field identifird by the field number
      data    : constant Object_Data_Access := Instance( this, "Date_Field");
      Length  : Integer := 0;
      Q       : Query_Context renames Data.Query(id);
   begin
      Validate( data, id );

      Length  := Integer( Q.Field(fNumber).Length );
      if Length = -1 then
         raise Field_Is_Null;
      end if;

      if Get_Field_Type( This, Id, fNumber) = Date_Type then
         declare
            Time_String : String(1..length);
            Year        : Year_Number;
            Month       : Month_Number;
            Day         : Day_Number;
            tmp         : String(1..5);

            next        : Positive := 1;

            -- copy a date element into the tmp string and return the start offset
            procedure Copy_Element( Str : String ) is
            begin
               tmp := (others => ' ');

               for i in 1..tmp'Length loop
                  if Str(next) = '-' then
                     next := next + 1;
                     exit;
                  end if;
                  tmp(i) := Str(next);

                  exit when next = length;
                  next := next + 1;
               end loop;

            exception
               when Others =>
                  put_line("Error in Copy_Element : " & Str &
                           " at " & Integer'Image(next) );
                  raise;

            end Copy_Element;

         begin
            Move( Source => Copy_String( Get_Address( data.Query(id).Row,
                                                      fNumber ),
                                         length ),
                  Target => Time_String);

            Copy_Element( Time_String );
            Year  := Year_Number'Value( tmp );

            Copy_Element( Time_String );
            Month := Month_Number'Value( tmp );

            Copy_Element( Time_String );
            Day   := Day_Number'Value( tmp );

            return Time_Of( Year, Month, Day );

         exception
            when others =>
               Put_Line(Q.Field(fNumber).c_type'Img &
                        " is not a supported date type. Error...");
                raise Field_Parse_Error;
         end;
      else
         Put_Line(Q.Field(fNumber).c_type'Img &
                  " is not a supported date type.");
         raise Field_Type_Error;
      end if;

   end Date_Field;

   ----------------
   -- Date_Field --
   ----------------
   function Date_Field(
      this    : in Object;
      id      : in Query_ID;
      name    : in String ) return Time is
      -- Get the named string field
   begin
      return Date_Field( this, id, Get_Field_Number( this, id, name ));
   end Date_Field;

   ----------------
   -- Blob_Field --
   ----------------
   function Blob_Field
     ( This    : in  Object;
       Id      : in  Query_ID;
       fNumber : in  Field_Number) return Byte_Array_access
   is
      Data    : constant Object_Data_Access := Instance( this, "Blob_Field");
      Length  : Positive;
      Q       : Query_Context renames Data.Query(id);
   begin
      Validate( data, id );
      Length := Get_Field_Length( This, Id, FNumber);

      if Get_Field_Type( This, Id, FNumber) = Blob_Type then
         declare
            Ar : Byte_Array_access := new Byte_Array(0..Length-1);
            T : Byte_Array(0..Length-1) ;
            for T'Address use Get_Address(Q.Row,FNumber);
         begin
            Ar(0..Length-1) := T(0..Length-1);
            return Ar;
         end;
      else
         Put_Line(Q.Field(fNumber).c_type'Img &
                  " is not a supported blob type.");
         raise Field_Type_Error;
      end if;
   exception
      when others =>
         Put_Line("Blob_Field: Exception while retrieving blob field: " &
                  FNumber'img);
	return null;
   end Blob_Field;


   ----------------
   -- Blob_Field --
   ----------------
   function Blob_Field
     ( this      : in Object;
       id        : in Query_ID;
       name      : in String ) return Byte_Array_Access
   is
      fNumber : constant Field_Number := Get_Field_Number( this, id, name );
   begin
      return Blob_Field( This, Id, FNumber);
   end Blob_Field;


   -------------------
   -- Integer_Field --
   -------------------
   function Integer_Field(
      This    : in Object;
      Id      : in Query_ID;
      fNumber : in Field_Number ) return Integer is
      --  return a number field as an integer.
      Data    : constant Object_Data_Access := Instance( this, "Integer_Field");
      Length  : Positive;
      Q       : Query_Context renames Data.Query(id);
   begin
      Validate( Data, id );

      Length := Positive( Data.Query(id).Field(fNumber).length );

      if Q.Field(fNumber).c_type = FIELD_TYPE_TINY or
         Q.Field(fNumber).c_type = FIELD_TYPE_SHORT or
         Q.Field(fNumber).c_type = FIELD_TYPE_LONG then
         return Integer'Value(
            Copy_String( Get_Address(Data.Query(id).Row, fNumber),Length));
      else
         Put_Line(Get_Field_Type(This, Id, Fnumber)'Img &
                  " is not a supported integer type.");
         raise Field_Type_Error;
      end if;
   end Integer_Field;

   -------------------
   -- Integer_Field --
   -------------------
   function Integer_Field(
      This    : in Object;
      Id      : in Query_ID;
      Name    : in String ) return Integer is
      -- return the number field by name
   begin
      return Integer_Field( this, id, Get_Field_Number( this, id, name ) );
   end Integer_Field;

   -----------------
   -- Float_Field --
   -----------------
   function Float_Field(
      This    : in Object;
      Id      : in Query_ID;
      fNumber : in Field_Number ) return Float is
      -- return a float field
      Data    : constant Object_Data_Access := Instance( this, ".Float_Field");
      Length  : Positive;
      Q       : Query_Context    renames Data.Query(id);
   begin
      Validate( Data, Id );

      Length := Positive( Q.Field(FNumber).Length );

      if Q.Field(fNumber).c_type = FIELD_TYPE_FLOAT or
         Q.Field(fNumber).c_type = FIELD_TYPE_DOUBLE then
         return Float'Value(
            Copy_String( Get_Address(data.Query(id).Row, fNumber),length));
      else
         Put_Line(Get_Field_Type(This, Id, Fnumber)'Img &
                     " is not a supported float type.");
         raise Field_Type_Error;
      end if;
   end Float_Field;

   -----------------
   -- Float_Field --
   -----------------
   function Float_Field(
      This    : in Object;
      Id      : in Query_ID;
      Name    : in String ) return Float is
      -- return the number field by name
   begin
      return Float_Field( this, id, Get_Field_Number( this, id, name ) );
   end Float_Field;


   ---------------
   -- Insert_ID --
   ---------------
   --
   --  return the Inserted ID for the last command
   --
   function Insert_ID(This : in object) return Natural is
      data    : Object_Data_Access := Instance( this, ".Insert_ID");
   begin
      return Natural(Mysql_Insert_Id(Data.Server));
   end;

end GNU.DB.MySQL;
