-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/dbi/mysql/gnu-db-mysql.ads,v $
--  Description     : Ada Database Objects - Test client                     --
--  Author          : Michael Erdmann                                        --
--  Created         : 18.1.2002                                              --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2004/05/29 13:29:19 $                           --
--  Version         : $Revision: 1.10 $                                       --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2002-2004 Denis Chalon, M.Erdmann                          --
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This component implements a thick binding to MySQL, which is             --
--  available for Linix. This interface allows Ada 95 clients to communicate --
--  with the MYSQL server at any host.                                       --
--                                                                           --
--  Queries are sent to the server a sql strings. The result of a query      --
--  may be processes by the client row by row. Eeach query receives a        --
--  Query id which allows to use multiple queries at the same time.          --
--                                                                           --
--  The protocol between client and server is the following one:             --
--                                                                           --
--    Set User Id                                                            --
--    Connect to Server                                                      --
--    Select a data base                                                     --
--                                                                           --
--    while Application is active                                            --
--       Issue a query                                                       --
--       while rows available                                                --
--           process data;                                                   --
--       drop Query                                                          --
--                                                                           --
--    disconnect                                                             --
--                                                                           --
--                                                                           --
--  Component Data                                                           --
--  ==============                                                           --
--  User  - Name of the User logged in                                       --
--                                                                           --
--  Query_ID     - Id of a query. This resource is allocated by the Query    --
--                 command and can be released by the Drop_Query.            --
--  Field_Number - The colum number of a field in the query result.          --
--                                                                           --
--  Operations on the component data                                         --
--  ================================                                         --
--  see component specification below and documentation.                     --
--                                                                           --
--  Error Handling                                                           --
--  ==============                                                           --
--  Error handling is done by exceptions. See documentattion.                --
--                                                                           --
--                                                                           --
--  Extension                                                                --
--  =========                                                                --
--  None                                                                     --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  Only 100 fields per query                                                --
--  Number of Queries per connection is 10.                                  --
--                                                                           --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
with Unchecked_Deallocation ;
with System.Storage_Elements;
use  System;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;

package GNU.DB.MySQL is

   ---=====================================================================---
   ---===             C O M P O N E N T   I N T E R F A C E             ===---
   ---=====================================================================---
   type Object is private;
   type Handle is access Object;

   procedure Initialize( this  : in out Object);
   procedure Finalize( this : in out Object );

   Usage_Error      : exception;
   Not_Initialized  : exception;
   Out_Of_Resources : exception;

   type Query_ID is new Natural range 0..10;
   Null_Query_ID : constant Query_ID := Query_ID'First;

   type Field_Type is (String_Type, Date_Type, Blob_Type, Integer_Type,
                            Float_Type, Unsupported_Type);

   subtype Field_Number is Integer range 1..100;

   type Byte is range 0..255;
   for Byte'Size use 8;
   type Byte_Array is array(Natural range <>) of Byte;
   type Byte_Array_Access is access all Byte_Array;

   procedure Free is new Unchecked_Deallocation(
      Byte_Array, Byte_Array_Access );

   DB_Name_Failure  : exception;
   Field_Is_Null    : exception;

   ---=====================================================================---
   ---===                  A T T R I B U T E S                       ===---
   ---=====================================================================---

   procedure User(
      This : in out Object;
      Name : in String );

   procedure Password(
      this : in out Object;
      Pwd  : in String );

   ---=====================================================================---
   ---===                     M E T H O D S                             ===---
   ---=====================================================================---

   ---------------------------------------------------------------------------
   --| Description    : Connect to the given server, use the user name which
   --|                  has been set by the User Attribute.
   --| Preconditions  : none.
   --| Postconditions : Server is connected
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Connect(
      this   : in out Object;
      Server : in String;
      DB     : in String := "";
      Port   : in Natural := 0;
      Socket : in String := "";
      Flag   : in Natural := 0);

   Connect_Failure : exception;

   ---------------------------------------------------------------------------
   --| Description    : Disconnect from the server.
   --| Preconditions  : connected to the server
   --| Postconditions : disconnected from the server
   --| Exceptions     : None
   --| Note           : -
   ---------------------------------------------------------------------------
   procedure Disconnect(
      this : in out Object );

   ---------------------------------------------------------------------------
   --| Description    : Select a database at the server
   --| Preconditions  : Connected to a server
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Select_DB(
      this : in out Object; Name : in String );

   ---------------------------------------------------------------------------
   --| Description    : Perform an sql query. Input is a normal mysql query
   --|                  string.
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Query(
      This  : in Object;
      Query : in String ) return Query_ID;

   Error_During_Query : exception;
   No_Query_Result    : exception;

   function Nbr_Of_Rows(
      This  : in Object;
      Id    : in Query_ID ) return Integer;

   ---------------------------------------------------------------------------
   --| Description    : get last insert ID
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Insert_ID(This : in object) return Natural;

   ---------------------------------------------------------------------------
   --| Description    : Drop the query
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Drop_Query(
      This : in out Object;
      Id   : in Query_ID );

   ---------------------------------------------------------------------------
   --| Description    : Perform an sql query.
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Next(
      This  : in out Object;
      Query : in Query_ID);

   No_More_Rows : exception;

   ---------------------------------------------------------------------------
   --| Description    : Get the field number
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Get_Field_Number(
      This : in Object;
      Id   : in Query_ID;
      Name : in String )   return Field_Number;

   ---------------------------------------------------------------------------
   --| Description    : Escape a string for Mysql, this permit to store datas with
   --|                  special caracters as '/ \0 ... useful for blobs
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Escape( Obj : Object ; Str : in String) return Unbounded_String;


   ---------------------------------------------------------------------------
   --| Description    : Get the field of the current colume and return the
   --|                  specified data type. Access via field number or a
   --|                  field name is provided for all data types
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Get_Field_Length
     ( This      : in Object;
       Id        : in Query_Id;
       FNumber   : in Field_Number ) return Natural;

   function Get_Field_Length
     ( This      : in Object;
       Id        : in Query_Id;
       Name      : in String ) return Natural;

   function Get_Field_Type(
      This : in Object;
      Id   : in Query_ID;
      Name : in String )   return Field_Type;

   function Get_Field_Type(
      This   : in Object;
      Id     : in Query_ID;
      FNumber: in Field_Number ) return Field_Type;

   ---------------------------------------------------------------------------
   --| Description    : Get the type of the field specified by name or by
   --|                  field number
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Get_Field_Name
     ( This      : in Object;
       Id        : in Query_Id;
       FNumber   : in Field_Number ) return String;

   ---------------------------------------------------------------------------
   --| Description    : Get the name of the field specified by
   --|                  field number
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   Unknown_Field     : exception;
   Field_Type_Error  : exception;
   Field_Parse_Error : exception;

   function String_Field(
      This : in Object;
      Id   : in Query_ID;
      name : in String ) return Unbounded_String;

   function String_Field(
      This    : in Object;
      Id      : in Query_ID;
      fNumber : in Field_Number ) return Unbounded_String;

   function Date_Field(
      This      : in Object;
      Id        : in Query_ID;
      Fnumber   : in Field_Number ) return Time;

   function Date_Field(
      This      : in Object;
      Id        : in Query_ID;
      Name      : in String ) return Time;


   function Blob_Field
     ( this      : in Object;
       id        : in Query_ID;
       fNumber   : in Field_Number ) return Byte_Array_access;

   function Blob_Field
   ( this      : in Object;
     id        : in Query_ID;
     name      : in String ) return Byte_Array_access;


   function Integer_Field(
      This      : in Object;
      Id        : in Query_ID;
      Fnumber   : in Field_Number ) return Integer;


   function Integer_Field(
      This      : in Object;
      Id        : in Query_ID;
      Name      : in String ) return Integer;

   function Float_Field(
      this      : in Object;
      id        : in Query_ID;
      fNumber   : in Field_Number ) return Float;

   function Float_Field(
      This      : in Object;
      Id        : in Query_ID;
      Name      : in String ) return Float;

   ---=====================================================================---
   ---===                   E X T E N S I O N                           ===---
   ---=====================================================================---


   ---=====================================================================---
private
   type Object_Data;
   type Object_Data_Access is access Object_Data;

   type Object is record
         Data : Object_Data_Access := null;
      end record;

end GNU.DB.MySQL;


