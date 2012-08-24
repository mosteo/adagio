-------------------------------------------------------------------------------
--									                                                  --
--  Filename        : $Source: /cvsroot/gnade/gnade/dbi/sqlite/gnu-db-sqlite.ads,v $
--  Description     : SQLite Base Package                                    --
--  Author          : Ching Bon Lam                                          --
--  Created         : 26.7.2003                                              --
--  Last Modified By: $Author: cblam $				                             --
--  Last Modified On: $Date: 2004/02/07 19:54:20 $		                       --
--  Status          : $State: Exp $					                             --
--									                                                  --
--  Copyright (C) 2003 Ching Bon Lam                                         --
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
--  Thick binding to sqlite (http://www.sqlite.org).                         --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  Sqlite homepage : http://www.sqlite.org                                  --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               Ching Bon Lam  <cblam@gmx.net>                              --
--									                                                  --
-------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package GNU.DB.SQLite is

   type Object is private;
   type Ptr is access all Object;

   type File_Mode_Type is (READ, WRITE, READ_WRITE);

   subtype Ustring is Ada.Strings.Unbounded.Unbounded_String;

   type Row_Array is array(integer range <>) of Ustring;
   type Row_Reference is access Row_Array;
   -- one row of data returned by the callback

   type Table_Array is array(integer range <>) of Row_Reference;
   type Table_Reference is access Table_Array;

   type Table_Controlled is new Ada.Finalization.Controlled with record
      Table : Table_Reference;
   end record;
   -- one table of data returned by 'get_table'

   type Exec_Callback is access function
     (argc        : integer;
      argv        : Row_Array;
      columnNames : Row_Array)
      return integer;
   -- callback type used for exec_with_callback

   ----------------
   -- exceptions --
   ----------------

   DB_NOT_OPEN       : exception;
   DB_ALREADY_OPEN   : exception;

   -- SQLITE_OK         : exception; -- not an error of course
   SQLITE_ERROR      : exception;
   SQLITE_INTERNAL   : exception;
   SQLITE_PERM       : exception;
   SQLITE_ABORT      : exception;
   SQLITE_BUSY       : exception;
   SQLITE_LOCKED     : exception;
   SQLITE_NOMEM      : exception;
   SQLITE_READONLY   : exception;
   SQLITE_INTERRUPT  : exception;
   SQLITE_IOERR      : exception;
   SQLITE_CORRUPT    : exception;
   SQLITE_NOTFOUND   : exception;
   SQLITE_FULL       : exception;
   SQLITE_CANTOPEN   : exception;
   SQLITE_PROTOCOL   : exception;
   SQLITE_EMPTY      : exception;
   SQLITE_SCHEMA     : exception;
   SQLITE_TOOBIG     : exception;
   SQLITE_CONSTRAINT : exception;
   SQLITE_MISMATCH   : exception;
   SQLITE_MISUSE     : exception;
   SQLITE_NOLFS      : exception;
   SQLITE_AUTH       : exception;
   SQLITE_FORMAT     : exception;
   SQLITE_ROW        : exception;
   SQLITE_DONE       : exception;
   UNDEFINED_ERROR   : exception;

   --------------------------
   -- primitive operations --
   --------------------------

   procedure open
     (db       : in out Object;
      filename : in     string;
      mode     : in     File_Mode_Type := READ_WRITE;
      errmsg   :    out Ustring);
   -- Opens a DB from a file. If the file is non-existence, it will create
   -- the file. Raises DB_ALREADY_OPEN if you call it on an open db.
   -- exceptions: DB_ALREADY_OPEN

   procedure close
     (db : in out Object);
   -- Closes a DB that's open. Raises DB_NOT_OPEN if the db is not open.
   -- exceptions: DB_NOT_OPEN

   procedure exec
     (db     : in out Object;
      sql    : in     string;
      errmsg : out    Ustring);
   -- Executes the "sql" query and returns nothing. It's the same as
   -- exec_with_callback, however, the callback is left out. Most usable
   -- for queries that doesn't return any result.
   -- Raises DB_NOT_OPEN when db is not open. Raises SQLITE exceptions when
   -- there is something wrong with sqlite itself.
   -- exceptions : DB_NOT_OPEN, all SQLITE exceptions

   procedure exec_with_callback
     (db      : in out Object;
      sql     : in     string;
      cb      : in     Exec_Callback;
      errmsg  :    out Ustring);
   -- Does the same as exec, but it calls the callback for the processing of
   -- the results.
   -- exceptions : DB_NOT_OPEN, all SQLITE exceptions

   generic
      type Data_Type is private;

      type Exec_Callback_with_data is access function
        (pArg        : Data_Type;
         argc        : integer;
         argv        : Row_Array;
         columnNames : Row_Array)
         return integer;
   procedure exec_with_callback_with_data
     (db      : in out Object;
      sql     : in     string;
      cb      : in     Exec_Callback_with_data;
      arg     : in out Data_Type;
      errmsg  :    out Ustring);
   -- Does the same as exec_with_callback, but you can pass a datastructure
   -- with it.
   -- exceptions : DB_NOT_OPEN, all SQLITE exceptions

   procedure get_table
      (db     : in out Object;
       sql    : in     string;
       table  :    out Table_Controlled;
       errmsg :    out Ustring);
   -- Wrapper around exec_with_callback, it returns a rectangular table with
   -- each row a Row_Array.
   -- First row contains the columnNames of the fields.
   -- Don't forget to call free_table after you are done with table
   -- exceptions : DB_NOT_OPEN, all SQLITE exceptions

private

   type DB_Type is limited null record;
   type DB_Access is access all DB_Type;
   for DB_Access'Storage_Size use 0;
   pragma Convention (C, DB_Access);

   type Object is record
      open : boolean   := FALSE;
      db   : DB_Access := null;
   end record;

   procedure Initialize
     (Self : in out Table_Controlled);

   procedure Finalize
     (Self : in out Table_Controlled);


end GNU.DB.SQLite;