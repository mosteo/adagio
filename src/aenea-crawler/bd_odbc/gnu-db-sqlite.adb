with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

with System;

package body GNU.DB.SQLite is

   pragma Linker_Options("-lsqlite");

   package ASU renames Ada.Strings.Unbounded;
   package C   renames Interfaces.C;
   package CS  renames Interfaces.C.Strings;

   -------------------
   -- local support --
   -------------------

   type Pchars_ptr is access all CS.chars_ptr;
   -- equivalent of access string
   -- Most of the sqlite functions return an string that contains the error
   -- message when an error is occured. Sqlite uses "char**" for errmsg, which
   -- can be simulated by "access all CS.chars_ptr" in Ada95.

   procedure Free is
      new Ada.Unchecked_Deallocation(CS.chars_ptr, Pchars_ptr);

   subtype chars_ptr is CS.chars_ptr;
   -- equivalent of string

   type chars_ptr_array is array(C.size_t range <>) of aliased chars_ptr;
   -- same as char**

   package Pstring is
   new Interfaces.C.Pointers(C.size_t,
                             chars_ptr,
                             chars_ptr_array,
                             0);

   subtype Pstring_array is Pstring.Pointer;


   type exec_sqlite_callback is access function
     (pArg        : System.Address;
      argc        : C.int;
      argv        : Pstring_array;
      columnNames : Pstring_array)
      return C.int;
   -- sqlite.h:91
   -- typedef int (*sqlite_callback)(void*,int,char**, char**);

   function sqlite_exec
     (db     : DB_Access;
      sql    : C.char_array;
      cb     : exec_sqlite_callback;
      arg    : System.Address;
      errmsg : Pchars_ptr)
      return C.int;
   pragma Import(C, sqlite_exec, "sqlite_exec");
   -- sqlite.h:133
   -- int sqlite_exec(
   --   sqlite*,                      /* An open database */
   --   const char *sql,              /* SQL to be executed */
   --   sqlite_callback,              /* Callback function */
   --   void *,                       /* 1st argument to callback function */
   --   char **errmsg                 /* Error msg written here */
   -- );

   procedure check_return_code
      (code : in integer)
   is
   begin
      case code is
         when  0 => null;
         when  1 => raise SQLITE_ERROR;
         when  2 => raise SQLITE_INTERNAL;
         when  3 => raise SQLITE_PERM;
         when  4 => raise SQLITE_ABORT;
         when  5 => raise SQLITE_BUSY;
         when  6 => raise SQLITE_LOCKED;
         when  7 => raise SQLITE_NOMEM;
         when  8 => raise SQLITE_READONLY;
         when  9 => raise SQLITE_INTERRUPT;
         when 10 => raise SQLITE_IOERR;
         when 11 => raise SQLITE_CORRUPT;
         when 12 => raise SQLITE_NOTFOUND;
         when 13 => raise SQLITE_FULL;
         when 14 => raise SQLITE_CANTOPEN;
         when 15 => raise SQLITE_PROTOCOL;
         when 16 => raise SQLITE_EMPTY;
         when 17 => raise SQLITE_SCHEMA;
         when 18 => raise SQLITE_TOOBIG;
         when 19 => raise SQLITE_CONSTRAINT;
         when 20 => raise SQLITE_MISMATCH;
         when 21 => raise SQLITE_MISUSE;
         when 22 => raise SQLITE_NOLFS;
         when 23 => raise SQLITE_AUTH;
         when 24 => raise SQLITE_FORMAT;
         when others => raise UNDEFINED_ERROR;
      end case;
   end check_return_code;
   -- hardcoded in the source of sqlite

   procedure exec_real
      (db    : in out Object;
       sql   : in     string;
       proxy : in     exec_sqlite_callback;
       err   :    out Ustring)
   is
      tmperr : Pchars_ptr := new CS.chars_ptr;
      ret    : integer;
   begin
      tmperr.all := CS.Null_Ptr;

      -- use the proxy here
      ret := integer(sqlite_exec(db.db,
                                 C.To_C(sql),
                                 proxy,
                                 System.Null_Address,
                                 tmperr));

      if (CS."/="(tmperr.all, CS.Null_Ptr)) then
         err := ASU.To_Unbounded_String(CS.Value(tmperr.all));
      end if;

      Free(tmperr);

      check_return_code(ret);
   end;

   -----------
   -- close --
   -----------

   procedure close
      (db : in out Object)
   is

      procedure sqlite_close
         (db : in out DB_Access);
      pragma Import(C, sqlite_close, "sqlite_close");
      -- sqlite.h:86
      -- void sqlite_close(sqlite *);

   begin

      if not db.open then
         raise DB_NOT_OPEN;
      end if;

      sqlite_close(db.db);
      db.db   := null;
      db.open := FALSE;

   end close;

   ----------
   -- exec --
   ----------

   procedure exec
     (db     : in out Object;
      sql    : in string;
      errmsg : out Ustring)
   is

      use C;

      function nothing
        (pArg        : System.Address;
         argc        : C.int;
         argv        : Pstring_array;
         columnNames : Pstring_array)
         return C.int
      is
      begin

         return 0;

      end nothing;
      -- create a proxy callback that converts all C data
      -- to Ada data and pass it on to the callback that the user provided

   begin

      if not db.open then
         raise DB_NOT_OPEN;
      end if;

      exec_real(db, sql, nothing'Unrestricted_Access, errmsg);

   end exec;

   ------------------------
   -- exec_with_callback --
   ------------------------

   procedure exec_with_callback
     (db      : in out Object;
      sql     : in     string;
      cb      : in     Exec_Callback;
      errmsg  :    out Ustring)
   is

      use C;

      function proxy
        (pArg        : System.Address;
         argc        : C.int;
         argv        : Pstring_array;
         columnNames : Pstring_array)
         return C.int
      is
      begin
         if (argc <= 0) then
            return 0;
         end if;

         declare
            i            : C.size_t := 0;
            argv2        : Row_Array(0 .. integer(argc) - 1 );
            columnNames2 : Row_Array(0 .. integer(argc) - 1 );
         begin
            for i in 0 .. C.size_t(argc) - 1 loop
               argv2(integer(i))        := ASU.To_Unbounded_String(CS.Value(Pstring.Value(argv)(i)));
               columnNames2(integer(i)) := ASU.To_Unbounded_String(CS.Value(Pstring.Value(columnNames)(i)));
            end loop;

            return C.int(cb(integer(argc), argv2, columnNames2));
         end;
      end proxy;
      -- create a proxy callback that converts all C data
      -- to Ada data and pass it on to the callback that the user provided

   begin

      if not db.open then
         raise DB_NOT_OPEN;
      end if;

      exec_real(db, sql, proxy'Unrestricted_Access, errmsg);

   end exec_with_callback;

   ----------------------------------
   -- exec_with_callback_with_data --
   ----------------------------------

   procedure exec_with_callback_with_data
     (db      : in out Object;
      sql     : in     string;
      cb      : in     Exec_Callback_with_data;
      arg     : in out Data_Type;
      errmsg  :    out Ustring)
   is

      use C;

      function proxy
        (pArg        : System.Address;
         argc        : C.int;
         argv        : Pstring_array;
         columnNames : Pstring_array)
         return C.int
      is
      begin

         if (argc <= 0) then
            return 0;
         end if;

         declare
            i            : C.size_t := 0;
            argv2        : Row_Array(0 .. integer(argc) - 1 );
            columnNames2 : Row_Array(0 .. integer(argc) - 1 );
         begin
            for i in 0 .. C.size_t(argc) - 1 loop
               argv2(integer(i))        := ASU.To_Unbounded_String(CS.Value(Pstring.Value(argv)(i)));
               columnNames2(integer(i)) := ASU.To_Unbounded_String(CS.Value(Pstring.Value(columnNames)(i)));
            end loop;

            return C.int(cb(arg, integer(argc), argv2, columnNames2));
         end;

      end proxy;
      -- create a proxy callback that converts all C data
      -- to Ada data and pass it on to the callback that the user provided

   begin

      if not db.open then
         raise DB_NOT_OPEN;
      end if;

      exec_real(db, sql, proxy'Unrestricted_Access, errmsg);

   end exec_with_callback_with_data;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self : in out Table_Controlled)
   is

      procedure Free_Row
         is new Ada.Unchecked_Deallocation(Row_Array, Row_Reference);

      procedure Free_Table
         is new Ada.Unchecked_Deallocation(Table_Array, Table_Reference);

   begin

      for I in Self.Table'Range loop
         Free_Row(Self.Table(I));
      end loop;

      Free_Table(Self.table);

   end Finalize;

   ---------------
   -- get_table --
   ---------------

   procedure get_table
      (db     : in out Object;
       sql    : in     string;
       table  :    out Table_Controlled;
       errmsg :    out Ustring)
   is

      type Node;
      type PNode is access all Node;
      type Node is record
         next  : PNode := null;
         info  : Row_Reference;
      end record;
      -- type Node declaration

      procedure Free
         is new Ada.Unchecked_Deallocation(Node, PNode);

      Include_Column_Names : boolean := TRUE;
      Current              : PNode   := new Node;
      Head                 : PNode   := Current;
      Count_Columns        : integer := 0;
      Count_Rows           : integer := 0;

      function callback
         (argc        : integer;
          argv        : Row_Array;
          columnNames : Row_Array)
         return integer
      is
         previous : PNode;
      begin
         if Include_Column_Names then
            Count_Rows    := 1;
            Count_Columns := columnNames'Last;

            -- copy columnNames to Node.info

            Current.info := new Row_Array(columnNames'Range);
            Current.info.all := columnNames;

            Include_Column_Names := FALSE;
         end if;

         Count_Rows := Count_Rows + 1;

         -- insert new Node after the last one

         previous := Current;
         Current := new Node;
         previous.next := Current;

         Current.info := new Row_Array(argv'Range);
         Current.info.all := argv;

         return 0;
      end callback;
   begin

      Exec_with_callback(db,
                         sql,
                         callback'Unrestricted_Access,
                         errmsg);

      declare
         tmp_table : Table_Reference := new Table_Array(1 .. Count_Rows);

         previous  : PNode;
      begin

         Current := Head;

         for I in tmp_table'Range loop

            tmp_table(I) := Current.info;
            -- copy row

            -- go to next Node
            previous := Current;
            Current  := Current.next;

            if I = tmp_table'Last and Current /= null then
               Ada.Text_IO.Put_Line("error with count");
            end if;

            Free(previous);
            -- Free previous Node
        end loop;

         table.table := tmp_table;

      end;
   end get_table;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Table_Controlled)
   is
   begin

      null;

   end Initialize;

   ----------
   -- open --
   ----------

   procedure open
     (db       : in out Object;
      filename : in     string;
      mode     : in     File_Mode_Type := READ_WRITE;
      errmsg   :    out Ustring)
   is

      function sqlite_open
        (filename : C.char_array;
         mode     : C.int;
         errmsg   : Pchars_ptr)
         return DB_Access;
      pragma Import(C, sqlite_open, "sqlite_open");
      -- sqlite.h:78
      -- sqlite *sqlite_open(const char *filename, int mode, char **errmsg);

   begin

      if db.open then
         raise DB_ALREADY_OPEN;
      end if;

      declare
         tmperr : Pchars_ptr := new CS.chars_ptr;
      begin
         tmperr.all := CS.Null_Ptr;

         -- file mode is not implemented in current implementation
         -- of sqlite

         db.db := sqlite_open(C.To_C(filename), 0, tmperr);

         if (CS."/="(tmperr.all, CS.Null_Ptr)) then
            errmsg := ASU.To_Unbounded_String(CS.Value(tmperr.all));
         else
            db.open := TRUE;
         end if;

         Free(tmperr);
      end;

   end open;

end GNU.DB.SQLite;
