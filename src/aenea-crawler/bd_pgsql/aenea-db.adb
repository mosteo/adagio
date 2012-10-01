 ------------------------------------------------------------------------------
 --                         ADAGIO - ADALID - AENEA.                         --
 --                                                                          --
 --                            Copyright (C) 2003                            --
 --                                 A. Mosteo.                               --
 --                                                                          --
 --  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
 --                                                                          --
 --  If you have any questions in regard to this software, please address    --
 --  them to the above email.                                                --
 --                                                                          --
 --  This program is free software; you can redistribute it and/or modify    --
 --  it under the terms of the GNU General Public License as published by    --
 --  the Free Software Foundation; either version 2 of the License, or (at   --
 --  your option) any later version.                                         --
 --                                                                          --
 --  This program is distributed in the hope that it will be useful, but     --
 --  WITHOUT ANY WARRANTY; without even the implied warranty of              --
 --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
 --  General Public License for more details.                                --
 --                                                                          --
 --  You should have received a copy of the GNU General Public License       --
 --  along with this library; if not, write to the Free Software Foundation, --
 --  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
 --                                                                          --
 --  You are not allowed to use any part of this code to develop a program   --
 --  whose output would be used to harass or prosecute other users of the    --
 --  networks Adagio connects with. All data collected with Adagio or a tool --
 --  containing Adagio code about other network users must remain            --
 --  confidential and cannot be made public by any mean, nor be used to      --
 --  harass or legally prosecute these users.                                --
 ------------------------------------------------------------------------------
 --  $Id: aenea-db.adq,v 1.6 2004/01/21 21:05:24 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Trace;

with Adagio.Os;

with Gnu.Db.Postgresql;

with Pragmarc.Date_Handler;

with Ada.Calendar;
with Ada.Containers;
use  Ada;


package body Aenea.Db is

   package Pg renames Gnu.Db.Postgresql;

   use type Ada.Containers.Count_Type;
   use type Pg.Connstatus;
   use type Pg.ExecStatus;

   type Database is access all Pg.Database;

   Db : Database;

   function To_String (T : in Ada.Calendar.Time) return String is
      use Ada.Calendar;
      use Pragmarc.Date_Handler;
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      H : Hour_Number;
      Mm: Minute_Number;
      S : Minute_Duration;
   begin
      Split (T, Y, M, D, H, Mm, S);
      return
         Year_Image_Long (Y) &"/"&
         Month_Image_Numeric (M) &"/"&
         Day_Image (D) &" "&
         Hour_Image_24 (H) &":"&
         Minute_Image (Mm) &":"&
         Seconds_Image (S);
   end To_String;

   -------------------------
   -- Reconnect_If_Needed --
   -------------------------

   procedure Reconnect_If_Needed is
   begin
      -- If connection is bad try to reconnect
      if Pg.Status (Db.all) /= Pg.CONNECTION_OK then
         Trace.Log ("Insert_Row: Connection down, before reconnect: " &
            Pg.ConnStatus'Image (Pg.Status (Db.all)), Trace.Error);
         Pg.Reset (Db.all);
         Trace.Log ("Insert_Row: Connection down, attempted reconnect: " &
            Pg.ConnStatus'Image (Pg.Status (Db.all)), Trace.Error);
      end if;
   end Reconnect_If_Needed;

    ------------------------------------------------------------------------
    -- Init                                                               --
    ------------------------------------------------------------------------
    procedure Init is
      R  : Pg.Result;
    begin
      Db := new Pg.Database (new String'(S (Globals.Options.Db_pgsql_ConnectString)));
      Trace.Log ("Connection to DB: " & Pg.ConnStatus'Image (Pg.Status (Db.all)), Trace.Informative);

      -- Attempt tables creation if they don't exist:
      if False then
         begin
            Pg.Execute (R, Db.all,
                        "create table g2crawl(" &
                        "hubs integer, " &
                        "leaves integer, " &
                        "unique_nodes integer, " &
                        "estimated_size integer, " &
                        "tracked integer, " &
                        "fecha timestamp primary key)");
         exception
            when others =>
               null;
         end;
         begin
            Pg.Execute (R, Db.all,
                        "create table g2vendors(" &
                        "vendor varchar(4), " &
                        "amount integer, " &
                        "total integer, " &
                        "fecha timestamp, " &
                        "primary key (fecha, vendor))");
         exception
            when others =>
               null;
         end;
         begin
            Pg.Execute (R, Db.all,
                        "create table g2data(" &
                        "context varchar(10) not null, " &
                        "key varchar(32) not null, " &
                        "count int4 not null, " &
                        "fecha timestamp not null, " &
                        "CONSTRAINT g2data_pkey PRIMARY KEY (context, ""key"", fecha)) " &
                        " with oids");
         exception
            when others =>
               null;
         end;
      end if;
    end Init;

   ------------------------------------------------------------------------
   -- Get_Timestamp                                                      --
   ------------------------------------------------------------------------
   -- Get Current Timestamp from database local time
   function Get_Timestamp return String is
      R  : Pg.Result;
   begin
      Pg.Execute (R, Db.all, "select to_char(now(), 'YYYY/MM/DD HH24:MI:SS')");
      if Pg.Status (R) /= Pg.PGRES_TUPLES_OK then
         Trace.Log (
            "Get_Timestamp: " & Pg.ExecStatus'Image (Pg.Status (R)) & "/" & Pg.Error (Db.all), Trace.Error);
         return "";
      else
         return Pg.Value (R);
      end if;
   end Get_Timestamp;

    ------------------------------------------------------------------------
    -- Insert_row                                                         --
    ------------------------------------------------------------------------
    procedure Insert_row (Hubs, Leaves, Unique, Total, Tracked : in Natural) is
      R  : Pg.Result;
    begin
      Reconnect_If_Needed;

      Pg.Execute
        (R, Db.all,
         "insert into g2crawl (hubs, leaves, unique_nodes, estimated_size, tracked, fecha) " &
         "values (" &
         Hubs'Img    & ", " &
         Leaves'Img  & ", " &
         Unique'Img  & ", " &
         Total'Img   & ", " &
         Tracked'Img & ", " &
         "current_timestamp)");
      if Pg.Status (R) /= Pg.PGRES_COMMAND_OK then
         Trace.Log (
            "Insert_Row [1]: " & Pg.ExecStatus'Image (Pg.Status (R)) & "/" & Pg.Error (Db.all), Trace.Error);
      end if;
    exception
       when others =>
          Trace.Log ("Insert_Row [1]: " & Pg.Error (Db.all), Trace.Error);
    end Insert_row;

     ------------------------------------------------------------------------
    -- Insert_row                                                         --
    ------------------------------------------------------------------------
    procedure Insert_row (Vendor : in String; Amount, Total : in Natural; Timestamp : in String) is
      R  : Pg.Result;
    begin
       begin
          Pg.Execute (R, Db.all,
            "insert into g2vendors values (" &
            "'"&Vendor & "', " &
            Amount'Img & ", " &
            Total'Img & ", " &
            "to_timestamp('" & Timestamp & "', 'YYYY/MM/DD HH24:MI:SS'))");
         if Pg.Status (R) /= Pg.PGRES_COMMAND_OK then
            Trace.Log (
               "Insert_Row [2]: " & Pg.ExecStatus'Image (Pg.Status (R)) & "/" & Pg.Error (Db.all), Trace.Error);
         end if;
       end;
    exception
       when others =>
          Trace.Log ("Insert_Row [2]: " & Pg.Error (Db.all), Trace.Error);
   end Insert_row;

   ----------------
   -- Insert_Row --
   ----------------

   procedure Insert_Row
     (Context   : in String;
      Key       : in String;
      Count     : in Natural;
      Timestamp : in String)
   is
     R  : Pg.Result;
   begin
      if Key'Length > 80 then
         Insert_Row (Context, Key (Key'First .. Key'First + 79), Count, Timestamp);
      else
         Pg.Execute (R, Db.all,
                     "insert into g2data(context, key, count, fecha)  " &
                     " values (" &
                     "'" & Context & "', " &
                     "'" & Key & "', " &
                     Count'Img & ", " &
                     "to_timestamp('" & Timestamp & "', 'YYYY/MM/DD HH24:MI:SS'))");
         if Pg.Status (R) /= Pg.PGRES_COMMAND_OK then
            Trace.Log (
                       "Insert_Row [Data]: " & Pg.ExecStatus'Image (Pg.Status (R)) & "/" & Pg.Error (Db.all), Trace.Error);
         end if;
      end if;
   exception
      when others =>
         Trace.Log ("Insert_Row [Data]: " & Pg.Error (Db.all), Trace.Error);
   end Insert_Row;

   -----------------
   -- Insert_Rows --
   -----------------

   procedure Insert_Rows (Data : in Row_Lists.List) is
      use Row_Lists;
      Statement : Ustring := +"insert into g2data(context, key, count, fecha)  ";

      procedure Append_Row (I : in Cursor) is
         R   : Rows renames Element (I);
         Key : constant String := +R.Key;
         Kkk : Ustring;
      begin
         if Key'Length <= 80 then
            Kkk := +Key;
         else
            Kkk := + (Key (Key'First .. Key'First + 79));
         end if;

         Asu.Append (Statement,
                 "select " &
                 "'" & R.Context & "', " &
                 "'" & Kkk & "', " &
                 R.Count'Img & ", " &
                 "to_timestamp('" & R.Timestamp & "', 'YYYY/MM/DD HH24:MI:SS')");
         if I /= Last (Data) then
            Asu.Append (Statement, " union ");
         end if;
      end Append_Row;

      R : Pg.Result;
   begin
      Data.Iterate (Append_Row'Access);

      if Data.Length > 0 then
         Trace.Log ("Insert_Rows: About to execute: " & (+Statement), Trace.Debug);

         Pg.Execute (R, Db.all, +Statement);
         if Pg.Status (R) /= Pg.Pgres_Command_Ok then
            Trace.Log
              ("Insert_Rows: " & Pg.Execstatus'Image (Pg.Status (R)) & "/" &
               Pg.Error (Db.all),
               Trace.Error);
         end if;
      else
         Trace.Log ("Insert_Rows: Nothing to insert", Trace.Debug);
      end if;
   exception
      when others =>
         Trace.Log ("Insert_Rows: " & Pg.Error (Db.all), Trace.Error);
   end Insert_Rows;

begin
   Trace.Log ("Setting up DB link", Trace.Debug);
   Init;
   Trace.Log ("DB set up finished.", Trace.Debug);
exception
   when E : others =>
      Trace.Log ("DB failed with exception: " & Trace.Report (E),
                 Trace.Error);
      Adagio.Os.Message_Box ("Aenea", "Db: " & Trace.Report (E));
end Aenea.Db;
