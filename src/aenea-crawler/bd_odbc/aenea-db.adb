pragma Source_Reference( 1,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
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
  with Aenea.Net;
 with Aenea.Trace;
  with Pragmarc.Date_Handler;
  with Ada.Calendar;
  with Sql_standard; use Sql_standard;
 with Gnu.Db.Esql_Support; use Gnu.Db.Esql_Support;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Latin_1;    use Ada.Characters;
WITH GNU.DB.ESQL_ISO92_TYPES;   USE GNU.DB.ESQL_ISO92_TYPES;
WITH GNU.DB.ESQL_SUPPORT;       USE GNU.DB.ESQL_SUPPORT;
WITH GNU.DB.ESQL_SUPPORT.ODBC; 
USE  GNU.DB.ESQL_SUPPORT.ODBC; 
USE  GNU.DB.ESQL_SUPPORT;
WITH GNU.DB.SQLCLI;
USE  GNU.DB;

pragma Elaborate_All(GNU.DB.ESQL_SUPPORT.ODBC);

pragma Source_Reference( 45,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");


  package body Aenea.Db is
   SQLCODE          : SQL_STANDARD.SQLCODE_TYPE ;
   SQLSTATE         : SQL_STANDARD.SQLSTATE_TYPE;
   SQLCA            : aliased ESQL_SUPPORT.SQLCA_TYPE;
   
   GNADE_DB_DEFAULT : ODBC.CONNECTION_HANDLE := NULL;
   GNADE_G_CONTEXT  : ODBC.CONTEXT_TYPE;
   
     -- Database 
   GNADE_DB_DB : ODBC.CONNECTION_HANDLE
;
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
     ------------------------------------------------------------------------ 
    -- Get_Timestamp                                                      -- 
    ------------------------------------------------------------------------ 
    function Get_Timestamp return String is
    begin
       return To_String (Ada.Calendar.Clock);
    end Get_Timestamp;
     ------------------------------------------------------------------------ 
    -- Init                                                               -- 
    ------------------------------------------------------------------------ 
    procedure Init is
    begin
              -- Connect 
            begin
               GNADE_DB_DB:=ODBC.Connect("quraqua", "", "");
               
            end;
pragma Source_Reference( 86,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
        Trace.Log ("Connection with database successful.", Trace.Informative);
    end Init;
     ------------------------------------------------------------------------ 
    -- Insert_row                                                         -- 
    ------------------------------------------------------------------------ 
    procedure Insert_row (Hubs, Leaves, Total, Tracked : in Natural) is
            
               GNADE_L_CONTEXT : ODBC.CONTEXT_TYPE;
       Hubss, Leavess, Totall, Trackedd : INT;
pragma Source_Reference( 99,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
    begin
       Hubss    := INT (Hubs);
       Leavess  := INT (Leaves);
       Totall   := INT (Total);
       Trackedd := INT (Tracked);
               begin
                  GNADE_DB_DB:=ODBC.Connect("quraqua", "", "");
                  
               end;
pragma Source_Reference( 106,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 110 : *** Query ***
               declare
                  Hubss_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                  Leavess_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                  Totall_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                  Trackedd_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
               begin
                  -- Prepare
                  ODBC.PREPARE(GNADE_DB_DB,
                     GNADE_G_CONTEXT,
                     " insert INTO g2crawl( hubs, leaves, estimated_size, tracked," &
                     " fecha) values(? ,? ,? ,? , current_timestamp)",
                     SQLCA'Address,
                     Is_Reopenable =>FALSE);
                  -- Evaluate_Result
                  SQLCODE  := SQLCA.SQLCODE;
                  SQLSTATE := SQLCA.STATE;
                  IF ( SQLSTATE(1..2) /= ESQL_SUPPORT.SUCCESS_CLASS  AND
                     SQLSTATE(1..2) /= ESQL_SUPPORT.WARNING_CLASS  AND 
                     SQLSTATE(1..2) /= ESQL_SUPPORT.NOTFOUND_CLASS)  OR
                     SQLCODE in SQL_STANDARD.SQL_ERROR THEN
                     Raise_Exception(DB_Error'Identity,
                       "Line 108 in package Aenea.Db : Prepare Query failed " &  Latin_1.LF & SQLCA.message);
                  END IF;
                  -- Parameter Binding Hubss                                           
                  ODBC.BINDPARAMETER(
                         GNADE_DB_DB,
                          1,
                         Hubss'Address,
                         Hubss'Size,
                         ISO92_INT_TYPE,
                         Hubss_Indicator'Access
                         
                     );
                  -- Parameter Binding Leavess                                         
                  ODBC.BINDPARAMETER(
                         GNADE_DB_DB,
                          2,
                         Leavess'Address,
                         Leavess'Size,
                         ISO92_INT_TYPE,
                         Leavess_Indicator'Access
                         
                     );
                  -- Parameter Binding Totall                                          
                  ODBC.BINDPARAMETER(
                         GNADE_DB_DB,
                          3,
                         Totall'Address,
                         Totall'Size,
                         ISO92_INT_TYPE,
                         Totall_Indicator'Access
                         
                     );
                  -- Parameter Binding Trackedd                                        
                  ODBC.BINDPARAMETER(
                         GNADE_DB_DB,
                          4,
                         Trackedd'Address,
                         Trackedd'Size,
                         ISO92_INT_TYPE,
                         Trackedd_Indicator'Access
                         
                     );
                  -- Execute
                  ODBC.EXECUTE(GNADE_DB_DB,SQLCA'Address);
                  -- Evaluate_Result
                  SQLCODE  := SQLCA.SQLCODE;
                  SQLSTATE := SQLCA.STATE;
                  IF ( SQLSTATE(1..2) /= ESQL_SUPPORT.SUCCESS_CLASS  AND
                     SQLSTATE(1..2) /= ESQL_SUPPORT.WARNING_CLASS  AND 
                     SQLSTATE(1..2) /= ESQL_SUPPORT.NOTFOUND_CLASS)  OR
                     SQLCODE in SQL_STANDARD.SQL_ERROR THEN
                     Raise_Exception(DB_Error'Identity,
                       "Line 108 in package Aenea.Db :  Query  failed " &  Latin_1.LF & SQLCA.message);
                  END IF;
                  exception
                     when The_Error : Others =>
                  ESQL_SUPPORT.DEBUG(
                     "Line 108 in package Aenea.Db : *** Exception in query ****",
                     "",
                     SQLCA'Address);
                  
                        raise;
               end;
pragma Source_Reference( 110,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
  --      Trace.Log ("Insertion: " & SqlState, Trace.Always); 
        if SqlState /= "00000" then
          Trace.Log ("Database error [crawl]: " & SqlState & SqlCode'Img, Trace.Error);
       else
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 117 : *** Query ***
                  begin
                     -- Prepare
                     -- Execute
                     ODBC.EXECUTE(GNADE_DB_DB,SQLCA'Address, 
                        Statement => " COMMIT");
                     -- Evaluate_Result
                     SQLCODE  := SQLCA.SQLCODE;
                     SQLSTATE := SQLCA.STATE;
                     exception
                        when The_Error : Others =>
                     ESQL_SUPPORT.DEBUG(
                        "Line 117 in package Aenea.Db : *** Exception in query ****",
                        "",
                        SQLCA'Address);
                     
                           raise;
                  end;
pragma Source_Reference( 117,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
       end if;
               ODBC.DISCONNECT(GNADE_DB_DB,GNADE_G_CONTEXT,Commit => FALSE);
pragma Source_Reference( 120,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
     exception
       when E : DB_Error =>
          Trace.Log ("Database insertion failed [crawl]: " & Trace.Report (E),
             Trace.Error);
       when E : others =>
          Trace.Log ("Database insertion failed [crawl]: " & Trace.Report (E),
             Trace.Error);
    end Insert_row;
     ------------------------------------------------------------------------ 
    -- Insert_row                                                         -- 
    ------------------------------------------------------------------------ 
    procedure Insert_row (Vendor : in String; Amount, Total : in Natural; Timestamp : in String) is
               
                  GNADE_L_CONTEXT : ODBC.CONTEXT_TYPE;
       Sql_Vendor : Gnade.VARCHAR(4);
       Sql_Amount : INT;
       Sql_Total  : INT;
       Sql_Time   : Gnade.VARCHAR(20);
       Sql_Pct    : DOUBLE_PRECISION;
pragma Source_Reference( 141,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
        Epoch : Calendar.Time := Calendar.Time_Of (1976, 9, 6);
       use type Calendar.Time;
    begin
       To_VARCHAR (Vendor, Sql_Vendor);
       Sql_Amount := INT (Amount);
       Sql_Total  := INT (Total);
       To_VARCHAR (Timestamp, Sql_Time);
       Sql_Pct    := DOUBLE_PRECISION (Float (Amount) / Float (Total));
                  begin
                     GNADE_DB_DB:=ODBC.Connect("quraqua", "", "");
                     
                  end;
pragma Source_Reference( 152,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 156 : *** Query ***
                  declare
                     Sql_Vendor_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                     Sql_Amount_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                     Sql_Total_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                     Sql_Time_Indicator : aliased SQL_STANDARD.INDICATOR_TYPE := 0;
                  begin
                     -- Prepare
                     ODBC.PREPARE(GNADE_DB_DB,
                        GNADE_G_CONTEXT,
                        " insert INTO g2vendors values(? ,? ,? ,? )",
                        SQLCA'Address,
                        Is_Reopenable =>FALSE);
                     -- Evaluate_Result
                     SQLCODE  := SQLCA.SQLCODE;
                     SQLSTATE := SQLCA.STATE;
                     -- Parameter Binding Sql_Vendor                                      
                     ODBC.BINDPARAMETER(
                            GNADE_DB_DB,
                             1,
                            Sql_Vendor.Value'Address,
                            Sql_Vendor.Value'Size,
                            ISO92_CHAR_TYPE,
                            Sql_Vendor.Length'Access
                            
                        );
                     -- Parameter Binding Sql_Amount                                      
                     ODBC.BINDPARAMETER(
                            GNADE_DB_DB,
                             2,
                            Sql_Amount'Address,
                            Sql_Amount'Size,
                            ISO92_INT_TYPE,
                            Sql_Amount_Indicator'Access
                            
                        );
                     -- Parameter Binding Sql_Total                                       
                     ODBC.BINDPARAMETER(
                            GNADE_DB_DB,
                             3,
                            Sql_Total'Address,
                            Sql_Total'Size,
                            ISO92_INT_TYPE,
                            Sql_Total_Indicator'Access
                            
                        );
                     -- Parameter Binding Sql_Time                                        
                     ODBC.BINDPARAMETER(
                            GNADE_DB_DB,
                             4,
                            Sql_Time.Value'Address,
                            Sql_Time.Value'Size,
                            ISO92_CHAR_TYPE,
                            Sql_Time.Length'Access
                            
                        );
                     -- Execute
                     ODBC.EXECUTE(GNADE_DB_DB,SQLCA'Address);
                     -- Evaluate_Result
                     SQLCODE  := SQLCA.SQLCODE;
                     SQLSTATE := SQLCA.STATE;
                     exception
                        when The_Error : Others =>
                     ESQL_SUPPORT.DEBUG(
                        "Line 154 in package Aenea.Db : *** Exception in query ****",
                        "",
                        SQLCA'Address);
                     
                           raise;
                  end;
pragma Source_Reference( 156,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
        if SqlState /= "00000" then
          Trace.Log ("Database error [vendor]: " & SqlState & SqlCode'Img, Trace.Error);
       else
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 161 : *** Query ***
                     begin
                        -- Prepare
                        -- Execute
                        ODBC.EXECUTE(GNADE_DB_DB,SQLCA'Address, 
                           Statement => " COMMIT");
                        -- Evaluate_Result
                        SQLCODE  := SQLCA.SQLCODE;
                        SQLSTATE := SQLCA.STATE;
                        exception
                           when The_Error : Others =>
                        ESQL_SUPPORT.DEBUG(
                           "Line 161 in package Aenea.Db : *** Exception in query ****",
                           "",
                           SQLCA'Address);
                        
                              raise;
                     end;
pragma Source_Reference( 161,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
       end if;
                  ODBC.DISCONNECT(GNADE_DB_DB,GNADE_G_CONTEXT,Commit => FALSE);
pragma Source_Reference( 164,"aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq");
     exception
       when E : DB_Error =>
          Trace.Log ("Database insertion failed [vendor]: " & Trace.Report (E),
             Trace.Error);
       when E : others =>
          Trace.Log ("Database insertion failed [vendor]: " & Trace.Report (E),
             Trace.Error);
    end Insert_row;
     begin
    null;
 --   Init; 
 end Aenea.Db;
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 : ***************************************************
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :         !!!  D O   N O T   E D I T   !!! 
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :  This file is gnerated by GNADE ESQL translator 
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 : 
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :  ESQL VERSION : 1.4.3a
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :  SOURCE FILE  : aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :  OPTIONS      : 
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 :      aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq
--% aenea-crawler/../aenea-crawler/bd_odbc/aenea-db.adq at 179 : ***************************************************
