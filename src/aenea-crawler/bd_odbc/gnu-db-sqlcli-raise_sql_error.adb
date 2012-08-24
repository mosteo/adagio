-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2001 Juergen Pfeifer
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
-------------------------------------------------------------------------------
with Ada.Exceptions;

separate (GNU.DB.SQLCLI)
procedure Raise_SQL_Error
  (ProcedureName : in String;
   ErrorMessage  : in String    := "";
   RC            : in SQLRETURN := SQL_ADA95_BINDING_ERROR;
   State         : in SQLSTATE  := EMPTY_SQLSTATE)
is
   use Ada;

   Group : constant String := State (1 .. 2);
   Code  : constant String := State (3 .. 5);

   function Msg return String;
   procedure Check_State;

   function Msg return String is
   begin
      return "[Proc=" & ProcedureName & "]" & ErrorMessage;
   end Msg;

   procedure Check_State is
   begin
      if Group /= "00" and then Group /= "01" and then Group /= "  " then
         if Group = "07" then
            if Code = "001" then
               Ada.Exceptions.Raise_Exception
                 (Wrong_Parameter_Count'Identity, Msg);
            elsif Code = "002" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Count_Field'Identity, Msg);
            elsif Code = "005" then
               Ada.Exceptions.Raise_Exception
                 (Prepared_Stmt_Not_Cursor'Identity, Msg);
            elsif Code = "006" then
               Ada.Exceptions.Raise_Exception
                 (Restricted_Type_Violation'Identity, Msg);
            elsif Code = "009" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Descriptor_Index'Identity, Msg);
            elsif Code = "S01" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Default_Parameter'Identity, Msg);
            end if;
         elsif Group = "08" then
            Ada.Exceptions.Raise_Exception (Connection_Error'Identity, Msg);
         elsif Group = "21" then
            Ada.Exceptions.Raise_Exception
              (Column_List_Mismatch'Identity, Msg);
         elsif Group = "22" then
            Ada.Exceptions.Raise_Exception (Format_Error'Identity, Msg);
         elsif Group = "23" then
            Ada.Exceptions.Raise_Exception (Integrity_Violation'Identity, Msg);
         elsif Group = "24" then
            Ada.Exceptions.Raise_Exception
              (Invalid_Cursor_State'Identity, Msg);
         elsif Group = "25" then
            Ada.Exceptions.Raise_Exception
              (Invalid_Transaction_State'Identity, Msg);
         elsif Group = "28" then
            Ada.Exceptions.Raise_Exception (Invalid_Auth_Spec'Identity, Msg);
         elsif Group = "34" then
            Ada.Exceptions.Raise_Exception (Invalid_Cursor_Name'Identity, Msg);
         elsif Group = "3C" then
            Ada.Exceptions.Raise_Exception
              (Duplicate_Cursor_Name'Identity, Msg);
         elsif Group = "3D" then
            Ada.Exceptions.Raise_Exception
              (Invalid_Catalog_Name'Identity, Msg);
         elsif Group = "3F" then
            Ada.Exceptions.Raise_Exception (Invalid_Schema_Name'Identity, Msg);
         elsif Group = "40" then
            if Code = "001" then
               Ada.Exceptions.Raise_Exception (Serialization_Failure'Identity,
                                               Msg);
            elsif Code = "002" then
               Ada.Exceptions.Raise_Exception (Integrity_Violation'Identity,
                                               Msg);
            elsif Code = "003" then
               Ada.Exceptions.Raise_Exception
                 (Unknown_Stmt_Completion'Identity, Msg);
            end if;
         elsif Group = "42" then
            if Code = "000" then
               if ProcedureName = "SQLConnect" then
                  Ada.Exceptions.Raise_Exception
                    (Connection_Error'Identity, Msg);
               else
                  Ada.Exceptions.Raise_Exception (Syntax_Error'Identity, Msg);
               end if;
            elsif Code = "S01" then
               Ada.Exceptions.Raise_Exception
                 (Table_Already_Exists'Identity, Msg);
            elsif Code = "S02" then
               Ada.Exceptions.Raise_Exception
                 (Table_Not_Found'Identity, Msg);
            elsif Code = "S11" then
               Ada.Exceptions.Raise_Exception
                 (Index_Already_Exists'Identity, Msg);
            elsif Code = "S12" then
               Ada.Exceptions.Raise_Exception
                 (Index_Not_Found'Identity, Msg);
            elsif Code = "S21" then
               Ada.Exceptions.Raise_Exception
                 (Column_Already_Exists'Identity, Msg);
            elsif Code = "S22" then
               Ada.Exceptions.Raise_Exception
                 (Column_Not_Found'Identity, Msg);
            end if;
         elsif Group = "44" then
            Ada.Exceptions.Raise_Exception
              (With_Check_Violation'Identity, Msg);
         elsif Group = "HY" then
            if Code = "001" then
               Ada.Exceptions.Raise_Exception
                 (Operation_Canceled'Identity, Msg);
            elsif Code = "003" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_App_Buffer'Identity, Msg);
            elsif Code = "004" then
               Ada.Exceptions.Raise_Exception
                 (Memory_Allocation_Error'Identity, Msg);
            elsif Code = "007" then
               Ada.Exceptions.Raise_Exception
                 (Statement_Not_Prepared'Identity, Msg);
            elsif Code = "008" then
               Ada.Exceptions.Raise_Exception
                 (Operation_Canceled'Identity, Msg);
            elsif Code = "009" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Null_Pointer'Identity, Msg);
            elsif Code = "010" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Function_Sequence'Identity, Msg);
            elsif Code = "090" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Buffer_Length'Identity, Msg);
            elsif Code = "095" then
               Ada.Exceptions.Raise_Exception
                 (Functiontype_Out_Of_Range'Identity, Msg);
            elsif Code = "096" then
               Ada.Exceptions.Raise_Exception
                 (Invalid_Information_Type'Identity, Msg);
            elsif Code = "C00" then
               Ada.Exceptions.Raise_Exception
                 (Not_Implemented'Identity, Msg);
            elsif Code = "T00" then
               Ada.Exceptions.Raise_Exception
                 (Timeout_Expired'Identity, Msg);
            end if;
         elsif Group = "IM" then
            if Code = "001" then
               Ada.Exceptions.Raise_Exception
                 (Not_Implemented'Identity, Msg);
            else
               Ada.Exceptions.Raise_Exception (Driver_Error'Identity, Msg);
            end if;
         end if;
      end if;
   end Check_State;

begin
   case RC is
      when SQL_STILL_EXECUTING =>
         Ada.Exceptions.Raise_Exception (Still_Executing'Identity, Msg);

      when SQL_NO_DATA =>
         raise No_Data;

      when SQL_NEED_DATA =>
         raise Need_Data;

      when SQL_INVALID_HANDLE =>
         Ada.Exceptions.Raise_Exception (Invalid_Handle'Identity, Msg);

      when SQL_ADA95_DATA_ERROR =>
         Ada.Exceptions.Raise_Exception (Data_Error'Identity, Msg);

      when SQL_ADA95_INVALID_ENUM =>
         Ada.Exceptions.Raise_Exception
           (Unhandled_Enum'Identity,
            "[Proc=" & ProcedureName & "] ODBC Call returned enumeration " &
            "value that is not handled by the Ada95 binding: " & ErrorMessage);

      when SQL_ADA95_TYPE_ERROR =>
         Ada.Exceptions.Raise_Exception (Type_Error'Identity, Msg);

      when SQL_ADA95_NO_UNICODE =>
         Ada.Exceptions.Raise_Exception (No_Unicode_Support'Identity, Msg);

      when others =>
         if Generate_Detailed_Exceptions then
            Check_State;
         end if;
         Ada.Exceptions.Raise_Exception (Database_Error'Identity, Msg);
   end case;
end Raise_SQL_Error;
