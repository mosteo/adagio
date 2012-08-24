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
with Ada.Text_IO;
with Ada.Exceptions;

package body GNU.DB.SQLCLI.Info.Debug is
   procedure Dump (File             : in Ada.Text_IO.File_Type;
                   ConnectionHandle : in SQLHDBC) is

      package DI renames GNU.DB.SQLCLI.Info.Driver_Information;
      package I_IO is new Ada.Text_IO.Enumeration_IO (SQL_INFO_TYPE);
      package SF_IO is new Ada.Text_IO.Enumeration_IO (SQL_API_FUNCTION);
      procedure Dump_Info (T : in SQL_INFO_TYPE);

      use I_IO; use SF_IO; use Ada.Text_IO;

      procedure Dump_Info (T : in SQL_INFO_TYPE) is
         I : constant Driver_Info'Class := SQLGetInfo (ConnectionHandle, T);
      begin
         Put (File, T, Debug_Label_Width);
         Put (File, ": ");
         Put (File, DI.To_String (I));
         New_Line (File);
      end Dump_Info;

      Fct_Bitmap : SQL_API_FUNCTION_BITMAP;
   begin
      for I in SQL_INFO_TYPE'Range loop
         declare
         begin
            Dump_Info (I);
         exception
            when Event : others =>
               Put (File, I, Debug_Label_Width);
               Put (File, ": ***** ");
               Put (File, Ada.Exceptions.Exception_Name (Event));
               Put (File, ", ");
               Put (File, Ada.Exceptions.Exception_Message (Event));
               New_Line (File);
         end;
      end loop;

      SQLGetFunctions (ConnectionHandle, Fct_Bitmap);

      for I in SQL_API_FUNCTION'Range loop
         declare
         begin
            Put (File, I, Debug_Label_Width);
            Put (File, ": ");
            Put (File, Boolean'Image (SQLGetFunctions (Fct_Bitmap, I)));
            New_Line (File);
         exception
            when Event : others =>
               Put (File, I, Debug_Label_Width);
               Put (File, ": ***** ");
               Put (File, Ada.Exceptions.Exception_Name (Event));
               Put (File, ", ");
               Put (File, Ada.Exceptions.Exception_Message (Event));
               New_Line (File);
         end;
      end loop;
   end Dump;

   procedure Dump (ConnectionHandle : in SQLHDBC) is
   begin
      Dump (Ada.Text_IO.Standard_Output, ConnectionHandle);
   end Dump;

end GNU.DB.SQLCLI.Info.Debug;
