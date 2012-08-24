-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2002 Juergen Pfeifer
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
with Ada.Text_IO; use Ada.Text_IO;

package body GNU.DB.SQLCLI.Dispatch.Debug is

   package DF_IO is new Ada.Text_IO.Enumeration_IO (Attr.T); use DF_IO;

   procedure Verify
   is
      G : Attr_Get_Func;
      S : Attr_Set_Proc;
      Stop : Boolean := False;
   begin
      for I in Attr.T'Range loop
         G := Get_Func (I);
         S := Set_Proc (I);

         if G = null then
            Put (I); Put (" has no 'Get' function"); New_Line;
            Stop := True;
         end if;
         if S = null then
            Put (I); Put (" has no 'Set' funktion"); New_Line;
            Stop := True;
         end if;
      end loop;
      pragma Assert (not Stop);
   end Verify;

end GNU.DB.SQLCLI.Dispatch.Debug;
