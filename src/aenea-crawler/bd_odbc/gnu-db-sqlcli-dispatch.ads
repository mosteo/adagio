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
with GNU.DB.SQLCLI.Generic_Attr;

generic
   with package Attr is new GNU.DB.SQLCLI.Generic_Attr (<>);
package GNU.DB.SQLCLI.Dispatch is

   subtype Ctx is Attr.Context;

   type Attr_Get_Func is access
     function (Handle    : Ctx;
               Attribute : Attr.T;
               MaxLength : SQLSMALLINT := 0;
               Data      : Attr.Aux;
               ErrorCode : access SQLRETURN)
              return Attr.Attribute_Value_Pair'Class;

   function Get_Func (Index : Attr.T) return Attr_Get_Func;
   pragma Inline_Always (Get_Func);


   type Attr_Set_Proc is access
     procedure (Handle    : in Ctx;
                AV_Pair   : in Attr.Attribute_Value_Pair'Class;
                Data      : in Attr.Aux;
                ErrorCode : out SQLRETURN);

   function Set_Proc (Index : Attr.T) return Attr_Set_Proc;
   pragma Inline_Always (Set_Proc);

private
   type Get_Dispatch is array (Attr.T) of Attr_Get_Func;
   type Set_Dispatch is array (Attr.T) of Attr_Set_Proc;

   Get_Dispatch_Array : Get_Dispatch := (others => null);
   Set_Dispatch_Array : Set_Dispatch := (others => null);

end GNU.DB.SQLCLI.Dispatch;
