-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-list.adb,v $
--  Description     : Simple list package                                    --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 16-Mar-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2004/03/20 08:30:20 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
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
--  <a>                                                                      --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--  </a>                                                                     --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - The handling of the tree nodes is currently not task save          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Unchecked_Deallocation;
with Ada.Strings.Unbounded;                    use Ada.Strings.Unbounded;

package body GNU.DB.Support.List is

   Version : constant String :=
     "$Id: gnu-db-support-list.adb,v 1.3 2004/03/20 08:30:20 merdmann Exp $";
   --- ==================================================================== ---
   ---               L O C A L   S U P P O R T   D A T A                    ---
   --- ==================================================================== ---
   type Container_Data is record
         Head : List_Element_Access      := null;
         Tail : List_Element_Access      := null;
      end record;

   --- ==================================================================== ---
   ---               S U P P O R T    P R O C E  D U R E S                  ---
   --- ==================================================================== ---

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
      This : in out Secure ) is
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      This : in out Secure ) is
   begin
      null;
   end Finalize;

   -------------------
   -- New_Container --
   -------------------
   function New_Container(
      Element : in List_Element_Access ) return Container_Data_Access is
      Result  : Container_Data_Access := new Container_Data;
   begin
      Result.Head := Element;
      Result.Tail := Element;
      return Result;
   end New_Container;
   --- ==================================================================== ---
   ---                  P U B L I C    M E T H O D S                        ---
   --- ==================================================================== ---

   ----------
   -- Head --
   ----------
   function Head(
      Element : in List_Element_Access ) return List_Element_Access is
   begin
      if Element.Container /= null then
         return Element.Container.Head;
      else
         raise Not_In_Any_List;
      end if;
   end Head;

   ------------
   -- Append --
   ------------
   procedure Append(
      This    : in Container_Data_Access;
      Element : in List_Element_Access ) is
   begin
      Element.Container := This;
      Element.Next      := null;

      This.Tail.Next := Element;
      Element.Previous := This.Tail;

      This.Tail := Element;
   end Append;

   ------------
   -- Append --
   ------------
   procedure Append(
      This    : in Container_Data_Access;
      Element : in List_Element'Class ) is
   begin
      Append( This, Create_Copy(Element) );
   end Append;

   ------------
   -- Append --
   ------------
   function "&"(
      First : in List_Element_Access;
      Next  : in List_Element_Access ) return List_Element_Access is
   begin
      if First.Container = null then
         First.Container := New_Container(First);
      end if;

      Append( First.Container, Next );
      return First;
   end "&";

   ------------
   -- Append --
   ------------
   function "&"(
      First : in List_Element_Access;
      Next  : in List_Element'Class ) return List_Element_Access is
   begin
      if First.Container = null then
         First.Container := New_Container(First);
      end if;

      Append( First.Container, Next );
      return First;
   end "&";

   ------------
   -- Append --
   ------------
   function "&"(
      First  : in List_Element'Class;
      Next   : in List_Element'Class ) return List_Element_Access is
      Result : List_Element_Access := Create_Copy(First);
   begin
      if Result.Container = null then
         Result.Container := New_Container(Result);
      end if;
      Append( Result.Container, Next );
      return Result;
   end "&";

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      Element : in List_Element_Access ) is
      -- delete all list elements till the end of the list
      P,Q     : List_Element_Access := Element;
   begin
      while P /= null loop
         Q := P.Next;
         Destroy( P.all, P );
         P := Q;
      end loop;
   end Destroy;

   -------------
   -- Perform --
   -------------
   procedure Perform(
      First : in List_Element_Access;
      IC    : in out List_Iterator_Type'Class ) is
      P     : List_Element_Access := First;
   begin
      while P /= null loop
         Action( IC, P );
         P := P.Next;
      end loop;
   end Perform;

end GNU.DB.Support.List;
