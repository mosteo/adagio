-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-list-strings.adb,v $
--  Description     : Handle simple string lists                             --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-Mar-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2002/03/22 20:57:30 $                           --
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

package body GNU.DB.Support.List.Strings is

   Version : constant String := "$Id: gnu-db-support-list-strings.adb,v 1.2 2002/03/22 20:57:30 merdmann Exp $";
   --- ==================================================================== ---
   ---               L O C A L   S U P P O R T   D A T A                    ---
   --- ==================================================================== ---
   type String_Element_Access is access all String_List_Element;

   --- ==================================================================== ---
   ---               S U P P O R T    P R O C E  D U R E S                  ---
   --- ==================================================================== ---

   --------------
   -- Iterator --
   --------------
   type List_Iterator is new List_Iterator_Type with record
      Result : Unbounded_String := Null_Unbounded_String;
   end record;

   ------------
   -- Action --
   ------------
   procedure Action(
      This : in out List_Iterator;
      P    : in out List_Element_Access ) is
      Item : String_List_Element renames String_List_Element(P.all);
   begin
      if This.Result /= Null_Unbounded_String then
         This.Result := This.Result & "," & Item.Value;
      else
         This.Result := Item.Value;
      end if;
   end Action;

   --- ==================================================================== ---
   ---                  P U B L I C    M E T H O D S                        ---
   --- ==================================================================== ---

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      Element : in String_List_Element;
      Ptr     : in out List_Element_Access ) is
      -- this function should return the memory of the table description
      procedure Free is
        new Unchecked_Deallocation( String_List_Element, String_Element_Access);
   begin
      Free( String_Element_Access(Ptr) );
   end Destroy;

   -----------------
   -- Create_Copy --
   -----------------
   function Create_Copy(
      Element : in String_List_Element ) return List_Element_Access is
      Result  : List_Element_Access := new String_List_Element;
   -- create a 1:1 copy of the table element.
   begin
      String_List_Element(Result.all) := Element;
      return Result;
   end Create_Copy;

   ---------------
   -- To_String --
   ---------------
   function To_String(
      Element : in List_Element_Access ) return String is
      -- convert the list into a comma seperated string
      IC      : List_Iterator;
   begin
      IC.Result := Null_Unbounded_String;
      Perform( Element, IC );
      return To_String( IC.Result );
   end To_String;

   --------------------
   -- To_String_List --
   --------------------
   function To_String_List_Element(
      S      : in String ) return List_Element_Access is
      Result : String_Element_Access := new String_List_Element;
   begin
      Result.Value := To_Unbounded_String(S);
      return List_Element_Access(Result);
   end To_String_List_Element;

   ------------
   -- Append --
   ------------
   function "&"(
      Element : in List_Element_Access;
      S       : in String ) return List_Element_Access is
   begin
      return Element & To_String_List_Element(S);
   end "&";

end GNU.DB.Support.List.Strings;
