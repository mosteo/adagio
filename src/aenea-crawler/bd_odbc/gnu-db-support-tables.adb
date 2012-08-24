-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-tables.adb,v $
--  Description     : Symbol Table Manager for the ESQL translator
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>
--  Created On      : 06-Jan-2001
--  Last Modified By: $Author: stephen_leake $
--  Last Modified On: $Date: 2003/10/27 01:24:58 $
--  Status          : $State: Exp $
--
--  Copyright (C) 2000-2003 Michael Erdmann
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This package
--
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - The handling of the tree nodes is currently not task save          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------

package body GNU.DB.Support.Tables is

   Version : constant String := "$Id: gnu-db-support-tables.adb,v 1.4 2003/10/27 01:24:58 stephen_leake Exp $";
   pragma Unreferenced (Version);

   --- ==================================================================== ---
   ---               L O C A L   S U P P O R T   D A T A                    ---
   --- ==================================================================== ---
   subtype Index_Type is Character;
   -- this is the index type. It included all possbible characaters of the
   -- of the ASCII symbol set.

   Null_Leaf : constant Leaf_Access := null;

   type Leaf_Array is array( Index_Type ) of Leaf_Access;
   -- This is the list of pointers per index and leaf. For each index value a
   -- pointer is stored which points to another leaf.

   type Leaf_Type is record
         Valid_Information : Boolean    := False;
         Value             : Index_Type ;
         Next              : Leaf_Array := (others=>Null_Leaf);
         Information       : Symbol_Information_Type;
   end record;

   --- ==================================================================== ---
   ---               S U P P O R T    P R O C E  D U R E S                  ---
   --- ==================================================================== ---

   --------------
   -- New_Leaf --
   --------------

   function New_Leaf(
      T : Tree_Node_Access ) return Leaf_Access is
      pragma Unreferenced (T);
      ---
      Result : Leaf_Access := new Leaf_Type;
      ---
   begin
      Result.Valid_Information := False;
      Result.Next        := (others=>null);
      return Result;
   end New_Leaf;

   ------------
   -- Locate --
   ------------

   function Locate(
      T           : in Tree_Node_Access;
      Name        : in String;
      Expand_Tree : in Boolean := False ) return Leaf_Access is
      --
      -- Locate a name in the index tree by walking trough till the
      -- end of the string. At the end of this process, the variable
      -- current points either to a leaf which carries information or
      -- or returns null. The user has to verify that the found leaf
      -- is not an enpty leaf.
      -- Is the nd_tree option not set, the search is stoped as soon
      -- as no next leaf pointer is defined. If the option is set an
      -- empty tree leaf is inserted. This feature is need to insert
      -- a tree element.
      --
      Current : Leaf_Access := T.Root;
      Tmp     : Leaf_Access;
      C       : Index_Type;
   begin
      for I in Name'Range loop
         C := Name(I);

         Tmp := Current.Next(C);
         if Tmp = null then
            if not Expand_Tree then
               Current := null;
               exit;
            end if;

            Tmp := New_Leaf(T);
            Tmp.Value := C;

            Current.Next(C) := Tmp;
         end if;

         Current := Tmp;
      end loop;

      return Current;
   end Locate;

   --- ==================================================================== ---
   ---                  P U B L I C    M E T H O D S                        ---
   --- ==================================================================== ---

   -----------------
   -- Create_Tree --
   -----------------
   function Create_Tree return Tree_Node_Access is
      -- Create Tree.
      Result : Tree_Node_Access := new Tree_Node;
      ---
   begin
      Result.Root := New_Leaf(Result);

      return Result;
   end Create_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert(
      T       : in out Tree_Node_Access;
      Name    : in String;
      Info    : Symbol_Information_Type ) is
      --
      -- Insert the name into the index tree by walking trough till the
      -- end of the string. At the end of this process, the variable
      -- current points either to a leaf which carries information or
      -- not.
      Current : Leaf_Access;
   begin
      Current := Locate( T, Name, Expand_Tree => True );

      if not Current.Valid_Information then
         Current.Valid_Information := True;
         Current.Information       := Info;
      else
         raise Already_Stored;
      end if;
   end Insert;

   -----------
   -- Fetch --
   -----------

   procedure Fetch(
      T       : in out Tree_Node_Access ;
      Name    : in String;
      Info    : out Symbol_Information_Type ) is
      -- Fetch the symbol table entry for a given name
      Current : constant Leaf_Access := Locate( T, Name );
   begin
      if Current = null then
         raise Entry_Not_Found;
      end if;

      if not Current.Valid_Information then
         raise Entry_Not_Found;
      else
         Info := Current.Information;
      end if;
   end Fetch;

   ------------
   -- Update --
   ------------

   procedure Update(
      T       : in out Tree_Node_Access ;
      Name    : in String;
      Info    : in Symbol_Information_Type ) is
      -- Modify a given entry by simply overwritin the data if
      -- it is already stored,
      Current : Leaf_Access := Locate( T, Name );
   begin
      if Current = null then
         raise Entry_Not_Found;
      end if;

      if Current.Valid_Information then
         Current.Information := Info;
      else
         raise Entry_Not_Found;
      end if;
   end Update;

   ------------
   -- Delete --
   ------------

   procedure Delete(
      T       : in out Tree_Node_Access ;
      Name    : in String ) is
      -- Delete by marking the information as invalid
      Current : Leaf_Access := Locate( T, Name );
   begin
      if Current /= null then
         Current.Valid_Information := False;
      end if;
   end Delete;

end GNU.DB.Support.Tables;
