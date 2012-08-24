-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-tables.ads,v $
--  Description     : Symbol Table Manager for the ESQL
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>
--  Created On      : 29-Oct-2000
--  Last Modified By: $Author: stephen_leake $
--  Last Modified On: $Date: 2003/09/28 17:52:40 $
--  Status          : $State: Exp $
--
--  Copyright (C) 2000-2002 Michael Erdmann
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
--  This package supports an index tree for symbol tables. The package is    --
--  only inteded for use with the esql translator and the related support    --
--  packages.                                                                --
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
generic

    type Symbol_Information_Type is private;

package GNU.DB.Support.Tables is

   type Tree_Node is private;
   type Tree_Node_Access is access Tree_Node;

   Usage_Error     : exception;
   Already_Stored  : exception;
   Entry_Not_Found : exception;

   ---------------------------------------------------------------------------
   -- Description:
   --    This function creates an instance of the index tree
   -- Preconditions:
   --    None
   -- Postconditions:
   --    The tree node instance is intialized
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   function Create_Tree return Tree_Node_Access;

   ---------------------------------------------------------------------------
   -- Description:
   --    The information is stored in the symbol table under the
   --    given Name.
   --
   -- Preconditions:
   --    P1 - The Tree_Node pointer has to be created by means
   --         of Create_Tree.
   --    P2 - The Name is not already stored in the tree
   -- Postconditions:
   --    C1 - The Name and the information are stored in the table.
   -- Exceptions:
   --    Usage_Error    : Pre.cond. P1 violated
   --    Already_Stored : Pre.cond. P2 vialoated
   --
   -- Notes:
   --
   ---------------------------------------------------------------------------
   procedure Insert(
      T       : in out Tree_Node_Access;
      Name    : in String;
      Info    : Symbol_Information_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --
   -- Preconditions:
   --
   -- Postconditions:
   --
   -- Exceptions:
   --
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   procedure Fetch(
      T       : in out Tree_Node_Access ;
      Name    : in String;
      Info    : out Symbol_Information_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --
   -- Preconditions:
   --
   -- Postconditions:
   --
   -- Exceptions:
   --
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   procedure Update(
      T       : in out Tree_Node_Access;
      Name    : in String;
      Info    : in Symbol_Information_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --
   -- Preconditions:
   --
   -- Postconditions:
   --
   -- Exceptions:
   --
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   procedure Delete(
      T       : in out Tree_Node_Access;
      Name    : in String );

   -- ===================================================================== --
private

   type Leaf_Type;
   type Leaf_Access is access Leaf_Type;

   type Tree_Node is record
         Root : Leaf_Access := null;     -- root element of the index tree
   end record;
   -- this is the tree node, which contains all the data which
   -- is used to maintain and operate on the index tree.

end GNU.DB.Support.Tables;
