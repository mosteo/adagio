-------------------------------------------------------------------------------
--                                                                           --
--                     GNADE  : GNat Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-list.ads,v $
--  Description     : Simple list package                                    --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 16-Mar-2002                                            --
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
with Ada.Finalization;                   use Ada.Finalization;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

package GNU.DB.Support.List is

   type List_Element is abstract tagged private;
   type List_Element_Access is access all List_Element'Class;

   type Secure( H : List_Element_Access ) is private;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function "&"(
      First   : in List_Element_Access;
      Next    : in List_Element_Access ) return List_Element_Access;

   function "&"(
      First   : in List_Element_Access;
      Next    : in List_Element'Class ) return List_Element_Access;

   function "&"(
      First  : in List_Element'Class;
      Next   : in List_Element'Class ) return List_Element_Access;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Destroy(
      Element : in List_Element_Access );

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Head(
      Element : in List_Element_Access ) return List_Element_Access;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   type List_Iterator_Type is abstract tagged null record;

   procedure Action(
      IC      : in out List_Iterator_Type;
      Element : in out List_Element_Access) is abstract;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Perform(
      First : in List_Element_Access;
      IC    : in out List_Iterator_Type'Class );

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Destroy(
      Element : in List_Element;
      Ptr     : in out List_Element_Access ) is abstract;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Create_Copy(
      Element : in List_Element ) return List_Element_Access is abstract;

   Not_In_Any_List : exception;

private
   -- ==================================================================== --
   type Container_Data;
   type Container_Data_Access is access Container_Data;

   type Secure( H : List_Element_Access ) is new Controlled with record
         null;
      end record;

   procedure Initialize(
      This : in out Secure );
   procedure Finalize(
      This : in out Secure );

   type List_Element is abstract tagged record
         Previous  : List_Element_Access := null;
         Next      : List_Element_Access := null;
         Container : Container_Data_Access := null;
      end record;

end GNU.DB.Support.List;
