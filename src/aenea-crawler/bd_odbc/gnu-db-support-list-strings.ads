-------------------------------------------------------------------------------
--                                                                           --
--                     GNADE  : GNat Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-list-strings.ads,v $
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

package GNU.DB.Support.List.Strings is

   type String_List_Element is new List_Element with private;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function To_String_List_Element(
      S : in String ) return List_Element_Access;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function "&"(
      Element : in List_Element_Access;
      S       : in String ) return List_Element_Access;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function To_String(
      Element : in List_Element_Access ) return String;

   --- =================================================================== ---
private
   type String_List_Element is new List_Element with record
      Value : Unbounded_String := Null_Unbounded_String;
   end record;

   function Create_Copy(
      Element : in String_List_Element ) return List_Element_Access;

   procedure Destroy(
      Element : in String_List_Element;
      Ptr     : in out List_Element_Access );

end GNU.DB.Support.List.Strings;
