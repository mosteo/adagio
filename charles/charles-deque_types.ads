pragma License (Modified_GPL);

------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--              Copyright (C) 2001-2003 Matthew J Heaney                    --
--                                                                          --
-- The Charles Container Library ("Charles") is free software; you can      --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- Charles is maintained by Matthew J Heaney.                               --
--                                                                          --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
-- mailto:matthewjheaney@earthlink.net                                      --
--                                                                          --
------------------------------------------------------------------------------
generic

   type Element_Type is private;

   Elements_Per_Node : Positive;

   with function "=" (L, R : Element_Type) return Boolean;

package Charles.Deque_Types is

   pragma Preelaborate;

   type Element_Array is
      array (Positive range <>) of aliased Element_Type;

   function "=" (L, R : Element_Array) return Boolean is abstract;

   type Node_Type is
      record
         Elements : Element_Array (1 .. Elements_Per_Node);
      end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;

   type Node_Access is access all Node_Type;

   type Node_Access_Array is
      array (Positive range <>) of Node_Access;

   type Node_Access_Array_Access is
      access all Node_Access_Array;

   type Map_Type is
      record
         Nodes  : Node_Access_Array_Access;
         First  : Integer'Base := 1;
         Last   : Integer'Base := 0;
         Length : Integer'Base := 0;
         Bias   : Integer'Base := 0;
      end record;

   function "=" (L, R : Map_Type) return Boolean is abstract;

   procedure Initialize (Map : in out Map_Type);

   procedure Adjust (Map : in out Map_Type);

   procedure Finalize (Map : in out Map_Type);

   procedure Swap (L, R : in out Map_Type);


   procedure Assign
     (Map    : in out Map_Type;
      Length : in     Natural);

   function Is_Equal (L, R : Map_Type) return Boolean;

   procedure Clear (Map : in out Map_Type);


   procedure Prepend
     (Map           : in out Map_Type;
      Node          :    out Node_Access;
      Element_Index :    out Integer'Base);

   procedure Delete_First (Map : in out Map_Type);

   procedure Append
     (Map           : in out Map_Type;
      Element_Index :    out Integer'Base);

   procedure Delete_Last (Map : in out Map_Type);


   procedure Insert
     (Map      : in out Map_Type;
      Before   : in     Natural;
      New_Item : in     Element_Type);

   procedure Insert_Range
     (Map      : in out Map_Type;
      Before   : in     Integer'Base;
      Length   : in     Natural);


   procedure Delete_Back
     (Map   : in out Map_Type;
      Index : in     Natural);

   procedure Delete
     (Map   : in out Map_Type;
      Index : in     Natural);

   procedure Delete
     (Map         : in out Map_Type;
      First, Last : in     Integer'Base);


   function Element
     (Map   : Map_Type;
      Index : Natural) return Element_Type;

   procedure Element
     (Map           : in     Map_Type;
      Index         : in     Natural;
      Node_Index    :    out Positive;
      Element_Index :    out Positive);

   procedure Replace_Element
     (Map   : in Map_Type;
      Index : in Natural;
      Item  : in Element_Type);

   generic
      with procedure Process
        (Node        : access Node_Type;
         First, Last : in     Positive);
   procedure Generic_Iteration
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base);

   generic
      with procedure Process
        (Node        : access Node_Type;
         First, Last : in     Positive);
   procedure Generic_Reverse_Iteration
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base);

   generic
      with function Predicate (Element : Element_Type)
         return Boolean;
   function Generic_Find
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) return Integer'Base;

   generic
      with function Predicate (Element : Element_Type)
         return Boolean;
   function Generic_Reverse_Find
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) return Integer'Base;

end Charles.Deque_Types;
