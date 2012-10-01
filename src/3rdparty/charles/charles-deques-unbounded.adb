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
with System;

package body Charles.Deques.Unbounded is

   use Ada.Finalization;
   use type System.Address;
   use Rep_Types;


   procedure Initialize (Container : in out Container_Type) is
   begin
      Initialize (Container.Map);
   end;

   procedure Adjust (Container : in out Container_Type) is
   begin
      Adjust (Container.Map);
   end;

   procedure Finalize (Container : in out Container_Type) is
   begin
      Finalize (Container.Map);
   end;


   function "=" (Left, Right : Container_Type) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.Map, Right.Map);
   end;


   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Map.Length;
   end;


   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Length (Container) = 0;
   end;


   procedure Clear (Container : in out Container_Type) is
   begin
      Clear (Container.Map);
   end;


   procedure Swap (Left, Right : in out Container_Type) is
   begin
      Swap (Left.Map, Right.Map);
   end;


   function To_Container
     (Length : Natural) return Container_Type is

      Map : Map_Type;

      Node_Count : constant Natural :=
         (Length + Elements_Per_Node - 1) / Elements_Per_Node;

   begin

      Map.Length := Length;

      if Node_Count /= 0 then

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

         for I in Map.Nodes'Range loop
            Map.Nodes (I) := new Node_Type;
         end loop;

         Map.First := Map.Nodes'First;

         Map.Last := Map.Nodes'Last;

         Map.Bias := 0;

      end if;

      return Container_Type'(Controlled with Map);

   end To_Container;


   function To_Container
     (Length : Natural;
      Item   : Element_Type) return Container_Type is

      Map : Map_Type;

      Node_Count : constant Natural :=
         (Length + Elements_Per_Node - 1) / Elements_Per_Node;

   begin

      Map.Length := Length;

      if Node_Count /= 0 then

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

         for I in Map.Nodes'Range loop

            Map.Nodes (I) :=
               new Node_Type'(Elements => (others => Item));

         end loop;

         Map.First := Map.Nodes'First;

         Map.Last := Map.Nodes'Last;

         Map.Bias := 0;

      end if;

      return Container_Type'(Controlled with Map);

   end To_Container;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
   begin
      Assign (Target.Map, Length);
   end;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type) is

      procedure Process (Element : in out Element_Type) is
      begin
         Element := Item;
      end;

      procedure Iterate is
         new Generic_Modify_Elements;
   begin
      Assign (Target.Map, Length);
      Iterate (Target, First (Target), Back (Target));
   end;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is

      Index : Natural := 0;

      procedure Process (Element : Element_Type) is
      begin
         Replace_Element (Target.Map, Index, Element);
         Index := Index + 1;
      end;

      procedure Iterate is
         new Generic_Select_Elements;
   begin
      Assign (Target, Length (Source));
      Iterate (Source, First (Source), Back (Source));
   end;



   procedure Prepend
     (Container : in out Container_Type) is

      Node          : Node_Access;
      Element_Index : Integer'Base;
   begin
      Prepend (Container.Map, Node, Element_Index);
   end;


   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is

      Node          : Node_Access;
      Element_Index : Integer'Base;
   begin
      Prepend (Container.Map, Node, Element_Index);
      Node.Elements (Element_Index) := New_Item;
   end;


   procedure Delete_First (Container : in out Container_Type) is
   begin
      Delete_First (Container.Map);
   end;


   procedure Append
     (Container : in out Container_Type) is

      Element_Index : Integer'Base;
   begin
      Append (Container.Map, Element_Index);
   end;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is

      Map : Map_Type renames Container.Map;

      I : Integer'Base;
   begin
      Append (Map, I);
      Map.Nodes (Map.Last).Elements (I) := New_Item;
   end;



   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Delete_Last (Container.Map);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      New_Item  : in     Element_Type) is

      Before_Index : constant Integer'Base :=
         Index_Type'Pos (Before) - Index_Type'Pos (Index_Type'First);
   begin
      Insert (Container.Map, Before_Index, New_Item);
   end;




   procedure Insert_Range
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Length    : in     Natural) is

      Before_Index : constant Integer'Base :=
         Index_Type'Pos (Before) - Index_Type'Pos (Index_Type'First);
   begin
      Insert_Range
        (Container.Map,
         Before_Index,
         Length);
   end;



   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Index_Type) is

      Iterator_Index : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);
   begin
      Delete (Container.Map, Iterator_Index);
   end;


   procedure Delete
     (Container : in out Container_Type;
      First     : in     Index_Type;
      Back      : in     Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) -
         Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);
   begin
      Delete (Container.Map, First => F, Last => L);
   end;


   function Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Type is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);
   begin
      return Element (Container.Map, I);
   end;


   function Generic_Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Access is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         return Node.Elements (Element_Index)'Access;
      end;
   end;


   procedure Replace_Element
     (Container : Container_Type;
      Index     : Index_Type;
      By        : Element_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);
   begin
      Replace_Element (Container.Map, I, By);
   end;


--   procedure Assign
--     (Target : in out Element_Type;
--      Source : in     Container_Type;
--      Index  : in     Index_Type) is

   procedure Copy
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      :    out Element_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         Item := Node.Elements (Element_Index);
      end;
   end;


   procedure Swap
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
         E    : Element_Type renames Node.Elements (Element_Index);
         E_Copy : constant Element_Type := E;
      begin
         E := Item;
         Item := E_Copy;
      end;
   end;


   procedure Generic_Swap
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         Swap (Node.Elements (Element_Index), Item);
      end;
   end;


   procedure Swap_Element
     (Container   : in Container_Type;
      Left, Right : in Index_Type) is

      LI : constant Integer'Base :=
         Index_Type'Pos (Left) - Index_Type'Pos (Index_Type'First);

      RI : constant Integer'Base :=
         Index_Type'Pos (Right) - Index_Type'Pos (Index_Type'First);

      L_NI, R_NI : Positive;
      L_EI, R_EI : Positive;

      Map : Map_Type renames Container.Map;

   begin

      Element (Map, LI, L_NI, L_EI);
      Element (Map, RI, R_NI, R_EI);

      declare
         LN : Node_Type renames Map.Nodes (L_NI).all;
         LE : Element_Type renames LN.Elements (L_EI);

         RN : Node_Type renames Map.Nodes (R_NI).all;
         RE : Element_Type renames RN.Elements (R_EI);

         LE_Copy : constant Element_Type := LE;
      begin
         LE := RE;
         RE := LE_Copy;
      end;

   end Swap_Element;


   procedure Generic_Swap_Element
     (Container   : in Container_Type;
      Left, Right : in Index_Type) is

      LI : constant Integer'Base :=
         Index_Type'Pos (Left) - Index_Type'Pos (Index_Type'First);

      RI : constant Integer'Base :=
         Index_Type'Pos (Right) - Index_Type'Pos (Index_Type'First);

      L_NI, R_NI : Positive;
      L_EI, R_EI : Positive;

      Map : Map_Type renames Container.Map;

   begin

      Element (Map, LI, L_NI, L_EI);
      Element (Map, RI, R_NI, R_EI);

      declare
         LN : Node_Type renames Map.Nodes (L_NI).all;
         LE : Element_Type renames LN.Elements (L_EI);

         RN : Node_Type renames Map.Nodes (R_NI).all;
         RE : Element_Type renames RN.Elements (R_EI);
      begin
         Swap (LE, RE);
      end;

   end Generic_Swap_Element;


   procedure Generic_Select_Element
     (Container : Container_Type;
      Index     : Index_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         Process (Node.Elements (Element_Index));
      end;
   end;


   procedure Generic_Modify_Element
     (Container : Container_Type;
      Index     : Index_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         Process (Node.Elements (Element_Index));
      end;
   end;


   procedure Generic_Access_Element
     (Container : Container_Type;
      Index     : Index_Type) is

      I : constant Integer'Base :=
         Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First);

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Container.Map, I, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Container.Map.Nodes (Node_Index).all;
      begin
         Process (Node.Elements (Element_Index)'Access);
      end;
   end;


   procedure Generic_Iteration
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is
   begin
      for I in First .. Index_Type'Pred (Back) loop
         Process (Container, I);
      end loop;
   end;


   procedure Generic_Reverse_Iteration
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is
   begin
      for I in reverse First .. Index_Type'Pred (Back) loop
         Process (Container, I);
      end loop;
   end;


   procedure Generic_Select_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in First_Element .. Last_Element loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;



   procedure Generic_Modify_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in First_Element .. Last_Element loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;


   procedure Generic_Access_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in First_Element .. Last_Element loop
            Process (Node.Elements (I)'Access);
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;



   procedure Generic_Reverse_Select_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in reverse First_Element .. Last_Element loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;



   procedure Generic_Reverse_Modify_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in reverse First_Element .. Last_Element loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;


   procedure Generic_Reverse_Access_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      procedure Process
        (Node          : access Node_Type;
         First_Element : Positive;
         Last_Element  : Positive) is
      begin
         for I in reverse First_Element .. Last_Element loop
            Process (Node.Elements (I)'Access);
         end loop;
      end;

      procedure Iterate is
         new Rep_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Container.Map, F, L);
   end;



   function Front (Container : Container_Type)
      return Index_Type'Base is
   begin
      return Index_Type'Pred (Index_Type'First);
   end;


   function First (Container : Container_Type)
      return Index_Type is
   begin
      return Index_Type'First;
   end;


   function Last (Container : Container_Type)
      return Index_Type'Base is

      First : constant Integer'Base :=
         Index_Type'Pos (Index_Type'First);

      Offset : constant Integer'Base := Container.Map.Length - 1;

      Result : constant Index_Type'Base :=
         Index_Type'Val (First + Offset);
   begin
      return Result;
   end;


   function Back (Container : Container_Type)
      return Index_Type'Base is

      First : constant Integer'Base :=
         Index_Type'Pos (Index_Type'First);

      Offset : constant Integer'Base := Container.Map.Length;

      Result : constant Index_Type'Base :=
         Index_Type'Val (First + Offset);
   begin
      return Result;
   end;


   function Generic_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) return Index_Type'Base is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      function Find is
         new Rep_Types.Generic_Find (Predicate);

      Offset : constant Integer'Base :=
         Find (Container.Map, F, L);

      Result : constant Index_Type'Base :=
         Index_Type'Val (Index_Type'Pos (Index_Type'First) + Offset);
   begin
      return Result;
   end;


   function Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base is

      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;

      function Find is
         new Generic_Find (Predicate);
   begin
      return Find (Container, First => First, Back => Back);
   end;


   function Find
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base is
   begin
      return Find (Container,
                   First => First (Container),
                   Back => Back (Container),
                   Item => Item);
   end;


   function Generic_Reverse_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) return Index_Type'Base is

      F : constant Integer'Base :=
         Index_Type'Pos (First) - Index_Type'Pos (Index_Type'First);

      L : constant Integer'Base :=
         Index_Type'Pos (Index_Type'Pred (Back)) -
         Index_Type'Pos (Index_Type'First);

      function Find is
         new Rep_Types.Generic_Reverse_Find (Predicate);

      Offset : constant Integer'Base :=
         Find (Container.Map, F, L);

      Result : constant Index_Type'Base :=
         Index_Type'Val (Index_Type'Pos (Index_Type'First) + Offset);
   begin
      return Result;
   end;


   function Reverse_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base is

      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;

      function Reverse_Find is
         new Generic_Reverse_Find (Predicate);
   begin
      return Reverse_Find (Container, First => First, Back => Back);
   end;


   function Reverse_Find
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base is
   begin
      return Reverse_Find (Container,
                           First => First (Container),
                           Back => Back (Container),
                           Item => Item);
   end;


end Charles.Deques.Unbounded;
