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
with System;  use type System.Address;
with Ada.Unchecked_Deallocation;

package body Charles.Lists.Double.Discriminated_Unbounded is

   type Node_Type (Discriminant : Discriminant_Type) is
      record
         Succ    : Node_Access;
         Pred    : Node_Access;
         Element : aliased Element_Type (Discriminant);
      end record;
      
   function "=" (L, R : Node_Type) return Boolean is abstract;
   pragma Warnings (Off, "=");


   procedure Set_Succ 
     (Node : Node_Access;
      Succ : Node_Access) is
   begin
      Node.Succ := Succ;
   end;
   
   procedure Set_Pred
     (Node : Node_Access;
      Pred : Node_Access) is
   begin
      Node.Pred := Pred;
   end;   
      
   function Succ (Node : Node_Access) return Node_Access is
   begin
      return Node.Succ;
   end;
   
   function Pred (Node : Node_Access) return Node_Access is
   begin
      return Node.Pred;
   end;


   procedure Deallocate is 
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   procedure Free (X : in out Node_Access) is
   begin
      Deallocate (X);
   end;
   
   use Node_Lists;


   procedure Initialize (Container : in out Container_Type) is
      List : List_Type renames Container.List;
   begin
      List.Back := new Node_Type (Discriminant_Type'First);
      Initialize (List);
   end;   


   procedure Adjust (Container : in out Container_Type) is
   
      List : List_Type renames Container.List;
      
      Source_Back   : constant Node_Access := List.Back;
      Source_Length : constant Natural := List.Length;
      
      Source : Node_Access := Succ (Source_Back);

   begin
   
      begin
         List.Back := new Node_Type (Discriminant_Type'First);
      exception
         when others =>
            List.Back := null;
            List.Length := 0;
            raise;
      end;
      
      Initialize (List);
      
      while Source /= Source_Back loop
      
         declare
            Target : constant Node_Access :=
               new Node_Type'(Discriminant => Source.Discriminant,
                              Succ         => List.Back,
                              Pred         => Pred (List.Back),
                              Element      => Source.Element);
         begin
            Insert (List, Before => List.Back, Node => Target);
         end;
         
         Source := Succ (Source);
         
      end loop;
      
      pragma Assert (List.Length = Source_Length);
      
   end Adjust;
   

   procedure Finalize (Container : in out Container_Type) is
   begin
      Finalize (Container.List'Access);
   end;


   function Is_Equal_Element 
     (L, R : Node_Access) return Boolean is
     
      pragma Inline (Is_Equal_Element);
   begin
      return L.Element = R.Element;
   end;      
   
   function "=" (Left, Right : Container_Type) return Boolean is
   
      function "=" is 
         new Node_Lists.Generic_Equal (Is_Equal_Element);
   begin
      if Left'Address = Right'Address then
         return True;
      end if;
      
      return Left.List = Right.List;
   end;

   
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (L, R : Node_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Element < R.Element;
      end;
      
      function "<" is 
         new Node_Lists.Generic_Less (Is_Less);
   begin
      if Left'Address = Right'Address then
         return False;
      end if;
      
      return Left.List < Right.List;
   end;



   function Length (Container : Container_Type) return Natural is
   begin
      return Container.List.Length;
   end;

   
   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Length (Container) = 0;
   end;

   
   procedure Clear (Container : in out Container_Type) is
   begin
      Clear (Container.List);
   end;

   
   procedure Swap (Left, Right : in out Container_Type) is   
   begin
      Swap (Left.List, Right.List);
   end;
           
   
      
--   function To_Container 
--     (First  : Iterator_Type;
--      Back   : Iterator_Type) return Container_Type is

--      Target_Back : Node_Access := new Node_Type (Discriminant_Type'First);
--      Length      : Natural := 0;
--      
--      I : Iterator_Type := First;
--      
--   begin
--      
--      Target_Back.Next := Target_Back;
--      Target_Back.Prev := Target_Back;
--   
--      while I /= Back loop
--      
--         declare
--            Target_Last : constant Node_Access := Target_Back.Prev;

--            Node : constant Node_Access := 
--               new Node_Type'(Discriminant => I.Node.Discriminant,
--                              Element => I.Node.Element,
--                              Next    => Target_Back,
--                              Prev    => Target_Last);
--         begin
--            Target_Back.Prev := Node;
--            Target_Last.Next := Node;
--         end;
--         
--         Length := Length + 1;
--         
--         I := Succ (I);
--         
--      end loop;

--      return (Ada.Finalization.Controlled with Target_Back, Length);
--      
--   exception
--      when others =>
--         
--         Do_Finalize (Target_Back);
--         raise;
--               
--   end To_Container;   

            

   
--   procedure Assign
--     (Target : in out Container_Type;
--      Source : in     Container_Type) is
--   begin
--      if Target'Address = Source'Address then
--         return;
--      end if;
--      
--      declare
--         C : Container_Type := Source;
--      begin
--         Swap (Target, C);
--      end;
--   end Assign;


--   procedure Assign
--     (Target : in out Container_Type;
--      Source : in     Container_Type;
--      First  : in     Iterator_Type;
--      Back   : in     Iterator_Type) is
--      
--      pragma Assert (Target'Address /= Source'Address);

--      C : Container_Type := To_Container (Source, First, Back);
--   begin
--      Swap (Target, C);
--   end;
   
   
--   procedure Insert
--     (Container : in out Container_Type;
--      Before    : in     Node_Access;
--      New_Item  : in     Element_Type;
--      New_Node  :    out Node_Access) is
--   begin
--      New_Node := new Node_Type'(Discriminant => New_Item.Discriminant,
--                                 Element => New_Item, 
--                                 Next    => Before, 
--                                 Prev    => Before.Prev);

--      Before.Prev.Next := New_Node;      
--      Before.Prev := New_Node; 

--      Container.Length := Container.Length + 1;
--   end;


--   procedure Insert
--     (Container    : in out Container_Type;
--      Before       : in     Node_Access;
--      Discriminant : in     Discriminant_Type;
--      New_Node     :    out Node_Access) is
--   begin
--      New_Node := new Node_Type (Discriminant);
--      
--      New_Node.Next := Before;
--      New_Node.Prev := Before.Prev;

--      Before.Prev.Next := New_Node;      
--      Before.Prev := New_Node; 

--      Container.Length := Container.Length + 1;
--   end;

       

   procedure Prepend
     (Container    : in out Container_Type;
      Discriminant : in     Discriminant_Type) is      
   begin
      Insert (Container, First (Container), Discriminant);
   end;


   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert (Container, First (Container), New_Item);
   end;

      
     
   procedure Delete_First (Container : in out Container_Type) is
   
      Iterator : Iterator_Type := First (Container);
   begin
      Delete (Container, Iterator);
   end;
   
      
   procedure Append
     (Container    : in out Container_Type;
      Discriminant : in     Discriminant_Type) is      
   begin
      Insert (Container, Back (Container), Discriminant);
   end;
      
      

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert (Container, Back (Container), New_Item);
   end;
      
      
   procedure Delete_Last (Container : in out Container_Type) is
   
      Iterator : Iterator_Type := Last (Container);
   begin
      Delete (Container, Iterator);
   end;

   
   procedure Insert
     (Container    : in out Container_Type;
      Before       : in     Iterator_Type;
      Discriminant : in     Discriminant_Type;
      Iterator     :    out Iterator_Type) is
      
      Pred_Before : constant Node_Access := Pred (Before.Node);
   begin
      Iterator.Node := new Node_Type (Discriminant);
      
      Iterator.Node.Succ := Before.Node;
      Iterator.Node.Pred := Pred_Before;
                        
      Node_Lists.Insert 
        (List   => Container.List, 
         Before => Before.Node, 
         Node   => Iterator.Node);
   end;

   
   procedure Insert
     (Container    : in out Container_Type;
      Before       : in     Iterator_Type;
      Discriminant : in     Discriminant_Type) is

      Iterator : Iterator_Type;   
   begin
      Insert (Container, Before, Discriminant, Iterator);
   end;



   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type) is
   begin
      Iterator.Node := 
         new Node_Type'(Discriminant => New_Item.Discriminant,
                        Succ         => Before.Node,
                        Pred         => Pred (Before.Node),
                        Element      => New_Item);
                                 
      Node_Lists.Insert 
        (List   => Container.List, 
         Before => Before.Node,          
         Node   => Iterator.Node);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type) is
   
      Iterator : Iterator_Type;
   begin
      Insert (Container, Before, New_Item, Iterator);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
   begin
      Delete_And_Increment (Container.List, Iterator.Node);
   end;

      
--   procedure Delete_Sans_Increment
--     (Container : in out Container_Type;
--      Iterator  : in out Iterator_Type) is
--   begin
--      Delete (Container, Iterator.Node);
--      Iterator := Back (Container);
--   end;


--   procedure Delete_Sans_Assign
--     (Container : in out Container_Type;
--      Iterator  : in     Iterator_Type) is
--      
--      Node : Node_Access := Iterator.Node;
--   begin
--      Delete (Container, Node);
--   end;


   procedure Delete
     (Container : in out Container_Type; 
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type) is      
   begin
      Delete (Container.List, First.Node, Back.Node);
   end;
   
         

--   procedure Unchecked_Delete
--     (Container : in out Container_Type;
--      Iterator  : in     Iterator_Type) is
--      
--      Node : Node_Access := Iterator.Node;
--   begin
--      Delete (Container, Node);
--   end;

      

--   procedure Delete_Sans_Assign
--     (Container : in out Container_Type; 
--      First     : in     Iterator_Type;
--      Back      : in     Iterator_Type) is
--      
--      Iterator : Iterator_Type := First;
--   begin
--      while Iterator /= Back loop
--         Delete (Container, Iterator);
--      end loop;
--   end;
   
         

   procedure Generic_Delete 
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Count     :    out Natural) is
      
      function Predicate (Node : Node_Access) return Boolean is
         pragma Inline (Predicate);
      begin
         return Predicate (Node.Element);
      end;
      
      procedure Delete is
         new Node_Lists.Generic_Delete; 
   begin
      Delete (Container.List, First.Node, Back.Node, Count);
   end;
     


   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Item      : in     Element_Type;
      Count     :    out Natural) is
      
      function Predicate (Element : Element_Type) return Boolean is
         pragma Inline (Predicate);
      begin
         return Item = Element;
      end;
      
      procedure Delete is 
         new Discriminated_Unbounded.Generic_Delete;
   begin
      Delete (Container, First, Back, Count);
   end;
      

   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type;
      Item      : in     Element_Type) is
      
      Count : Natural;
   begin
      Delete (Container, First, Back, Item, Count);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type;
      Count     :    out Natural) is
      
      First : Iterator_Type := Discriminated_Unbounded.First (Container);
   begin
      Delete (Container, First, Back (Container), Item, Count);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type) is
   
      Count : Natural;
   begin
      Delete (Container, Item, Count);
   end;


   procedure Reverse_List (First, Back : in Iterator_Type) is
   begin
      Node_Lists.Reverse_List (First.Node, Back.Node);
   end;
         
   procedure Reverse_List (Container : in Container_Type) is
   begin
      Reverse_List (First (Container), Back (Container));
   end;



   function First 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => First (Container.List));
   end;

   
   function First_Element
     (Container : Container_Type) return Element_Type is
     
      Node : constant Node_Access := First (Container.List);
   begin
      return Node.Element;
   end;

   
   function Last 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Last (Container.List));
   end;

   
   function Last_Element
     (Container : Container_Type) return Element_Type is
     
      Node : constant Node_Access := Last (Container.List);
   begin
      return Node.Element;
   end;


   function Back 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Back (Container.List));
   end;
      

   
   function Element 
     (Iterator : Iterator_Type) return Element_Type is
   begin
      return Iterator.Node.Element;
   end;
   

   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access is
   begin
      return Iterator.Node.Element'Access;
   end;
      
   
   procedure Replace_Element
     (Iterator : Iterator_Type;
      By       : Element_Type) is
   begin
      Iterator.Node.Element := By;
   end;

      
   procedure Copy_Element
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type) is
   begin
      Item := Iterator.Node.Element;
   end;


   procedure Swap_Element
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   
      E : constant Element_Type := Iterator.Node.Element;
   begin
      Iterator.Node.Element := Item;
      Item := E;
   end;

   procedure Generic_Swap_Element
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   begin
      Swap (Iterator.Node.Element, Item);
   end;


   procedure Swap (Left, Right : in Iterator_Type) is
      LE : constant Element_Type := Left.Node.Element;
   begin
      Left.Node.Element := Right.Node.Element;
      Right.Node.Element := LE;
   end;
   
   procedure Generic_Swap (Left, Right : in Iterator_Type) is
      LE : Element_Type renames Left.Node.Element;
      RE : Element_Type renames Right.Node.Element;
   begin
      Swap (LE, RE);
   end;
   
   procedure Swap_Iterator (Left, Right : in out Iterator_Type) is
      LI : constant Iterator_Type := Left;
   begin
      Left := Right;
      Right := LI;
   end;


   procedure Generic_Select_Element
     (Iterator : in Iterator_Type) is
   begin
      Process (Iterator.Node.Element);
   end;

   procedure Generic_Modify_Element
     (Iterator : in Iterator_Type) is
   begin
      Process (Iterator.Node.Element);
   end;

   procedure Generic_Access_Element
     (Iterator : in Iterator_Type) is
   begin
      Process (Iterator.Node.Element'Access);
   end;


   procedure Generic_Iteration
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := First;
   begin
      while Iter /= Back loop
         Process (Iter);
         Iter := Succ (Iter);
      end loop;
   end;
     

   procedure Generic_Reverse_Iteration
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := Back;
   begin
      while Iter /= First loop
         Iter := Pred (Iter);
         Process (Iter);
      end loop;
   end;
     

   procedure Generic_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := First;
   begin
      while Iter /= Back loop
         Process (Iter.Node.Element);
         Iter := Succ (Iter);
      end loop;
   end;
      

   procedure Generic_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := First;
   begin
      while Iter /= Back loop
         Process (Iter.Node.Element);
         Iter := Succ (Iter);
      end loop;
   end;
      

   procedure Generic_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := First;
   begin
      while Iter /= Back loop
         Process (Iter.Node.Element'Access);
         Iter := Succ (Iter);
      end loop;
   end;
      

   procedure Generic_Reverse_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := Back;
   begin
      while Iter /= First loop
         Iter := Pred (Iter);
         Process (Iter.Node.Element);
      end loop;
   end;
      

   procedure Generic_Reverse_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := Back;
   begin
      while Iter /= First loop
         Iter := Pred (Iter);
         Process (Iter.Node.Element);
      end loop;
   end;
      

   procedure Generic_Reverse_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iter : Iterator_Type := Back;
   begin
      while Iter /= First loop
         Iter := Pred (Iter);
         Process (Iter.Node.Element'Access);
      end loop;
   end;
      

   function Generic_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type) return Iterator_Type is
      
      Iter : Iterator_Type := First;
   begin
      while Iter /= Back loop
         if Predicate (Iter.Node.Element) then
            return Iter;
         end if;
         
         Iter := Succ (Iter);
      end loop;
      
      return Back;
   end;
      

   function Find 
     (First : Iterator_Type;
      Back  : Iterator_Type;
      Item  : Element_Type) return Iterator_Type is
      
      function Predicate (Element : Element_Type) return Boolean is
         pragma Inline (Predicate);
      begin
         return Item = Element;
      end;
      
      function Find is 
         new Generic_Find (Predicate);
   begin
      return Find (First, Back);
   end;

      
   function Find 
     (Container : Container_Type;
      Item      : Element_Type) return Iterator_Type is
   begin
      return Find (First (Container), Back (Container), Item);
   end;


   function Generic_Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type) return Iterator_Type is
      
      Iter : Iterator_Type := Back;
   begin
      while Iter /= First loop
         Iter := Pred (Iter);
         
         if Predicate (Iter.Node.Element) then
            return Iter;
         end if;
      end loop;
      
      return Back;
   end;


   function Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type;
      Item  : Element_Type) return Iterator_Type is
   
      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;
      
      function Reverse_Find is
         new Generic_Reverse_Find (Predicate);
   begin
      return Reverse_Find (First, Back);
   end;


   function Reverse_Find 
     (Container : Container_Type;
      Item      : Element_Type) return Iterator_Type is
   begin
      return Reverse_Find (First (Container), Back (Container), Item);
   end;


   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type) is
   begin
      if Container'Address = Source'Address then
         return;
      end if;
      
      Splice (Container.List, Before.Node, Source.List);   
   end;


   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      Iterator  : in Iterator_Type) is      
   begin
      Splice (Container.List, Before.Node, Iterator.Node);
   end;


   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      Iterator  : in     Iterator_Type) is      
   begin
      if Container'Address = Source'Address then
         Splice (Container.List, Before.Node, Iterator.Node);
      else
         Splice (Container.List, Before.Node, Source.List, Iterator.Node);
      end if; 
   end;


   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      First     : in Iterator_Type;
      Back      : in Iterator_Type) is
   begin
      Splice (Container.List, Before.Node, First.Node, Back.Node);
   end;



   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type) is
   begin
      if Container'Address = Source'Address then
         Splice 
           (Container.List, 
            Before.Node, 
            First.Node, 
            Back.Node);
      else
         Splice 
           (Container.List, 
            Before.Node, 
            Source.List, 
            First.Node, 
            Back.Node);
      end if;
   end Splice;



   function Succ 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Succ (Iterator.Node));
   end;


   function Succ 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type is
   begin
      return (Node => Succ (Iterator.Node, Offset));
   end;
      
     
   function Pred 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Pred (Iterator.Node));
   end;


   function Pred
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type is
   begin
      return (Node => Pred (Iterator.Node, Offset));
   end;


   procedure Increment (Iterator : in out Iterator_Type) is
   begin   
      Iterator := Succ (Iterator);
   end;
   
   procedure Increment 
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural) is
   begin
      Iterator := Succ (Iterator, Offset);
   end;
   
   procedure Decrement (Iterator : in out Iterator_Type) is
   begin
      Iterator := Pred (Iterator);
   end;
   
   procedure Decrement
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural) is
   begin
      Iterator := Pred (Iterator, Offset);
   end;     
   
   
   function Offset
     (From, To : Iterator_Type) return Natural is
   begin
      return Offset (From.Node, To.Node);
   end;
   


   procedure Generic_Unique 
     (Container : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type) is
      
      function Predicate (I, J : Node_Access) return Boolean is
         pragma Inline (Predicate);
      begin
         return Predicate (I.Element, J.Element);
      end;
      
      procedure Unique is
         new Node_Lists.Generic_Unique;
   begin
      Unique (Container.List, First.Node, Back.Node);
   end;


   procedure Unique 
     (Container : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type) is
      
      function Predicate (I, J : Node_Access) return Boolean is
         pragma Inline (Predicate);
      begin
         return I.Element = J.Element;
      end;
      
      procedure Unique is
         new Node_Lists.Generic_Unique;
   begin
      Unique (Container.List, First.Node, Back.Node);
   end;

   
   procedure Unique 
     (Container : in out Container_Type) is
   begin 
      Unique (Container, First (Container), Back (Container));
   end;


   procedure Generic_Merge 
     (Container : in out Container_Type;
      Source    : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type) is
      
      function Is_Less (L, R : Node_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Element < R.Element;
      end;
      
      procedure Merge is
         new Node_Lists.Generic_Merge (Is_Less);
   begin
      if Container'Address = Source'Address then
         return;
      end if;

      Merge (Container.List, Source.List, First.Node, Back.Node);            
   end;


   procedure Generic_Sort (First, Back : Iterator_Type) is
     
      function Is_Less (L, R : Node_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Element < R.Element;
      end;
      
      procedure Sort is
         new Node_Lists.Generic_Quicksort (Is_Less);
   begin
      Sort (First.Node, Back.Node);
   end;


   function Is_Equal (Left, Right : Iterator_Type) return Boolean is
   begin
      return Left.Node.Element = Right.Node.Element;
   end;
   
   function Is_Equal 
     (Left  : Iterator_Type;
      Right : Element_Type) return Boolean is
   begin
      return Left.Node.Element = Right;
   end;
      
   function Is_Equal 
     (Left  : Element_Type;
      Right : Iterator_Type) return Boolean is
   begin
      return Left = Right.Node.Element;
   end;

end Charles.Lists.Double.Discriminated_Unbounded;
