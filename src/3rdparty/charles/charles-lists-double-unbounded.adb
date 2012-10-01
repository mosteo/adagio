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
with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Lists.Double.Unbounded is

   procedure Free is 
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);


   procedure Initialize (Container : in out Container_Type) is
   
      Back : Node_Access renames Container.Back;
   begin
      Back := new Node_Type;
      
      Back.Next := Back;
      Back.Prev := Back;
      
      Container.Length := 0;
   end;
   
   
   procedure Do_Finalize (Back : in out Node_Access) is
      First : Node_Access := Back.Next;
   begin
      while First /= Back loop
         declare
            X : Node_Access := First;
         begin
            First := First.Next;
            Free (X);
         end;
      end loop;
            
      Free (Back);
   end Do_Finalize;
   
   
   procedure Adjust (Container : in out Container_Type) is
   
      Source_Back : constant Node_Access := Container.Back;      
   begin   
      Container.Back := null;
      
      declare
         Iter : Node_Access := Source_Back.Next;
         Back_Iter : constant Node_Access := Source_Back;
         
         Back : Node_Access := new Node_Type;
         Last : Node_Access;
      begin
         Back.Next := Back;
         Back.Prev := Back;
         
         Last := Back;
      
         while Iter /= Back_Iter loop
         
            Last.Next := new Node_Type'(Element => Iter.Element,
                                        Next    => Back,
                                        Prev    => Last);

            Last := Last.Next;
            Iter := Iter.Next;

         end loop;

         Container.Back := Back;
      exception
         when others =>
            Do_Finalize (Back);         
            raise;
      end;               
   end Adjust;


   procedure Finalize (Container : in out Container_Type) is
   begin
      if Container.Back /= null then
         Do_Finalize (Container.Back);         
      end if;
   end Finalize;
      
   
   function "=" (Left, Right : Container_Type) return Boolean is
   
      LI : Iterator_Type := First (Left);
      RI : Iterator_Type := First (Right);
      
   begin

      if Left'Address = Right'Address then
         return True;
      end if;
      
      if Left.Length /= Right.Length then
         return False;
      end if;
      
      for I in 1 .. Left.Length loop
         if LI.Node.Element /= RI.Node.Element then
            return False;
         end if;
         
         LI := Succ (LI);
         RI := Succ (RI);
      end loop;
      
      return True;

   end "=";

   
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (L, R : Node_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Element < R.Element;
      end;
      
      function Succ (Iter : Node_Access) return Node_Access is
         pragma Inline (Succ);
      begin
         return Iter.Next;
      end;      
      
      function Lexicographical_Compare is
         new Algorithms.Generic_Lexicographical_Compare (Node_Access);
         
      LF : constant Node_Access := Left.Back.Next;
      LB : constant Node_Access := Left.Back;
      
      RF : constant Node_Access := Right.Back.Next;
      RB : constant Node_Access := Right.Back;
      
   begin

      if Left'Address = Right'Address then
         return False;
      end if;
      
      if Left.Length > Right.Length then
         return False;
      end if;
         
      return Lexicographical_Compare (LF, LB, RF, RB);   

   end Generic_Less;



   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Length;
   end;

   
   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Length = 0;
   end;

   
   procedure Clear (Container : in out Container_Type) is
   begin
      while Container.Length > 0 loop
         Delete_Last (Container);
      end loop;   
   end Clear;

   
   procedure Swap (Left, Right : in out Container_Type) is
   
      L_Node : constant Node_Access := Left.Back;
      L_Length : constant Natural := Left.Length;

   begin

      Left.Back := Right.Back;
      Left.Length := Right.Length;
      
      Right.Back := L_Node;
      Right.Length := L_Length;

   end Swap;
         
   
   
   
   function To_Container (Length : Natural) return Container_Type is
   
      Back : Node_Access := new Node_Type;
      
   begin
      
      Back.Next := Back;
      Back.Prev := Back;
   
      for I in 1 .. Length loop
      
         declare
            Node : constant Node_Access := new Node_Type;
            First : constant Node_Access := Back.Next;
         begin
            Node.Next := First;
            Node.Prev := Back;
            
            Back.Next := Node;      
            First.Prev := Node;
         end;
         
      end loop;

      return (Ada.Finalization.Controlled with Back, Length);
      
   exception
      when others =>
         
         Do_Finalize (Back);
         raise;
               
   end To_Container;   
      
      
   function To_Container 
     (Length : Natural;
      Item   : Element_Type) return Container_Type is
   
      Back : Node_Access := new Node_Type;
      
   begin
      
      Back.Next := Back;
      Back.Prev := Back;
   
      for I in 1 .. Length loop
      
         declare
            First : constant Node_Access := Back.Next;

            Node : constant Node_Access := 
               new Node_Type'(Element => Item,
                              Next    => First,
                              Prev    => Back);
         begin
            Back.Next := Node;      
            First.Prev := Node;
         end;
         
      end loop;

      return (Ada.Finalization.Controlled with Back, Length);
      
   exception
      when others =>
         
         Do_Finalize (Back);
         raise;
               
   end To_Container;   
      
      
   function To_Container 
     (First  : Iterator_Type;
      Back   : Iterator_Type) return Container_Type is

      Target_Back : Node_Access := new Node_Type;
      Length      : Natural := 0;
      
      I : Iterator_Type := First;
      
   begin
      
      Target_Back.Next := Target_Back;
      Target_Back.Prev := Target_Back;
   
      while I /= Back loop
      
         declare
            Target_Last : constant Node_Access := Target_Back.Prev;

            Node : constant Node_Access := 
               new Node_Type'(Element => I.Node.Element,
                              Next    => Target_Back,
                              Prev    => Target_Last);
         begin
            Target_Back.Prev := Node;
            Target_Last.Next := Node;
         end;
         
         Length := Length + 1;
         
         I := Succ (I);
         
      end loop;

      return (Ada.Finalization.Controlled with Target_Back, Length);
      
   exception
      when others =>
         
         Do_Finalize (Target_Back);
         raise;
               
   end To_Container;   


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
      
      Source : Container_Type := To_Container (Length);
   begin
      Swap (Target, Source);
   end;
            

   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type) is
      
      Source : Container_Type := To_Container (Length, Item);
   begin
      Swap (Target, Source);
   end;
            

   
   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;
      
      declare
         C : Container_Type := Source;
      begin
         Swap (Target, C);
      end;
   end Assign;


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
   
   
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Node_Access;
      New_Item  : in     Element_Type;
      New_Node  :    out Node_Access) is
   begin
      New_Node := new Node_Type'(Element => New_Item, 
                                 Next    => Before, 
                                 Prev    => Before.Prev);

      Before.Prev.Next := New_Node;      
      Before.Prev := New_Node; 

      Container.Length := Container.Length + 1;
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Node_Access;
      New_Node  :    out Node_Access) is
      
      Prev : constant Node_Access := Before.Prev;
   begin
      New_Node := new Node_Type;
      
      New_Node.Next := Before;
      New_Node.Prev := Prev;

      Before.Prev.Next := New_Node;      
      Before.Prev := New_Node; 

      Container.Length := Container.Length + 1;
   end;


   procedure Delete 
     (Container : in out Container_Type;
      Node      : in out Node_Access) is

      pragma Assert (Node /= null);
      pragma Assert (Node /= Container.Back);
   begin      
      Container.Length := Container.Length - 1;

      Node.Next.Prev := Node.Prev;
      Node.Prev.Next := Node.Next;

      Free (Node);
   end Delete;
       

   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert (Container, First (Container), New_Item);
   end;


   procedure Prepend
     (Container : in out Container_Type) is      
   begin
      Insert (Container, Before => First (Container));
   end;

      
   procedure Delete_First (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, First (Container));
   end;
   
      
   procedure Append
     (Container : in out Container_Type) is      
   begin
      Insert (Container, Back (Container));
   end;
            

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is      
   begin
      Insert (Container, Back (Container), New_Item);
   end;
      
      
   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, Last (Container));
   end;

   
   
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type) is
   
      New_Node : Node_Access;
   begin
      Insert (Container, Before.Node, New_Node);
   end;



   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type) is
   
      New_Node : Node_Access;
   begin
      Insert (Container, Before.Node, New_Item, New_Node);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type) is
   begin
      Insert (Container, Before.Node, New_Item, Iterator.Node);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Iterator  :    out Iterator_Type) is
   begin
      Insert (Container, Before.Node, Iterator.Node);
   end;


   procedure Insert_Range
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Length    : in     Natural;
      Iterator  :    out Iterator_Type) is
      
   begin
   
      if Length = 0 then
         Iterator := Before;
         return;
      end if;
      
      declare
         C : Container_Type := To_Container (Length);
      begin
         Iterator := First (C);
         Splice (Container, Before, Source => C);
      end;
      
   end Insert_Range;
   

   procedure Insert_Range
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Length    : in     Natural) is
      
      Iterator : Iterator_Type;
   begin
      Insert_Range (Container, Before, Length, Iterator);
   end;
   


   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
      
      Node : Node_Access := Iterator.Node;
      
   begin
   
      if Node = null or Node = Container.Back then
         return;
      end if;

      Iterator := Succ (Iterator);
      Delete (Container, Node);
      
   end Delete;

      
   procedure Delete_Sans_Increment
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
      
      Node : Node_Access := Iterator.Node;
      
   begin
   
      if Node = null or Node = Container.Back then
         return;
      end if;
      
      Iterator := Back (Container);
      Delete (Container, Node);
      
   end Delete_Sans_Increment;

      
   procedure Delete_Sans_Assign
     (Container : in out Container_Type;
      Iterator  : in     Iterator_Type) is
      
      Node : Node_Access := Iterator.Node;

   begin
   
      if Node = null or Node = Container.Back then
         return;
      end if;

      Delete (Container, Node);
      
   end Delete_Sans_Assign;

      

   procedure Delete
     (Container : in out Container_Type; 
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type) is      

      Container_Back : constant Iterator_Type := 
         Unbounded.Back (Container);
      
   begin
   
      if First = Null_Iterator or Back = Null_Iterator then
         return;
      end if;

      while First /= Back and First /= Container_Back loop
         Delete (Container, First);
      end loop;
      
   end Delete;
   
         

--   procedure Unchecked_Delete
--     (Container : in out Container_Type;
--      Iterator  : in     Iterator_Type) is
--      
--      Node : Node_Access := Iterator.Node;
--   begin
--      Delete (Container, Node);
--   end;

--      

--   procedure Unchecked_Delete
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
   
         


   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type) is
      
      Iter : Iterator_Type := First (Container);
      Back_Iter : constant Iterator_Type := Back (Container);
   begin
      while Iter /= Back_Iter loop
         if Iter.Node.Element = Item then
            Delete (Container, Iter);
         else
            Iter := Succ (Iter);
         end if;
      end loop;
   end;
      
      
   procedure Generic_Delete (Container : in out Container_Type) is
      Iter : Iterator_Type := First (Container);
      Back_Iter : constant Iterator_Type := Back (Container);
   begin
      while Iter /= Back_Iter loop
         if Predicate (Iter.Node.Element) then
            Delete (Container, Iter);
         else
            Iter := Succ (Iter);
         end if;
      end loop;      
   end;


   
   procedure Reverse_List (Container : in out Container_Type) is

      procedure Swap (L, R : Node_Access) is
      
         LN : constant Node_Access := L.Next;
         LP : constant Node_Access := L.Prev;
         
         RN : constant Node_Access := R.Next;
         RP : constant Node_Access := R.Prev;
         
      begin

         LP.Next := R;
         RN.Prev := L;

         L.Next := RN;
         R.Prev := LP;
         
         if LN = R then
         
            pragma Assert (RP = L);
            
            L.Prev := R;            
            R.Next := L;
            
         else
         
            L.Prev := RP;
            RP.Next := L;
            
            R.Next := LN;
            LN.Prev := R;
            
         end if;

      end Swap;

      I : Node_Access := Container.Back.Next;
      J : Node_Access := Container.Back;

   begin
   
      while I /= J loop
      
         J := J.Prev;
         
         exit when I = J;
         
         Swap (I, J);
         
         J := J.Next;
         
         exit when I = J;
         
         I := I.Prev;
         
         exit when I = J;
         
         Swap (J, I);
         
         I := I.Next;
                    
      end loop;
      
   end Reverse_List;



   function First 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back.Next);
   end;


   function First_Element
     (Container : Container_Type) return Element_Type is
     
      I : constant Iterator_Type := First (Container);
   begin
      return I.Node.Element;
   end;

         
   function Back 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back);
   end;
      

   function Last 
     (Container : Container_Type) return Iterator_Type is
   begin
      return Pred (Back (Container));
   end;


   function Last_Element 
     (Container : Container_Type) return Element_Type is
   
      I : constant Iterator_Type := Last (Container);
   begin
      return I.Node.Element;
   end;
   

--   function Front 
--     (Container : Container_Type) return Iterator_Type is
--   begin
--      return Pred (First (Container));
--   end;
      
   
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


   procedure Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   
      E : constant Element_Type := Iterator.Node.Element;
   begin
      Iterator.Node.Element := Item;
      Item := E;
   end;

   procedure Generic_Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   begin
      Swap (Iterator.Node.Element, Item);
   end;


   procedure Swap_Element (Left, Right : in Iterator_Type) is
      LE : constant Element_Type := Left.Node.Element;
   begin
      Left.Node.Element := Right.Node.Element;
      Right.Node.Element := LE;
   end;
   
   procedure Generic_Swap_Element (Left, Right : in Iterator_Type) is
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


   function Is_In
     (Item        : Element_Type;
      First, Back : Iterator_Type) return Boolean is
   begin
      return Find (First, Back, Item) /= Back;
   end;      
      
      
   function Is_In
     (Item      : Element_Type;
      Container : Container_Type) return Boolean is
   begin
      return Is_In (Item, First (Container), Back (Container));
   end;
      

   function Is_In
     (Iterator    : Iterator_Type;
      First, Back : Iterator_Type) return Boolean is
       
      I : Iterator_Type := First;
   begin
      if Iterator = Null_Iterator
        or else First = Null_Iterator
        or else Back = Null_Iterator
      then
         return False; --or true?
      end if;
      
      while I /= Back loop
         if I = Iterator then
            return True;
         end if;
         
         I := Succ (I);
      end loop;
      
      return False;
   end Is_In;
      

           


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
   
      if Container'Address = Source'Address 
        or else Before = Null_Iterator
        or else Source.Length = 0
      then
         return;
      end if;
      
      Before.Node.Prev.Next := Source.Back.Next;
      Source.Back.Next.Prev := Before.Node.Prev;
      
      Before.Node.Prev := Source.Back.Prev;
      Source.Back.Prev.Next := Before.Node;
      
      Source.Back.Next := Source.Back;
      Source.Back.Prev := Source.Back;
      
      Container.Length := Container.Length + Source.Length;
      Source.Length := 0;

   end Splice;


   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      Iterator  : in Iterator_Type) is
      
   begin
   
      if Before = Null_Iterator
        or else Iterator = Null_Iterator
        or else Iterator = Back (Container)
        or else Iterator = Before
        or else Succ (Iterator) = Before 
      then
         return;
      end if;
      
      pragma Assert (Container.Length > 0);

      Iterator.Node.Prev.Next := Iterator.Node.Next;
      Iterator.Node.Next.Prev := Iterator.Node.Prev;
   
      Before.Node.Prev.Next := Iterator.Node;
      Iterator.Node.Prev := Before.Node.Prev;
      
      Before.Node.Prev := Iterator.Node;
      Iterator.Node.Next := Before.Node;
      
   end Splice;


   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      Iterator  : in     Iterator_Type) is
      
   begin

      if Before = Null_Iterator
        or else Iterator = Null_Iterator
        or else Iterator = Back (Source)
        or else Iterator = Before
        or else Succ (Iterator) = Before
      then
         return;
      end if;
   
      pragma Assert (Source.Length > 0);

      Iterator.Node.Prev.Next := Iterator.Node.Next;
      Iterator.Node.Next.Prev := Iterator.Node.Prev;
   
      Before.Node.Prev.Next := Iterator.Node;
      Iterator.Node.Prev := Before.Node.Prev;
      
      Before.Node.Prev := Iterator.Node;
      Iterator.Node.Next := Before.Node;
      
      if Container'Address /= Source'Address then
         Container.Length := Container.Length + 1;
         Source.Length := Source.Length - 1;
      end if;
      
   end Splice;


   procedure Splice
     (Container : in Container_Type;
      Before    : in Iterator_Type;
      First     : in Iterator_Type;
      Back      : in Iterator_Type) is
      
      pragma Warnings (Off, Container);

      Last  : Node_Access;
      
   begin
   
      if Before = Null_Iterator
        or else First = Null_Iterator
        or else Back = Null_Iterator
        or else Before = Back
        or else First = Back
      then 
         return;
      end if;
      
      pragma Assert (not Is_In (Before, First, Back));
      pragma Assert (not Is_In (Unbounded.Back (Container), First, Back));
            
      Last := Back.Node.Prev;
      
      First.Node.Prev.Next := Back.Node;
      Back.Node.Prev := First.Node.Prev;
      
      First.Node.Prev := Before.Node.Prev;
      Before.Node.Prev.Next := First.Node;
      
      Last.Next := Before.Node;
      Before.Node.Prev := Last;
      
   end Splice;



   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type;
      First     : in     Iterator_Type;
      Back      : in     Iterator_Type) is

      Count : Positive;
      Last  : Node_Access;
      
   begin

      if Container'Address = Source'Address then
         Splice (Container, Before, First, Back);
         return;
      end if;
      
      if Before = Null_Iterator
        or else First = Null_Iterator
        or else Back = Null_Iterator
        or else First = Back
      then
         return;
      end if;
      
      pragma Assert (Before /= Back);
      pragma Assert (Source.Length > 0);
      pragma Assert (not Is_In (Unbounded.Back (Source), First, Back));

      Count := Offset (From => First, To => Back);
      pragma Assert (Count <= Source.Length);
      
      Last := Back.Node.Prev;
      
      First.Node.Prev.Next := Back.Node;
      Back.Node.Prev := First.Node.Prev;
      
      First.Node.Prev := Before.Node.Prev;
      Before.Node.Prev.Next := First.Node;
      
      Last.Next := Before.Node;
      Before.Node.Prev := Last;
      
      Container.Length := Container.Length + Count;
      Source.Length := Source.Length - Count;
      
   end Splice;



   function Succ 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Iterator.Node.Next);
   end;


   function Succ 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type is
      
      Result : Iterator_Type := Iterator;
   begin
      for I in 1 .. Offset loop
         Result := Succ (Result);
      end loop;
      
      return Result;
   end;
      
     
   function Pred 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Iterator.Node.Prev);
   end;


   function Pred
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type is
      
      Result : Iterator_Type := Iterator;
   begin
      for I in 1 .. Offset loop
         Result := Pred (Result);
      end loop;
      
      return Result;
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
     
      Result : Integer'Base := 0;        
      I : Iterator_Type := From;
   begin
      while I /= To loop
         Result := Result + 1;
         I := Succ (I);
      end loop;
      
      return Result;
   end;
   

   procedure Generic_Unique (Container : in out Container_Type) is

      I : Iterator_Type := First (Container);
      J : Iterator_Type;
      
      B : constant Iterator_Type := Back (Container);

   begin

      if Container.Length = 0 then
         return;
      end if;
      
      J := Succ (I);
      
      while J /= B loop
            
         if Predicate (J.Node.Element, I.Node.Element) then

            Delete (Container, J);

         else

            I := J;
            J := Succ (I);

         end if;

      end loop;
      
   end Generic_Unique;


   procedure Unique (Container : in out Container_Type) is

      procedure Do_Unique is 
         new Generic_Unique (Predicate => "=");
   begin 
      Do_Unique (Container);
   end;

   

   procedure Generic_Merge 
     (Container : in out Container_Type;
      Source    : in out Container_Type) is
   
      LI : Iterator_Type := First (Container);
      LB : constant Iterator_Type := Back (Container);
      
      RI : Iterator_Type := First (Source);
      RB : constant Iterator_Type := Back (Source);

   begin

      if Container'Address = Source'Address then
         return;
      end if;
      
      while RI /= RB loop
      
         if LI = LB then
            Splice (Container, LB, Source);
            return;
         end if;
         
         if RI.Node.Element < LI.Node.Element then

            declare
               RJ : constant Iterator_Type := RI;
            begin
               RI := Succ (RI);
               Splice (Container, LI, Source, RJ);
            end;
            
         else

            LI := Succ (LI);
            
         end if;

      end loop;      
      
   end Generic_Merge;


   procedure Generic_Quicksort
     (Container : in out Container_Type) is

      procedure Partition 
        (Pivot : in Node_Access;
         Back  : in Node_Access) is

         Node : Node_Access := Pivot.Next;
         
      begin
      
         while Node /= Back loop

            if Node.Element < Pivot.Element then

               declare
                  Prev : constant Node_Access := Node.Prev;
                  Next : constant Node_Access := Node.Next;
               begin
                  Prev.Next := Next;
                  Next.Prev := Prev;
                  
                  Node.Next := Pivot;
                  Node.Prev := Pivot.Prev;
                  
                  Pivot.Prev := Node;
                  Node.Prev.Next := Node;

                  Node := Next;               
               end;

            else

               Node := Node.Next;

            end if;

         end loop;      

      end Partition;


      procedure Sort (Front, Back : Node_Access) is

         Pivot : constant Node_Access := Front.Next;

      begin

         if Pivot /= Back then

            Partition (Pivot, Back);

            Sort (Front, Pivot);

            Sort (Pivot, Back);

         end if;

      end Sort;   
      
   begin

      Sort (Container.Back, Container.Back);   
      
   end Generic_Quicksort;


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

end Charles.Lists.Double.Unbounded;
