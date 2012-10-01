------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--            Charles.Lists.Double.Limited_Unbounded (body)                 --
--                                                                          --
--                                                                          --
--              Copyright (C) 2001-2002 Matthew J Heaney                    --
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
with Ada.Unchecked_Deallocation;
with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Lists.Double.Limited_Unbounded is

   use type System.Address;

   procedure Free is 
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);


--   procedure Insert
--     (Container : in out Container_Type;
--      Before    : in     Node_Access;
--      New_Node  :    out Node_Access) is
--   begin
--      New_Node := new Node_Type;

--      New_Node.Next := Before;
--      New_Node.Prev := Before.Prev;

--      Before.Prev.Next := New_Node;
--      Before.Prev := New_Node;      

--      Container.Length := Container.Length + 1;
--   end Insert;


--   procedure Delete 
--     (Container : in out Container_Type;
--      Node      : in out Node_Access) is
--   begin
--      Container.Length := Container.Length - 1;

--      pragma Assert (Node /= Container.Back);

--      Node.Next.Prev := Node.Prev;
--      Node.Prev.Next := Node.Next;

--      Free (Node);
--   end Delete;
       


   procedure Initialize (Container : in out Container_Type) is

      Back : Node_Access renames Container.Back;
      
   begin

      Back := new Node_Type;
      
      Back.Next := Back;
      Back.Prev := Back;
      
      Container.Length := 0;

   end Initialize;
   
   
   procedure Do_Finalize (Back : in out Node_Access) is
   begin

      Back.Prev.Next := null;
      
      while Back /= null loop
      
         declare
            X : Node_Access := Back;
         begin
            Back := Back.Next;
            Free (X);
         end;
         
      end loop;
      
   end Do_Finalize;


   procedure Finalize (Container : in out Container_Type) is
   
      Back : Node_Access renames Container.Back;
   begin
      if Back /= null then
         Do_Finalize (Back);
      end if;      
   end;
      
   
   function Back 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back);
   end;
      

--   function Front 
--     (Container : Container_Type) return Iterator_Type renames Back;
      
   
   function First 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back.Next);
   end;
   
   function First_Element
     (Container : Container_Type) return Element_Type is
     
      Node : constant Node_Access := Container.Back.Next;
   begin
      return Node.Element;
   end;

   
   function Last 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back.Prev);
   end;
   
   function Last_Element
     (Container : Container_Type) return Element_Type is
   
      Node : constant Node_Access := Container.Back.Prev;
   begin
      return Node.Element;
   end;
   

   function Generic_Equal 
     (Left, Right : Container_Type) return Boolean is
   
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

   end Generic_Equal;

   
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (L, R : Iterator_Type) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Node.Element < R.Node.Element;
      end;
      
      function Compare is
         new Algorithms.Generic_Lexicographical_Compare (Iterator_Type);
               
   begin

      if Left'Address = Right'Address then
         return False;
      end if;
      
      if Left.Length > Right.Length then
         return False;
      end if;
         
      return Compare (First (Left), Back (Left), First (Right), Back (Right));

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
      while Container.Length /= 0 loop
         Delete_Last (Container);
      end loop;       
   end;

   
   procedure Swap (Left, Right : in out Container_Type) is
   
      L_Back : constant Node_Access := Left.Back;
      L_Length : constant Natural := Left.Length;

   begin

      Left.Back := Right.Back;
      Left.Length := Right.Length;
      
      Right.Back := L_Back;
      Right.Length := L_Length;

   end Swap;


   procedure Prepend (Container : in out Container_Type) is
   begin
      Insert (Container, Before => First (Container));
   end;


   procedure Delete_First (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, First (Container));
   end;
   
            
   procedure Append (Container : in out Container_Type) is
   begin
      Insert (Container, Before => Back (Container));
   end;


   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, Last (Container));
   end;

   
--   procedure Insert
--     (Container : in out Container_Type;
--      Before    : in     Node_Access;
--      New_Node  :    out Node_Access) is
--   begin
--      New_Node := new Node_Type;

--      New_Node.Next := Before;
--      New_Node.Prev := Before.Prev;

--      Before.Prev.Next := New_Node;
--      Before.Prev := New_Node;      

--      Container.Length := Container.Length + 1;
--   end Insert;

   
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Iterator  :    out Iterator_Type) is
      
      Prev : constant Node_Access := Before.Node.Prev; --RM 11.6?
   begin
      Iterator.Node := new Node_Type;

      Iterator.Node.Next := Before.Node;
      Iterator.Node.Prev := Prev;
      
      Prev.Next := Iterator.Node;
      Before.Node.Prev := Iterator.Node;

      Container.Length := Container.Length + 1;
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Iterator_Type) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Before, Iterator);
   end;


   procedure Insert_Range
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Length    : in     Natural;
      Iterator  :    out Iterator_Type) is
   begin
      if Before = Null_Iterator then
         Iterator := Null_Iterator; --?
         return;
      end if;

      if Length = 0 then
         Iterator := Before;
         return;
      end if;
      
      declare
         Source : Container_Type;
      begin
         for I in 1 .. Length loop
            Append (Source);
         end loop;

         Iterator := First (Source);

         Splice (Container, Before, Source);
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
   

--   procedure Delete 
--     (Container : in out Container_Type;
--      Node      : in out Node_Access) is
--   begin
--      Container.Length := Container.Length - 1;

--      pragma Assert (Node /= Container.Back);

--      Node.Next.Prev := Node.Prev;
--      Node.Prev.Next := Node.Next;

--      Free (Node);
--   end Delete;

   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
      
      Node : Node_Access := Iterator.Node;
   begin
      if Node = null or else Node = Container.Back then
         return;
      end if;
            
      Iterator := Succ (Iterator);
      
      Node.Next.Prev := Node.Prev;
      Node.Prev.Next := Node.Next;
      
      Container.Length := Container.Length - 1;
  
      Free (Node);
   end Delete;

      
   procedure Delete_Sans_Increment
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is      

      Node : Node_Access := Iterator.Node;
   begin
      if Node = null or else Node = Container.Back then
         return;
      end if;
            
      Iterator := Back (Container);
      
      Node.Next.Prev := Node.Prev;
      Node.Prev.Next := Node.Next;
      
      Container.Length := Container.Length - 1;
  
      Free (Node);
   end Delete_Sans_Increment;

      
   procedure Delete_Sans_Assign
     (Container : in out Container_Type;
      Iterator  : in     Iterator_Type) is      
      
      Node : Node_Access := Iterator.Node;
   begin
      if Node = null or else Node = Container.Back then
         return;
      end if;
                  
      Node.Next.Prev := Node.Prev;
      Node.Prev.Next := Node.Next;
      
      Container.Length := Container.Length - 1;
  
      Free (Node);
   end Delete_Sans_Assign;


   procedure Delete
     (Container : in out Container_Type; 
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type) is      
   begin
      if First = Null_Iterator 
        or else Back = Null_Iterator
      then
         return;
      end if;
      
      while First /= Back 
        and then First /= Limited_Unbounded.Back (Container)
      loop
         Delete (Container, First);
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
      
   
   procedure Generic_Replace_Element
     (Iterator : Iterator_Type;
      By       : Element_Type) is
   begin
      Assign (Target => Iterator.Node.Element, Source => By);
   end;

      
   procedure Generic_Copy
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type) is
   begin
      Copy (Source => Iterator.Node.Element, Target => Item);
   end;


   procedure Generic_Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   begin
      Swap (Iterator.Node.Element, Item);
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



   procedure Splice
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Source    : in out Container_Type) is
      
   begin
   
      if Before = Null_Iterator 
        or else Container'Address = Source'Address 
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

      pragma Assert (not Is_In (Limited_Unbounded.Back (Container), 
                                First, Back));
      
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
      pragma Assert (not Is_In (Limited_Unbounded.Back (Source), First, Back));
            
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


     
   function Pred 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Iterator.Node.Prev);
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


end Charles.Lists.Double.Limited_Unbounded;
