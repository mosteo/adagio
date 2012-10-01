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
with System;
with Ada.Unchecked_Deallocation;
with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Lists.Single.Unbounded is

   use type System.Address;

   function Prev 
     (Front : Node_Access;
      Node  : Node_Access) return Node_Access is
      
      Result : Node_Access := Front;
   begin
      while Result.Next /= Node loop
         Result := Result.Next;
      end loop;
      
      return Result;
   end;   


   procedure Free is 
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);


   procedure Initialize (Container : in out Container_Type) is
   begin
      Container.Back := new Node_Type;
      
      Container.Back.Next := Container.Back;
      
      Container.Last := Container.Back;

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
   end;


   procedure Copy 
     (Source_Back : in     Node_Access;
      Back, Last  :    out Node_Access) is
      
      Iter : Node_Access := Source_Back.Next;
      Back_Iter : constant Node_Access := Source_Back;
      
   begin
      
      Back := new Node_Type;
      Back.Next := Back;

      Last := Back;
   
      while Iter /= Back_Iter loop
      
         begin
            Last.Next := new Node_Type'(Iter.Element, Back);
         exception
            when others =>  
               Do_Finalize (Back);
               raise;
         end;               
         
         Last := Last.Next;
         Iter := Iter.Next;

      end loop;
      
   end Copy;
   

   procedure Adjust (Container : in out Container_Type) is
   begin
      Copy (Source_Back => Container.Back, 
            Back        => Container.Back, 
            Last        => Container.Last);
   exception
      when others =>
         Container.Back := null;
         Container.Last := null;
         Container.Length := 0;

         raise;
   end Adjust;


   procedure Finalize (Container : in out Container_Type) is
   begin
      if Container.Back /= null then
         Do_Finalize (Container.Back);
         Container.Last := null;
         Container.Length := 0;
      end if;
   exception
      when others =>
         Container.Back := null;
         Container.Last := null;
         Container.Length := 0;

         raise;
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
   end;

   
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (L, R : Iterator_Type) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Node.Element < R.Node.Element;
      end;
      
      function Lexicographical_Compare is
         new Algorithms.Generic_Lexicographical_Compare (Iterator_Type);
         
      LF : constant Iterator_Type := First (Left);
      LB : constant Iterator_Type := Back (Left);
      
      RF : constant Iterator_Type := First (Right);
      RB : constant Iterator_Type := Back (Right);

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
         Delete_First (Container);
      end loop;
   end;

   
   procedure Swap (Left, Right : in out Container_Type) is
   
      L_Last : constant Node_Access := Left.Last;
      L_Back : constant Node_Access := Left.Back;
      L_Length : constant Natural := Left.Length;

   begin

      Left.Last := Right.Last;
      Left.Back := Right.Back;
      Left.Length := Right.Length;
      
      Right.Last := L_Last;
      Right.Back := L_Back;
      Right.Length := L_Length;

   end Swap;
      

   procedure New_Container 
     (Length : in     Natural;
      Back   :    out Node_Access;
      Last   :    out Node_Access) is
   
   begin

      Back := new Node_Type;      

      Back.Next := Back;
      Last := Back;
   
      for I in 1 .. Length loop
      
         begin
            Last.Next := new Node_Type;
         exception
            when others =>               
               Do_Finalize (Back);
               raise;
         end;

         Last := Last.Next;
         Last.Next := Back;
         
      end loop;

   end New_Container;
   
   
   
   procedure New_Container 
     (Length : in     Natural;
      Item   : in     Element_Type;
      Back   :    out Node_Access;
      Last   :    out Node_Access) is
   
   begin

      Back := new Node_Type;      

      Back.Next := Back;
      Last := Back;
   
      for I in 1 .. Length loop
      
         begin
            Last.Next := new Node_Type'(Item, Back);         
         exception
            when others =>   
               Do_Finalize (Back);         
               raise;
         end;
               
         Last := Last.Next;
         
      end loop;

   end New_Container;
   
   
   
   function To_Container (Length : Natural) return Container_Type is
   
      Back, Last : Node_Access;

      use Ada.Finalization;
   begin
      New_Container (Length, Back => Back, Last => Last);

      return (Controlled with Back   => Back, 
                              Last   => Last,
                              Length => Length);
   end;   
      
      
   function To_Container 
     (Length : Natural;
      Item   : Element_Type) return Container_Type is   

      Back, Last : Node_Access;

      use Ada.Finalization;   
   begin
      New_Container (Length, Item, Back => Back, Last => Last);

      return (Controlled with Back   => Back, 
                              Last   => Last,
                              Length => Length);
   end;   
      
  
      
   function To_Container 
     (First  : Iterator_Type;
      Back   : Iterator_Type) return Container_Type is

      Target_Back : Node_Access := new Node_Type;

      Target_Last : Node_Access := Target_Back;

      Length : Integer'Base := 0;

      Iter : Iterator_Type := First;

      use Ada.Finalization;
   begin
      Target_Back.Next := Target_Back;
   
      while Iter /= Back loop
      
         begin
            Target_Last.Next := new Node_Type'(Iter.Node.Element, 
                                               Target_Back);         
         exception
            when others =>   
               Do_Finalize (Target_Back);         
               raise;
         end;
               
         Target_Last := Target_Last.Next;

         Length := Length + 1;

         Iter := Succ (Iter);
         
      end loop;

      return (Controlled with Back => Target_Back,
                              Last => Target_Last,
                              Length => Length);
   end To_Container;

      
   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
   
      Back, Last : Node_Access;   
   begin
      New_Container (Length, Back => Back, Last => Last);

      Finalize (Target);

      Target.Back := Back;
      Target.Last := Last;
      Target.Length := Length;
   end;
            

   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type) is
      
      Back, Last : Node_Access;   
   begin
      New_Container (Length, Item, Back => Back, Last => Last);

      Finalize (Target);

      Target.Back := Back;
      Target.Last := Last;
      Target.Length := Length;
   end;
       

   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;
      
      declare
         Back, Last : Node_Access;
      begin
         Copy (Source.Back, Back => Back, Last => Last);
         
         Finalize (Target);
         
         Target.Back := Back;
         Target.Last := Last;
         Target.Length := Source.Length;
      end;
   end Assign;





   procedure Prepend
     (Container : in out Container_Type) is      
   begin
      Insert_After (Container, Back (Container));
   end;

      
   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is      
   begin
      Insert_After (Container, Back (Container), New_Item);
   end;

      
   procedure Delete_First (Container : in out Container_Type) is
   begin
      Delete_After (Container, Back (Container));
   end;
   
      
     
   procedure Append
     (Container : in out Container_Type) is
   begin
      Insert_After (Container, Last (Container));
   end;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert_After (Container, Last (Container), New_Item);
   end;

      
--   procedure Insert_After
--     (Container : in out Container_Type;
--      Position  : in     Node_Access;
--      New_Node  :    out Node_Access) is
--   begin
--      New_Node := new Node_Type;
--      New_Node.Next := Position.Next;
--      
--      Position.Next := New_Node;      

--      Container.Length := Container.Length + 1;
--      
--      if Container.Last = Position then
--         Container.Last := New_Node;
--      end if;      
--   end;


--   procedure Insert_After
--     (Container : in out Container_Type;
--      Position  : in     Node_Access;
--      New_Item  : in     Element_Type;
--      New_Node  :    out Node_Access) is
--   begin
--      New_Node := new Node_Type'(New_Item, Position.Next);

--      Position.Next := New_Node;      

--      Container.Length := Container.Length + 1;
--      
--      if Container.Last = Position then
--         Container.Last := New_Node;
--      end if;      
--   end;


      
   procedure Insert_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Iterator  :    out Iterator_Type) is
      
      Next : constant Node_Access := Position.Node.Next; --RM 11.6?
      
   begin
   
      Iterator.Node := new Node_Type;
      Iterator.Node.Next := Next;
      
      Position.Node.Next := Iterator.Node;

      Container.Length := Container.Length + 1;
      
      if Container.Last = Position.Node then
         Container.Last := Iterator.Node;
      end if;
      
   end Insert_After;


   procedure Insert_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type) is

      Iterator : Iterator_Type;
   begin
      Insert_After (Container, Position, Iterator);
   end;



   procedure Insert_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type) is

   begin
   
      Iterator.Node := new Node_Type'(New_Item, Position.Node.Next);

      Position.Node.Next := Iterator.Node;      

      Container.Length := Container.Length + 1;
      
      if Container.Last = Position.Node then
         Container.Last := Iterator.Node;
      end if;      
      
   end Insert_After;


   procedure Insert_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      New_Item  : in     Element_Type) is

      Iterator : Iterator_Type;
   begin
      Insert_After (Container, Position, New_Item, Iterator);
   end;


--   procedure Insert_After
--     (Container   : in out Container_Type;
--      Position    : in     Iterator_Type;
--      First, Back : in     Iterator_Type) is
--      
--      Iter : Iterator_Type := First;
--      
--      Node, X : Node_Access;
--      
--      N : Integer'Base := 0;
--   
--   begin

--      if First = Back then
--         return;
--      end if;
--      
--      Node := new Node_Type'(Iter.Node.Element, null);
--      
--      X := Node;
--      
--      loop      

--         N := N + 1;                                 
--                                                     
--         Succ (Iter);
--         
--         exit when Iter = Back;
--         
--         Node.Next := new Node_Type'(Iter.Node.Element, null);
--         
--         Node := Node.Next;
--                                
--      end loop;
--      
--      Node.Next := Position.Node.Next;

--      Position.Node.Next := Node;
--      
--      Container.Length := Container.Length + N;
--   
--   exception
--      when others =>
--      
--         while X /= null loop
--            declare
--               Next : Node_Access := X.Next;
--            begin
--               Free (X);
--               X := Next;
--            end;
--         end loop;
--         
--         raise;

--   end Insert_After;

      
--   procedure Delete
--     (Container : in out Container_Type;
--      Iterator  : in out Iterator_Type) is
--      
--      X : Node_Access := Iterator.Node;
--   begin      
--      if X = null or else X = Container.Back then
--         return; --no-op or error?
--      end if;
--      
--      Container.Length := Container.Length - 1;
--         
--      Iterator.Node := X.Next;

--      declare
--         Prev_Node : constant Node_Access := 
--            Prev (Front => Container.Back, Node => X);
--      begin
--         Prev_Node.Next := Iterator.Node;
--      
--         if Container.Last = X then
--            Container.Last := Prev_Node;
--         end if;         
--      end;
--      
--      Free (X);
--   end Delete;
   
      
--   procedure Delete_After
--     (Container : in out Container_Type;
--      Iterator  : in     Iterator_Type) is
--      
--      pragma Assert (Container.Length > 0);
--      pragma Assert (Iterator.Node /= Container.Back);
--            
--      X : Node_Access := Iterator.Node.Next;
--   begin
--      if X = Container.Back then
--         return;
--      end if;
--      
--      Iterator.Node.Next := X.Next;
--      
--      if X = Container.Last then
--         Container.Last := Iterator.Node;
--      end if;
--      
--      Free (X);
--      
--      Container.Length := Container.Length - 1;
--   end;

   procedure Delete_After
     (Container : in out Container_Type;
      Iterator  : in     Iterator_Type) is
      
      X : Node_Access := Iterator.Node.Next;
   begin
      if X = Container.Back then
         return;
      end if;
      
      Container.Length := Container.Length - 1;

      Iterator.Node.Next := X.Next;
      
      if Container.Last = X then
         Container.Last := Iterator.Node;
      end if;
      
      Free (X);            
   end Delete_After;

      
--   procedure Delete
--     (Container : in out Container_Type; 
--      First     : in out Iterator_Type;
--      Back      : in     Iterator_Type) is
--      
--      Front : constant Iterator_Type := Pred (Container, First);
--   begin
--      First := Back;
--      Delete_After (Container, Front, Back);
--   end;      
   
      

   procedure Delete_After
     (Container   : in out Container_Type; 
      Front, Back : in     Iterator_Type) is
      
      X : Node_Access;

   begin

      while Front.Node.Next /= Back.Node 
        and then Front.Node.Next /= Container.Back
      loop
      
         X := Front.Node.Next;
         
         Front.Node.Next := X.Next;

         Container.Length := Container.Length - 1;
         
         if Container.Last = X then
            Container.Last := Front.Node;
         end if;
         
         Free (X);
         
      end loop;      
         
   end Delete_After;
   
         
   procedure Delete
     (Container : in out Container_Type;
      Item      : in     Element_Type) is
      
      Node : Node_Access := Container.Back;
      X    : Node_Access;
   begin
      while Node.Next /= Container.Back loop

         if Node.Next.Element = Item then

            X := Node.Next;

            Node.Next := X.Next;
            
            if Container.Last = X then
               Container.Last := Node;
            end if;
            
            Container.Length := Container.Length - 1;

            Free (X);

         else

            Node := Node.Next;

         end if;

      end loop;
   end Delete;
      
      
   procedure Generic_Delete (Container : in out Container_Type) is

      Node : Node_Access := Container.Back;
      X    : Node_Access;
   begin
      while Node.Next /= Container.Back loop

         if Predicate (Node.Next.Element) then

            X := Node.Next;
            
            Node.Next := X.Next;
            
            if X = Container.Last then
               Container.Last := Node;
            end if;
            
            Container.Length := Container.Length - 1;

            Free (X);
            
         else

            Node := Node.Next;

         end if;

      end loop;
   end Generic_Delete;


--   procedure Splice_After
--     (Container : in out Container_Type;
--      Position  : in     Node_Access;
--      Source    : in out Container_Type) is
--      
--   begin
--   
--      Source.Last.Next := Position.Next;
--      
--      Position.Next := Source.Back.Next;

--      if Container.Last = Position then
--         Container.Last := Source.Last;
--      end if;
--      
--      Source.Last := Source.Back;
--      Source.Back.Next := Source.Back;
--      
--      Container.Length := Container.Length + Source.Length;
--      
--      Source.Length := 0;
--      
--   end Splice_After;


   procedure Splice_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Source    : in out Container_Type) is

   begin
   
      if Container'Address = Source'Address
        or else Position = Null_Iterator
        or else Source.Length = 0 
      then
         return;
      end if;

      Source.Last.Next := Position.Node.Next;      
      Position.Node.Next := Source.Back.Next;

      if Container.Last = Position.Node then
         Container.Last := Source.Last;
      end if;
      
      Source.Last := Source.Back;
      Source.Back.Next := Source.Back;
      
      Container.Length := Container.Length + Source.Length;      
      Source.Length := 0;
      
      pragma Assert (Succ (Last (Container)) = Back (Container));

   end Splice_After;

   

--   procedure Splice_After
--     (Container : in out Container_Type;
--      Position  : in     Node_Access;
--      Source    : in out Container_Type;
--      Pred      : in     Node_Access) is

--      Node : constant Node_Access := Pred.Next;

--   begin
--         
--      Pred.Next := Node.Next;   

--      Node.Next := Position.Next;
--      
--      Position.Next := Node;

--      if Container'Address = Source'Address then
--      
--         if Container.Last = Position then
--            Container.Last := Node;

--         elsif Container.Last = Node then
--            Container.Last := Pred;
--            
--         end if;
--         
--      else
--               
--         if Container.Last = Position then
--            Container.Last := Node;
--         end if;

--         if Source.Last = Node then
--            Source.Last := Pred;
--         end if;

--         Container.Length := Container.Length + 1;
--         Source.Length := Source.Length - 1;
--         
--      end if;

--   end Splice_After;   


   procedure Splice_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Pred      : in     Iterator_Type) is
      
      Node : Node_Access;

   begin
         
      if Position = Null_Iterator
        or else Pred = Null_Iterator
        or else Position = Pred
        or else Succ (Pred) = Back (Container)
      then 
         return;
      end if;
      
      pragma Assert (Container.Length > 0);
      
      Node := Pred.Node.Next;

      Pred.Node.Next := Node.Next;   

      Node.Next := Position.Node.Next;
      
      Position.Node.Next := Node;

      if Container.Last = Position.Node then
         Container.Last := Node;

      elsif Container.Last = Node then
         Container.Last := Pred.Node;
         
      end if;
      
      pragma Assert (Succ (Last (Container)) = Back (Container));

   end Splice_After;



   procedure Splice_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Source    : in out Container_Type;
      Pred      : in     Iterator_Type) is

      Node : Node_Access;
      
   begin
   
      if Container'Address = Source'Address then
         Splice_After (Container, Position, Pred);
         return;
      end if;
      
      if Position = Null_Iterator
        or else Pred = Null_Iterator
        or else Succ (Pred) = Back (Container)
      then 
         return;
      end if;

      pragma Assert (Source.Length > 0);
      pragma Assert (Pred /= Position);

      Node := Pred.Node.Next;
            
      Pred.Node.Next := Node.Next;   

      Node.Next := Position.Node.Next;
      
      Position.Node.Next := Node;
      
      if Container.Last = Position.Node then
         Container.Last := Node;
      end if;

      if Source.Last = Node then
         Source.Last := Pred.Node;
      end if;

      Container.Length := Container.Length + 1;
      Source.Length := Source.Length - 1;
      
      pragma Assert (Succ (Last (Container)) = Back (Container));
      pragma Assert (Succ (Last (Source)) = Back (Source));
               
   end Splice_After;
   
   
--   procedure Splice_After 
--     (Container : in out Container_Type;
--      Position  : in     Node_Access;
--      Source    : in out Container_Type;
--      Front     : in     Node_Access;
--      Last      : in     Node_Access) is
--      
--      First : constant Node_Access := Front.Next;
--      
--   begin
--   
--      Front.Next := Last.Next;
--     
--      Last.Next := Position.Next;
--      
--      Position.Next := First;
--      
--      if Container'Address = Source'Address then
--      
--         if Container.Last = Last then
--            Container.Last := Front;

--         elsif Container.Last = Position then
--            Container.Last := Last;
--            
--         end if;
--         
--         pragma Assert (Succ (Unbounded.Last (Container)) = 
--                        Back (Container));

--      else
--      
--         if Source.Last = Last then
--            Source.Last := Front;
--         end if;

--         if Container.Last = Position then
--            Container.Last := Last;
--         end if;
--         
--         pragma Assert (Succ (Unbounded.Last (Container)) = 
--                        Back (Container));
--
--         pragma Assert (Succ (Unbounded.Last (Source)) = 
--                        Back (Source));
--         
--      end if;
--                        
--   end Splice_After;


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
   
   
   procedure Splice_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Front     : in     Iterator_Type;
      Last      : in     Iterator_Type) is
      
      First : Node_Access;
      
   begin

      if Position = Null_Iterator
        or else Front = Null_Iterator
        or else Last = Null_Iterator
        or else Front = Last 
        or else Position = Front 
      then
         return;
      end if;
      
      pragma Assert (not Is_In (Back (Container), Succ (Front), Succ (Last)));
      
      First := Front.Node.Next;
      
      Front.Node.Next := Last.Node.Next;
     
      Last.Node.Next := Position.Node.Next;
      
      Position.Node.Next := First;
            
      if Container.Last = Last.Node then
         Container.Last := Front.Node;

      elsif Container.Last = Position.Node then
         Container.Last := Last.Node;
         
      end if;
      
      pragma Assert (Succ (Unbounded.Last (Container)) = Back (Container));

   end Splice_After;
   

   
   procedure Splice_After
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Source    : in out Container_Type;
      Front     : in     Iterator_Type;
      Last      : in     Iterator_Type) is
      
      Count : Positive;
      First : Node_Access;

   begin
   
      if Container'Address = Source'Address then
         Splice_After (Container, Position, Front, Last);
         return;
      end if;

      if Position = Null_Iterator
        or else Front = Null_Iterator
        or else Last = Null_Iterator
        or else Front = Last
      then
         return;
      end if;
      
      pragma Assert (Source.Length > 0);
      pragma Assert (not Is_In (Back (Source), Succ (Front), Succ (Last)));
 
      Count := Offset (Front, Last);

      First := Front.Node.Next;
      
      Front.Node.Next := Last.Node.Next;
     
      Last.Node.Next := Position.Node.Next;
      
      Position.Node.Next := First;
      
      if Source.Last = Last.Node then
         Source.Last := Front.Node;
      end if;

      if Container.Last = Position.Node then
         Container.Last := Last.Node;
      end if;
         
      Container.Length := Container.Length + Count;
      Source.Length := Source.Length - Count;
      
      pragma Assert (Succ (Unbounded.Last (Container)) = Back (Container));
      pragma Assert (Succ (Unbounded.Last (Source)) = Back (Source));

   end Splice_After;



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
      return (Node => Container.Last);
   end;
   
   function Last_Element
     (Container : Container_Type) return Element_Type is
     
      Node : constant Node_Access := Container.Last;
   begin
      return Node.Element;
   end;
   

   function Back 
     (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Back);
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
      
      LE : Element_Type renames Left.Node.Element;
      RE : Element_Type renames Right.Node.Element;
      
      LE_Copy : constant Element_Type := LE;
   begin
      LE := RE;
      RE := LE_Copy;
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
     

   procedure Copy_Element
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type) is
   begin
      Item := Iterator.Node.Element;
   end;

      
   procedure Generic_Select_Element 
     (Iterator : Iterator_Type) is
   begin
      Process (Iterator.Node.Element);
   end;
     
   procedure Generic_Modify_Element
     (Iterator : Iterator_Type) is
   begin
      Process (Iterator.Node.Element);
   end;
     
   procedure Generic_Access_Element
     (Iterator : Iterator_Type) is
   begin
      Process (Iterator.Node.Element'Access);
   end;


   procedure Generic_Iteration
     (First, Back : Iterator_Type) is
     
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator);
         Iterator := Succ (Iterator);
      end loop;
   end;
   
     
   procedure Generic_Single_Reverse_Iteration
     (First, Back : Iterator_Type) is
      
      procedure Iterate (Iterator : Iterator_Type) is
      begin
         if Iterator = Back then
            return;
         end if;
         
         Iterate (Succ (Iterator));
         Process (Iterator);
      end;
      
   begin

      Iterate (First);

   end Generic_Single_Reverse_Iteration;


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

      
   procedure Generic_Single_Reverse_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is

      procedure Iterate (Iterator : Iterator_Type) is
      begin
         if Iterator = Back then
            return;
         end if;
         
         Iterate (Succ (Iterator));
         Process (Iterator.Node.Element);
      end;
      
   begin

      Iterate (First);
      
   end Generic_Single_Reverse_Select_Elements;


   procedure Generic_Single_Reverse_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is

      procedure Iterate (Iterator : Iterator_Type) is
      begin
         if Iterator = Back then
            return;
         end if;
         
         Iterate (Succ (Iterator));
         Process (Iterator.Node.Element);
      end;
      
   begin

      Iterate (First);
      
   end Generic_Single_Reverse_Modify_Elements;



   procedure Generic_Single_Reverse_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is

      procedure Iterate (Iterator : Iterator_Type) is
      begin
         if Iterator = Back then
            return;
         end if;
         
         Iterate (Succ (Iterator));
         Process (Iterator.Node.Element'Access);
      end;
      
   begin

      Iterate (First);
      
   end Generic_Single_Reverse_Access_Elements;


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
      

      

   function Generic_Single_Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type) return Iterator_Type is
      
      Result : Iterator_Type := Back;
      
      procedure Iterate (Iterator : Iterator_Type) is
      begin
         if Iterator = Back then
            return;
         end if;
         
         Iterate (Succ (Iterator));
         
         if Result = Back 
            and then Predicate (Iterator.Node.Element)
         then
            Result := Iterator;
         end if;
      end;
      
   begin

      Iterate (First);
      
      return Result;
      
   end Generic_Single_Reverse_Find;      


   function Single_Reverse_Find 
     (First : Iterator_Type;
      Back  : Iterator_Type;
      Item  : Element_Type) return Iterator_Type is
      
      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;
      
      function Reverse_Find is
         new Generic_Single_Reverse_Find (Predicate);
   begin
      return Reverse_Find (First, Back);
   end;

      
   function Single_Reverse_Find 
     (Container : Container_Type;
      Item      : Element_Type) return Iterator_Type is
   begin
      return Single_Reverse_Find (First (Container), Back (Container), Item);
   end;


   function Succ 
     (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Iterator.Node.Next);
   end;


   function Pred 
     (Container : Container_Type;
      Iterator  : Iterator_Type) return Iterator_Type is
   begin
      if Iterator.Node = Container.Back then
         return (Node => Container.Last);
      else
         return (Node => Prev (Container.Back, Iterator.Node));
      end if;
   end;

     
   function Pred 
     (Front    : Iterator_Type;
      Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Prev (Front.Node, Iterator.Node));
   end;
   
     
   procedure Generic_Unique (Container : in out Container_Type) is

      I : Iterator_Type := First (Container);
      J : Iterator_Type;
      
      B : constant Iterator_Type := Back (Container);

   begin

      if Container.Length = 0 then
         return;
      end if;
            
      loop
      
         J := Succ (I);
         
         exit when J = B;
            
         if Predicate (J.Node.Element, I.Node.Element) then
            Delete_After (Container, I);
         else
            I := J;
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
      LJ : Iterator_Type;
      LB : constant Iterator_Type := Back (Container);
      
      RI : Iterator_Type := First (Source);
      --RF : constant Iterator_Type := Front (R);
      RB : constant Iterator_Type := Back (Source);
      
   begin

      if Container'Address = Source'Address then
         return;
      end if;
      
      if RI = RB then
         return;
      end if;

      if LI = LB then
         Splice_After (Container, LB, Source);
         return;
      end if;
      
      LJ := LB;
      
      loop
      
--         Debug ("1L: ", L);
--         Debug ("1R: ", R);
--         
--         Debug ("1xL: ", Last (L), LB);
--         Debug ("1xR: ", Last (R), RB);
      
         if RI.Node.Element < LI.Node.Element then

            RI := Succ (RI);

            Splice_After (Container, LJ, Source, RB);

            if RI = RB then
               return;
            end if;

            LJ := Succ (LJ);

         else
         
            LJ := LI;

            LI := Succ (LI);

            if LI = LB then
            
--               Debug ("3L: ", L);
--               Debug ("3R: ", R);

               Splice_After (Container, LJ, Source);

               return;

            end if;
            
            exit;

         end if;
      
      end loop;

      loop      
         
--         Debug ("2L: ", L);
--         Debug ("2R: ", R);

--         Debug ("2xL: ", Last (L), LB);
--         Debug ("2xR: ", Last (R), RB);

         if RI.Node.Element < LI.Node.Element then

            RI := Succ (RI);

            Splice_After (Container, LJ, Source, RB);
            
            if RI = RB then
               return;
            end if;
      
            LJ := Succ (LJ);

         else

            LJ := LI;

            LI := Succ (LI);
            
            if LI = LB then
            
--               Debug ("4L: ", L);
--               Debug ("4R: ", R);
--               
--               Debug ("5L: ", LJ, LB);
--               Debug ("6L: ", Last (L), LB);
--               
--               Debug ("5R: ", RI, RB);
--               Debug ("6R: ", Last (R), RB);

               Splice_After (Container, LJ, Source);

               return;

            end if;

         end if;

      end loop;      

   end Generic_Merge;


   procedure Generic_Quicksort
     (Container : in out Container_Type) is

      IB : constant Iterator_Type := Unbounded.Back (Container);
      
      procedure Partition 
        (Front : in Node_Access;
         Back  : in Node_Access) is
         
         Pivot_Prev : Node_Access := Front;
         Pivot      : constant Node_Access := Front.Next;

         Node_Prev : Node_Access := Pivot;
         
      begin

--         Partition_Before (Iterator_Type'(Node => Front.Next),
--                           Iterator_Type'(Node => Back));
      
         while Node_Prev.Next /= Back loop
                  
            if Node_Prev.Next.Element < Pivot.Element then

               declare
                  Node : constant Node_Access := Node_Prev.Next;
                  Next : constant Node_Access := Node.Next;
               begin
                  Node_Prev.Next := Next;
                  
                  Node.Next := Pivot;
                  
                  Pivot_Prev.Next := Node;
                  
                  Pivot_Prev := Node;
               end;

            else

               Node_Prev := Node_Prev.Next;

            end if;

--         Partition_After (Iterator_Type'(Node => Front.Next),
--                          Iterator_Type'(Node => Back));

         end loop;
         
         if Back = IB.Node then
            Container.Last := Node_Prev;
         end if;

      end Partition;


      procedure Sort (Front, Back : Node_Access) is
      
         Pivot : constant Node_Access := Front.Next;

      begin
   
         if Pivot /= Back then
         
--            Sort (Iterator_Type'(Node => Pivot),
--                  Iterator_Type'(Node => Back));
                  
            Partition (Front, Back);

            Sort (Front, Pivot);

            Sort (Pivot, Back);

         end if;

      end Sort;   
      
   begin

      Sort (Container.Back, Container.Back);   
      
      pragma Assert (Succ (Last (Container)) = Back (Container));
      
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
   
   procedure Decrement 
     (Container : in     Container_Type;
      Iterator  : in out Iterator_Type) is
   begin
      Iterator := Pred (Container, Iterator);
   end;
   

   procedure Decrement
     (Front    : in     Iterator_Type;
      Iterator : in out Iterator_Type) is
   begin
      Iterator := Pred (Front, Iterator);
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
   

end Charles.Lists.Single.Unbounded;
