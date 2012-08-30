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

with Charles.Algorithms.Generic_Lexicographical_Compare;
with Charles.Algorithms.Generic_Compare;

package body Charles.Double_Lists is


   procedure Free_List (Head : in out Node_Access) is
   
      Node : Node_Access;
      
   begin
   
      while Head /= Null_Node loop
               
         Node := Head;
         
         Head := Succ (Head);
         
         Free (Node);
                  
      end loop;
      
   end Free_List;   
      

   procedure Initialize (List : in out List_Type) is
   
      Back : Node_Access renames List.Back;
      pragma Assert (Back /= Null_Node);
      
   begin
         
      Set_Succ (Node => Back, Succ => Back);
      Set_Pred (Node => Back, Pred => Back);
      
      List.Length := 0;
      
   end;
   
   
--   procedure Do_Finalize (Back : in out Node_Access) is
--      First : Node_Access := Get_Next (Back);
--   begin
--      while First /= Back loop
--         declare
--            X : Node_Access := First;
--         begin
--            First := Get_Next (First);
--            Free (X);
--         end;
--      end loop;
--            
--      Free (Back);
--   end Do_Finalize;
   
   
--   procedure Adjust (List : in out List_Type) is
--   
--      Source_Back : constant Node_Access := List.Back;      
--   begin   
--      List.Back := null;
--      
--      declare
--         Iter : Node_Access := Get_Next (Source_Back);
--         Back_Iter : constant Node_Access := Source_Back;
--         
--         Back : Node_Access := New_Node;
--         Last : Node_Access;
--      begin
--         Set_Next (Node => Back, Next => Back);
--         Set_Prev (Node => Back, Prev => Back);
--         
--         Last := Back;
--      
--         while Iter /= Back_Iter loop
--         
--            declare
--               Node : constant Node_Access := 
--                  New_Node (Curr => Iter, 
--                            Next => Back,
--                            Prev => Last);
--            begin
--               Set_Next (Last, Next => Node);
--            end;

--            Last := Get_Next (Last);
--            Iter := Get_Next (Iter);

--         end loop;

--         List.Back := Back;
--      exception
--         when others =>
--            Do_Finalize (Back);         
--            raise;
--      end;               
--   end Adjust;


   procedure Finalize (List : access List_Type) is
   begin
   
      if List.Back = Null_Node then
         return;
      end if;

      Set_Succ (Node => Last (List.all), Succ => Null_Node);

      List.Length := 0;

      declare
         Head : Node_Access := List.Back;
      begin
         List.Back := Null_Node;
         Free_List (Head);
      end;
      
   end Finalize;
      
   
   function Generic_Equal (Left, Right : List_Type) return Boolean is
   
      function Compare is
         new Algorithms.Generic_Compare 
               (Node_Access, 
                Succ, 
                Is_Equal);
               
   begin
      
      if Left.Length /= Right.Length then
         return False;
      end if;
      
      return Compare (First (Left), Back (Left), First (Right));
      
   end Generic_Equal;

   
   function Generic_Less
     (Left, Right : List_Type) return Boolean is

      function Lexicographical_Compare is
         new Algorithms.Generic_Lexicographical_Compare 
               (Node_Access, 
                Succ, 
                Is_Less);
               
   begin
      
      if Left.Length > Right.Length then
         return False;
      end if;
         
      return Lexicographical_Compare 
               (First (Left), Back (Left), 
                First (Right), Back (Right));   

   end Generic_Less;



   function Length (List : List_Type) return Natural is
   begin
      return List.Length;
   end;
   

   procedure Clear (List : in out List_Type) is
      Head : Node_Access := First (List);
   begin
      if List.Length = 0 then
         pragma Assert (Head = List.Back);
         pragma Assert (Last (List) = List.Back);         
         return;
      end if;

      pragma Assert (Head /= List.Back);
      pragma Assert (Last (List) /= List.Back);
      
      Set_Succ (Node => Last (List), Succ => Null_Node);
      
      Initialize (List);
      
      Free_List (Head);
   end;

   
   procedure Swap (Left, Right : in out List_Type) is
   
      L_Back : constant Node_Access := Left.Back;
      L_Length : constant Natural := Left.Length;

   begin

      Left.Back := Right.Back;
      Left.Length := Right.Length;
      
      Right.Back := L_Back;
      Right.Length := L_Length;

   end Swap;
         
   
   
   
--   function To_List (Length : Natural) return List_Type is
--   
--      Back : Node_Access := new Node_Type;
--      
--   begin
--      
--      Back.Next := Back;
--      Back.Prev := Back;
--   
--      for I in 1 .. Length loop
--      
--         declare
--            Node : constant Node_Access := new Node_Type;
--            First : constant Node_Access := Back.Next;
--         begin
--            Node.Next := First;
--            Node.Prev := Back;
--            
--            Back.Next := Node;      
--            First.Prev := Node;
--         end;
--         
--      end loop;

--      return (Ada.Finalization.Controlled with Back, Length);
--      
--   exception
--      when others =>
--         
--         Do_Finalize (Back);
--         raise;
--               
--   end To_List;   
--      
--      
--   function To_List 
--     (Length : Natural;
--      Item   : Element_Type) return List_Type is
--   
--      Back : Node_Access := new Node_Type;
--      
--   begin
--      
--      Back.Next := Back;
--      Back.Prev := Back;
--   
--      for I in 1 .. Length loop
--      
--         declare
--            First : constant Node_Access := Back.Next;

--            Node : constant Node_Access := 
--               new Node_Type'(Element => Item,
--                              Next    => First,
--                              Prev    => Back);
--         begin
--            Back.Next := Node;      
--            First.Prev := Node;
--         end;
--         
--      end loop;

--      return (Ada.Finalization.Controlled with Back, Length);
--      
--   exception
--      when others =>
--         
--         Do_Finalize (Back);
--         raise;
--               
--   end To_List;   
--      
--      
--   function To_List 
--     (First  : Iterator_Type;
--      Back   : Iterator_Type) return List_Type is

--      Target_Back : Node_Access := new Node_Type;
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
--               new Node_Type'(Element => I.Node.Element,
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
--   end To_List;   


--   procedure Assign
--     (Target : in out List_Type;
--      Length : in     Natural) is
--      
--      Source : List_Type := To_List (Length);
--   begin
--      Swap (Target, Source);
--   end;
--            

--   procedure Assign
--     (Target : in out List_Type;
--      Length : in     Natural;
--      Item   : in     Element_Type) is
--      
--      Source : List_Type := To_List (Length, Item);
--   begin
--      Swap (Target, Source);
--   end;
--            

--   
--   procedure Assign
--     (Target : in out List_Type;
--      Source : in     List_Type) is
--   begin
--      if Target'Address = Source'Address then
--         return;
--      end if;
--      
--      declare
--         C : List_Type := Source;
--      begin
--         Swap (Target, C);
--      end;
--   end Assign;


--   procedure Assign
--     (Target : in out List_Type;
--      Source : in     List_Type;
--      First  : in     Iterator_Type;
--      Back   : in     Iterator_Type) is
--      
--      pragma Assert (Target'Address /= Source'Address);

--      C : List_Type := To_List (Source, First, Back);
--   begin
--      Swap (Target, C);
--   end;
   
   
   procedure Insert
     (List   : in out List_Type;
      Before : in     Node_Access;
      Node   : in     Node_Access) is
      
      pragma Assert (Succ (Node) = Before);
      pragma Assert (Pred (Node) = Pred (Before));
   begin      
--      Set_Succ (Node => Node, Succ => Before);
--      Set_Pred (Node => Node, Pred => Pred (Before));

      Set_Succ (Node => Pred (Before), Succ => Node);      
      Set_Pred (Node => Before, Pred => Node); 

      List.Length := List.Length + 1;
   end;


--   procedure Push_Front 
--     (List : in out List_Type) is
--      
--      Before   : Iterator_Type := First (List);
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Node);
--   end;


--   procedure Push_Front 
--     (List : in out List_Type;
--      New_Item  : in     Element_Type) is
--      
--      Before   : Iterator_Type := First (List);
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Item, New_Node);
--   end;

--      
--     
--   procedure Pop_Front (List : in out List_Type) is
--   
--      Iter : Iterator_Type := First (List);
--   begin
--      Delete (List, Iter.Node);
--   end;
--   
--      
--   procedure Push_Back
--     (List : in out List_Type) is
--      
--      Before   : Iterator_Type := Back (List);
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Node);
--   end;
--      
--      

--   procedure Push_Back
--     (List : in out List_Type;
--      New_Item  : in     Element_Type) is
--      
--      Before   : Iterator_Type := Back (List);
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Item, New_Node);
--   end;
--      
--      
--   procedure Pop_Back (List : in out List_Type) is
--   
--      Iter : Iterator_Type := Last (List);
--   begin
--      Delete (List, Iter.Node);
--   end;

--   
--   
--   procedure Insert
--     (List : in out List_Type;
--      Before    : in     Iterator_Type) is
--   
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Node);
--   end;



--   procedure Insert
--     (List : in out List_Type;
--      Before    : in     Iterator_Type;
--      New_Item  : in     Element_Type) is
--   
--      New_Node : Node_Access;
--   begin
--      Insert (List, Before.Node, New_Item, New_Node);
--   end;


--   procedure Insert
--     (List : in out List_Type;
--      Before    : in     Iterator_Type;
--      New_Item  : in     Element_Type;
--      Iterator  :    out Iterator_Type) is
--   begin
--      Insert (List, Before.Node, New_Item, Iterator.Node);
--   end;


--   procedure Insert
--     (List : in out List_Type;
--      Before    : in     Iterator_Type;
--      Iterator  :    out Iterator_Type) is
--   begin
--      Insert (List, Before.Node, Iterator.Node);
--   end;


--   procedure Insert_Range
--     (List   : in out List_Type;
--      Before : in     Node_Access;
--      Length : in     Natural;
--      Node   :    out Node_Access) is
--      
--   begin
--   
--      if Length = 0 then
--         Node := Before;
--         return;
--      end if;
--      
--      declare
--         New_List : List_Type := To_List (Length);
--      begin
--         Node := First (New_List);
--         Splice (List, Before, Source => New_List);
--      end;
--      
--   end Insert_Range;
   
   

   procedure Delete_And_Free
     (List : in out List_Type;
      Node : in out Node_Access) is
   begin
      if Node = Null_Node 
        or else Node = List.Back
      then
         return;
      end if;
        
      List.Length := List.Length - 1;

      Set_Pred (Node => Succ (Node), Pred => Pred (Node));
      Set_Succ (Node => Pred (Node), Succ => Succ (Node));

      Free (Node);
   end;
       
         
   procedure Delete_And_Increment
     (List : in out List_Type;
      Node : in out Node_Access) is
   begin
      if Node = Null_Node 
        or else Node = List.Back
      then
         return;
      end if;
        
      List.Length := List.Length - 1;
      
      declare
         Curr : Node_Access := Node;
         Pred : constant Node_Access := Double_Lists.Pred (Curr);
      begin
         Node := Succ (Node);

         Set_Pred (Node => Node, Pred => Pred);
         Set_Succ (Node => Pred, Succ => Node);

         Free (Curr);
      end;
   end Delete_And_Increment;
       

   procedure Delete
     (List  : in out List_Type; 
      First : in out Node_Access;
      Back  : in     Node_Access) is      

      Prev : Node_Access;      
      Curr : Node_Access;
      
   begin
   
      if First = Null_Node
        or else Back = Null_Node
      then
         return;
      end if;

      if First = Double_Lists.First (List)
        and then Back = List.Back
      then
         Clear (List);
         return;
      end if;

      Prev := Pred (First);

      while First /= Back 
        and then First /= List.Back
      loop
         Curr := First;
         
         First := Succ (First);

         List.Length := List.Length - 1;

         Set_Pred (Node => First, Pred => Prev);
         Set_Succ (Node => Prev, Succ => First);

         Free (Curr);
      end loop;
                        
   end Delete;
   
         


   procedure Generic_Delete 
     (List  : in out List_Type;
      First : in out Node_Access;
      Back  : in     Node_Access;
      Count :    out Natural) is 
           
   begin
   
      if First = Null_Node
        or else Back = Null_Node
      then
         return;
      end if;
      
      Count := 0;
      
      while First /= Back 
        and then First /= List.Back
      loop
         if Predicate (First) then
            Delete_And_Increment (List, First);
            Count := Count + 1;
         else
            First := Succ (First);
         end if;
      end loop;      
      
   end Generic_Delete;


   
   procedure Reverse_List 
     (First : in     Node_Access;
      Back  : in     Node_Access) is

      procedure Swap (L, R : Node_Access) is
      
         LN : constant Node_Access := Succ (L);
         LP : constant Node_Access := Pred (L);
         
         RN : constant Node_Access := Succ (R);
         RP : constant Node_Access := Pred (R);
         
      begin

         Set_Succ (Node => LP, Succ => R);
         Set_Pred (Node => RN, Pred => L);

         Set_Succ (Node => L, Succ => RN);
         Set_Pred (Node => R, Pred => LP);
         
         if LN = R then
         
            pragma Assert (RP = L);
            
            Set_Pred (Node => L, Pred => R);            
            Set_Succ (Node => R, Succ => L);
            
         else
         
            Set_Pred (Node => L, Pred => RP);
            Set_Succ (Node => RP, Succ => L);
            
            Set_Succ (Node => R, Succ => LN);
            Set_Pred (Node => LN, Pred => R);
            
         end if;

      end Swap;

      I : Node_Access := First;
      J : Node_Access := Back;

   begin
   
      if I = Null_Node
        or else J = Null_Node
      then
         return;
      end if;
   
      while I /= J loop
      
         J := Pred (J);
         
         exit when I = J;
         
         Swap (I, J);
         
         J := Succ (J);
         
         exit when I = J;
         
         I := Pred (I);
         
         exit when I = J;
         
         Swap (J, I);
         
         I := Succ (I);
                    
      end loop;
      
   end Reverse_List;



   function First 
     (List : List_Type) return Node_Access is
   begin
      return Succ (List.Back);
   end;

   
   function Back 
     (List : List_Type) return Node_Access is
   begin
      return List.Back;
   end;
      

   function Last 
     (List : List_Type) return Node_Access is
   begin
      return Pred (List.Back);
   end;


   function Succ 
     (Node   : Node_Access;
      Offset : Natural) return Node_Access is
      
      Result : Node_Access := Node;
   begin
      for I in 1 .. Offset loop
         Result := Succ (Result);
      end loop;
      
      return Result;
   end;


   function Pred
     (Node   : Node_Access;
      Offset : Natural) return Node_Access is
      
      Result : Node_Access := Node;
   begin
      for I in 1 .. Offset loop
         Result := Pred (Result);
      end loop;
      
      return Result;
   end;


   function Offset
     (From, To : Node_Access) return Natural is
     
      Result : Integer'Base := 0;        
      Node   : Node_Access := From;
   begin
      while Node /= To loop
         Result := Result + 1;
         Node := Succ (Node);
      end loop;
      
      return Result;
   end;


         
   
   procedure Swap_Iterator (Left, Right : in out Node_Access) is
      LN : constant Node_Access := Left;
   begin
      Left := Right;
      Right := LN;
   end;


--   function Generic_Find 
--     (First : Node_Access;
--      Back  : Node_Access) return Node_Access is
--      
--      Iter : Node_Access := First;
--   begin
--      while Iter /= Back loop
--         if Predicate (Iter.all) then
--            return Iter;
--         end if;
--         
--         Iter := Get_Next (Iter);
--      end loop;
--      
--      return Back;
--   end;
      

--   function Find 
--     (First : Iterator_Type;
--      Back  : Iterator_Type;
--      Item  : Element_Type) return Iterator_Type is
--      
--      function Predicate (Element : Element_Type) return Boolean is
--      begin
--         return Item = Element;
--      end;
--      
--      function Find is 
--         new Generic_Find (Predicate);
--   begin
--      return Find (First, Back);
--   end;

--      
--   function Find 
--     (List : List_Type;
--      Item      : Element_Type) return Iterator_Type is
--   begin
--      return Find (First (List), Back (List), Item);
--   end;


--   function Generic_Reverse_Find 
--     (First : Node_Access;
--      Back  : Node_Access) return Node_Access is
--      
--      Iter : Node_Access := Back;
--   begin
--      while Iter /= First loop
--         Iter := Get_Prev (Iter);
--         
--         if Predicate (Iter.all) then
--            return Iter;
--         end if;
--      end loop;
--      
--      return Back;
--   end;


--   function Reverse_Find 
--     (First : Iterator_Type;
--      Back  : Iterator_Type;
--      Item  : Element_Type) return Iterator_Type is
--   
--      function Predicate (Element : Element_Type) return Boolean is
--      begin
--         return Item = Element;
--      end;
--      
--      function Reverse_Find is
--         new Generic_Reverse_Find (Predicate);
--   begin
--      return Reverse_Find (First, Back);
--   end;


--   function Reverse_Find 
--     (List : List_Type;
--      Item      : Element_Type) return Iterator_Type is
--   begin
--      return Reverse_Find (First (List), Back (List), Item);
--   end;


   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type) is
      
   begin
   
      if Before = Null_Node then --?
         return;
      end if;
         
      if Source.Length = 0 then
         return;
      end if;

      Set_Succ (Node => Pred (Before), Succ => First (Source));
      Set_Pred (Node => First (Source), Pred => Pred (Before));
      
      Set_Pred (Node => Before, Pred => Last (Source));
      Set_Succ (Node => Last (Source), Succ => Before);
      
      Set_Succ (Node => Source.Back, Succ => Source.Back);
      Set_Pred (Node => Source.Back, Pred => Source.Back);
      
      List.Length := List.Length + Source.Length;
      Source.Length := 0;

   end Splice;


   procedure Splice
     (List   : in List_Type;
      Before : in Node_Access;
      Node   : in Node_Access) is
      
   begin
   
      if Before = Null_Node
        or else Node = Null_Node
        or else Node = List.Back
        or else Node = Before
        or else Succ (Node) = Before
      then
         return;
      end if;
   
      pragma Assert (List.Length > 0);
      
      Set_Succ (Node => Pred (Node), Succ => Succ (Node));
      Set_Pred (Node => Succ (Node), Pred => Pred (Node));
   
      Set_Succ (Node => Pred (Before), Succ => Node);
      Set_Pred (Node => Node, Pred => Pred (Before));
      
      Set_Pred (Node => Before, Pred => Node);
      Set_Succ (Node => Node, Succ => Before);
      
   end Splice;


   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type;
      Node   : in     Node_Access) is    

   begin
   
      if Before = Null_Node
         or else Node = Null_Node
         or else Node = Source.Back
      then
         return;
      end if;
      
      pragma Assert (Source.Length > 0);
   
      Set_Succ (Node => Pred (Node), Succ => Succ (Node));
      Set_Pred (Node => Succ (Node), Pred => Pred (Node));
   
      Set_Succ (Node => Pred (Before), Succ => Node);
      Set_Pred (Node => Node, Pred => Pred (Before));
      
      Set_Pred (Node => Before, Pred => Node);
      Set_Succ (Node => Node, Succ => Before);
      
      List.Length := List.Length + 1;
      Source.Length := Source.Length - 1;
      
   end Splice;


   procedure Splice
     (List   : in List_Type;
      Before : in Node_Access;
      First  : in Node_Access;
      Back   : in Node_Access) is

      Last  : Node_Access;
      
   begin

      if Before = Null_Node
        or else First = Null_Node
        or else Back = Null_Node
        or else Before = Back
        or else First = Back
      then
         return;
      end if;
      
      pragma Assert (List.Length > 0);
            
      Last := Pred (Back);
      
      Set_Succ (Node => Pred (First), Succ => Back);
      Set_Pred (Node => Back, Pred => Pred (First));
      
      Set_Pred (Node => First, Pred => Pred (Before));
      Set_Succ (Node => Pred (Before), Succ => First);
      
      Set_Succ (Node => Last, Succ => Before);
      Set_Pred (Node => Before, Pred => Last);
      
   end Splice;



   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type;
      First  : in     Node_Access;
      Back   : in     Node_Access) is

      Count : Positive;
      Last  : Node_Access;
      
   begin
   
      if Before = Null_Node
        or else First = Null_Node
        or else Back = Null_Node
        or else First = Back
      then
         return;
      end if;

      if First = Double_Lists.First (Source)
        and then Back = Source.Back
      then
         Count := Source.Length;
      else
         Count := Offset (First, Back);
      end if;
      
      Last := Pred (Back);
      
      Set_Succ (Node => Pred (First), Succ => Back);
      Set_Pred (Node => Back, Pred => Pred (First));
      
      Set_Pred (Node => First, Pred => Pred (Before));
      Set_Succ (Node => Pred (Before), Succ => First);
      
      Set_Succ (Node => Last, Succ => Before);
      Set_Pred (Node => Before, Pred => Last);
      
      List.Length := List.Length + Count;
      Source.Length := Source.Length - Count;
      
   end Splice;
     

   procedure Generic_Unique 
     (List  : in out List_Type;
      First : in     Node_Access;
      Back  : in     Node_Access) is

      I : Node_Access := First;
      J : Node_Access;
      
   begin

      if First = Null_Node
        or else Back = Null_Node
        or else First = Back
      then
         return;
      end if;
      
      J := Succ (I);
      
      while J /= Back loop
            
         if Predicate (J, I) then

            Delete_And_Increment (List, J);

         else

            I := J;
            J := Succ (I);

         end if;

      end loop;
      
   end Generic_Unique;


   procedure Generic_Merge 
     (List   : in out List_Type;
      Source : in out List_Type;
      First  : in     Node_Access;
      Back   : in     Node_Access) is
   
      LI : Node_Access := Double_Lists.First (List);      
      RI : Node_Access := First;

   begin
   
      if First = Null_Node 
        or else Back = Null_Node
      then
         return;
      end if;

      while RI /= Back loop
      
         if LI = List.Back then
            Splice (List, List.Back, Source, RI, Back);
            return;
         end if;
         
         if Is_Less (RI, LI) then
            declare
               RJ : constant Node_Access := RI;
            begin
               RI := Succ (RI);
               Splice (List, LI, Source, RJ);
            end;            
         else
            LI := Succ (LI);
         end if;

      end loop;      
      
   end Generic_Merge;


--   procedure Generic_Merge 
--     (List   : in out List_Type;
--      Source : in out List_Type) is
--   
--      LI : Node_Access := First (List);      
--      RI : Node_Access := First (Source);

--   begin

--      while RI /= Source.Back loop
--      
--         if LI = List.Back then
--            Splice (List, List.Back, Source);
--            return;
--         end if;
--         
--         if Is_Less (RI, LI) then

--            declare
--               RJ : constant Node_Access := RI;
--            begin
--               RI := Succ (RI);
--               Splice (List, LI, Source, RJ);
--            end;
--            
--         else

--            LI := Succ (LI);
--            
--         end if;

--      end loop;      
--      
--   end Generic_Merge;
   


   procedure Generic_Quicksort (First, Back : Node_Access) is

      procedure Partition 
        (Pivot : in Node_Access;
         Back  : in Node_Access) is

         Node : Node_Access := Succ (Pivot);
         
      begin
      
         while Node /= Back loop

            if Is_Less (Node, Pivot) then

               declare
                  Prev : constant Node_Access := Pred (Node);
                  Next : constant Node_Access := Succ (Node);
               begin
                  Set_Succ (Node => Prev, Succ => Next);
                  Set_Pred (Node => Next, Pred => Prev);
                  
                  Set_Succ (Node => Node, Succ => Pivot);
                  Set_Pred (Node => Node, Pred => Pred (Pivot));
                  
                  Set_Pred (Node => Pivot, Pred => Node);
                  Set_Succ (Node => Pred (Node), Succ => Node);

                  Node := Next;               
               end;

            else

               Node := Succ (Node);

            end if;

         end loop;      

      end Partition;


      procedure Sort (Front, Back : Node_Access) is

         Pivot : constant Node_Access := Succ (Front);

      begin

         if Pivot /= Back then

            Partition (Pivot, Back);

            Sort (Front, Pivot);

            Sort (Pivot, Back);

         end if;

      end Sort;   
      
   begin

      if First = Null_Node
         or else Back = Null_Node
      then
         return;
      end if;

      Sort (Front => Pred (First), Back => Back);   
      
   end Generic_Quicksort;


end Charles.Double_Lists;

