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

with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body Charles.Maps.Hashed.Unbounded is

   use Hash_Table_Types.Iterator_Vectors;


   type Node_Type is
      record
         Succ    : Node_Access;
         Pred    : Node_Access;
         Key     : aliased Key_Type;
         Element : aliased Element_Type;
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


   function Hash (Node : Node_Access) return Integer'Base is
   begin
      return Hash (Node.Key);
   end;


         
   procedure Initialize (Container : in out Container_Type) is
      List : List_Type renames Container.Hash_Table.L;
   begin
      List.Back := new Node_Type;
      Initialize (List);
   end;   


   procedure Adjust (List : access List_Type) is
      
      Source_Back   : constant Node_Access := List.Back;
      Source_Length : constant Natural := List.Length;
      
      Source : Node_Access := Succ (Source_Back);

   begin
   
      begin
         List.Back := new Node_Type;
      exception
         when others =>
            List.Back := null;
            List.Length := 0;
            raise;
      end;
      
      Initialize (List.all);
      
      while Source /= Source_Back loop
      
         declare
            Target : constant Node_Access :=
               new Node_Type'(Succ       => List.Back,
                              Pred       => Pred (List.Back),
                              Key        => Source.Key,
                              Element    => Source.Element);
         begin
            Insert (List.all, Before => List.Back, Node => Target);
         end;
         
         Source := Succ (Source);
         
      end loop;
      
      pragma Assert (List.Length = Source_Length);
      
   end Adjust;
   

   procedure Adjust (Container : in out Container_Type) is
   
      HT : Hash_Table_Type renames Container.Hash_Table;
   begin
      Adjust (HT.L'Access);
      Adjust (HT);
   end;


   procedure Finalize (Container : in out Container_Type) is
   begin
      Finalize (Container.Hash_Table.L'Access);
   end;


   function First (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => First (Container.Hash_Table.L));
   end;      


   function First_Key (Container : Container_Type) return Key_Type is
      Node : constant Node_Access := First (Container.Hash_Table.L);
   begin
      return Node.Key;
   end;

   
   function First_Element (Container : Container_Type) return Element_Type is
      Node : constant Node_Access := First (Container.Hash_Table.L);
   begin
      return Node.Element;
   end;
   

   function Last (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Last (Container.Hash_Table.L));
   end;      


   function Last_Key (Container : Container_Type) return Key_Type is
      Node : constant Node_Access := Last (Container.Hash_Table.L);
   begin
      return Node.Key;
   end;

   
   function Last_Element (Container : Container_Type) return Element_Type is
      Node : constant Node_Access := Last (Container.Hash_Table.L);
   begin
      return Node.Element;
   end;
   

   
   function Back (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Back (Container.Hash_Table.L));
   end;
   
   
   function Key (Iterator : Iterator_Type) return Key_Type is
   begin
      return Iterator.Node.Key;
   end;

   
   function Generic_Key (Iterator : Iterator_Type) return Key_Access is
   begin
      return Iterator.Node.Key'Access;
   end;
   

   function Generic_Modify_Key 
     (Iterator : Iterator_Type) return Key_Access is
   begin
      return Iterator.Node.Key'Access;
   end;

   
   function Element (Iterator : Iterator_Type) return Element_Type is
   begin
      return Iterator.Node.Element;
   end;
   

   function Generic_Element (Iterator : Iterator_Type) 
      return Element_Access is
   begin
      return Iterator.Node.Element'Access;
   end;

   
   procedure Replace_Element
     (Iterator : in Iterator_Type;
      By       : in Element_Type) is
   begin
      Iterator.Node.Element := By;
   end;
   
   
   function Succ 
     (Iterator  : Iterator_Type) return Iterator_Type is      
   begin
      return (Node => Succ (Iterator.Node));
   end;
      

   function Succ 
     (Iterator  : Iterator_Type;
      Offset    : Natural) return Iterator_Type is      
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

      
   function Is_Equal_Element (L, R : Node_Access) return Boolean is
      pragma Inline (Is_Equal_Element);
   begin
      return L.Element = R.Element;
   end;
   
   
   function "=" (Left, Right : Container_Type) return Boolean is
      function "=" is new Node_Lists.Generic_Equal (Is_Equal_Element);
   begin
      if Left'Address = Right'Address then
         return True;
      end if;
      
      return Left.Hash_Table.L = Right.Hash_Table.L;
   end;
   

   function Size (Container : Container_Type) return Natural is
   begin
      return Length (Container.Hash_Table.V);
   end;

   
   function Length (Container : Container_Type) return Natural is
   begin
      return Length (Container.Hash_Table.L);
   end;
   

   function Is_Equal_Key 
     (N : Node_Access;
      K : Key_Type) return Boolean is 
      
      pragma Inline (Is_Equal_Key);
   begin
      return Is_Equal_Key (N.Key, K);
   end;
   
   
   package Element_Keys is
      new Hash_Table_Types.Generic_Keys (Key_Type, Hash, Is_Equal_Key);

      
   procedure Insert
     (List   : in out List_Type;
      Before : in     Node_Access;
      Key    : in     Key_Type;
      Node   :    out Node_Access) is
      
      pragma Inline (Insert);
      
      Pred_Before : constant Node_Access := Pred (Before);
      
   begin

      Node := new Node_Type;      
      
      Node.Succ := Before;
      Node.Pred := Pred_Before;
      
      Node.Key := Key;
      
      Insert (List, Before => Before, Node => Node);
      
   end;

   
   package Element_Insertion is 
      new Element_Keys.Generic_Insertion (Insert);

   use Element_Keys, Element_Insertion;
   
   
   function Find 
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Find (Container.Hash_Table, Key));
   end;

   
   function Element 
     (Container : Container_Type;
      Key       : Key_Type) return Element_Type is
      
      Node : constant Node_Access :=
         Find (Container.Hash_Table, Key);
   begin
      return Node.Element;
   end;
   


   function Is_In
     (Key       : Key_Type;
      Container : Container_Type) return Boolean is
   begin
      return Find (Container, Key) /= Back (Container);
   end;


   function Count 
     (Container : Container_Type;
      Key       : Key_Type) return Natural is
   begin
      return Count (Container.Hash_Table, Key);
   end;
   

   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Length (Container) = 0;
   end;
   
   
   procedure Clear (Container : in out Container_Type) is
   begin
      Clear (Container.Hash_Table);
   end;
   
   
   procedure Swap (Left, Right : in out Container_Type) is
   begin
      Swap (Left.Hash_Table, Right.Hash_Table);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is      
      
   begin

      Resize (Container, Length (Container) + 1);   

      Conditional_Insert_Sans_Resize
        (Container.Hash_Table, 
         Key, 
         Iterator.Node, 
         Success);
      
      if Success then
         Iterator.Node.Element := New_Item;
      end if;
      
   end Insert;
   
   
   
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is      
      
   begin
   
      Resize (Container, Length (Container) + 1); 
   
      Conditional_Insert_Sans_Resize 
        (Container.Hash_Table, 
         Key, 
         Iterator.Node, 
         Success);

   end Insert;


   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;      
   begin
      Insert (Container, Key, New_Item, Iterator, Success);
   end;

      
   
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;      
   begin
      Insert (Container, Key, Iterator, Success);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, New_Item, Iterator);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, Iterator);
   end;



   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is      

   begin
   
      if Size (Container) = 0 then
         Resize (Container, 1);
      end if;
      
      Conditional_Insert_Sans_Resize 
        (Container.Hash_Table, 
         Key, 
         Iterator.Node, 
         Success);
      
      if Success then
         Iterator.Node.Element := New_Item;
      end if;
      
   end Insert_Sans_Resize;
   
   
   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert_Sans_Resize (Container, Key, New_Item, Iterator, Success);
   end;

   
   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is   
         
   begin
   
      if Size (Container) = 0 then
         Resize (Container, 1);
      end if;

      Conditional_Insert_Sans_Resize 
        (Container.Hash_Table, 
         Key, 
         Iterator.Node, 
         Success);
         
   end Insert_Sans_Resize;


   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     Key_Type) is   
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert_Sans_Resize (Container, Key, Iterator, Success);
   end;


   procedure Replace_Element
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      New_Item  : in     Element_Type) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert (Container, Key, New_Item, Iterator, Success); 
      
      if not Success then
         Iterator.Node.Element := New_Item;
      end if;
   end;
      


--comment-out for now
--   procedure Replace_Element
--     (Container : in out Container_Type;
--      Position  : in     Iterator_Type;
--      Key       : in     Key_Type;
--      New_Item  : in     Element_Type) is
--      
--      Iterator : Iterator_Type;
--      Success  : Boolean;
--   begin
--      Insert (Container, Position, Key, New_Item, Iterator, Success); 
--      
--      if not Success then
--         Iterator.Node.Element := New_Item;
--      end if;
--   end;
      

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Count     :    out Natural) is
   begin
      Delete (Container.Hash_Table, Key, Count);
   end;
   

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type) is
      
      Count : Natural;
   begin
      Delete (Container, Key, Count);
   end;
         
      
   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
      
      HT : Hash_Table_Type renames Container.Hash_Table;
      
      Node : Node_Access := Iterator.Node;
      
   begin
   
      if Node = null
        or else Node = Back (HT.L)
      then
         return;
      end if;      
      
      Iterator := Succ (Iterator);

      Pre_Delete (HT, Node);
      Delete_And_Free (HT.L, Node);
            
   end Delete;

      
--   procedure Delete_Sans_Increment
--     (Container : in out Container_Type;
--      Iterator  : in out Iterator_Type) is
--   begin
--      if Iterator = Null_Iterator
--        or else Iterator = Back (Container)
--      then
--         return;
--      end if;         

--      Pre_Delete (Container.Hash_Table, Iterator.I);
--      Delete_Sans_Increment (Container.Hash_Table.L, Iterator.I);
--   end;



--   procedure Delete_Sans_Assign
--     (Container : in out Container_Type;
--      Iterator  : in     Iterator_Type) is
--   begin
--      if Iterator = Null_Iterator
--        or else Iterator = Back (Container)
--      then
--         return;
--      end if;         

--      Pre_Delete (Container.Hash_Table, Iterator.I);
--      Delete_Sans_Assign (Container.Hash_Table.L, Iterator.I);
--   end;


   procedure Delete_First (Container : in out Container_Type) is
      Iterator : Iterator_Type := First (Container);
   begin
      Delete (Container, Iterator);
   end;
   
   procedure Delete_Last (Container : in out Container_Type) is
      Iterator : Iterator_Type := Last (Container);
   begin
      Delete (Container, Iterator);
   end;



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
      
      Delete (Container.Hash_Table, First.Node, Back.Node);
   end;
      

--   procedure Delete_Sans_Assign
--     (Container : in out Container_Type;
--      First     : in     Iterator_Type;
--      Back      : in     Iterator_Type) is      
--      
--      I : Node_Lists.Iterator_Type := First.I;
--   begin
--      if I = Node_Lists.Null_Iterator 
--        or else Back = Null_Iterator
--      then
--         return;
--      end if;
--      
--      Delete (Container.Hash_Table, I, Back.I);
--   end;



   procedure Copy_Key
     (Iterator : in     Iterator_Type;
      Key      :    out Key_Type) is
   begin
      Key := Iterator.Node.Key;
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
   begin
      Swap (Left, Right.Node.Element);
   end;
   
            
   procedure Generic_Swap_Element
     (Left, Right : in Iterator_Type) is
   begin
      Swap (Left.Node.Element, Right.Node.Element);
   end;
      

   procedure Swap_Iterator (Left, Right : in out Iterator_Type) is
      LI : constant Iterator_Type := Left;
   begin
      Left := Right;
      Right := LI;
   end;


   procedure Generic_Select_Key
     (Iterator : in Iterator_Type) is
   begin
      Process (Iterator.Node.Key);
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
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := First;
   begin
      while I /= Back loop   
         Process (I);
         I := Succ (I);
      end loop;
   end;
      

   procedure Generic_Reverse_Iteration
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := Back;
   begin
      while I /= First loop   
         I := Pred (I);
         Process (I);
      end loop;
   end;
      

   procedure Generic_Select_Keys
     (First, Back : in Iterator_Type) is
      
      I : Iterator_Type := First;
   begin
      while I /= Back loop
         Process (I.Node.Key);
         I := Succ (I);
      end loop;
   end;
   
   procedure Generic_Reverse_Select_Keys
     (First, Back : in Iterator_Type) is
      
      I : Iterator_Type := Back;
   begin
      while I /= First loop
         I := Pred (I);
         Process (I.Node.Key);
      end loop;
   end;
      
          
   procedure Generic_Select_Elements
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := First;
   begin
      while I /= Back loop
         Process (I.Node.Element);
         I := Succ (I);
      end loop;
   end;

   procedure Generic_Reverse_Select_Elements
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := Back;
   begin
      while I /= First loop
         I := Pred (I);
         Process (I.Node.Element);
      end loop;
   end;

     
   procedure Generic_Modify_Elements
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := First;
   begin
      while I /= Back loop
         Process (I.Node.Element);
         I := Succ (I);
      end loop;
   end;
   

   procedure Generic_Reverse_Modify_Elements
     (First, Back : in Iterator_Type) is
     
      I : Iterator_Type := Back;
   begin
      while I /= First loop
         I := Pred (I);
         Process (I.Node.Element);
      end loop;
   end;


   procedure Generic_Access_Elements
     (First, Back : in Iterator_Type) is
           
      I : Iterator_Type := First;
   begin
      while I /= Back loop
         Process (I.Node.Element'Access);
         I := Succ (I);
      end loop;
   end;


   procedure Generic_Reverse_Access_Elements
     (First, Back : in Iterator_Type) is
           
      I : Iterator_Type := Back;
   begin
      while I /= First loop
         I := Pred (I);
         Process (I.Node.Element'Access);
      end loop;
   end;



   procedure Resize
     (Container : in out Container_Type;
      Length    : in     Natural) is      
   begin
      Resize (Container.Hash_Table, Length);   
   end;


   function Lower_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Lower_Bound (Container.Hash_Table, Key));
   end;
   
   function Upper_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Upper_Bound (Container.Hash_Table, Key));
   end;
   
   procedure Equal_Range
     (Container : in     Container_Type;
      Key       : in     Key_Type;
      First     :    out Iterator_Type;
      Back      :    out Iterator_Type) is
   begin
      Equal_Range (Container.Hash_Table, Key, First.Node, Back.Node);
   end;


   function Is_Equal (Left, Right : Iterator_Type) return Boolean is
   begin
      return Is_Equal_Element (Left.Node, Right.Node);
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


end Charles.Maps.Hashed.Unbounded;
