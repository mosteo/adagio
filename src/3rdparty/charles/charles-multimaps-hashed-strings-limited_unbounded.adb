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

package body Charles.Multimaps.Hashed.Strings.Limited_Unbounded is

   use Hash_Table_Types.Iterator_Vectors;

   type Node_Type (Key_Length : Natural) is
      limited record
         Succ    : Node_Access;
         Pred    : Node_Access;
         Key     : String (1 .. Key_Length);
         Element : aliased Element_Type;
      end record;
      

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

         
   procedure Initialize (Container : in out Container_Type) is
      List : List_Type renames Container.Hash_Table.L;
   begin
      List.Back := new Node_Type (Key_Length => 0);
      Initialize (List);
   end;   

   procedure Finalize (Container : in out Container_Type) is
   begin
      Finalize (Container.Hash_Table.L'Access);
   end;


   function Hash (Node : Node_Access) return Integer'Base is
   begin
      return Hash (Node.Key);
   end;

   function First (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => First (Container.Hash_Table.L));
   end;      
   
   function First_Key (Container : Container_Type) return String is
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
   
   function Last_Key (Container : Container_Type) return String is
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
   
   
--   function Key (Iterator : Iterator_Type) return Key_Type is
--   begin
--      return To_Access (Iterator.I).Key;
--   end;
   
   function Key (Iterator : Iterator_Type) return String is
   begin
      return Iterator.Node.Key;
   end;
   
   function Key_Length (Iterator : Iterator_Type) return Natural is
   begin
      return Iterator.Node.Key'Length;
   end;
   
--   function Generic_Key 
--      (Iterator : Iterator_Type) return Key_Access is
--   begin
--      return To_Access (Iterator.I).Key'Access;
--   end;
--   
--   function Generic_Modify_Key 
--     (Iterator : Iterator_Type) return Key_Access is
--   begin
--      return To_Access (Iterator.I).Key'Access;
--   end;
   
   
   function Element (Iterator : Iterator_Type) return Element_Type is
   begin
      return Iterator.Node.Element;
   end;
   
   function Generic_Element (Iterator : Iterator_Type) 
      return Element_Access is
   begin
      return Iterator.Node.Element'Access;
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
      

   function Generic_Equal (Left, Right : Container_Type) return Boolean is
      function Is_Equal (L, R : Node_Access) return Boolean is
      begin
         return L.Element = R.Element;
      end;
      
      function "=" is new Node_Lists.Generic_Equal (Is_Equal);
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
      K : String) return Boolean is
      
      pragma Inline (Is_Equal_Key);
   begin
      return Is_Equal_Key (N.Key, K);
   end;

        
   package Keys is
      new Hash_Table_Types.Generic_Keys (String, Hash, Is_Equal_Key);   


   procedure Insert
     (List   : in out List_Type;
      Before : in     Node_Access;
      Key    : in     String;
      Node   :    out Node_Access) is
      
      pragma Inline (Insert);
      
      Pred_Before : constant Node_Access := Pred (Before);
      
   begin

      Node := new Node_Type (Key_Length => Key'Length);      
      
      Node.Succ := Before;
      Node.Pred := Pred_Before;
      
      Node.Key := Key;
      
      Insert (List, Before => Before, Node => Node);
      
   end Insert;

      
   package Key_Insertion is
      new Keys.Generic_Insertion (Insert);

   use Keys, Key_Insertion;


   function Find
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Find (Container.Hash_Table, Key));
   end;   
   
   function Is_In
     (Key       : String;
      Container : Container_Type) return Boolean is
      
      Iterator : constant Iterator_Type := Find (Container, Key);
   begin
      return Iterator /= Back (Container);
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
      Key       : in     String;
      Iterator  :    out Iterator_Type) is
   begin
      Resize (Container, Length (Container) + 1);

      Unconditional_Insert_Sans_Resize 
        (Container.Hash_Table, 
         Key, 
         Iterator.Node);
   end;
      
   
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, Iterator);
   end;



   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type) is
      
   begin

      if Size (Container) = 0 then      
         Resize (Container, 1);
      end if;

      Unconditional_Insert_Sans_Resize 
        (Container.Hash_Table, 
         Key, 
         Iterator.Node);
            
   end Insert_Sans_Resize;


   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     String) is
      
      Iterator : Iterator_Type;
   begin
      Insert_Sans_Resize (Container, Key, Iterator);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String;
      Count     :    out Natural) is
   begin
      Delete (Container.Hash_Table, Key, Count);
   end;
   

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String) is
      
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

      
--   procedure Delete_and_Increment
--     (Container : in out Container_Type;
--      Iterator  : in out Iterator_Type) is
--   begin
--      Delete_Node_And_Increment (Container.Buckets.all, Iterator.Node);
--      Container.Length := Container.Length - 1;
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
      

   function Count
     (Container : Container_Type;
      Key       : String) return Natural is
   begin
      return Count (Container.Hash_Table, Key);
   end;


      
   function Lower_Bound
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Lower_Bound (Container.Hash_Table, Key));
   end;
   
   function Upper_Bound
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Upper_Bound (Container.Hash_Table, Key));
   end;
   
   procedure Equal_Range
     (Container : in     Container_Type;
      Key       : in     String;
      First     :    out Iterator_Type;
      Back      :    out Iterator_Type) is
   begin
      Equal_Range (Container.Hash_Table, Key, First.Node, Back.Node);
   end;



   procedure Resize
     (Container : in out Container_Type;
      Length    : in     Natural) is
   begin
      Resize (Container.Hash_Table, Length);
   end;


   procedure Copy_Key
     (Iterator : in     Iterator_Type;
      Key      :    out String;
      Last     :    out Integer) is      
   begin
      Last := Key'First + Iterator.Node.Key'Length - 1;
      Key (Key'First .. Last) := Iterator.Node.Key;
   end;


   procedure Generic_Swap
     (Iterator : in     Iterator_Type;
      Item     : in out Element_Type) is
   begin
      Swap (Iterator.Node.Element, Item);
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
      
      I : Node_Access := First.Node;
      J : constant Node_Access := Back.Node;
   begin
      while I /= J loop
         Process (I.Key);
         I := Succ (I);
      end loop;
   end;
          

   procedure Generic_Reverse_Select_Keys
     (First, Back : in Iterator_Type) is
      
      I : Node_Access := Back.Node;
      J : constant Node_Access := First.Node;
   begin
      while I /= J loop
         I := Pred (I);
         Process (I.Key);
      end loop;
   end;
          

   procedure Generic_Select_Elements
     (First, Back : in Iterator_Type) is
      
      I : Node_Access := First.Node;
      J : constant Node_Access := Back.Node;
   begin
      while I /= J loop
         Process (I.Element);
         I := Succ (I);
      end loop;
   end;

   procedure Generic_Reverse_Select_Elements
     (First, Back : in Iterator_Type) is
      
      I : Node_Access := Back.Node;
      J : constant Node_Access := First.Node;
   begin
      while I /= J loop
         I := Pred (I);
         Process (I.Element);
      end loop;
   end;

     
     
   procedure Generic_Modify_Elements
     (First, Back : in Iterator_Type) is
     
      I : Node_Access := First.Node;
      J : constant Node_Access := Back.Node;
   begin
      while I /= J loop
         Process (I.Element);
         I := Succ (I);
      end loop;
   end;


   procedure Generic_Reverse_Modify_Elements
     (First, Back : in Iterator_Type) is
     
      I : Node_Access := Back.Node;
      J : constant Node_Access := First.Node;
   begin
      while I /= J loop
         I := Pred (I);
         Process (I.Element);
      end loop;
   end;


   procedure Generic_Access_Elements
     (First, Back : in Iterator_Type) is
     
      I : Node_Access := First.Node;
      J : constant Node_Access := Back.Node;
   begin
      while I /= J loop
         Process (I.Element'Access);
         I := Succ (I);
      end loop;
   end;


   procedure Generic_Reverse_Access_Elements
     (First, Back : in Iterator_Type) is
     
      I : Node_Access := Back.Node;
      J : constant Node_Access := First.Node;
   begin
      while I /= J loop
         I := Pred (I);
         Process (I.Element'Access);
      end loop;
   end;


end Charles.Multimaps.Hashed.Strings.Limited_Unbounded;

