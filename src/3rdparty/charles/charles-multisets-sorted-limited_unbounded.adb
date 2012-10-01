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

with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body Charles.Multisets.Sorted.Limited_Unbounded is

   type Node_Type is
      limited record
         Parent  : Node_Access;
         Left    : Node_Access;
         Right   : Node_Access;
         Color   : Color_Type;
         Element : aliased Element_Type;
      end record;      

   function Parent (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Parent;
   end;
      
   function Left (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Left;
   end;

   function Right (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Right;
   end;
      
   function Color (Node : Node_Access)
      return Color_Type is
   begin
      return Node.Color;
   end;
      
   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access) is
   begin
      Node.Parent := Parent;
   end;

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access) is
   begin
      Node.Left := Left;
   end;

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access) is
   begin
      Node.Right := Right;
   end;

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type) is
   begin
      Node.Color := Color;
   end;
   
   
   procedure Free is 
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);


   use Trees;


   procedure Delete_Tree (X : in out Node_Access) is
      Y : Node_Access;
   begin
      while X /= null loop
         Y := X.Right;
         Delete_Tree (Y);
         Y := X.Left;
         Free (X);
         X := Y;
      end loop;
   end;
 
   
   procedure Clear (Container : in out Container_Type) is
      X : Node_Access := Root (Container.Tree);
   begin
      Initialize (Container.Tree);
      Delete_Tree (X);
   end;



   procedure Initialize (Container : in out Container_Type) is
   
      Tree : Tree_Type renames Container.Tree;
   begin
      Tree.Back := new Node_Type;
      Tree.Back.Color := Red;
      
      Initialize (Tree);
   end;
   
     
   procedure Finalize (Container : in out Container_Type) is
   
      Tree : Tree_Type renames Container.Tree;
      
      Back : Node_Access := Tree.Back;
      Root : Node_Access;

   begin
   
      if Back = null then
         return;
      end if;
      
      Root := Trees.Root (Tree);
      
      Tree.Back := null;
      Tree.Length := 0;
   
      Delete_Tree (Root);           
      Free (Back);
      
   end Finalize;
   

   function Generic_Equal 
     (Left, Right : Container_Type) return Boolean is
     
      function "=" is
         new Trees.Generic_Equal ("=");
   begin
      if Left'Address = Right'Address then
         return True;
      end if;
      
      return Left.Tree = Right.Tree;
   end;
   
   
   function Succ (Iterator : Iterator_Type) return Iterator_Type is
   begin
      return (Node => Succ (Iterator.Node));
   end;


   function Succ 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type is
   begin
      return (Node => Succ (Iterator.Node, Offset));
   end;


   function Offset
     (From, To : Iterator_Type) return Natural is
   begin
      return Offset (From.Node, To.Node);
   end;
   

   function Is_Less 
     (Left  : Node_Access;
      Right : Key_Type) return Boolean is
      
      pragma Inline (Is_Less);
   begin
      return Left.Element < Right;
   end;
   
   
   function Is_Less 
     (Left  : Iterator_Type;
      Right : Key_Type) return Boolean is
   begin
      return Is_Less (Left.Node, Right);
   end;

   function Is_Less
     (Left  : Key_Type;
      Right : Node_Access) return Boolean is
      
      pragma Inline (Is_Less);
   begin
      return Left < Right.Element;
   end;
      
   function Is_Less 
     (Left  : Key_Type;
      Right : Iterator_Type) return Boolean is
   begin
      return Is_Less (Left, Right.Node);
   end;
   
         
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (L, R : Node_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.Element < R.Element;
      end;
         
      function "<" is new Trees.Generic_Less (Is_Less);

   begin
      if Left'Address = Right'Address then
         return False;
      end if;
      
      return Left.Tree < Right.Tree;
   end;
   

   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Tree.Length;
   end;
   
   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Tree.Length = 0;
   end;
   

   package Keys is 
      new Trees.Generic_Keys (Key_Type, Is_Less, Is_Less);
      
   use Keys;
   
      
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;  
      Iterator  :    out Iterator_Type) is      
      
      function New_Node return Node_Access is      
         pragma Inline (New_Node);

         Node : Node_Access := new Node_Type;
      begin
         Set_Key (Node.Element, Key);
         Node.Color := Red;
         
         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;   
      
      procedure Insert is
         new Keys.Generic_Unconditional_Insert (New_Node);
   begin
      Insert (Container.Tree, Key, Iterator.Node);      
      pragma Debug (Check_Invariant (Container.Tree));
   end;
   
         
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type) is

      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, Iterator);
   end;
   
         
   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     Key_Type;  
      Iterator  :    out Iterator_Type) is      
      
      function New_Node return Node_Access is
         pragma Inline (New_Node);

         Node : Node_Access := new Node_Type;
      begin
         Set_Key (Node.Element, Key);
         Node.Color := Red;
         
         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;   
               
      procedure Insert_With_Hint is
         new Keys.Generic_Unconditional_Insert_With_Hint (New_Node);
   begin
      Insert_With_Hint 
        (Container.Tree, 
         Position.Node, 
         Key, 
         Iterator.Node);
      
      pragma Debug (Check_Invariant (Container.Tree));
   end;
   
   
      
   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     Key_Type) is

      Iterator : Iterator_Type;
   begin
      Insert (Container, Position, Key, Iterator);
   end;



   procedure Delete
     (Container : in out Container_Type;
      First     : in out Node_Access;
      Back      : in     Node_Access) is
      
      pragma Inline (Delete);

      Tree : Tree_Type renames Container.Tree;
      
   begin
   
      while First /= Back 
        and then First /= Tree.Back 
      loop
               
         declare
            Next : constant Node_Access := Succ (First);
         begin
            Delete (Tree, First);

            declare
               X : Node_Access := First;
            begin
               First := Next;
               Free (X);
            end;
         end;

         pragma Debug (Check_Invariant (Tree));
         
      end loop;
      
   end Delete;


   function First (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => First (Container.Tree));
   end;
   
   function Last (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Last (Container.Tree));
   end;
   
   function Back (Container : Container_Type) return Iterator_Type is
   begin
      return (Node => Container.Tree.Back);
   end;
  
   
   function Pred (Iterator : Iterator_Type) return Iterator_Type is
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

   function Element (Iterator : Iterator_Type) return Element_Type is
   begin
      return Iterator.Node.Element;
   end;
   
   procedure Swap (Left, Right : in out Container_Type) is
   begin
      Swap (Left.Tree, Right.Tree);
   end;
   

   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access is
   begin
      return Iterator.Node.Element'Access;
   end;
   

   function First_Element (Container : Container_Type) return Element_Type is
   begin
      return First (Container.Tree).Element;
   end;

   
   function Last_Element (Container : Container_Type) return Element_Type is
   begin
      return Last (Container.Tree).Element;
   end;
   

   function Generic_Modify_Element
     (Iterator : Iterator_Type) return Element_Access is
   begin
      return Iterator.Node.Element'Access;
   end;
   

   
   function Find 
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Find (Container.Tree, Key));
   end;
   
   
   function Is_In 
     (Key       : Key_Type;
      Container : Container_Type) return Boolean is
      
      Node : constant Node_Access := Find (Container.Tree, Key);
   begin
      return Node /= Container.Tree.Back;
   end;


   function Count
     (Container : Container_Type;
      Key       : Key_Type) return Natural is
   begin
      return Count (Container.Tree, Key);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
   begin
      if Iterator.Node = null 
        or else Iterator.Node = Container.Tree.Back
      then
         return;
      end if;
      
      declare
         Next : constant Node_Access := Succ (Iterator.Node);
      begin
         Delete (Container.Tree, Iterator.Node);

         declare
            X : Node_Access := Iterator.Node;
         begin
            Iterator.Node := Next;
            Free (X);
         end;
      end;

      pragma Debug (Check_Invariant (Container.Tree));
   end;   


   procedure Delete_Sans_Increment
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type) is
   begin
      if Iterator.Node = null 
        or else Iterator.Node = Container.Tree.Back
      then
         return;
      end if;
      
      Delete (Container.Tree, Iterator.Node);
      
      declare
         X : Node_Access := Iterator.Node;
      begin
         Iterator.Node := Container.Tree.Back;
         Free (X);
      end;
      
      pragma Debug (Check_Invariant (Container.Tree));
   end;

      
   procedure Delete_Sans_Assign
     (Container : in out Container_Type;
      Iterator  : in     Iterator_Type) is
      
      Tree : Tree_Type renames Container.Tree;
      X    : Node_Access := Iterator.Node;
      
   begin
   
      if X = null or else X = Tree.Back then
         return;
      end if;
      
      Delete (Tree, X);      
      Free (X);
      
      pragma Debug (Check_Invariant (Container.Tree));
      
   end Delete_Sans_Assign;
   

   procedure Delete_First (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, First (Container));
   end;

   
   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Delete_Sans_Assign (Container, Last (Container));
   end;
   


   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type) is
   begin
      if First.Node = null or Back.Node = null then
         return;
      end if;
      
      if First.Node = Trees.First (Container.Tree) 
        and then Back.Node = Container.Tree.Back 
      then
         Clear (Container);
      else
         Delete (Container, First.Node, Back.Node);
      end if;
   end Delete;


   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Count     :    out Natural) is
     
      First, Back : Node_Access;
   begin
      Equal_Range (Container.Tree, Key, First, Back);
      Count := Offset (First, Back);
      Delete (Container, First, Back);
   end;
   

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type) is
      
      First, Back : Node_Access;
   begin
      Equal_Range (Container.Tree, Key, First, Back);
      Delete (Container, First, Back);
   end;

      
      
   function Lower_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Lower_Bound (Container.Tree, Key));
   end;


   function Upper_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type is
   begin
      return (Node => Upper_Bound (Container.Tree, Key));
   end;
   
   
   procedure Equal_Range 
     (Container : in     Container_Type;
      Key       : in     Key_Type;
      First     :    out Iterator_Type;
      Back      :    out Iterator_Type) is
   begin
      Equal_Range 
        (Container.Tree,
         Key,
         First.Node,
         Back.Node);
   end;


   procedure Generic_Select_Element
     (Iterator : in Iterator_Type) is
   begin
      Process (Iterator.Node.Element);
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
      


end Charles.Multisets.Sorted.Limited_Unbounded;
