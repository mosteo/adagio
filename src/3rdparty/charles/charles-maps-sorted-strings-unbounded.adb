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
with System; use type System.Address;

package body Charles.Maps.Sorted.Strings.Unbounded is

   type Node_Type (Key_Size : Natural) is
      record    --make this a limited record in Ada0Y
         Parent  : Node_Access;
         Left    : Node_Access;
         Right   : Node_Access;
         Color   : Color_Type;
         Key     : String (1 .. Key_Size);
         Element : aliased Element_Type;
      end record;
      
   function "=" (L, R : Node_Type) return Boolean is abstract;
   pragma Warnings (Off, "=");
   

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
   
      Tree.Back := new Node_Type (Key_Size => 0);
      Tree.Back.Color := Red;
      
      Initialize (Tree);
      
   end;
      
      
   function Copy_Node (Source : Node_Access) return Node_Access is
      pragma Inline (Copy_Node);
   
      Target : Node_Access := 
         new Node_Type'(Key_Size => Source.Key_Size,
                        Parent => null,
                        Left   => null,
                        Right  => null,
                        Color  => Source.Color,
                        Key    => Source.Key,
                        Element => Source.Element);
   begin
      return Target;
   end;
   
   
   function Copy_Tree (Source_Root : Node_Access) return Node_Access is
      Target_Root : Node_Access := Copy_Node (Source_Root);
      P, X : Node_Access;
   begin
      if Source_Root.Right /= null then
         Target_Root.Right := Copy_Tree (Source_Root.Right);
      end if;
      
      P := Target_Root;      
      X := Source_Root.Left;
      
      while X /= null loop
         declare
            Y : Node_Access := Copy_Node (X);
         begin
            P.Left := Y;
            Y.Parent := P;
            
            if X.Right /= null then
               Y.Right := Copy_Tree (X.Right);
            end if;
            
            P := Y;
            X := X.Left;
         end;
      end loop;            

      return Target_Root;
   exception
      when others =>
         Delete_Tree (Target_Root);
         raise;
   end;

      
   procedure Adjust (Container : in out Container_Type) is

      Tree : Tree_Type renames Container.Tree;

      Length : constant Natural := Tree.Length;
      
      X : constant Node_Access := Root (Tree);
      Y : Node_Access;

   begin
   
      begin
         Tree.Back := new Node_Type (Key_Size => 0);
      exception
         when others =>
            Tree.Back := null;
            Tree.Length := 0;
            raise;
      end;

      Tree.Back.Color := Red;
      
      Initialize (Tree);

      if X /= null then
         Y := Copy_Tree (X);
         
         Set_Root (Tree, Y);
         Set_First (Tree, Min (Y));
         Set_Last (Tree, Max (Y)); 
         
         Tree.Length := Length;
      end if;       
      
   end Adjust;

   
   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is

   begin
   
      if Target'Address = Source'Address then
         return;
      end if;
      
      if Is_Empty (Source) then
         Clear (Target);
         return;
      end if;
      
      declare
         Source_Root : constant Node_Access := Root (Source.Tree);
         Target_Root : Node_Access := Copy_Tree (Source_Root);
      begin
         begin
            Clear (Target);
         exception
            when others => 
               Delete_Tree (Target_Root);
               raise;
         end;

         Set_Root (Target.Tree, Target_Root);
         Set_First (Target.Tree, Min (Target_Root));
         Set_Last (Target.Tree, Max (Target_Root)); 
         
         Target.Tree.Length := Source.Tree.Length;
      end;
         
   end Assign;

     
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

   
   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Tree.Length;
   end;

   
   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Tree.Length = 0;
   end;


   function Is_Less (L, R : Node_Access) return Boolean is
      pragma Inline (Is_Less);
   begin
      return L.Key < R.Key;
   end;
      
   function Is_Less (Left, Right : Iterator_Type) return Boolean is
   begin
      return Is_Less (Left.Node, Right.Node);
   end;


   function Is_Less
     (L : Node_Access;
      R : String) return Boolean is
      
      pragma Inline (Is_Less);
   begin
      return L.Key < R;
   end;
   
   function Is_Less 
     (Left  : Iterator_Type;
      Right : String) return Boolean is
   begin
      return Is_Less (Left.Node, Right);
   end;


   function Is_Less
     (L : String;
      R : Node_Access) return Boolean is
      
      pragma Inline (Is_Less);
   begin
      return L < R.Key;
   end;

   function Is_Less 
     (Left  : String;
      Right : Iterator_Type) return Boolean is
   begin
      return Is_Less (Left, Right.Node);
   end;


   package Keys is new Trees.Generic_Keys (String, Is_Less, Is_Less);
   use Keys;
   
   
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is
     
      function New_Node return Node_Access is
         pragma Inline (New_Node);
         
         Node : constant Node_Access :=
            new Node_Type'(Key_Size => Key'Length,
                           Parent => null,
                           Left   => null,
                           Right  => null,
                           Color  => Red,
                           Key    => Key,
                           Element => New_Item);
      begin
         return Node;
      end;
 
      procedure Insert is
         new Keys.Generic_Conditional_Insert (New_Node);
   begin
      Insert (Container.Tree, Key, Iterator.Node, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;      
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Key, New_Item, Iterator, Success);
   end;
  

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Success   :    out Boolean) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, New_Item, Iterator, Success);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      New_Item  : in     Element_Type) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert (Container, Key, New_Item, Iterator, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is
     
      function New_Node return Node_Access is
         Node : Node_Access := new Node_Type (Key_Size => Key'Length);
      begin
         Node.Key := Key;
         Node.Color := Red;
         
         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;
 
      procedure Insert is
         new Keys.Generic_Conditional_Insert (New_Node);
   begin
      Insert (Container.Tree, Key, Iterator.Node, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Key, Iterator, Success);
   end;
  

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String;
      Success   :    out Boolean) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Key, Iterator, Success);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Key       : in     String) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert (Container, Key, Iterator, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is
     
      function New_Node return Node_Access is
         pragma Inline (New_Node);
         
         Node : constant Node_Access :=
            new Node_Type'(Key_Size => Key'Length,
                           Parent => null,
                           Left   => null,
                           Right  => null,
                           Color  => Red,
                           Key    => Key,
                           Element => New_Item);
      begin
         return Node;
      end;
 
      procedure Insert is
         new Keys.Generic_Conditional_Insert_With_Hint (New_Node);
   begin
      Insert (Container.Tree, Position.Node, Key, Iterator.Node, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;      
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Position, Key, New_Item, Iterator, Success);
   end;
  

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type;
      Success   :    out Boolean) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Position, Key, New_Item, Iterator, Success);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      New_Item  : in     Element_Type) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert (Container, Position, Key, New_Item, Iterator, Success);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type;
      Success   :    out Boolean) is
     
      function New_Node return Node_Access is
         Node : Node_Access := new Node_Type (Key_Size => Key'Length);
      begin
         Node.Key := Key;
         Node.Color := Red;
         
         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;
 
      procedure Insert is
         new Keys.Generic_Conditional_Insert_With_Hint (New_Node);
   begin
      Insert (Container.Tree, Position.Node, Key, Iterator.Node, Success);
   end;
      

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Position, Key, Iterator, Success);
   end;
  

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      Success   :    out Boolean) is
      
      Iterator : Iterator_Type;
   begin
      Insert (Container, Position, Key, Iterator, Success);
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String) is
      
      Iterator : Iterator_Type;
      Success  : Boolean;
   begin
      Insert (Container, Position, Key, Iterator, Success);
   end;


   procedure Replace_Element
     (Container : in out Container_Type;
      Key       : in     String;
      By        : in     Element_Type;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Key, By, Iterator, Success);
      
      if not Success then
         Replace_Element (Iterator, By);
      end if;
   end;
         
      
   procedure Replace_Element
     (Container : in out Container_Type;
      Key       : in     String;
      By        : in     Element_Type) is

      Iterator : Iterator_Type;
   begin
      Replace_Element (Container, Key, By, Iterator);
   end;


   procedure Replace_Element
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      By        : in     Element_Type;
      Iterator  :    out Iterator_Type) is
      
      Success : Boolean;
   begin
      Insert (Container, Position, Key, By, Iterator, Success);
      
      if not Success then
         Replace_Element (Iterator, By);
      end if;
   end;

      
   procedure Replace_Element
     (Container : in out Container_Type;
      Position  : in     Iterator_Type;
      Key       : in     String;
      By        : in     Element_Type) is
      
      Iterator : Iterator_Type;
   begin
      Replace_Element (Container, Position, Key, By, Iterator);
   end;
      


   function Find 
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Find (Container.Tree, Key));
   end;
   
   
   function Is_In
     (Key       : String;
      Container : Container_Type) return Boolean is
      
      Node : constant Node_Access := Find (Container.Tree, Key);
   begin
      return Node /= Container.Tree.Back;
   end;


   function Element
     (Container : Container_Type;
      Key       : String) return Element_Type is
      
      Node : constant Node_Access := Find (Container.Tree, Key);
   begin
      return Node.Element;
   end;


   function Is_Equal (L, R : Node_Access) return Boolean is
      pragma Inline (Is_Equal);
   begin
      return L.Element = R.Element;
   end;
      
   function Is_Equal (Left, Right : Iterator_Type) return Boolean is
   begin
      return Is_Equal (Left.Node, Right.Node);
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
   
      if X = null or else X = Container.Tree.Back then
         return;
      end if;
      
      Delete (Tree, X);      
      Free (X);
      
      pragma Debug (Check_Invariant (Tree));

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
   end;         

   
   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String;
      Count     :    out Natural) is  

      First, Back : Node_Access;
   begin
      Equal_Range (Container.Tree, Key, First, Back);
      Count := Offset (First, Back);
      Delete (Container, First, Back);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Key       : in     String) is
      
      First, Back : Node_Access;
   begin
      Equal_Range (Container.Tree, Key, First, Back);
      Delete (Container, First, Back);
   end;
      

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
   

   function Key (Iterator : Iterator_Type) return String is
   begin
      return Iterator.Node.Key;
   end;
   
   function Element (Iterator : Iterator_Type) return Element_Type is
   begin
      return Iterator.Node.Element;
   end;
   
   procedure Replace_Element
     (Iterator : Iterator_Type;
      By       : Element_Type) is
   begin
      Iterator.Node.Element := By;
   end;
   

   procedure Copy_Key
     (Iterator : in     Iterator_Type;
      Key      :    out String;
      Last     :    out Integer) is
   begin
      Last := Key'First + Key'Length - 1;
      Key (Key'First .. Last) := Iterator.Node.Key;
   end;
   

   procedure Copy_Element
     (Iterator : in     Iterator_Type;
      Item     :    out Element_Type) is
   begin
      Item := Iterator.Node.Element;
   end;

--TODO:
--   function Generic_Key 
--     (Iterator : Iterator_Type) return Key_Access is
--   begin
--      return Iterator.Node.Key'Access;
--   end;

--   function Generic_Modify_Key 
--     (Iterator : Iterator_Type) return Key_Access is
--   begin
--      return Iterator.Node.Key'Access;
--   end;

   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access is
   begin
      return Iterator.Node.Element'Access;
   end;

   
   function First_Key (Container : Container_Type) return String is
   begin
      return First (Container.Tree).Key;
   end;

   
   function Last_Key (Container : Container_Type) return String is
   begin
      return Last (Container.Tree).Key;
   end;


   function First_Element (Container : Container_Type) return Element_Type is
   begin
      return First (Container.Tree).Element;
   end;

   
   function Last_Element (Container : Container_Type) return Element_Type is
   begin
      return Last (Container.Tree).Element;
   end;


   function Lower_Bound 
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Lower_Bound (Container.Tree, Key));
   end;
      

   function Upper_Bound
     (Container : Container_Type;
      Key       : String) return Iterator_Type is
   begin
      return (Node => Upper_Bound (Container.Tree, Key));
   end;
         
   
   function Count
     (Container : Container_Type;
      Key       : String) return Natural is
   begin
      return Count (Container.Tree, Key);
   end;
   

   procedure Equal_Range
     (Container : in     Container_Type;
      Key       : in     String;
      First     :    out Iterator_Type;
      Back      :    out Iterator_Type) is
   begin
      Equal_Range (Container.Tree, Key, First.Node, Back.Node);
   end;


   procedure Swap (Left, Right : in out Container_Type) is
   begin
      Swap (Left.Tree, Right.Tree);
   end;

   function Is_Equal is
      new Trees.Generic_Equal (Is_Equal);

   function "=" (Left, Right : Container_Type) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;
      
      return Is_Equal (Left.Tree, Right.Tree);
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
     
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator);
         Iterator := Succ (Iterator);
      end loop;
   end;
      


   procedure Generic_Reverse_Iteration
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := Back;
   begin
      while Iterator /= First loop
         Iterator := Pred (Iterator);
         Process (Iterator);
      end loop;
   end;         
     

   procedure Generic_Select_Keys
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator.Node.Key);
         Iterator := Succ (Iterator);
      end loop;
   end;
          

   procedure Generic_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator.Node.Element);
         Iterator := Succ (Iterator);
      end loop;
   end;
     

   procedure Generic_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator.Node.Element);
         Iterator := Succ (Iterator);
      end loop;
   end;
     

   procedure Generic_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := First;
   begin
      while Iterator /= Back loop
         Process (Iterator.Node.Element'Access);
         Iterator := Succ (Iterator);
      end loop;
   end;
      
     

   procedure Generic_Reverse_Select_Keys
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
     
      Iterator : Iterator_Type := Back;
   begin
      while Iterator /= First loop
         Iterator := Pred (Iterator);
         Process (Iterator.Node.Key);
      end loop;
   end;


   procedure Generic_Reverse_Select_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
     
      Iterator : Iterator_Type := Back;
   begin
      while Iterator /= First loop
         Iterator := Pred (Iterator);
         Process (Iterator.Node.Element);
      end loop;
   end;


   procedure Generic_Reverse_Modify_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is
      
      Iterator : Iterator_Type := Back;
   begin
      while Iterator /= First loop
         Iterator := Pred (Iterator);
         Process (Iterator.Node.Element);
      end loop;
   end;

     

   procedure Generic_Reverse_Access_Elements
     (First : in Iterator_Type;
      Back  : in Iterator_Type) is

      Iterator : Iterator_Type := Back;
   begin
      while Iterator /= First loop
         Iterator := Pred (Iterator);
         Process (Iterator.Node.Element'Access);
      end loop;
   end;

      

end Charles.Maps.Sorted.Strings.Unbounded;
