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
generic
   
   type Node_Access is private;
   
   type Color_Type is (<>);
   
   Null_Node : Node_Access;
   
   Red   : in Color_Type;
   Black : in Color_Type;
   
   with function Parent (Node : Node_Access) 
      return Node_Access is <>;
   
   with procedure Set_Parent 
     (Node   : Node_Access;
      Parent : Node_Access) is <>;
   
   with function Left (Node : Node_Access) 
      return Node_Access is <>;

   with procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access) is <>;

   with function Right (Node : Node_Access) 
      return Node_Access is <>;
   
   with procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access) is <>;
      
   with function Color (Node : Node_Access)
      return Color_Type is <>;
      
   with procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type) is <>;
      
package Charles.Red_Black_Trees is

   pragma Pure;
   
   type Tree_Type is
      record
         Back   : Node_Access;
         Length : Natural;
      end record;
      
   function "=" (L, R : Tree_Type) return Boolean is abstract;
  
   procedure Initialize (Tree : in out Tree_Type);
   
   procedure Set_Root
     (Tree : Tree_Type;
      Root : Node_Access);
   
   procedure Set_First
     (Tree  : Tree_Type;
      First : Node_Access);
   
   procedure Set_Last
     (Tree : Tree_Type;
      Last : Node_Access);

   function Min (Node : Node_Access) return Node_Access;
   
   function Max (Node : Node_Access) return Node_Access;
   
   procedure Check_Invariant (Tree : Tree_Type);
   
   function Root (Tree : Tree_Type) return Node_Access;
   
   function First (Tree : Tree_Type) return Node_Access;
   
   function Last (Tree : Tree_Type) return Node_Access;

   function Succ (Node : Node_Access) return Node_Access;
   
   function Succ (Node : Node_Access; Offset : Natural) return Node_Access;
   
   function Pred (Node : Node_Access) return Node_Access;
   
   function Pred (Node : Node_Access; Offset : Natural) return Node_Access;
   
   function Offset (From, To : Node_Access) return Natural;
   
   procedure Swap (Left, Right : in out Tree_Type);
      
   generic
      with function Is_Equal (L, R : Node_Access) return Boolean;
   function Generic_Equal (Left, Right : Tree_Type) return Boolean;
      

   generic
      with function Is_Less (L, R : Node_Access) return Boolean;
   function Generic_Less (Left, Right : Tree_Type) return Boolean;


   procedure Delete
     (Tree : in out Tree_Type;
      Node : in     Node_Access);

   
   generic
   
      type Key_Type (<>) is limited private;
      
      with function Is_Less_Key_Node 
             (L : Key_Type; 
              R : Node_Access) return Boolean;
              
      with function Is_Less_Node_Key 
             (L : Node_Access; 
              R : Key_Type) return Boolean;
      
   package Generic_Keys is
   
      generic
         with function New_Node return Node_Access;
      procedure Generic_Conditional_Insert
        (Tree    : in out Tree_Type;
         Key     : in     Key_Type;
         Node    :    out Node_Access;
         Success :    out Boolean);
            
      generic
         with function New_Node return Node_Access;         
      procedure Generic_Conditional_Insert_With_Hint
        (Tree     : in out Tree_Type;
         Position : in     Node_Access;
         Key      : in     Key_Type;
         Node     :    out Node_Access;
         Success  :    out Boolean);
         

      generic
         with function New_Node return Node_Access;
      procedure Generic_Unconditional_Insert
        (Tree : in out Tree_Type;
         Key  : in     Key_Type;
         Node :    out Node_Access);
         
      generic
         with function New_Node return Node_Access;
      procedure Generic_Unconditional_Insert_With_Hint
        (Tree     : in out Tree_Type;
         Position : in     Node_Access;
         Key      : in     Key_Type;
         Node     :    out Node_Access);

                     
      function Find 
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access;
         
      function Lower_Bound
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access;
         
      function Upper_Bound
        (Tree : Tree_Type;
         Key  : Key_Type) return Node_Access;
         
      procedure Equal_Range
        (Tree        : in     Tree_Type;
         Key         : in     Key_Type;
         First, Back :    out Node_Access);
         
      function Count
        (Tree : Tree_Type;
         Key  : Key_Type) return Natural;
            
   end Generic_Keys;

 
end Charles.Red_Black_Trees;
