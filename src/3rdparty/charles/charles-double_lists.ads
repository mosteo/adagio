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

   Null_Node : in Node_Access;
   
   with procedure Set_Succ 
      (Node : Node_Access;
       Succ : Node_Access) is <>;
       
   with procedure Set_Pred
      (Node : Node_Access;
       Pred : Node_Access) is <>;
       
   with function Succ
     (Node : Node_Access) return Node_Access is <>;
     
   with function Pred
     (Node : Node_Access) return Node_Access is <>;
     
   with procedure Free (X : in out Node_Access) is <>;
   
package Charles.Double_Lists is

   pragma Pure;
   

   type List_Type is 
      record
         Back   : Node_Access;
         Length : Natural;
      end record;
      
   function "=" (L, R : List_Type) return Boolean is abstract;

      
   procedure Initialize (List : in out List_Type);
   
   --procedure Adjust (List : in out List_Type);

   procedure Finalize (List : access List_Type);   
      
   generic
      with function Is_Equal (L, R : Node_Access) return Boolean is <>;
   function Generic_Equal (Left, Right : List_Type) return Boolean;
   
   generic
      with function Is_Less (L, R : Node_Access) return Boolean is <>;
   function Generic_Less
     (Left, Right : List_Type) return Boolean;
     
   function Length (List : List_Type) return Natural;
   pragma Inline (Length);

   procedure Clear (List : in out List_Type);
   
   procedure Swap (Left, Right : in out List_Type);
   
   function First (List : List_Type) return Node_Access;
   pragma Inline (First);
   
   function Last (List : List_Type) return Node_Access;
   pragma Inline (Last);
   
   function Back (List : List_Type) return Node_Access;
   pragma Inline (Back);
   

   function Succ 
     (Node   : Node_Access;
      Offset : Natural) return Node_Access;
      
   function Pred
     (Node   : Node_Access;
      Offset : Natural) return Node_Access;
      
   function Offset
     (From, To : Node_Access) return Natural;
   
      
   --function To_List (Length : Natural) return List_Type;
   
--   function To_List 
--     (Length : Natural;
--      Item   : Element_Type) return List_Type;
--      
--   function To_List 
--     (First  : Node_Access;
--      Back   : Node_Access) return List_Type;

--   procedure Assign
--     (Target : in out List_Type;
--      Length : in     Natural);
--   
--   procedure Assign
--     (Target : in out List_Type;
--      Length : in     Natural;
--      Item   : in     Element_Type);
--   
--   procedure Assign
--     (Target : in out List_Type;
--      Source : in     List_Type);

   procedure Insert
     (List   : in out List_Type;
      Before : in     Node_Access;
      Node   : in     Node_Access); 


--   procedure Insert_Range
--     (List   : in out List_Type;
--      Before : in     Node_Access;
--      Length : in     Natural;
--      Node   :    out Node_Access);



   procedure Delete_And_Free
     (List : in out List_Type;
      Node : in out Node_Access);


   procedure Delete_And_Increment
     (List : in out List_Type;
      Node : in out Node_Access);
      
   procedure Delete
     (List  : in out List_Type; 
      First : in out Node_Access;
      Back  : in     Node_Access);
            

   generic
      with function Predicate (Node : Node_Access)
        return Boolean is <>;
   procedure Generic_Delete 
     (List  : in out List_Type;
      First : in out Node_Access;
      Back  : in     Node_Access;
      Count :    out Natural);
         
   
   procedure Reverse_List 
     (First : in Node_Access;
      Back  : in Node_Access);
      
   
   generic
      with function Predicate (L, R : Node_Access) return Boolean is <>;
   procedure Generic_Unique 
     (List  : in out List_Type;
      First : in     Node_Access;
      Back  : in     Node_Access);

--   generic
--      with function Is_Less (L, R : Node_Access) return Boolean is <>;
--   procedure Generic_Merge_Source 
--     (List   : in out List_Type;
--      Source : in out List_Type);
      
   generic
      with function Is_Less (L, R : Node_Access) return Boolean is <>;
   procedure Generic_Merge 
     (List   : in out List_Type;
      Source : in out List_Type;
      First  : in     Node_Access;
      Back   : in     Node_Access);
      

   generic
      with function Is_Less (L, R : Node_Access) return Boolean is <>;
   procedure Generic_Quicksort (First, Back : Node_Access);


   procedure Swap_Iterator (Left, Right : in out Node_Access);
   pragma Inline (Swap_Iterator);
   

--   generic
--      with function Predicate (Node : Node_Access)
--         return Boolean is <>;
--   function Generic_Find 
--     (First : Node_Access;
--      Back  : Node_Access) return Node_Access;
--      
--   generic
--      with function Predicate (Node : Node_Access)
--         return Boolean is <>;
--   function Generic_Reverse_Find 
--     (First : Node_Access;
--      Back  : Node_Access) return Node_Access;
      
      
   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type);

   procedure Splice
     (List   : in List_Type;
      Before : in Node_Access;
      Node   : in Node_Access);

   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type;
      Node   : in     Node_Access);

   procedure Splice
     (List   : in List_Type;
      Before : in Node_Access;
      First  : in Node_Access;
      Back   : in Node_Access);

   procedure Splice
     (List   : in out List_Type;
      Before : in     Node_Access;
      Source : in out List_Type;
      First  : in     Node_Access;
      Back   : in     Node_Access);


end Charles.Double_Lists;

