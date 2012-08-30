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
with Charles.Hash_Tables;
pragma Elaborate_All (Charles.Hash_Tables);

with Charles.Double_Lists;
pragma Elaborate_All (Charles.Double_Lists);

with Ada.Finalization;

generic

   type Key_Type (<>) is limited private;

   type Element_Type is limited private;
   
   with function Hash (Key : Key_Type) return Integer'Base is <>;
   
   with function Hash (Item : Element_Type) return Integer'Base is <>;
   
   with function Is_Equal_Key
     (Item : Element_Type;
      Key  : Key_Type) return Boolean is <>;
      
   with procedure Set_Key
     (Item : in out Element_Type;
      Key  : in     Key_Type) is <>;
   
package Charles.Multisets.Hashed.Limited_Unbounded is

   pragma Preelaborate;
   

   subtype Key_Subtype is Key_Type;

   subtype Element_Subtype is Element_Type;
   

   type Container_Type is limited private;
   
   type Iterator_Type is private;
   
   Null_Iterator : constant Iterator_Type;
   

   generic
      with function "=" (L, R : Element_Type) return Boolean is <>;
   function Generic_Equal (Left, Right : Container_Type) return Boolean;
      
   function Length (Container : Container_Type) return Natural;
   
   function Is_Empty (Container : Container_Type) return Boolean;
   
   procedure Clear (Container : in out Container_Type);
   
   procedure Swap (Left, Right : in out Container_Type);
   
   
   procedure Insert
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Iterator  :    out Iterator_Type);      

   procedure Insert_Sans_Resize
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Iterator  :    out Iterator_Type);      
               

   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type);
      
   procedure Delete
     (Container : in out Container_Type;
      Key       : in     Key_Type;
      Count     :    out Natural);
      
   procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type);
      
   procedure Delete_First (Container : in out Container_Type);
   
   procedure Delete_Last (Container : in out Container_Type);
      
   procedure Delete
     (Container : in out Container_Type;
      First     : in out Iterator_Type;
      Back      : in     Iterator_Type);
      

   function Is_In
     (Key       : Key_Type;
      Container : Container_Type) return Boolean;

   function Find 
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type;
         
   function Lower_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type;

   function Upper_Bound
     (Container : Container_Type;
      Key       : Key_Type) return Iterator_Type; 

   procedure Equal_Range
     (Container   : in     Container_Type;
      Key         : in     Key_Type; 
      First, Back :    out Iterator_Type);


   function Count
     (Container : Container_Type;
      Key       : Key_Type) return Natural;
      
   function First (Container : Container_Type) return Iterator_Type;
   
   function First_Element (Container : Container_Type) return Element_Type;

   function Last (Container : Container_Type) return Iterator_Type;
   
   function Last_Element (Container : Container_Type) return Element_Type;

   function Back (Container : Container_Type) return Iterator_Type;
   
      
   function Succ 
     (Iterator  : Iterator_Type) return Iterator_Type;
      
   function Succ 
     (Iterator  : Iterator_Type;
      Offset    : Natural) return Iterator_Type;
      
   function Pred 
     (Iterator : Iterator_Type) return Iterator_Type;

   function Pred 
     (Iterator : Iterator_Type;
      Offset   : Natural) return Iterator_Type;
     
   function Offset
     (From, To : Iterator_Type) return Natural;
     
   procedure Increment (Iterator : in out Iterator_Type);
   
   procedure Increment 
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);
      
   procedure Decrement (Iterator : in out Iterator_Type);
   
   procedure Decrement 
     (Iterator : in out Iterator_Type;
      Offset   : in     Natural);


   function Element (Iterator : Iterator_Type) return Element_Type;
   
   generic
      type Element_Access is access constant Element_Type;
   function Generic_Element
     (Iterator : Iterator_Type) return Element_Access;
   
   generic
      type Element_Access is access all Element_Type;
   function Generic_Modify_Element
     (Iterator : Iterator_Type) return Element_Access;
   

   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Select_Element
     (Iterator : in Iterator_Type);
     
   generic
      with procedure Process (Iterator : in Iterator_Type) is <>;
   procedure Generic_Iteration
     (First, Back : in Iterator_Type);
     
   generic
      with procedure Process (Iterator : in Iterator_Type) is <>;
   procedure Generic_Reverse_Iteration
     (First, Back : in Iterator_Type);
   

   generic
      with procedure Process 
        (Element : in Element_Type) is <>;
   procedure Generic_Select_Elements
     (First, Back : in Iterator_Type);

   generic
      with procedure Process 
        (Element : in Element_Type) is <>;
   procedure Generic_Reverse_Select_Elements
     (First, Back : in Iterator_Type);



   function Size (Container : Container_Type) return Natural;
   
   procedure Resize 
     (Container : in out Container_Type;
      Length    : in     Natural);   
            
   procedure Hash_Range
     (Container   : in     Container_Type;
      Index       : in     Natural;
      First, Back :    out Iterator_Type);
      
private

   type Node_Type;   
   type Node_Access is access Node_Type;
   
   procedure Set_Succ
     (Node : Node_Access;
      Succ : Node_Access);   
   pragma Inline (Set_Succ);
   
   procedure Set_Pred
     (Node : Node_Access;
      Pred : Node_Access);
   pragma Inline (Set_Pred);
   
   function Succ (Node : Node_Access) return Node_Access;
   pragma Inline (Succ);
   
   function Pred (Node : Node_Access) return Node_Access;
   pragma Inline (Pred);   

   procedure Free (X : in out Node_Access);
   pragma Inline (Free);   

   package Node_Lists is
      new Charles.Double_Lists (Node_Access, null);
   use Node_Lists;

   
   function Hash (Node : Node_Access) return Integer'Base;
   pragma Inline (Hash);
   
   package Hash_Table_Types is
      new Charles.Hash_Tables (List_Type, Node_Access, null);
   use Hash_Table_Types;

         
   type Container_Type is
      new Ada.Finalization.Limited_Controlled with record
         Hash_Table : Hash_Table_Type;
      end record;
      
   procedure Initialize (Container : in out Container_Type);
            
   procedure Finalize (Container : in out Container_Type);
   
   type Iterator_Type is
      record
         Node : Node_Access;
      end record;

   Null_Iterator : constant Iterator_Type := (Node => null);

      
end Charles.Multisets.Hashed.Limited_Unbounded;
        