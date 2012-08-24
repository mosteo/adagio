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
with Charles.Vectors.Unbounded;
pragma Elaborate_All (Charles.Vectors.Unbounded);

generic

   type List_Type is limited private;
   
   type Iterator_Type is private;
   
   Null_Iterator : in Iterator_Type;
   
   with function Hash (I : Iterator_Type) return Integer'Base is <>;
      
   with function Length (List : List_Type) return Natural is <>;
   
   with function First (List : List_Type) return Iterator_Type is <>;
 
   with function Last (List : List_Type) return Iterator_Type is <>;

   with function Back (List : List_Type) return Iterator_Type is <>;
   
   with function Succ (Iterator : Iterator_Type) return Iterator_Type is <>;
   
   with function Pred (Iterator : Iterator_Type) return Iterator_Type is <>;
   
   with procedure Clear (List : in out List_Type) is <>;
   
   with procedure Swap (Left, Right : in out List_Type) is <>;
         
   with procedure Delete
     (List  : in out List_Type;
      First : in out Iterator_Type;
      Back  : in     Iterator_Type) is <>;
      
   with procedure Splice
     (List     : in List_Type;
      Before   : in Iterator_Type;
      Iterator : in Iterator_Type) is <>;
     
   with function "=" (L, R : Iterator_Type) return Boolean is <>;
      
package Charles.Hash_Tables is

   pragma Preelaborate;
        
         
   package Iterator_Vectors is
      new Charles.Vectors.Unbounded 
        (Index_Type   => Natural, 
         Element_Type => Iterator_Type, 
         "="          => "=");
      
   subtype List_Subtype is List_Type;   
   subtype Vector_Subtype is Iterator_Vectors.Container_Type;
   
   type Hash_Table_Type is
      record
         L : aliased List_Subtype;
         V : Vector_Subtype;
      end record;
      
   procedure Adjust (Hash_Table : in out Hash_Table_Type);
    
--   procedure Assign
--     (Target : in out Hash_Table_Type;
--      Source : in     Hash_Table_Type);

   procedure Clear (Hash_Table : in out Hash_Table_Type);

   procedure Swap (Left, Right : in out Hash_Table_Type);

   
   procedure Pre_Delete 
     (Hash_Table : in out Hash_Table_Type;
      Iterator   : in     Iterator_Type);
      
   procedure Delete 
     (Hash_Table : in out Hash_Table_Type;
      First      : in out Iterator_Type;
      Back       : in     Iterator_Type);
         
   generic

      type Key_Type (<>) is limited private;   
      
      with function Hash (Key : Key_Type) return Integer'Base is <>;
               
      with function Is_Equal 
        (Iterator : Iterator_Type;
         Key      : Key_Type) return Boolean is <>;
        
   package Generic_Keys is
         
      function Count
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Natural;

      function Find
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type;
         
      function Lower_Bound
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type;
         
      function Upper_Bound
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type;
     
      procedure Equal_Range
        (Hash_Table  : in     Hash_Table_Type;
         Key         : in     Key_Type;
         First, Back :    out Iterator_Type);

      generic
      
         with procedure Insert 
           (List     : in out List_Type;
            Before   : in     Iterator_Type;
            Key      : in     Key_Type;
            Iterator :    out Iterator_Type) is <>;
            
      package Generic_Insertion is
         
         procedure Conditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean);
                       
         procedure Conditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Position   : in     Iterator_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean);
            
         procedure Unconditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type);
      
      end Generic_Insertion;

         
      procedure Delete
        (Hash_Table : in out Hash_Table_Type;
         Key        : in     Key_Type;
         Count      :    out Natural);
         
   end Generic_Keys;
      

   procedure Resize 
     (Hash_Table : in out Hash_Table_Type;
      Length     : in     Natural);   
      
   procedure Hash_Range
     (Hash_Table  : in     Hash_Table_Type;
      Index       : in     Natural;
      First, Back :    out Iterator_Type);


end Charles.Hash_Tables;


