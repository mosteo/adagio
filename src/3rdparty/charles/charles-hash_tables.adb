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
with Charles.Prime_Numbers;

package body Charles.Hash_Tables is

   use Iterator_Vectors;


   function Hash 
     (I : Iterator_Type;
      N : Positive) return Index_Subtype is
      
      pragma Inline (Hash);
   begin
      return Index_Subtype'First + Hash (I) mod N;
   end;


   procedure Post_Assign (Hash_Table : in out Hash_Table_Type) is

      V : Vector_Subtype renames Hash_Table.V;
      L : List_Subtype renames Hash_Table.L;
      
      N : constant Positive := Length (Hash_Table.V);
            
      BL : constant Iterator_Type := Back (L);

      E : Iterator_Type := First (L);

   begin

      while E /= BL loop

         declare
            IV : constant Index_Subtype := Hash (E, N);
            IL : constant Iterator_Type := Element (V, IV);
         begin
            if IL = BL then
               Replace_Element (V, Index => IV, By => E);
            end if;
         end;
      
         E := Succ (E);
         
      end loop;

   end Post_Assign;
            

   procedure Adjust (Hash_Table : in out Hash_Table_Type) is
   
      V : Vector_Subtype renames Hash_Table.V;
      L : List_Subtype renames Hash_Table.L;
      
      BL : constant Iterator_Type := Back (L);
      
      procedure Process (E : in out Iterator_Type) is
      begin
         E := BL;
      end;
      
      procedure Iterate is
         new Iterator_Vectors.Generic_Modify_Elements;

   begin
         
      Iterate (V, First (V), Back (V));

      if Length (L) /= 0 then
         Post_Assign (Hash_Table);
      end if;
   
   end Adjust;


--   procedure Assign
--     (Target : in out Hash_Table_Type;
--      Source : in     Hash_Table_Type) is
--      
--      TL : List_Subtype renames Target.L;
--      TV : Vector_Subtype renames Target.V;
--      
--      SL : List_Subtype renames Source.L;
--      SV : Vector_Subtype renames Source.V;

--   begin

--      Assign (Target => TV, Length => Length (SV), Item => Back (TL));
--      
--      Assign (Target => TL, Source => SL);
--      
--      if not Is_Empty (TL) then
--         Post_Assign (Target);
--      end if;            

--   end Assign;


   procedure Clear (Hash_Table : in out Hash_Table_Type) is
      V : Vector_Subtype;
   begin
      --Clear (Hash_Table.V);  --?
      Swap (Hash_Table.V, V);  --?
      Clear (Hash_Table.L);
   end;


   procedure Swap (Left, Right : in out Hash_Table_Type) is
   begin
      Swap (Left.V, Right.V);
      Swap (Left.L, Right.L);
   end;


   procedure Pre_Delete 
     (Hash_Table : in out Hash_Table_Type;
      Iterator   : in     Iterator_Type) is
      
      V : Vector_Subtype renames Hash_Table.V;
      L : List_Subtype renames Hash_Table.L;      

      BL : constant Iterator_Type := Back (L);
      pragma Assert (Iterator /= BL);
      
      N : constant Positive := Length (V);
      
      IV : constant Index_Subtype := Hash (Iterator, N);

      JL : constant Iterator_Type := Pred (Iterator);
      KL : constant Iterator_Type := Succ (Iterator);
      
   begin

      if JL = BL 
        or else Hash (JL, N) /= IV
      then
      
         if KL = BL 
           or else Hash (KL, N) /= IV 
         then
            Replace_Element (V, Index => IV, By => BL);            
         else
            Replace_Element (V, Index => IV, By => KL);
         end if;
         
      end if;
                           
   end Pre_Delete;


   procedure Delete 
     (Hash_Table : in out Hash_Table_Type;
      First      : in out Iterator_Type;
      Back       : in     Iterator_Type) is
      
      V : Vector_Subtype renames Hash_Table.V;
      L : List_Subtype renames Hash_Table.L;      

      N : Positive;
               
      BL : constant Iterator_Type := Hash_Tables.Back (L);
      
      IV, JV : Index_Subtype;
      JL : Iterator_Type;
      
   begin
            
      if First = Back then
         return;
      end if;
      
      if First = BL then --?
         return;
      end if;

      N := Length (V);
      
      IV := Hash (First, N);
      pragma Assert (Hash (Element (V, IV), N) = IV);
      
      JL := Succ (First);
      
      loop            
         
         if JL = Back then
                        
            if Pred (First) = BL 
              or else Hash (Pred (First), N) /= IV
            then
            
               if JL = BL 
                 or else Hash (JL, N) /= IV 
               then
                  Replace_Element (V, Index => IV, By => BL);
               else
                  Replace_Element (V, Index => IV, By => JL);
               end if;
               
            end if;
            
            Delete (L, First, Back);
            
            return;
            
         end if;         

         pragma Assert (JL /= BL);
         
         JV := Hash (JL, N);

         if JV /= IV then
         
            if Pred (First) = BL 
              or else Hash (Pred (First), N) /= IV
            then
               Replace_Element (V, Index => IV, By => BL);
            end if;
            
            Delete (L, First, Back => JL);

            exit;
            
         end if;
         
         JL := Succ (JL);
         
      end loop;
      
      loop
      
         IV := JV;         
                        
         loop            
            
            JL := Succ (JL);
            
            if JL = Back then
            
               if JL = BL 
                 or else Hash (JL, N) /= IV
               then
                  Replace_Element (V, Index => IV, By => BL);
               else
                  Replace_Element (V, Index => IV, By => JL);
               end if;
               
               Delete (L, First, Back);
               
               return;
               
            end if;
         
            pragma Assert (JL /= BL);

            JV := Hash (JL, N);
   
            if JV /= IV then
            
               Replace_Element (V, Index => IV, By => BL);                  
               Delete (L, First, Back => JL);

               exit;
               
            end if;
            
         end loop;
         
      end loop;
                  
   end Delete;
      

   function Find
     (Hash_Table : Hash_Table_Type;
      IV, JV, KV : Index_Subtype) return Iterator_Type is
               
      pragma Assert (JV < IV);
      pragma Assert (IV < KV);

      V : Vector_Subtype renames Hash_Table.V;
      L : List_Subtype renames Hash_Table.L;
               
      BL : constant Iterator_Type := Back (L);
      pragma Assert (Element (V, IV) = BL);

      function Predicate (E : Iterator_Type) return Boolean is
         pragma Inline (Predicate);
      begin
         return E /= BL;
      end;
      
      function Find_Prev_Hash is
         new Iterator_Vectors.Generic_Reverse_Find;      
      
      function Find_Next_Hash is
         new Iterator_Vectors.Generic_Find;
         
      XV : Index_Subtype;
      XL : Iterator_Type;
      
   begin

      if IV - JV < KV - IV then

         XV := Find_Prev_Hash (V, First => JV, Back => IV);
         pragma Assert (XV /= IV);
         
         XL := Element (V, Index => XV);
         pragma Assert (XL /= BL);
         pragma Assert (Hash (XL, Length (V)) < IV);
                        
         loop
         
            XL := Succ (XL);
            
            pragma Assert (XL /= BL);
            
            XV := Hash (XL, Length (V));
            
            exit when XV > IV;
            
            pragma Assert (XV < IV);
            
         end loop;
                        
      else
            
         XV := Find_Next_Hash (V, First => IV + 1, Back => KV);            
         XL := Element (V, XV);
         
         pragma Assert (XL /= BL);
         pragma Assert (Hash (XL, Length (V)) > IV);
      
      end if;
      
      return XL;
      
   end Find;


   package body Generic_Keys is
   
      function Hash 
        (K : Key_Type;
         N : Positive) return Index_Subtype is
         
         pragma Inline (Hash);
      begin
         return Index_Subtype'First + Hash (K) mod N;
      end;
   
      function Find
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type is
         
         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         N : constant Natural := Length (V);
         
         BL : constant Iterator_Type := Back (L);
         
         IV : Index_Subtype;
         IL : Iterator_Type;
         
      begin
      
         if N = 0 then
            return BL;
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
            return BL;
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
               
         loop
            
            if Is_Equal (IL, Key) then
               return IL;
            end if;
            
            IL := Succ (IL);
            
            if IL = BL then
               return BL;
            end if;
            
            if Hash (IL, N) /= IV then
               return BL;
            end if;
            
         end loop;                  
      
      end Find;


      function Lower_Bound
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type is
         
         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         BV : constant Index_Subtype := Iterator_Vectors.Back (V);
         BL : constant Iterator_Type := Hash_Tables.Back (L);
         
         function Predicate (E : Iterator_Type) return Boolean is
            pragma Inline (Predicate);
         begin
            return E /= BL;
         end;
         
         function Find_Next_Hash is
            new Iterator_Vectors.Generic_Find;

         IV : Index_Subtype;
         IL : Iterator_Type;
         
         N : constant Natural := Length (V);
         
      begin
      
         if N = 0 then
            return BL;
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
         
            declare
               JV : constant Index_Subtype := Find_Next_Hash (V, IV + 1, BV);
            begin
               if JV = BV then
                  return BL;
               else
                  return Element (V, Index => JV);
               end if;
            end; 
                       
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
               
         loop
            
            exit when Is_Equal (IL, Key);
            
            IL := Succ (IL);
            
            if IL = BL then               
               return BL;
            end if;
            
            if Hash (IL, N) /= IV then            
               return IL;
            end if;
            
         end loop;                  
                     
         return IL;
        
      end Lower_Bound;


      function Upper_Bound
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Iterator_Type is
         
         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         BV : constant Index_Subtype := Iterator_Vectors.Back (V);
         BL : constant Iterator_Type := Hash_Tables.Back (L);
         
         function Predicate (E : Iterator_Type) return Boolean is
            pragma Inline (Predicate);
         begin
            return E /= BL;
         end;
         
         function Find_Next_Hash is
            new Iterator_Vectors.Generic_Find;

         IV : Index_Subtype;
         IL : Iterator_Type;
         
         N : constant Natural := Length (V);
         
      begin
      
         if N = 0 then
            return BL;
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
         
            declare
               JV : constant Index_Subtype := Find_Next_Hash (V, IV + 1, BV);
            begin
               if JV = BV then
                  return BL;
               else
                  return Element (V, Index => JV);
               end if;
            end;
            
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
               
         loop
            
            exit when Is_Equal (IL, Key);
            
            IL := Succ (IL);
            
            if IL = BL then               
               return BL;
            end if;
            
            if Hash (IL, N) /= IV then            
               return IL;
            end if;
            
         end loop;                  

         declare
            Result : Iterator_Type := Succ (IL);
         begin                              
            loop
               if Result = BL then
                  return Result;
               end if;
               
               if Hash (Result, N) /= IV then            
                  return Result;
               end if;
               
               if not Is_Equal (Result, Key) then
                  return Result;
               end if;
               
               Result := Succ (Result);               
            end loop;                    
         end;
         
      end Upper_Bound;
 

      procedure Equal_Range
        (Hash_Table  : in     Hash_Table_Type;
         Key         : in     Key_Type;
         First, Back :    out Iterator_Type) is
         
         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         BV : constant Index_Subtype := Iterator_Vectors.Back (V);
         BL : constant Iterator_Type := Hash_Tables.Back (L);
         
         function Predicate (E : Iterator_Type) return Boolean is
            pragma Inline (Predicate);
         begin
            return E /= BL;
         end;
         
         function Find_Next_Hash is
            new Iterator_Vectors.Generic_Find;

         IV : Index_Subtype;
         IL : Iterator_Type;
         
         N : constant Natural := Length (V);
         
      begin
      
         if N = 0 then

            First := BL;
            Back := First;
            
            return;
            
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
         
            declare
               JV : constant Index_Subtype := Find_Next_Hash (V, IV + 1, BV);
            begin
               if JV = BV then
                  First := BL;
               else
                  First := Element (V, Index => JV);
               end if;
            end;
            
            Back := First;               
               
            return;
            
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
               
         loop
            
            exit when Is_Equal (IL, Key);
            
            IL := Succ (IL);
            
            if IL = BL then
               
               First := BL;
               Back := First;
               
               return;

            end if;
            
            if Hash (IL, N) /= IV then
            
               First := IL;
               Back := First;
               
               return;
               
            end if;
            
         end loop;                  
                     
         First := IL;
         Back := Succ (First);

         loop
            
            if Back = BL then
               return;
            end if;
            
            if Hash (Back, N) /= IV then            
               return;
            end if;
            
            if not Is_Equal (Back, Key) then
               return;
            end if;
            
            Back := Succ (Back);
            
         end loop;                  
       
      end Equal_Range;
      
      
      package body Generic_Insertion is

         procedure Insert
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            IV         : in     Index_Subtype;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean) is

            V : Vector_Subtype renames Hash_Table.V;
            L : List_Subtype renames Hash_Table.L;
                     
            BL : constant Iterator_Type := Back (L);
            
            XV : Index_Subtype := IV;
            
            XL : Iterator_Type := Element (V, XV);
            pragma Assert (XL /= BL);
            
         begin
         
            loop
            
               if Is_Equal (XL, Key) then
                  Iterator := XL;
                  Success := False;
                  return;
               end if;
               
               XL := Succ (XL);
               
               if XL = BL then
                  Insert (L, BL, Key, Iterator);
                  Success := True;
                  return;
               end if;
               
               XV := Hash (XL, N => Length (V));
               
               if XV > IV then
                  Insert (L, XL, Key, Iterator);
                  Success := True;
                  return;
               end if;
               
               pragma Assert (XV = IV);
                  
            end loop;               
            
         end Insert;
         


         procedure Insert
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            IV         : in     Index_Subtype;
            Iterator   :    out Iterator_Type) is

            V : Vector_Subtype renames Hash_Table.V;
            L : List_Subtype renames Hash_Table.L;
                     
            BL : constant Iterator_Type := Back (L);
            
            XV : Index_Subtype := IV;
            
            XL : Iterator_Type := Element (V, XV);
            pragma Assert (XL /= BL);
            
         begin
         
            if Is_Equal (XL, Key) then
            
               --It's not clear if we should insert the new item
               --prior to the first matching element.
               --It may be more appropriate to search the last 
               --matching element, and then insert the new item
               --immediately after that.
               --
               --Yes, it's faster to insert prior to the first
               --matching element, because we don't have to scan all
               --the matching elements.
               --
               --However, one possibility is that the user inserts an item
               --that he knows is the first one with this key, and then
               --hangs on to the iterator that designates this first
               --element in the sequence of matching keys.  He may assume
               --that future insertions with the same key will follow
               --the first element with the key, so that his iterator
               --continues to designate first element in the group of
               --matching keys.

               Insert (L, XL, Key, Iterator);
               Replace_Element (V, Index => XV, By => Iterator);
               
               return;
               
            end if;
               
            loop
            
               XL := Succ (XL);
               
               if XL = BL then
                  Insert (L, BL, Key, Iterator);
                  return;
               end if;
               
               XV := Hash (XL, N => Length (V));
               
               if XV > IV then
                  Insert (L, XL, Key, Iterator);
                  return;
               end if;
               
               pragma Assert (XV = IV);
                  
               if Is_Equal (XL, Key) then
               
                  --See comment above.
                  
                  Insert (L, XL, Key, Iterator);
                  
                  return;
                  
               end if;
            
            end loop;               
            
         end Insert;


         procedure Conditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean) is
            
            V : Vector_Subtype renames Hash_Table.V;
            L : List_Subtype renames Hash_Table.L;
            
            N : constant Natural := Length (L);
               
            BL : constant Iterator_Type := Back (L);

            IV, JV, KV : Index_Subtype;
            IL, JL, KL : Iterator_Type;
            
         begin
         
            --Resize (Hash_Table, Length => N + 1);
               
            IV := Hash (Key, N => Length (V));
            IL := Element (V, Index => IV);
            
            if IL /= BL then
               Insert (Hash_Table, Key, IV, Iterator, Success);
               return;
            end if;
            
            Success := True;
            
            if N = 0 then        
               Insert (L, BL, Key, Iterator);      
               Replace_Element (V, Index => IV, By => Iterator);
               return;
            end if;
                     
            JL := First (L);
            pragma Assert (JL /= BL);
            
            JV := Hash (JL, N => Length (V));
            
            if IV < JV then
               Insert (L, JL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
               
            pragma Assert (IV > JV);
             
            if N = 1 then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
                  
            KL := Last (L);
            pragma Assert (KL /= BL);
            
            KV := Hash (KL, N => Length (V));
            
            if IV > KV then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
            
            pragma Assert (IV < KV);

            if N = 2 then
               Insert (L, KL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
                                 
            Insert (L, Find (Hash_Table, IV, JV, KV), Key, Iterator);
            Replace_Element (V, Index => IV, By => Iterator);             
                  
         end Conditional_Insert_Sans_Resize;

               
         procedure Conditional_Insert_Sans_Resize_With_Hint
           (Hash_Table : in out Hash_Table_Type;
            Position   : in     Iterator_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean) is
            
            V : Vector_Subtype renames Hash_Table.V;
            L : List_Subtype renames Hash_Table.L;
            
            N : constant Natural := Length (L);
               
            BL : constant Iterator_Type := Back (L);

            IV, JV, KV : Index_Subtype;
            IL, JL, KL : Iterator_Type;
            
            XV : Index_Subtype;
            
         begin
                     
            IV := Hash (Key, N => Length (V));
            IL := Element (V, Index => IV);
            
            if IL /= BL then
            
               --Can we do better than this, if we have a hint?
               --Probably, but handling this branch differently
               --for the hint case makes more sense for the
               --multiset and multimap.
               
               Insert (Hash_Table, Key, IV, Iterator, Success);
               return;
               
            end if;
            
            Success := True;
            
            if N = 0 then
               pragma Assert (Position = BL);
               Insert (L, BL, Key, Iterator);      
               Replace_Element (V, Index => IV, By => Iterator);
               return;
            end if;

            JL := First (L);
            pragma Assert (JL /= BL);
            
            JV := Hash (JL, N => Length (V));
            
            if IV < JV then
               Insert (L, JL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
               
            pragma Assert (IV > JV);
             
            if N = 1 then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
                  
            KL := Last (L);
            pragma Assert (KL /= BL);
            
            KV := Hash (KL, N => Length (V));
            
            if IV > KV then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
            
            pragma Assert (IV < KV);

            if N = 2 then
               Insert (L, KL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;

            XV := Hash (Position, N => Length (V));                  
                  
            if XV < IV then
               pragma Assert (XV >= JV);
               JV := XV;
            else
               pragma Assert (XV > IV);
               pragma Assert (XV <= KV);
               KV := XV;
            end if;
               
            Insert (L, Find (Hash_Table, IV, JV, KV), Key, Iterator);
            Replace_Element (V, Index => IV, By => Iterator);             
                  
         end Conditional_Insert_Sans_Resize_With_Hint;


         procedure Conditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Position   : in     Iterator_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type;
            Success    :    out Boolean) is
            
         begin
                     
            if Position = Null_Iterator
              or else Position = Back (Hash_Table.L)
            then
               Conditional_Insert_Sans_Resize 
                 (Hash_Table, 
                  Key, 
                  Iterator, 
                  Success);
            else
               Conditional_Insert_Sans_Resize_With_Hint
                 (Hash_Table,
                  Position,
                  Key,
                  Iterator,
                  Success);
            end if;
           
         end;

                  
         procedure Unconditional_Insert_Sans_Resize
           (Hash_Table : in out Hash_Table_Type;
            Key        : in     Key_Type;
            Iterator   :    out Iterator_Type) is
            
            V : Vector_Subtype renames Hash_Table.V;
            L : List_Subtype renames Hash_Table.L;
            
            N : constant Natural := Length (L);
               
            BL : constant Iterator_Type := Back (L);

            IV, JV, KV : Index_Subtype;
            IL, JL, KL : Iterator_Type;
            
         begin
         
            --Resize (Hash_Table, Length => N + 1);
               
            IV := Hash (Key, N => Length (V));
            IL := Element (V, Index => IV);
            
            if IL /= BL then
               Insert (Hash_Table, Key, IV, Iterator);
               return;
            end if;
                     
            if N = 0 then        
               Insert (L, BL, Key, Iterator);      
               Replace_Element (V, Index => IV, By => Iterator);
               return;
            end if;
                     
            JL := First (L);
            pragma Assert (JL /= BL);
            
            JV := Hash (JL, N => Length (V));
            
            if IV < JV then
               Insert (L, JL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
               
            pragma Assert (IV > JV);
             
            if N = 1 then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
                  
            KL := Last (L);
            pragma Assert (KL /= BL);
            
            KV := Hash (KL, N => Length (V));
            
            if IV > KV then
               Insert (L, BL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
            
            pragma Assert (IV < KV);

            if N = 2 then
               Insert (L, KL, Key, Iterator);
               Replace_Element (V, IV, By => Iterator);
               return;
            end if;
                                 
            Insert (L, Find (Hash_Table, IV, JV, KV), Key, Iterator);
            Replace_Element (V, Index => IV, By => Iterator);             
                  
         end Unconditional_Insert_Sans_Resize;
            
      end Generic_Insertion;
      

      procedure Delete
        (Hash_Table : in out Hash_Table_Type;
         Key        : in     Key_Type;
         Count      :    out Natural) is

         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         N : constant Natural := Length (V);
         
         BL : constant Iterator_Type := Back (L);
         
         IV : Index_Subtype;
         IL, JL, KL : Iterator_Type;
         
      begin
      
         Count := 0;
         
         if N = 0 then
            return;
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
            return;
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
               
         JL := IL;
         
         while not Is_Equal (JL, Key) loop
            
            JL := Succ (JL);
            
            if JL = BL then
               return;
            end if;
            
            if Hash (JL, N) /= IV then
               return;
            end if;
            
         end loop;
         
         KL := JL;
                  
         loop
         
            Count := Count + 1;
            
            KL := Succ (KL);
            
            if KL = BL then            
               if JL = IL then
                  Replace_Element (V, Index => IV, By => BL);
               end if;
               
               exit;
            end if;
                        
            if Hash (KL, N) /= IV then
               if JL = IL then
                  Replace_Element (V, Index => IV, By => BL);
               end if;
               
               exit;
            end if;   
            
            if not Is_Equal (KL, Key) then
               if JL = IL then
                  Replace_Element (V, Index => IV, By => KL);
               end if;
               
               exit;
            end if;
            
         end loop;
         
         Delete (L, First => JL, Back => KL);
                     
      end Delete;

      
      
      function Count
        (Hash_Table : Hash_Table_Type;
         Key        : Key_Type) return Natural is

         V : Vector_Subtype renames Hash_Table.V;
         L : List_Subtype renames Hash_Table.L;      

         N : constant Natural := Length (V);
         
         BL : constant Iterator_Type := Back (L);
         
         IV : Index_Subtype;
         IL : Iterator_Type;
         
      begin
               
         if N = 0 then
            return 0;
         end if;
         
         IV := Hash (Key, N);
         IL := Element (V, Index => IV);
         
         if IL = BL then
            return 0;
         end if;
         
         pragma Assert (Hash (IL, N) = IV);
                        
         while not Is_Equal (IL, Key) loop
            
            IL := Succ (IL);
            
            if IL = BL then
               return 0;
            end if;
            
            if Hash (IL, N) /= IV then
               return 0;
            end if;
            
         end loop;
         
         declare
            Result : Integer'Base := 1;
         begin         
            loop            
               IL := Succ (IL);
               
               if IL = BL then            
                  return Result;
               end if;
                           
               if Hash (IL, N) /= IV then
                  return Result;
               end if;   
               
               if not Is_Equal (IL, Key) then
                  return Result;
               end if;
               
               Result := Result + 1;               
            end loop;
         end;
         
      end Count;


   end Generic_Keys;
   

   procedure Rehash
     (L : in List_Subtype;
      V : in Vector_Subtype) is
      
      BV : constant Index_Subtype := Back (V);
      BL : constant Iterator_Type := Back (L);
      
      function Predicate (E : Iterator_Type) return Boolean is
      begin
         return E /= BL;
      end;
               
      function Find is
         new Iterator_Vectors.Generic_Find;
                  
      IL     : Iterator_Type := First (L);
      JL, KL : Iterator_Type;
      
      IV, KV : Index_Subtype;
      
      N : constant Positive := Length (V);

   begin
   
      for X in 1 .. Length (L) loop

         JL := Succ (IL);

         IV := Hash (IL, N);

         KL := Element (V, Index => IV);
         
         if KL = BL then
            KV := Find (V, IV + 1, BV);

            if KV /= BV then
               KL := Element (V, Index => KV);
            end if;
         end if;

         Splice (L, Before => KL, Iterator => IL);
            
         Replace_Element (V, Index => IV, By => IL);
         
         IL := JL;
            
      end loop;
      
   end Rehash;

   
   procedure Resize
     (Hash_Table : in out Hash_Table_Type;
      Length     : in     Natural) is
      
      L : List_Subtype renames Hash_Table.L;
      V : Vector_Subtype renames Hash_Table.V;
      
      Old_Size : constant Natural := Iterator_Vectors.Length (V);
      
      New_Size : Positive;
      
   begin
   
      if Old_Size >= Length then
         return;
      end if;

      New_Size := Prime_Numbers.To_Prime (Length);

      if New_Size <= Old_Size then
         return;
      end if;
            
      declare
         New_V : Vector_Subtype := To_Container (New_Size, Item => Back (L));
      begin
         Rehash (L, New_V);
         Swap (V, New_V);
      end;
      
   end Resize;


   procedure Hash_Range
     (Hash_Table  : in     Hash_Table_Type;
      Index       : in     Natural;
      First, Back :    out Iterator_Type) is
     
      N : constant Positive := Length (Hash_Table.V);
       
      BL : constant Iterator_Type := Hash_Tables.Back (Hash_Table.L);
      
   begin
   
      First := Element (Hash_Table.V, Index);
      Back := First;
      
      if First = BL then
         return;
      end if;
      
      loop
      
         Back := Succ (Back);
         
         if Back = BL then
            return;
         end if;

         if Hash (Back, N) /= Index then
            return;
         end if;
         
      end loop;
      
   end Hash_Range;
      


end Charles.Hash_Tables;
