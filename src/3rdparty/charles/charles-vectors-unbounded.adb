------------------------------------------------------------------------------
--                                                                          --
--                        CHARLES CONTAINER LIBRARY                         --
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
with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Vectors.Unbounded is


   procedure Free is 
      new Ada.Unchecked_Deallocation (Element_Array,
                                      Element_Array_Access);
                                      

   function To_Integer (Index : Index_Type'Base) return Integer'Base is
      pragma Inline (To_Integer);

      F : constant Integer'Base := Index_Type'Pos (Index_Type'First);

      I : constant Integer'Base := Index_Type'Pos (Index);

      Offset : constant Integer'Base := I - F;
      
      Result : constant Integer'Base := 1 + Offset;
      
   begin
      return Result;
   end;

      
   function To_Index (I : Integer'Base) return Index_Type'Base is
      pragma Inline (To_Index);

      Offset : constant Integer'Base := I - 1;
      
      F : constant Integer'Base := Index_Type'Pos (Index_Type'First);
      
      J : constant Integer'Base := F + Offset;
      
      Result : constant Index_Type'Base := Index_Type'Val (J);
      
   begin
      return Result;
   end;


   procedure Adjust (Container : in out Container_Type) is   
   begin
   
      if Container.Elements = null then
         declare
            subtype Array_Subtype is
               Element_Array (Index_Type'First .. Last_Subtype'First);
         begin
            Container.Elements := new Array_Subtype;
         end;
         
         return;
      end if;            

      declare
         Source : Element_Array renames 
            Container.Elements (Index_Type'First .. Container.Last);
      begin
         Container.Elements := new Element_Array'(Source);
      exception
         when others =>
            Container.Elements := null;
            Container.Last := Last_Subtype'First;
            raise;
      end;            
      
   end Adjust;
   

   procedure Finalize (Container : in out Container_Type) is
   begin
      
      Container.Last := Last_Subtype'First;

      begin
         Free (Container.Elements);
      exception
         when others =>
            Container.Elements := null;
            raise;
      end;

   end Finalize;
      

   function "=" (Left, Right : Container_Type) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;
      
      if Left.Last /= Right.Last then
         return False;
      end if;
      
      for I in Index_Type'First .. Left.Last loop
         
         if Left.Elements (I) /= Right.Elements (I) then
            return False;
         end if;
         
      end loop;
      
      return True;
   end;
   
   
   function Generic_Less
     (Left, Right : Container_Type) return Boolean is

      function Is_Less (LI, RI : Index_Type'Base) return Boolean is
         pragma Inline (Is_Less);
      begin
         return Left.Elements (LI) < Right.Elements (RI);
      end;

      function Lexicographical_Compare is
         new Charles.Algorithms.Generic_Lexicographical_Compare 
           (Iterator_Type => Index_Type'Base,
            Succ          => Index_Type'Succ);

   begin -- Generic_Less

      if Left'Address = Right'Address then
         return False;
      end if;

      if Left.Last > Right.Last then
         return False;
      end if;

      return Lexicographical_Compare 
               (Left_First  => Index_Type'First, 
                Left_Back   => Index_Type'Succ (Left.Last),
                Right_First => Index_Type'First,
                Right_Back  => Index_Type'Succ (Right.Last));

   end Generic_Less;
   
   
--   package Element_Array_Access_Conversions is
--      new System.Address_To_Access_Conversions (Container_Type);
   
   function To_Access 
     (Container : Container_Type) return Element_Array_Access is

   begin

--      if Container.Elements = null then      

--         declare
--            use Element_Array_Access_Conversions;         

--            CP : constant Object_Pointer := To_Pointer (Container'Address);
--            
--            C : Container_Type renames CP.all;
--            
--            subtype Array_Subtype is
--               Element_Array (Index_Type'First .. Last_Subtype'First);
--         begin
--            C.Elements := new Array_Subtype;
--         end;

--      end if;
         
      return Container.Elements;      

   end To_Access;


   function Length (Container : Container_Type) return Natural is
   begin
      return To_Integer (Container.Last);
   end;
   
   
   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Last = Last_Subtype'First;
   end;
   
   
   procedure Clear (Container : in out Container_Type) is
   begin
      Container.Last := Last_Subtype'First;
   end;
      

   procedure Clear 
     (Container : in out Container_Type;
      Item      : in     Element_Type) is
      
      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Container.Elements /= null then
         Container.Elements (Range_Subtype) := (others => Item);         
      end if;
      
      Container.Last := Last_Subtype'First;
   end Clear;

   
   procedure Swap (Left, Right : in out Container_Type) is
   
      L_EA : constant Element_Array_Access := Left.Elements;
      L_Last : constant Last_Subtype := Left.Last;

   begin
   
      Left.Elements := Right.Elements;
      Left.Last := Right.Last;
      
      Right.Elements := L_EA;
      Right.Last := L_Last;

   end Swap;
      
      
   function To_Container (Length : Natural) return Container_Type is
   
      Last : constant Last_Subtype := To_Index (Length);
      
      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Last;

      EA : constant Element_Array_Access :=
         new Element_Array (Range_Subtype);
   begin
      return (Ada.Finalization.Controlled with EA, Last);
   end;


   function To_Container 
     (Length : Natural;
      Item   : Element_Type) return Container_Type is
      
      Last : constant Last_Subtype := To_Index (Length);
      
      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Last;
         
      subtype Array_Subtype is
         Element_Array (Range_Subtype);
         
      EA : constant Element_Array_Access :=
         new Array_Subtype'(others => Item);
   begin
      return (Ada.Finalization.Controlled with EA, Last);
   end;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
      
      Last : constant Last_Subtype := To_Index (Length);
         
   begin
   
      if Target.Elements /= null
        and then Target.Elements'Length >= Length
      then
         Target.Last := Last;
         return;
      end if;

      declare
         EA : constant Element_Array_Access := 
            new Element_Array (Index_Type'First .. Last);
      begin
         begin
            Free (Target.Elements);
         exception
            when others =>
               Target.Elements := null;
               Target.Last := Last_Subtype'First;
               raise;
         end;
    
         Target.Elements := EA;
         Target.Last := Last;
      end;

   end Assign;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type) is
      
      Last : constant Last_Subtype := To_Index (Length);
      
      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Last;

   begin
   
      if Target.Elements /= null
        and then Target.Elements'Length >= Length
      then
         --must do the array assignment first, to protect against errors
         Target.Elements (Range_Subtype) := (others => Item);
         Target.Last := Last;

         return;
      end if;
      
      declare
         EA : constant Element_Array_Access := 
            new Element_Array'(Range_Subtype => Item);
      begin
         begin
            Free (Target.Elements);
         exception
            when others =>
               Target.Elements := null;
               Target.Last := Last_Subtype'First;
               raise;
         end;
    
         Target.Elements := EA;
         Target.Last := Last;
      end;   
   
   end Assign;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;
      
      if Source.Elements = null then
         if Target.Elements = null then
            declare
               subtype Array_Subtype is
                  Element_Array (Index_Type'First .. Last_Subtype'First);
            begin
               Target.Elements := new Array_Subtype;
            end;
         end if;
         
         Target.Last := Last_Subtype'First;

         return;
      end if;
      
      if Target.Elements /= null
         and then To_Integer (Source.Last) <= Target.Elements'Length
      then
         --must perform the assignment first
         declare
            subtype Range_Subtype is Index_Type'Base range 
               Index_Type'First .. Source.Last;

            Tgt : Element_Array renames 
               Target.Elements (Range_Subtype);

            Src : Element_Array renames
               Source.Elements (Range_Subtype);
         begin
            Tgt := Src;
         end;
         
         Target.Last := Source.Last;
         
         return;
      end if;
      
      declare
         subtype Range_Subtype is Index_Type'Base range
            Index_Type'First .. Source.Last;

         X : Element_Array renames 
            Source.Elements (Range_Subtype);

         EA : constant Element_Array_Access :=
            new Element_Array'(X);
      begin
         begin
            Free (Target.Elements);
         exception
            when others =>
               Target.Elements := null;
               Target.Last := Last_Subtype'First;
               raise;
         end;
         
         Target.Elements := EA;
         Target.Last := Source.Last;
      end;
   end Assign;




   procedure Append 
     (Container : in out Container_Type) is
     
      New_Last : constant Index_Type := Index_Type'Succ (Container.Last);
      New_Length : constant Positive := To_Integer (New_Last);
      
      Size : Positive;
      
   begin
      
      if Container.Elements = null then
         pragma Assert (New_Last = Index_Type'First);
         
         declare            
            subtype Array_Subtype is
               Element_Array (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Array_Subtype;
            Container.Last := New_Last;
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= New_Length then
         Container.Last := New_Last;
         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1); --?

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype Range_Subtype is Index_Type range
               Index_Type'First .. Container.Last;
               
            Source : Element_Array renames
               Container.Elements (Range_Subtype);
         begin
            EA (Range_Subtype) := Source;
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;               

   end Append;


   function Append 
     (Container : access Container_Type) return Index_Type is
   begin
      Append (Container.all);
      return Last (Container.all);
   end;


   function Generic_Append 
     (Container : access Container_Type) return Element_Access is
     
      Index : constant Index_Type := Append (Container);
   begin
      return Container.Elements (Index)'Access;
   end;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is

      New_Last : constant Index_Type := Index_Type'Succ (Container.Last);
      New_Length : constant Positive := To_Integer (New_Last);
      
      Size : Positive;

   begin
      
      if Container.Elements = null then
         pragma Assert (New_Last = Index_Type'First);
         
         declare            
            subtype Array_Subtype is
               Element_Array (Index_Type'First .. Index_Type'First);
         begin
            Container.Elements := 
               new Array_Subtype'(others => New_Item);
               
            Container.Last := New_Last;
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= New_Length then
         --must do assignment first
         Container.Elements (New_Last) := New_Item;
         Container.Last := New_Last;

         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1);

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype Range_Subtype is Index_Type range
               Index_Type'First .. Container.Last;
               
            Source : Element_Array renames
               Container.Elements (Range_Subtype);
         begin
            EA (Range_Subtype) := Source;
            EA (New_Last) := New_Item;
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;               

   end Append;

      
   procedure Append_N
     (Container : in out Container_Type;
      Count     : in     Natural) is

      Old_Length : constant Natural := To_Integer (Container.Last);
      
      New_Length : constant Natural := Old_Length + Count;
      New_Last : constant Last_Subtype := To_Index (New_Length);
      
      Size : Positive;
      
   begin
      
      if Container.Elements = null then
         pragma Assert (Container.Last = Last_Subtype'First);
         
         declare            
            subtype Array_Subtype is
               Element_Array (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Array_Subtype;
            Container.Last := New_Last;
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= New_Length then
         Container.Last := New_Last;
         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1); --?

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype Range_Subtype is Index_Type range
               Index_Type'First .. Container.Last;
               
            Source : Element_Array renames
               Container.Elements (Range_Subtype);
         begin
            EA (Range_Subtype) := Source;
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;               

   end Append_N;
      

   
   procedure Append_N
     (Container : in out Container_Type;
      Count     : in     Natural;
      New_Item  : in     Element_Type) is

      Old_Length : constant Natural := To_Integer (Container.Last);
      
      New_Length : constant Natural := Old_Length + Count;
      New_Last : constant Last_Subtype := To_Index (New_Length);
      
      Size : Positive;
      
   begin
      
      if Container.Elements = null then
         pragma Assert (Container.Last = Last_Subtype'First);
         
         declare            
            subtype Array_Subtype is
               Element_Array (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Array_Subtype'(others => New_Item);
            Container.Last := New_Last;
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= New_Length then
         declare
            subtype Range_Subtype is Index_Type range 
               Index_Type'Succ (Container.Last) .. New_Last;
               
            Target : Element_Array renames
               Container.Elements (Range_Subtype);
         begin
            Target := (others => New_Item);
         end;
            
         Container.Last := New_Last;

         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1); --?

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype Range_Subtype is Index_Type range
               Index_Type'First .. Container.Last;
               
            Source : Element_Array renames
               Container.Elements (Range_Subtype);
         begin
            EA (Range_Subtype) := Source;
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         declare
            subtype Range_Subtype is Index_Type range 
               Index_Type'Succ (Container.Last) .. New_Last;
         begin
            EA (Range_Subtype) := (others => New_Item);
         exception
            when others =>
               Free (EA);
               raise;               
         end;

         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;               

   end Append_N;
   


   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Container.Last := Index_Type'Pred (Container.Last);
   end;


   procedure Delete_Last 
     (Container : in out Container_Type;
      Item      : in     Element_Type) is

      Old_Last : constant Index_Type := Container.Last;      
      New_Last : constant Last_Subtype := Index_Type'Pred (Old_Last);
   begin
      Container.Elements (Old_Last) := Item;
      Container.Last := New_Last;
   end;
   
   
   procedure Delete_Last_N
     (Container : in out Container_Type;
      Count     : in     Natural) is
   begin
      Container.Last := To_Index (To_Integer (Container.Last) - Count);
   end;


   procedure Delete_Last_N
     (Container : in out Container_Type;
      Count     : in     Natural;
      Item      : in     Element_Type) is
      
      Old_Last : constant Last_Subtype := Container.Last;
      Old_Length : constant Natural := To_Integer (Old_Last);
      
      New_Length : constant Natural := Old_Length - Count;
      New_Last : constant Last_Subtype := To_Index (New_Length);
      
      J : constant Positive := Old_Length - Count + 1;
      K : constant Index_Type'Base := To_Index (J);
   begin
      Container.Elements (K .. Old_Last) := (others => Item);
      Container.Last := New_Last;
   end;

   
   procedure Insert_N
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Count     : in     Natural) is

      Old_Last : constant Last_Subtype := Container.Last;
      Old_Length : constant Natural := To_Integer (Old_Last);
      
      New_Length : constant Natural := Old_Length + Count;
      New_Last : constant Last_Subtype := To_Index (New_Length);
      
      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Index_Type'Succ (Old_Last);
         
      Size : Positive;

   begin
   
      if Container.Elements = null then
         pragma Assert (Old_Last = Last_Subtype'First);
         
         declare
            First : constant Index_Subtype := Before;
            
            subtype Array_Subtype is 
               Element_Array (First .. New_Last);
         begin
            Container.Elements := new Array_Subtype;
            Container.Last := New_Last;
         end;         
         
         return;
      end if;
      
      if Count = 0 then
         return;
      end if;
      
      if Container.Elements'Length >= New_Length then
         declare
            Index : constant Index_Subtype := Before;
            
            First : constant Index_Type'Base := 
               To_Index (To_Integer (Index) + Count);
         begin
            Container.Elements (First .. New_Last) := 
                Container.Elements (Index .. Old_Last);

            Container.Last := New_Last;
         end;
                
         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1); --?

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         Index : constant Index_Subtype := Before;

         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype Range_Subtype is Index_Type'Base range
               Index_Type'First .. Index_Type'Pred (Index);
         begin
            EA (Range_Subtype) := Container.Elements (Range_Subtype);
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         declare
            First : constant Index_Type'Base :=
               To_Index (To_Integer (Index) + Count);
         begin
            EA (First .. New_Last) := 
               Container.Elements (Index .. Old_Last);
         exception
            when others =>
               Free (EA);
               raise;
         end;

         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;                     
      
   end Insert_N;

      
   procedure Insert_N
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Count     : in     Natural;
      New_Item  : in     Element_Type) is


      Old_Last : constant Last_Subtype := Container.Last;
      Old_Length : constant Natural := To_Integer (Old_Last);
      
      New_Length : constant Natural := Old_Length + Count;
      New_Last : constant Last_Subtype := To_Index (New_Length);
      
      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Index_Type'Succ (Old_Last);
         
      Size : Positive;

   begin
   
      if Container.Elements = null then
         pragma Assert (Old_Last = Last_Subtype'First);
         
         declare
            First : constant Index_Subtype := Before;

            subtype Array_Subtype is 
               Element_Array (First .. New_Last);
         begin
            Container.Elements := new Array_Subtype'(others => New_Item);
            Container.Last := New_Last;
         end;         
         
         return;
      end if;
      
      if Count = 0 then
         return;
      end if;

      if Container.Elements'Length >= New_Length then
         declare
            Index : constant Index_Subtype := Before;
      
            subtype New_Items_Array_Subtype is
               Element_Array (Index_Type'First .. To_Index (Count));
         begin
            Container.Elements (Index .. New_Last) := 
                New_Items_Array_Subtype'(others => New_Item) & 
                Container.Elements (Index .. Old_Last);

            Container.Last := New_Last;
         end;
                
         return;
      end if;
      
      Size := 2 * Integer'Max (Container.Elements'Length, 1); --?

      declare
         Length : constant Positive := To_Integer (Index_Type'Last);
      begin
         if Size > Length then
            Size := Length;
         end if;
      end;
      
      declare
         Index : constant Index_Subtype := Before;
      
         subtype Array_Subtype is
            Element_Array (Index_Type'First .. To_Index (Size));
            
         EA : Element_Array_Access := new Array_Subtype;
      begin
         declare
            subtype New_Items_Array_Subtype is
               Element_Array (Index_Type'First .. To_Index (Count));
               
            Src : Element_Array renames Container.Elements.all;
         begin
            EA (Index_Type'First .. New_Last) :=
               Src (Index_Type'First .. Index_Type'Pred (Index)) &
               New_Items_Array_Subtype'(others => New_Item) &
               Src (Index .. Old_Last);
         exception
            when others =>
               Free (EA);
               raise;
         end;
         
         begin
            Free (Container.Elements);
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;
            
         Container.Elements := EA;
         Container.Last := New_Last;
      end;                     
      
   end Insert_N;



   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type) is
   begin
      Insert_N (Container, Before, Count => 1);  
   end;
   
   
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert_N (Container, Before, Count => 1, New_Item => New_Item);
   end;
      

   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Index_Type) is
         
      Old_Last : constant Index_Type := Container.Last;
      New_Last : constant Last_Subtype := Index_Type'Pred (Old_Last);

      EA : Element_Array renames Container.Elements.all;

      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Old_Last;
   begin
      EA (Index_Subtype'(Index) .. New_Last) := 
         EA (Index_Type'Succ (Index) .. Old_Last);
         
      Container.Last := New_Last;
   end;

            
   procedure Do_Delete
     (Container : in out Container_Type;
      F, B      : in     Index_Type'Base) is

      N : constant Positive := Index_Type'Pos (B) - Index_Type'Pos (F);

      Old_Last : constant Index_Type := Container.Last;
      Old_Length : constant Positive := To_Integer (Old_Last);
      
      New_Length : constant Natural := Old_Length - N;
      New_Last : constant Last_Subtype := To_Index (New_Length);

      EA : Element_Array renames Container.Elements.all;
   begin      
      EA (F .. New_Last) := EA (B .. Old_Last); 
      Container.Last := New_Last;
   end;
   
   


   procedure Delete
     (Container : in out Container_Type; 
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base) is

   begin

      if Back <= First then
         return;
      end if;

      declare
         subtype First_Subtype is Index_Type'Base range
            Index_Type'First .. Container.Last;
            
         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);            
      begin
         Do_Delete (Container, First_Subtype'(First), Back_Subtype'(Back));
      end;

   end Delete;
   

   procedure Delete_N
     (Container : in out Container_Type;
      First     : in     Index_Type'Base;
      Count     : in     Natural) is

   begin
   
      if Count = 0 then
         return;
      end if;

      declare
         subtype First_Subtype is Index_Type'Base range
            Index_Type'First .. Container.Last;
            
         F : constant Positive := To_Integer (First_Subtype'(First));

         BX : constant Positive := F + Count;
         BY : constant Positive := To_Integer (Container.Last) + 1;
         B  : constant Positive := Integer'Min (BX, BY);
      begin
         Do_Delete (Container, First, To_Index (B));
      end;
               
   end Delete_N;


   procedure Do_Delete
     (Container : in out Container_Type;
      F, B      : in     Index_Type'Base;
      Item      : in     Element_Type) is

      N : constant Positive := Index_Type'Pos (B) - Index_Type'Pos (F);
      
      Old_Last : constant Index_Type := Container.Last;
      Old_Length : constant Positive := To_Integer (Old_Last);

      New_Length : constant Natural := Old_Length - N;
      New_Last : constant Last_Subtype := To_Index (New_Length);

      EA : Element_Array renames Container.Elements.all;
   begin      
      EA (F .. New_Last) := EA (B .. Old_Last);
      EA (Index_Type'Succ (New_Last) .. Old_Last) := (others => Item);

      Container.Last := New_Last;
   end;
   
   


   procedure Delete
     (Container : in out Container_Type; 
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base;
      Item      : in     Element_Type) is

   begin

      if Back <= First then
         return;
      end if;

      declare
         subtype First_Subtype is Index_Type'Base range
            Index_Type'First .. Container.Last;
            
         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);
      begin
         Do_Delete 
           (Container, 
            First_Subtype'(First), 
            Back_Subtype'(Back), 
            Item);
      end;

   end Delete;
   

   procedure Delete_N
     (Container : in out Container_Type;
      First     : in     Index_Type'Base;
      Count     : in     Natural;
      Item      : in     Element_Type) is

   begin
   
      if Count = 0 then
         return;
      end if;

      declare
         subtype First_Subtype is Index_Type'Base range
            Index_Type'First .. Container.Last;
            
         F : constant Positive := To_Integer (First_Subtype'(First));

         BX : constant Positive := F + Count;         
         BY : constant Positive := To_Integer (Container.Last) + 1;
         B  : constant Positive := Integer'Min (BX, BY);
      begin
         Do_Delete (Container, First, To_Index (B), Item);
      end;
               
   end Delete_N;


   function Size (Container : Container_Type) return Natural is
   begin
      if Container.Elements = null then
         return 0;
      else
         return Container.Elements'Length;
      end if;
   end;
   

   procedure Resize
     (Container : in out Container_Type;
      Size      : in     Natural) is      

   begin
   
      if Container.Elements = null then  
         pragma Assert (Container.Last = Last_Subtype'First);
    
         declare
            Last : constant Last_Subtype := To_Index (Size);
         begin
            Container.Elements := 
               new Element_Array (Index_Type'First .. Last);
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= Size then
         return;
      end if;

      declare
         Last : constant Index_Type := To_Index (Size);

         EA : Element_Array_Access :=
            new Element_Array (Index_Type'First .. Last);
            
         Target : Element_Array renames
            EA (Index_Type'First .. Container.Last);               

         Source : Element_Array renames
            Container.Elements (Index_Type'First .. Container.Last);
      begin      
         begin
            Target := Source;
         exception
            when others =>
               Free (EA);
               raise;
         end;

         begin 
            Free (Container.Elements);   
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;

         Container.Elements := EA;         
      end;
      
   end Resize;
   

   procedure Resize
     (Container : in out Container_Type;
      Size      : in     Natural;
      Item      : in     Element_Type) is

   begin
   
      if Container.Elements = null then  
         pragma Assert (Container.Last = Last_Subtype'First);
    
         declare
            Last : constant Last_Subtype := To_Index (Size);
            
            subtype Array_Subtype is 
               Element_Array (Index_Type'First .. Last);
         begin
            Container.Elements := new Array_Subtype'(others => Item);
         end;
         
         return;
      end if;
      
      if Container.Elements'Length >= Size then
         return;
      end if;

      declare
         Last : constant Index_Type := To_Index (Size);

         Source : Element_Array renames
            Container.Elements (Index_Type'First .. Container.Last);
            
         subtype Array_Subtype is
            Element_Array (Index_Type'Succ (Container.Last) .. Last);

         EA : Element_Array_Access :=
            new Element_Array'(Source & Array_Subtype'(others => Item));
      begin      
         begin 
            Free (Container.Elements);   
         exception
            when others =>
               Container.Elements := null;
               Container.Last := Last_Subtype'First;
               Free (EA);
               raise;
         end;

         Container.Elements := EA;         
      end;
      
   end Resize;


   function First 
     (Container : Container_Type) return Index_Type is
      pragma Warnings (Off, Container);
   begin
      return Index_Type'First;
   end;


   function Front 
     (Container : Container_Type) return Index_Type'Base is
      pragma Warnings (Off, Container);
   begin
      return Index_Type'Pred (Index_Type'First);
   end;


   function Last
     (Container : Container_Type) return Index_Type'Base is
   begin
      return Container.Last;
   end;
      

   function Back 
     (Container : Container_Type) return Index_Type'Base is
   begin   
      return Index_Type'Succ (Container.Last);
   end;
   

   function Element 
     (Container : Container_Type;
      Index     : Index_Type) return Element_Type is
      
      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      return Container.Elements (Index_Subtype'(Index));
   end;
      
   
   function Generic_Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Access is
      
      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      return Container.Elements (Index_Subtype'(Index))'Access;
   end;
      

   procedure Replace_Element
     (Container : in Container_Type;
      Index     : in Index_Type;
      By        : in Element_Type) is
      
      subtype Index_Subtype is Index_Type'Base range 
         Index_Type'First .. Container.Last;
   begin
      Container.Elements (Index_Subtype'(Index)) := By;
   end;
      

   procedure Copy_Element
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      :    out Element_Type) is

      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      Item := Container.Elements (Index_Subtype'(Index));
   end;


   procedure Swap_Element
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
      
      EA : Element_Array renames Container.Elements.all;
      
      E : constant Element_Type := EA (Index_Subtype'(Index));
   begin
      EA (Index_Subtype'(Index)) := Item;
      Item := E;
   end;


   procedure Generic_Swap_Element
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is
      
      subtype Index_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Last;
         
      EA : Element_Array renames Container.Elements.all;
      
      E : Element_Type renames EA (Index_Subtype'(Index));      
   begin
      Swap (E, Item);
   end;



   procedure Swap
     (Container   : in Container_Type;
      Left, Right : in Index_Type) is
      
      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
      
      EA : Element_Array renames Container.Elements.all;
      
      LE : constant Element_Type := EA (T'(Left));      
   begin
      EA (Left) := EA (T'(Right));
      EA (Right) := LE;
   end;
   
      
      
   procedure Generic_Swap
     (Container   : in Container_Type;
      Left, Right : in Index_Type) is
      
      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   
      EA : Element_Array renames Container.Elements.all;      
   begin
      Swap (EA (T'(Left)), EA (T'(Right)));
   end;



   procedure Generic_Select_Element
     (Container : in Container_Type;
      Index     : in Index_Type) is
      
      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      Process (Container.Elements (T'(Index)));
   end;
     

   procedure Generic_Modify_Element
     (Container : in Container_Type;
      Index     : in Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      Process (Container.Elements (T'(Index)));
   end;
     

   procedure Generic_Access_Element
     (Container : in Container_Type;
      Index     : in Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      Process (Container.Elements (T'(Index))'Access);
   end;
     
     
   procedure Generic_Iteration
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is      

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;

      for I in T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container, I);
      end loop;
   end;
     
     
   procedure Generic_Reverse_Iteration
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in reverse T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container, I);
      end loop;
   end;


   procedure Generic_Select_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I));
      end loop;
   end;
         

   procedure Generic_Modify_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I));
      end loop;
   end;
      
         

   procedure Generic_Access_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is
      
      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I)'Access);
      end loop;
   end;
         

   procedure Generic_Reverse_Select_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in reverse T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I));
      end loop;
   end;


   procedure Generic_Reverse_Modify_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in reverse T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I));
      end loop;
   end;



   procedure Generic_Reverse_Access_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return;
      end if;
      
      for I in reverse T'(First) .. T'(Index_Type'Pred (Back)) loop
         Process (Container.Elements (I)'Access);
      end loop;
   end;



   function Generic_Find 
     (Container : Container_Type;
      First     : Index_Type'Base;
      Back      : Index_Type'Base) return Index_Type'Base is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return Back;
      end if;
      
      for I in T'(First) .. T'(Index_Type'Pred (Back)) loop
         if Predicate (Container.Elements (I)) then
            return I;
         end if;
      end loop;
         
      return Back;
   end;
   
      
   function Find 
     (Container : Container_Type;
      First     : Index_Type'Base;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base is
      
      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;
   
      function Find is 
         new Generic_Find (Predicate);
   begin
      return Find (Container, First, Back);
   end;
      
      
   function Find 
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base is
   begin
      return Find (Container,
                   First (Container),
                   Back (Container),
                   Item);
   end;
   
      
   function Generic_Reverse_Find 
     (Container : Container_Type;
      First     : Index_Type'Base;
      Back      : Index_Type'Base) return Index_Type'Base is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      if Back <= First then
         return Back;
      end if;

      for I in reverse T'(First) .. T'(Index_Type'Pred (Back)) loop
         if Predicate (Container.Elements (I)) then
            return I;
         end if;
      end loop;
      
      return Back;
   end;


   function Reverse_Find 
     (Container : Container_Type;
      First     : Index_Type'Base;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base is
      
      function Predicate (Element : Element_Type) return Boolean is
      begin
         return Item = Element;
      end;
      
      function Reverse_Find is
         new Generic_Reverse_Find (Predicate);
   begin
      return Reverse_Find (Container, First, Back);
   end;
         
      
   function Reverse_Find 
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base is
   begin
      return Reverse_Find (Container,
                           First (Container),
                           Back (Container),
                           Item);
   end;


end Charles.Vectors.Unbounded;
