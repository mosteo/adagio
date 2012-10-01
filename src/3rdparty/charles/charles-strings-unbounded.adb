------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--                   Charles.Strings.Unbounded (body)                       --
--                                                                          --
--                                                                          --
--              Copyright (C) 2001-2002 Matthew J Heaney                    --
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

package body Charles.Strings.Unbounded is

   use Ada.Finalization;


   function To_New_Size 
     (Old_Size   : String_Access;
      New_Length : Natural) return Positive is
      
      New_Size : Positive := Integer'Max (1, Old_Size'Length);
   begin
      while New_Size < New_Length loop
         New_Size := 2 * New_Size;
      end loop;
      
      return New_Size;
   end;
      
   

   procedure Free (X : in out String_Access) is
      procedure Do_Free is
         new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      if X = Null_String_Access then      
         X := null;
      else
         Do_Free (X);
      end if;
   end Free;
   

   procedure Adjust (Container : in out Container_Type) is

      EA_Src : constant String_Access := Container.Elements;

   begin

      Container.Elements := new String'(EA_Src (1 .. Container.Length));
      
   exception
      when others =>
         Container.Elements := Null_String_Access;      
         Container.Length := 0;  

         raise;

   end Adjust;
   

   procedure Finalize (Container : in out Container_Type) is
   begin
      Free (Container.Elements);
   end;
   

   function "=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) = 
               Right.Elements (1 .. Right.Length);      
   end;      
      
   
   function "=" 
      (Left  : Container_Type;
       Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) = Right;
   end;
   

   function "=" 
      (Left  : String;
       Right : Container_Type) return Boolean is
   begin
      return Right = Left;
   end;



   function "<" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) <
               Right.Elements (1 .. Right.Length);
   end;
   

   function "<" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) < Right;
   end;
   

   function "<" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left < Right.Elements (1 .. Right.Length);
   end;


   function "<=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) <=
               Right.Elements (1 .. Right.Length);
   end;
   

   function "<=" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) <= Right;
   end;
   

   function "<=" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left <= Right.Elements (1 .. Right.Length);
   end;


   function ">=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) >=
               Right.Elements (1 .. Right.Length);
   end;
   

   function ">=" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Length) >= Right;
   end;
   

   function ">=" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left >= Right.Elements (1 .. Right.Length);
   end;


   function "&" (Left, Right : Container_Type) return Container_Type is

      N : constant Natural := Left.Length + Right.Length;
      
      subtype String_Subtype is String (1 .. N);
      
      LS : String renames Left.Elements (1 .. Left.Length);
      RS : String renames Right.Elements (1 .. Right.Length);

      S : constant String_Access := new String_Subtype'(LS & RS);
   begin
      return Container_Type'(Controlled with S, N);
   end;
      
   
   function "&" 
     (Left  : Container_Type;
      Right : String) return Container_Type is
      
      N : constant Natural := Left.Length + Right'Length;
      
      subtype String_Subtype is String (1 .. N);
      
      LS : String renames Left.Elements (1 .. Left.Length);

      S : constant String_Access := new String_Subtype'(LS & Right);
   begin
      return Container_Type'(Controlled with S, N);
   end;
      
      
   function "&" 
     (Left  : String;
      Right : Container_Type) return Container_Type is
      
      N : constant Natural := Left'Length + Right.Length;
      
      subtype String_Subtype is String (1 .. N);
      
      RS : String renames Right.Elements (1 .. Right.Length);

      S : constant String_Access := new String_Subtype'(Left & RS);
   begin
      return Container_Type'(Controlled with S, N);
   end;
      

   function "&" 
     (Left  : Container_Type;
      Right : Character) return Container_Type is
   begin
      return Left & String'(1 .. 1 => Right);
   end;
   
      
   function "&" 
     (Left  : Character;
      Right : Container_Type) return Container_Type is
   begin
      return String'(1 .. 1 => Left) & Right;
   end;



   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Length;
   end;
   

   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Length = 0;
   end;
   

   procedure Clear (Container : in out Container_Type) is
   begin   
      Container.Length := 0;
   end;


   procedure Swap (Left, Right : in out Container_Type) is
   
      L_EA : constant String_Access := Left.Elements;
      L_Length : constant Natural := Left.Length;

   begin
   
      Left.Elements := Right.Elements;
      Left.Length := Right.Length;
      
      Right.Elements := L_EA;
      Right.Length := L_Length;

   end Swap;
      
      
   function To_Container 
     (Length : Natural) return Container_Type is
     
      EA : constant String_Access := new String (1 .. Length);
   begin
      return (Controlled with EA, EA'Length);
   end;
         
         
   function To_Container 
     (Source : String) return Container_Type is
     
      subtype String_Subtype is String (1 .. Source'Length);
     
      EA : constant String_Access := new String_Subtype'(Source);
   begin
      return (Controlled with EA, EA'Length);
   end;
   

   function To_Container
     (Source : Character;
      Count  : Natural := 1) return Container_Type is
   begin
      return To_Container (String'(1 .. Count => Source));
   end;
         

   function To_Container 
     (Source : Container_Type;
      Index  : Positive;
      Length : Natural) return Container_Type is
   begin
      if Length = 0 then
         return (Controlled with Null_String_Access, 0);
      end if;
      
      declare
         I : constant Positive range 1 .. Source.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Source.Length then
            J := Source.Length;
         end if;
         
         declare
            Src : String renames Source.Elements (I .. J);

            subtype String_Subtype is String (1 .. Src'Length);

            EA : constant String_Access := new String_Subtype'(Src);
         begin
            return (Controlled with EA, EA'Length);
         end;
      end;
   end To_Container;
            
               
   function To_Container 
     (Source : Container_Type) return Container_Type is
   begin
      return To_Container (Source, 1, Source.Length);
   end;



   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
   begin
      if Target.Elements'Length < Length then
         declare
            EA : constant String_Access := new String (1 .. Length);
         begin            
            Free (Target.Elements);
            Target.Elements := EA;
         end;
      end if;
      
      Target.Length := Length;
   end Assign;
      

   procedure Assign
     (Target : in out Container_Type;
      Source : in     String) is
   begin
      if Target.Elements'Length < Source'Length then

         declare
            subtype String_Subtype is String (1 .. Source'Length);            
            EA : constant String_Access := new String_Subtype'(Source);
         begin
            Free (Target.Elements);
            Target.Elements := EA;
         end;

      else

         Target.Elements (1 .. Source'Length) := Source;

      end if;
      
      Target.Length := Source'Length;
   end Assign;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Character;
      Count  : in     Natural := 1) is
   begin
      Assign (Target, String'(1 .. Count => Source));
   end;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;
      
      Assign (Target, String'(Source.Elements (1 .. Source.Length)));
   end;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type;
      Index  : in     Positive;
      Length : in     Natural) is
   begin
      if Length = 0 then
         Target.Length := 0;
      else
         declare
            I : constant Positive range 1 .. Source.Length := Index;
            J : Positive := I + Length - 1;
         begin
            if J > Source.Length then
               J := Source.Length;
            end if;
            
            Assign (Target, String'(Source.Elements (I .. J)));
         end;
      end if;         
   end;


   procedure Assign_Slice
     (Target : in out Container_Type;
      Source : in     Container_Type;
      Low    : in     Positive;
      High   : in     Natural) is
   begin
      if Low > High then
         Target.Length := 0;
      else
         declare
            subtype Index_Subtype is Positive range 1 .. Source.Length;
            I : constant Index_Subtype := Low;
            J : constant Index_Subtype := High;
         begin
            Assign (Target, String'(Source.Elements (I .. J)));
         end;
      end if;
   end;


   function Size (Container : Container_Type) return Natural is
   begin
      return Container.Elements'Length;
   end;
   
   
   procedure Resize
     (Container : in out Container_Type;
      Size      : in     Natural) is
   begin
      if Size <= Container.Elements'Length then
         return;
      end if;
      
      declare
         EA : constant String_Access := new String (1 .. Size);

         Dst : String renames EA (1 .. Container.Length);
         Src : String renames Container.Elements (1 .. Container.Length);
      begin
         Dst := Src;

         Free (Container.Elements);
         Container.Elements := EA;
      end;
   end Resize;

      
   function Front 
     (Container : Container_Type) return Natural is
   begin
      return 0;
   end;
   

   function First 
     (Container : Container_Type) return Positive is
   begin
      return 1;
   end;
   

   function Last
     (Container : Container_Type) return Natural is
   begin
      return Container.Length;
   end;
   

   function Back 
     (Container : Container_Type) return Positive is
   begin
      return Container.Length + 1;
   end;
   

   function Element 
     (Container : Container_Type;
      Index     : Positive) return Character is
      
      subtype Length_Subtype is
         Positive range 1 .. Container.Length;
   begin
      return Container.Elements (Length_Subtype'(Index));
   end;
   
      
   procedure Replace_Element
     (Container : in Container_Type;
      Index     : in Positive;
      By        : in Character) is
      
      subtype Length_Subtype is
         Positive range 1 .. Container.Length;
   begin
      Container.Elements (Length_Subtype'(Index)) := By;   
   end;


   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural) is
   begin
      if Length = 0 then
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. Container.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Length then
            J := Container.Length;
         end if;
         
         declare
            N : constant Positive := J - I + 1;
         
            Dst : String renames 
               Container.Elements (I .. Container.Length - N);
               
            Src : String renames 
               Container.Elements (J + 1 .. Container.Length);
         begin
            Dst := Src;
         end;            
      end;
   end Delete;
   
                  
   procedure Delete_Slice
     (Container : in out Container_Type;
      From      : in     Positive;
      Through   : in     Natural) is
   begin
      if From > Through then
         return;
      end if;
      
      declare
         subtype Index_Subtype is Positive range 1 .. Container.Length;

         I : constant Index_Subtype := From;
         J : constant Index_Subtype := Through;

         N : constant Positive := J - I + 1;
         
         Dst : String renames
            Container.Elements (I .. Container.Length - N);
            
         Src : String renames
            Container.Elements (J + 1 .. Container.Length);
      begin
         Dst := Src;
      end;               
   end Delete_Slice;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     String) is
   begin
      if New_Item'Length = 0 then
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. Container.Length + 1 := Before;
         N : constant Positive := Container.Length + New_Item'Length;
      begin
         if N <= Container.Elements'Length then
            
            Container.Elements (I .. N) :=
               New_Item & 
               Container.Elements (I .. Container.Length);
               
         else
         
            declare
               New_Size : constant Positive := 
                  To_New_Size (Container.Elements, N);
                  
               S : constant String_Access := new String (1 .. New_Size);
            begin
               S (1 .. I - 1) := 
                  Container.Elements (1 .. I - 1);
                  
               S (I .. I + New_Item'Length - 1) := New_Item;

               S (I + New_Item'Length .. N) := 
                  Container.Elements (I .. Container.Length);
                  
               Free (Container.Elements);
               Container.Elements := S;
            end;
            
         end if;
         
         Container.Length := N;
      end;
   end Insert;
     

   procedure Do_Replace 
     (Container : in out Container_Type;
      I, J      : in     Positive;
      By        : in     String) is
      
      N : constant Positive := J - I + 1;
      M : constant Natural := Container.Length - N + By'Length;
   begin
      if M <= Container.Elements'Length then

         Container.Elements (I .. M) := 
            By & 
            Container.Elements (J + 1 .. Container.Length);
            
      else
     
         declare
            New_Size : constant Positive := 
               To_New_Size (Container.Elements, New_Length => M);
                            
            S : constant String_Access := new String (1 .. New_Size);
         begin
            S (1 .. I - 1) := Container.Elements (1 .. I - 1);
           
            S (I .. I + By'Length - 1) := By;
           
            S (I + By'Length .. M) := 
                Container.Elements (J + 1 .. Container.Length);
                
            Free (Container.Elements);
            Container.Elements := S;
         end;
        
      end if;
           
      Container.Length := M;
   end Do_Replace;


   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     String) is

   begin

      if Length = 0 then
         Insert (Container, Before => Index, New_Item => By);
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. Container.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Length then
            J := Container.Length;
         end if;

         Do_Replace (Container, I, J, By);
      end;
            
   end Replace;
   

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Character;
      Count     : in     Natural := 1) is
   begin
      Replace (Container, Index, Length, String'(1 .. Count => By));
   end;
   

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Container_Type) is
      
      Src : String renames By.Elements (1 .. By.Length);
   begin
      Replace (Container, Index, Length, String'(Src));
   end;
   

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Container_Type;
      Position  : in     Positive;
      Count     : in     Natural) is
   begin
      if Count = 0 then
         Delete (Container, Index, Length);
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. By.Length := Position;
         J : Positive := I + Count - 1;
      begin
         if J > By.Length then
            J := By.Length;
         end if;
         
         Replace (Container, Index, Length, By.Elements (I .. J));
      end;
   end Replace;
   

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     String) is
   begin
      if Low > High then
         Insert (Container, Before => Low, New_Item => By);
         return;
      end if;
      
      declare
         subtype Index_Subtype is Positive range 1 .. Container.Length;
         I : constant Index_Subtype := Low;
         J : constant Index_Subtype := High;
      begin
         Do_Replace (Container, I, J, By);
      end;                  
   end;


   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Character;
      Count     : in     Natural := 1) is
   begin
      Replace_Slice (Container, Low, High, String'(1 .. Count => By));
   end;
   

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Container_Type) is

      Src : String renames By.Elements (1 .. By.Length);
   begin
      Replace_Slice (Container, Low, High, Src);
   end;
   

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Container_Type;
      Position  : in     Positive;
      Count     : in     Natural) is
   begin
      if Count = 0 then
         Delete_Slice (Container, Low, High);
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. By.Length := Position;
         J : Positive := I + Count - 1;
      begin
         if J > By.Length then
            J := By.Length;
         end if;
         
         Replace_Slice (Container, Low, High, By.Elements (I .. J));
      end;
   end Replace_Slice;


   function To_String 
     (Container : Container_Type) return String is
   begin
      return Container.Elements (1 .. Container.Length);
   end;


   function To_String 
     (Container : Container_Type;
      Index     : Positive;
      Length    : Natural) return String is
   begin
      if Length = 0 then
         return "";
      end if;
      
      declare
         I : constant Positive range 1 .. Container.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Length then
            J := Container.Length;
         end if;
         
         declare
            N : constant Positive := J - I + 1;
            subtype String_Subtype is String (1 .. N);
         begin
            return String_Subtype (Container.Elements (I .. J));
         end;
      end;
   end To_String;



   function To_Access
     (Container : Container_Type) return String_Access is
     
      pragma Assert (Container.Elements'First = 1);
   begin
      return Container.Elements;
   end;


   function Slice
     (Container : Container_Type;
      Low       : Positive;
      High      : Natural) return String is
   begin
      if Low > High then
         return "";
      end if;
      
      declare
         subtype Index_Subtype is Positive range 1 .. Container.Length;

         I : constant Index_Subtype := Low;
         J : constant Index_Subtype := High;

         N : constant Positive := J - I + 1;
         
         subtype String_Subtype is String (1 .. N);
      begin
         return String_Subtype (Container.Elements (I .. J));
      end;
   end Slice;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     String) is
      
      New_Length : constant Natural := Container.Length + New_Item'Length;
   begin
      if Container.Elements'Length >= New_Length then

         declare
            F : constant Positive := Container.Length + 1;
            L : constant Natural := Container.Length + New_Item'Length;
         begin
            Container.Elements (F .. L) := New_Item;
         end;
         
      else

         declare
            New_Size : constant Positive := 
               To_New_Size (Container.Elements, New_Length);

            EA : constant String_Access := new String (1 .. New_Size);
         begin
            EA (1 .. Container.Length) := 
               Container.Elements (1 .. Container.Length);
            
            EA (Container.Length + 1 .. New_Length) := New_Item;
            
            --I don't know what this will do if New_Item 
            --is an alias for Container.Elements.all.
            --We should be safe, because we've already copied
            --the value, but you still have a descriptor (which is
            --what got pushed on the stack), but it will now
            --designate memory that has been deallocated.
            --
            Free (Container.Elements);
            Container.Elements := EA;
         end;                           

      end if;
      
      Container.Length := New_Length;
   end Append;
      
      
   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Character;
      Count     : in     Natural := 1) is
   begin
      Append (Container, String'(1 .. Count => New_Item));
   end;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Container_Type) is
   begin
      --It's not clear whether this will make a copy of the slice.
      --If a compiler writer tells me it does, then we can fix this.
      --However, we still have a possible aliasing issue if
      --user says append (c, to_access (c) (1 .. length (c)));
      --
      Append (Container, String'(New_Item.Elements (1 .. New_Item.Length)));
   end;
      

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural) is
   begin
      if Length = 0 then
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. New_Item.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > New_Item.Length then
            J := New_Item.Length;
         end if;
         
         Append (Container, String'(New_Item.Elements (I .. J)));
      end;
   end Append;
   

   procedure Append_Slice
     (Container : in out Container_Type;
      New_Item  : in     Container_Type;
      Low       : in     Positive;
      High      : in     Natural) is
   begin
      if Low > High then
         return;
      end if;
      
      declare
         subtype Index_Subtype is Positive range 1 .. New_Item.Length;
         I : constant Index_Subtype := Low;
         J : constant Index_Subtype := High;
      begin
         Append (Container, String'(New_Item.Elements (I .. J)));
      end;
   end Append_Slice;
   


   procedure Copy
     (Container : in     Container_Type;
      Item      :    out String;
      Last      :    out Natural) is
   begin
      Last := Item'First + Container.Length - 1;
      
      declare
         Src : String renames Container.Elements (1 .. Container.Length);
         Dst : String renames Item (Item'First .. Last);
      begin      
         Dst := Src;
      end;
   end Copy;
      
      
   procedure Copy
     (Container : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      Item      :    out String;
      Last      :    out Natural) is
      
      Src_First : Positive range 1 .. Container.Length;
      Src_Last  : Positive;
      
      Dst_Length : Natural;
   begin
      if Length = 0 then
         Last := Item'First - 1;
         return;
      end if;
      
      Src_First := Index;
      Src_Last := Src_First + Length - 1;
      
      if Src_Last > Container.Length then
         Src_Last := Container.Length;
      end if;
      
      Dst_Length := Src_Last - Src_First + 1;
      Last := Item'First + Dst_Length - 1;
      
      declare
         Src : String renames Container.Elements (Src_First .. Src_Last);
         Dst : String renames Item (Item'First .. Last);
      begin      
         Dst := Src;
      end;
   end Copy;
      
      
   procedure Copy_Slice
     (Container : in     Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      Item      :    out String;
      Last      :    out Natural) is

      Src_First : Positive range 1 .. Container.Length;
      Src_Last  : Positive range 1 .. Container.Length;
      
      Dst_Length : Natural;
   begin
      if Low > High then
         Last := Item'First - 1;
         return;
      end if;
      
      Src_First := Low;
      Src_Last := High;
      
      Dst_Length := Src_Last - Src_First + 1;
      Last := Item'First + Dst_Length - 1;
      
      declare
         Src : String renames Container.Elements (Src_First .. Src_Last);
         Dst : String renames Item (Item'First .. Last);
      begin      
         Dst := Src;
      end;   
   end Copy_Slice;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      Count     : in     Natural) is
   begin
      if Count = 0 then
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. Container.Length + 1 := Before;
         N : constant Positive := Container.Length + Count;
      begin
         if N <= Container.Elements'Length then
            
            Container.Elements (I + Count .. N) :=
               Container.Elements (I .. Container.Length);
               
         else
         
            declare
               New_Size : constant Positive := 
                  To_New_Size (Container.Elements, N);
                  
               S : constant String_Access := new String (1 .. New_Size);
            begin
               S (1 .. I - 1) := 
                  Container.Elements (1 .. I - 1);

               S (I + Count .. N) := 
                  Container.Elements (I .. Container.Length);
                  
               Free (Container.Elements);
               Container.Elements := S;
            end;
            
         end if;
         
         Container.Length := N;
      end;
   end Insert;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Character;
      Count     : in     Natural := 1) is
   begin
      Insert (Container, Before, String'(1 .. Count => New_Item));
   end;
   

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural) is
   begin
      if Length = 0 then
         return;
      end if;
      
      declare
         I : constant Positive range 1 .. New_Item.Length := Index;
         J : Positive := I + Length - 1;
      begin
         if J > New_Item.Length then
            J := New_Item.Length;
         end if;
         
         Insert (Container, Before, New_Item.Elements (I .. J));
      end;
   end Insert;
      
      
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Container_Type) is
   begin
      Insert (Container, Before, New_Item.Elements (1 .. New_Item.Length));
   end;
      



   function Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural is
   begin
      for I in Index .. Container.Length loop
         if Container.Elements (I) = Item then
            return I;
         end if;
      end loop;
      
      return 0;
   end;

      
   function Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : String) return Natural is
      
      Last : constant Integer'Base := 
         Container.Length - Item'Length + 1;
   begin
      --This is a naive algorithm.  We should really be using
      --a real string matching algorithm.
      --
      for I in Index .. Last loop
         if Container.Elements (I .. I + Item'Length - 1) = Item then
            return I;
         end if;
      end loop;
      
      return 0;
   end;
      
      

   function Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : Container_Type) return Natural is
      
      S : String renames Item.Elements (1 .. Item.Length);
   begin
      return Find (Container, Index, S);
   end;


   function Reverse_Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural is
   begin
      for I in reverse Index .. Container.Length loop
         if Container.Elements (I) = Item then
            return I;
         end if;
      end loop;
      
      return 0;
   end;
   
      
   function Reverse_Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : String) return Natural is

      Last : constant Integer'Base := 
         Integer'Min (Index, Container.Length - Item'Length + 1);
   begin
      for I in reverse 1 .. Last loop
         if Container.Elements (I .. I + Item'Length - 1) = Item then
            return I;
         end if;
      end loop;
      
      return 0;
   end;
   
      
   function Reverse_Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : Container_Type) return Natural is
      
      S : String renames Item.Elements (1 .. Container.Length);
   begin
      return Reverse_Find (Container, Index, S);
   end;



end Charles.Strings.Unbounded;
