------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--                   Charles.Strings.Bounded (body)                         --
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

with System;  use type System.Address;

package body Charles.Strings.Bounded is


   function "=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) = 
               Right.Elements (1 .. Right.Last);      
   end;      
      
   
   function "=" 
      (Left  : Container_Type;
       Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) = Right;
   end;
   

   function "=" 
      (Left  : String;
       Right : Container_Type) return Boolean is
   begin
      return Right = Left;
   end;



   function "<" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) <
               Right.Elements (1 .. Right.Last);
   end;
   

   function "<" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) < Right;
   end;
   

   function "<" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left < Right.Elements (1 .. Right.Last);
   end;


   function "<=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) <=
               Right.Elements (1 .. Right.Last);
   end;
   

   function "<=" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) <= Right;
   end;
   

   function "<=" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left <= Right.Elements (1 .. Right.Last);
   end;


   function ">=" (Left, Right : Container_Type) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) >=
               Right.Elements (1 .. Right.Last);
   end;
   

   function ">=" 
     (Left  : Container_Type;
      Right : String) return Boolean is
   begin
      return Left.Elements (1 .. Left.Last) >= Right;
   end;
   

   function ">=" 
     (Left  : String;
      Right : Container_Type) return Boolean is
   begin
      return Left >= Right.Elements (1 .. Right.Last);
   end;


   function Length (Container : Container_Type) return Natural is
   begin
      return Container.Last;
   end;
   

   function Is_Empty (Container : Container_Type) return Boolean is
   begin
      return Container.Last = 0;
   end;
   

   procedure Clear (Container : in out Container_Type) is
   begin   
      Container.Last := 0;
   end;



   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is
      
      subtype Length_Subtype is
         Natural range 0 .. Target.Size;
   begin
      Target.Last := Length_Subtype'(Length);
   end;
      

   procedure Assign
     (Target : in out Container_Type;
      Source : in     String) is
      
      subtype Length_Subtype is
         Natural range 0 .. Target.Size;
   begin
      Target.Last := Length_Subtype'(Source'Length);
      Target.Elements (1 .. Target.Last) := Source;
   end;


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
      
      Assign (Target, String'(Source.Elements (1 .. Source.Last)));
   end;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type;
      Index  : in     Positive;
      Length : in     Natural) is
   begin
      if Length = 0 then
         Target.Last := 0;
      else
         declare
            I : constant Positive range 1 .. Source.Last := Index;
            J : Positive := I + Length - 1;
         begin
            if J > Source.Last then
               J := Source.Last;
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
         Target.Last := 0;
      else
         declare
            subtype Index_Subtype is Positive range 1 .. Source.Last;
            I : constant Index_Subtype := Low;
            J : constant Index_Subtype := High;
         begin
            Assign (Target, String'(Source.Elements (I .. J)));
         end;
      end if;
   end;


   function Size (Container : Container_Type) return Natural is
   begin
      return Container.Size;
   end;
   
   
--   procedure Resize
--     (Container : in out Container_Type;
--      Size      : in     Natural) is
--   begin
--      null;
--   end;

      
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
      return Container.Last;
   end;
   

   function Back 
     (Container : Container_Type) return Positive is
   begin
      return Container.Last + 1;
   end;
   

   function Element 
     (Container : Container_Type;
      Index     : Positive) return Character is
      
      subtype Length_Subtype is
         Positive range 1 .. Container.Last;
   begin
      return Container.Elements (Length_Subtype'(Index));
   end;
   
      
   procedure Replace_Element
     (Container : in Container_Type;
      Index     : in Positive;
      By        : in Character) is
      
      I : constant Positive range 1 .. Container.Last := Index;
      
      S : String (1 .. Container.Size);
      for S'Address use Container.Elements'Address;
      --pragma Import (Ada, S);
      
      --It may be simpler to go ahead and pass Container
      --as an inout parameter, or to use the Rosen Trick.
   begin
      S (I) := By;   
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
         I : constant Positive range 1 .. Container.Last := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Last then
            J := Container.Last;
         end if;
         
         declare
            N : constant Positive := J - I + 1;
         
            Dst : String renames 
               Container.Elements (I .. Container.Last - N);
               
            Src : String renames 
               Container.Elements (J + 1 .. Container.Last);
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
         subtype Index_Subtype is Positive range 1 .. Container.Last;

         I : constant Index_Subtype := From;
         J : constant Index_Subtype := Through;

         N : constant Positive := J - I + 1;
         
         Dst : String renames
            Container.Elements (I .. Container.Last - N);
            
         Src : String renames
            Container.Elements (J + 1 .. Container.Last);
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
         I : constant Positive range 1 .. Container.Last + 1 := Before;
            
         N : constant Positive range 1 .. Container.Size := 
            Container.Last + New_Item'Length;
      begin            
         Container.Elements (I .. N) :=
            New_Item & 
            Container.Elements (I .. Container.Last);
         
         Container.Last := N;
      end;
   end Insert;
     

   procedure Do_Replace 
     (Container : in out Container_Type;
      I, J      : in     Positive;
      By        : in     String) is
      
      N : constant Positive := J - I + 1;

      M : constant Natural range 0 .. Container.Size := 
         Container.Last - N + By'Length;
   begin
      Container.Elements (I .. M) := 
         By & 
         Container.Elements (J + 1 .. Container.Last);
           
      Container.Last := M;
   end;


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
         I : constant Positive range 1 .. Container.Last := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Last then
            J := Container.Last;
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
      
      Src : String renames By.Elements (1 .. By.Last);
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
         I : constant Positive range 1 .. By.Last := Position;
         J : Positive := I + Count - 1;
      begin
         if J > By.Last then
            J := By.Last;
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
         subtype Index_Subtype is Positive range 1 .. Container.Last;
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

      Src : String renames By.Elements (1 .. By.Last);
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
         I : constant Positive range 1 .. By.Last := Position;
         J : Positive := I + Count - 1;
      begin
         if J > By.Last then
            J := By.Last;
         end if;
         
         Replace_Slice (Container, Low, High, By.Elements (I .. J));
      end;
   end Replace_Slice;


   function To_String 
     (Container : Container_Type) return String is
   begin
      return Container.Elements (1 .. Container.Last);
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
         I : constant Positive range 1 .. Container.Last := Index;
         J : Positive := I + Length - 1;
      begin
         if J > Container.Last then
            J := Container.Last;
         end if;
         
         declare
            N : constant Positive := J - I + 1;
            subtype String_Subtype is String (1 .. N);
         begin
            return String_Subtype (Container.Elements (I .. J));
         end;
      end;
   end To_String;



   function Slice
     (Container : Container_Type;
      Low       : Positive;
      High      : Natural) return String is
   begin
      if Low > High then
         return "";
      end if;
      
      declare
         subtype Index_Subtype is Positive range 1 .. Container.Last;

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
      
      New_Length : constant Natural range 0 .. Container.Size := 
         Container.Last + New_Item'Length;

      F : constant Positive range 1 .. Container.Size := 
         Container.Last + 1;
   begin
      Container.Elements (F .. New_Length) := New_Item;      
      Container.Last := New_Length;
   end;
      
      
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
      Append (Container, String'(New_Item.Elements (1 .. New_Item.Last)));
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
         I : constant Positive range 1 .. New_Item.Last := Index;
         J : Positive := I + Length - 1;
      begin
         if J > New_Item.Last then
            J := New_Item.Last;
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
         subtype Index_Subtype is Positive range 1 .. New_Item.Last;
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
      Last := Item'First + Container.Last - 1;
      
      declare
         Src : String renames Container.Elements (1 .. Container.Last);
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
      
      Src_First : Positive range 1 .. Container.Last;
      Src_Last  : Positive;
      
      Dst_Length : Natural;
   begin
      if Length = 0 then
         Last := Item'First - 1;
         return;
      end if;
      
      Src_First := Index;
      Src_Last := Src_First + Length - 1;
      
      if Src_Last > Container.Last then
         Src_Last := Container.Last;
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

      Src_First : Positive range 1 .. Container.Last;
      Src_Last  : Positive range 1 .. Container.Last;
      
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
         I : constant Positive range 1 .. Container.Last + 1 := Before;
         
         N : constant Positive range 1 .. Container.Size := 
            Container.Last + Count;
      begin
         Container.Elements (I + Count .. N) :=
            Container.Elements (I .. Container.Last);
         
         Container.Last := N;
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
         I : constant Positive range 1 .. New_Item.Last := Index;
         J : Positive := I + Length - 1;
      begin
         if J > New_Item.Last then
            J := New_Item.Last;
         end if;
         
         Insert (Container, Before, New_Item.Elements (I .. J));
      end;
   end Insert;
      
      
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Container_Type) is
   begin
      Insert (Container, Before, New_Item.Elements (1 .. New_Item.Last));
   end;
      



   function Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural is
   begin
      for I in Index .. Container.Last loop
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
         Container.Last - Item'Length + 1;
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
      
      S : String renames Item.Elements (1 .. Item.Last);
   begin
      return Find (Container, Index, S);
   end;


   function Reverse_Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural is
   begin
      for I in reverse Index .. Container.Last loop
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
         Integer'Min (Index, Container.Last - Item'Length + 1);
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
      
      S : String renames Item.Elements (1 .. Container.Last);
   begin
      return Reverse_Find (Container, Index, S);
   end;



end Charles.Strings.Bounded;
