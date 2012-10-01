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

with System;
with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Vectors.Bounded is

   use type System.Address;

   function To_Integer (Index : Index_Type'Base) return Integer'Base is
      pragma Inline (To_Integer);

      F : constant Integer'Base := Index_Type'Pos (Index_Type'First);

      I : constant Integer'Base := index_Type'Pos (Index);

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
   end "=";


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
      Container.Elements (Range_Subtype) := (others => Item);
      Container.Last := Last_Subtype'First;
   end;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural) is

      subtype Size_Subtype is Index_Type'Base range
         Last_Subtype'First .. Target.Elements'Last;
   begin
      Target.Last := Size_Subtype'(To_Index (Length));
   end;


   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type) is

      subtype Size_Subtype is Index_Type'Base range
         Last_Subtype'First .. Target.Elements'Last;

      Last : constant Size_Subtype := To_Index (Length);

      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Last;
   begin
      Target.Elements (Range_Subtype) := (others => Item);
      Target.Last := Last;
   end;


   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      declare
         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Target.Elements'Last;

         Last : constant Size_Subtype := Source.Last;

         subtype Range_Subtype is Index_Type'Base range
            Index_Type'First .. Source.Last;
      begin
         Target.Elements (Range_Subtype) := Source.Elements (Range_Subtype);
         Target.Last := Last;
      end;
   end Assign;


   procedure Append
     (Container : in out Container_Type) is

      subtype Size_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Elements'Last;

      Last : constant Size_Subtype := Index_Type'Succ (Container.Last);
   begin
      Container.Last := Last;
   end;



   function Generic_Append
     (Container : access Container_Type) return Element_Access is
   begin
      Append (Container.all);
      return Container.Elements (Container.Last)'Access;
   end;


   function Append
     (Container : access Container_Type) return Index_Type is
   begin
      Append (Container.all);
      return Container.Last;
   end;


   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type) is

      subtype Size_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Elements'Last;

      Last : constant Size_Subtype := Index_Type'Succ (Container.Last);
   begin
      Container.Elements (Last) := New_Item;
      Container.Last := Last;
   end;


   procedure Append_N
     (Container : in out Container_Type;
      Count     : in     Natural) is

      Old_Length : constant Natural := To_Integer (Container.Last);
      New_Length : constant Natural := Old_Length + Count;

      subtype Size_Subtype is Index_Type'Base range
         Last_Subtype'First .. Container.Elements'Last;

      New_Last : constant Size_Subtype := To_Index (New_Length);
   begin
      Container.Last := New_Last;
   end;


   procedure Append_N
     (Container : in out Container_Type;
      Count     : in     Natural;
      New_Item  : in     Element_Type) is

      Old_Length : constant Natural := To_Integer (Container.Last);
      New_Length : constant Natural := Old_Length + Count;

      subtype Size_Subtype is Index_Type'Base range
         Last_Subtype'First .. Container.Elements'Last;

      New_Last : constant Size_Subtype := To_Index (New_Length);

      subtype Range_Subtype is Index_Type'Base range
         Index_Type'Succ (Container.Last) .. New_Last;
   begin
      Container.Elements (Range_Subtype) := (others => New_Item);
      Container.Last := New_Last;
   end;


   procedure Delete_Last
     (Container : in out Container_Type) is
   begin
      Container.Last := Index_Type'Pred (Container.Last);
   end;


   procedure Delete_Last
     (Container : in out Container_Type;
      Item      : in     Element_Type) is
   begin
      Container.Elements (Container.Last) := Item;
      Container.Last := Index_Type'Pred (Container.Last);
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

      Old_Length : constant Natural := To_Integer (Container.Last);
      New_Length : constant Natural := Old_Length - Count;

      subtype Size_Subtype is Index_Type'Base range
         Last_Subtype'First .. Container.Elements'Last;

      New_Last : constant Size_Subtype := To_Index (New_Length);

      subtype Range_Subtype is Index_Type'Base range
         Index_Type'Succ (New_Last) .. Container.Last;
   begin
      Container.Elements (Range_Subtype) := (others => Item);
      Container.Last := New_Last;
   end;


   procedure Insert_N
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Count     : in     Natural) is

   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         Index : constant Before_Subtype := Before;

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length + Count;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         subtype Target_Subtype is Index_Type'Base range
            To_Index (To_Integer (Index) + Count) .. New_Last;

         subtype Source_Subtype is Index_Type'Base range
            Index .. Container.Last;
      begin
         Container.Elements (Target_Subtype) :=
            Container.Elements (Source_Subtype);

         Container.Last := New_Last;
      end;

   end Insert_N;


   procedure Insert_N
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Count     : in     Natural;
      New_Item  : in     Element_Type) is

   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         --In theory we need not check this if Count has value 0.
         Index : constant Before_Subtype := Before;

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length + Count;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         subtype Array_Subtype is
            Element_Array_Type (Index_Type'First .. To_Index (Count));
      begin
         Container.Elements (Index .. New_Last) :=
            Array_Subtype'(others => New_Item) &
            Container.Elements (Index .. Container.Last);

         Container.Last := New_Last;
      end;

   end Insert_N;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type) is
   begin
      Insert_N (Container, Before, 1);
   end;


   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type;
      New_Item  : in     Element_Type) is
   begin
      Insert_N (Container, Before, 1, New_Item);
   end;


   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;

      New_Last : constant Last_Subtype := Index_Type'Pred (Container.Last);

      E : Element_Array_Type renames Container.Elements;
   begin
      E (I .. New_Last) := E (Index_Type'Succ (I) .. Container.Last);
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

         F : constant First_Subtype := First;

         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         B : constant Back_Subtype := Back;

         N : constant Natural := Index_Type'Pos (B) - Index_Type'Pos (F);

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length - N;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         E : Element_Array_Type renames Container.Elements;
      begin
         E (F .. New_Last) := E (B .. Container.Last);
         Container.Last := New_Last;
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

         F : constant First_Subtype := First;

         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         B : constant Back_Subtype := To_Index (To_Integer (F) + Count);

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length - Count;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         E : Element_Array_Type renames Container.Elements;
      begin
         E (F .. New_Last) := E (B .. Container.Last);
         Container.Last := New_Last;
      end;

   end Delete_N;



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

         F : constant First_Subtype := First;

         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         B : constant Back_Subtype := Back;

         N : constant Natural := Index_Type'Pos (B) - Index_Type'Pos (F);

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length - N;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         E : Element_Array_Type renames Container.Elements;

         subtype Array_Subtype is
            Element_Array_Type (Index_Type'First .. To_Index (N));
      begin
         E (F .. Container.Last) :=
            E (B .. Container.Last) &
            Array_Subtype'(others => Item);

         Container.Last := New_Last;
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

         F : constant First_Subtype := First;

         subtype Back_Subtype is Index_Type'Base range
            Index_Type'First .. Index_Type'Succ (Container.Last);

         B : constant Back_Subtype := To_Index (To_Integer (F) + Count);

         Old_Length : constant Natural := To_Integer (Container.Last);
         New_Length : constant Natural := Old_Length - Count;

         subtype Size_Subtype is Index_Type'Base range
            Last_Subtype'First .. Container.Elements'Last;

         New_Last : constant Size_Subtype := To_Index (New_Length);

         E : Element_Array_Type renames Container.Elements;

         subtype Array_Subtype is
            Element_Array_Type (Index_Type'First .. To_Index (Count));
      begin
         E (F .. Container.Last) :=
            E (B .. Container.Last) &
            Array_Subtype'(others => Item);

         Container.Last := New_Last;
      end;

   end Delete_N;


   function Size (Container : Container_Type) return Natural is
   begin
      return Container.Elements'Length;
   end;


--   procedure Resize
--     (Container : in out Container_Type;
--      Size      : in     Natural) is
--
--      subtype Size_Subtype is Natural range
--         0 .. Container.Elements'Length;
--
--      X : constant Size_Subtype := Size;  --RM 11.6 issues?
--      pragma Warnings (Off, X);
--   begin
--      null;
--   end;


   function Front
     (Container : Container_Type) return Index_Type'Base is
   begin
      return Index_Type'Pred (Index_Type'First);
   end;


   function First
     (Container : Container_Type) return Index_Type is
   begin
      return Index_Type'First;
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

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      return Container.Elements (I);
   end;


   function Generic_Element
     (Container : access Container_Type;
      Index     : in     Index_Type) return Element_Access is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      return Container.Elements (I)'Access;
   end;




   procedure Replace_Element
     (Container : in out Container_Type;
      Index     : in     Index_Type;
      By        : in     Element_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      Container.Elements (I) := By;
   end;


   procedure Copy
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      :    out Element_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      Item := Container.Elements (I);
   end;



   procedure Swap
     (Container : in out Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;

      EA : Element_Array_Type renames Container.Elements;

      E : constant Element_Type := EA (I);
   begin
      EA (I) := Item;
      Item := E;
   end;


   procedure Generic_Swap
     (Container : in out Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;

      EA : Element_Array_Type renames Container.Elements;

      E : Element_Type renames EA (I);
   begin
      Swap (E, Item);
   end;



   procedure Swap_Element
     (Container   : in out Container_Type;
      Left, Right : in     Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      LI : constant T := Left;
      RI : constant T := Right;

      EA : Element_Array_Type renames Container.Elements;

      LE : constant Element_Type := EA (LI);
   begin
      EA (LI) := EA (RI);
      EA (RI) := LE;
   end;



   procedure Generic_Swap_Element
     (Container   : in out Container_Type;
      Left, Right : in Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      LI : constant T := Left;
      RI : constant T := Right;

      EA : Element_Array_Type renames Container.Elements;
   begin
      Swap (EA (LI), EA (RI));
   end;


   procedure Generic_Select_Element
     (Container : in Container_Type;
      Index     : in Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      Process (Container.Elements (I));
   end;


   procedure Generic_Modify_Element
     (Container : in out Container_Type;
      Index     : in     Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      Process (Container.Elements (I));
   end;


   procedure Generic_Access_Element
     (Container : access Container_Type;
      Index     : in     Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      I : constant T := Index;
   begin
      Process (Container.Elements (I)'Access);
   end;


   procedure Generic_Iteration
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in F .. L loop
            Process (Container, I);
         end loop;
      end;
   end Generic_Iteration;


   procedure Generic_Reverse_Iteration
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in reverse F .. L loop
            Process (Container, I);
         end loop;
      end;
   end Generic_Reverse_Iteration;


   procedure Generic_Select_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in F .. L loop
            Process (Container.Elements (I));
         end loop;
      end;
   end Generic_Select_Elements;


   procedure Generic_Modify_Elements
     (Container : in out Container_Type;
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in F .. L loop
            Process (Container.Elements (I));
         end loop;
      end;
   end Generic_Modify_Elements;



   procedure Generic_Access_Elements
     (Container : access Container_Type;
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in F .. L loop
            Process (Container.Elements (I)'Access);
         end loop;
      end;
   end Generic_Access_Elements;


   procedure Generic_Reverse_Select_Elements
     (Container : in Container_Type;
      First     : in Index_Type'Base;
      Back      : in Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in reverse F .. L loop
            Process (Container.Elements (I));
         end loop;
      end;
   end Generic_Reverse_Select_Elements;


   procedure Generic_Reverse_Modify_Elements
     (Container : in out Container_Type;
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in reverse F .. L loop
            Process (Container.Elements (I));
         end loop;
      end;
   end Generic_Reverse_Modify_Elements;




   procedure Generic_Reverse_Access_Elements
     (Container : access Container_Type;
      First     : in     Index_Type'Base;
      Back      : in     Index_Type'Base) is
   begin
      if Back <= First then
         return;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in reverse F .. L loop
            Process (Container.Elements (I)'Access);
         end loop;
      end;
   end Generic_Reverse_Access_Elements;


   function Generic_Find
     (Container : Container_Type;
      First     : Index_Type'Base;
      Back      : Index_Type'Base) return Index_Type'Base is
   begin
      if Back <= First then
         return Back;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in F .. L loop
            if Predicate (Container.Elements (I)) then
               return I;
            end if;
         end loop;
      end;

      return Back;
   end Generic_Find;


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
   begin
      if Back <= First then
         return Back;
      end if;

      declare
         subtype T is Index_Type'Base range
            Index_Type'First .. Container.Last;

         F : constant T := First;
         L : constant T := Index_Type'Pred (Back);
      begin
         for I in reverse F .. L loop
            if Predicate (Container.Elements (I)) then
               return I;
            end if;
         end loop;
      end;

      return Back;
   end Generic_Reverse_Find;


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


end Charles.Vectors.Bounded;
