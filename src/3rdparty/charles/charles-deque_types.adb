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

package body Charles.Deque_Types is

   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Access_Array,
                                      Node_Access_Array_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Type,
                                      Node_Access);

   procedure Initialize (Map : in out Map_Type) is
   begin
      null;
   end;


   procedure Adjust (Map : in out Map_Type) is
      Nodes : constant Node_Access_Array_Access := Map.Nodes;
   begin
      Map.Nodes := new Node_Access_Array (1 .. Nodes'Length);

      for I in Map.First .. Map.Last loop
         Map.Nodes (I) := new Node_Type'(Nodes (I).all);
      end loop;
   end;


   procedure Finalize (Map : in out Map_Type) is
   begin
      for I in Map.First .. Map.Last loop
         Free (Map.Nodes (I));
      end loop;

      Free (Map.Nodes);
   end;


   procedure Swap (L, R : in out Map_Type) is

      L_Nodes : constant Node_Access_Array_Access := L.Nodes;
      L_First : constant Integer'Base := L.First;
      L_Last : constant Integer'Base := L.Last;
      L_Length : Integer'Base := L.Length;
      L_Bias   : Integer'Base := L.Bias;

   begin

      L.Nodes := R.Nodes;
      L.First := R.First;
      L.Last := R.Last;
      L.Length := R.Length;
      L.Bias := R.Bias;

      R.Nodes := L_Nodes;
      R.First := L_First;
      R.Last := L_Last;
      R.Length := L_Length;
      R.Bias := L_Bias;

   end Swap;


   procedure Assign
     (Map    : in out Map_Type;
      Length : in     Natural) is

      Node_Count : constant Natural :=
         (Length + Elements_Per_Node - 1) / Elements_Per_Node;

   begin

      if Map.Nodes = null then

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

         for I in Map.Nodes'Range loop
            Map.Nodes (I) := new Node_Type;
         end loop;

         Map.First := 1;

         Map.Last := Node_Count;

         Map.Length := Length;

         Map.Bias := 0;

      elsif Map.Nodes'Length < Node_Count then

         for I in Map.First .. Map.Last loop
            Free (Map.Nodes (I));
         end loop;

         Free (Map.Nodes);

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

         for I in Map.Nodes'Range loop
            Map.Nodes (I) := new Node_Type;
         end loop;

         Map.First := 1;

         Map.Last := Node_Count;

         Map.Length := Length;

         Map.Bias := 0;

      elsif Map.Last - Map.First + 1 > Node_Count then

         for I in Map.First + Node_Count .. Map.Last loop
            Free (Map.Nodes (I));
         end loop;

         Map.Last := Map.First + Node_Count - 1;

         Map.Length := Length;

         Map.Bias := 0;

      elsif Map.Last - Map.First + 1 < Node_Count then

         declare
            NC : constant Positive :=
               Map.Last - Map.First + 1;

            Diff : constant Positive :=
               Node_Count - NC;
         begin
            Map.Nodes (1 .. Node_Count) :=
               Map.Nodes (Map.First .. Map.Last) &
               Node_Access_Array'(1 .. Diff => new Node_Type);
         end;

         Map.First := 1;

         Map.Last := Node_Count;

         Map.Bias := 0;

         Map.Length := Length;

      else

         Map.Bias := 0;

         Map.Length := Length;

      end if;

   end Assign;




   function Is_Equal (L, R : Map_Type) return Boolean is

      L_NI : Positive;
      L_EI : Positive;

      R_NI : Positive;
      R_EI : Positive;

   begin

      if L.Length /= R.Length then
         return False;
      end if;

      for I in 0 .. L.Length - 1 loop

         Element (L, I, L_NI, L_EI);
         Element (R, I, R_NI, R_EI);

         if L.Nodes (L_NI).Elements (L_EI) /=
            R.Nodes (R_NI).Elements (R_EI)
         then
            return False;
         end if;

      end loop;

      return True;

   end Is_Equal;


   procedure Clear (Map : in out Map_Type) is
   begin
      for I in Map.First .. Map.Last loop
         Free (Map.Nodes (I));
      end loop;

      Map.Last := Map.First - 1;
      Map.Length := 0;
      Map.Bias := 0;
   end;



   procedure Prepend
     (Map           : in out Map_Type;
      Node          :    out Node_Access;
      Element_Index :    out Integer'Base) is

   begin

      if Map.Last < Map.First then

         pragma Assert (Map.Length = 0);

         if Map.Nodes = null then
            Map.Nodes := new Node_Access_Array (1 .. 1); --?
         end if;

         Map.First := 1 + Map.Nodes'Length / 2;
         Map.Last := Map.First;

         Map.Nodes (Map.First) := new Node_Type;

         Map.Bias := Elements_Per_Node / 2;

         --Map.Nodes (Map.First).Elements (1) := Item;
         Node := Map.Nodes (Map.First);
         Element_Index := Map.Bias + 1;

         Map.Length := 1;

         return;

      end if;

      if Map.Bias > 0 then

--         declare
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            Node.Elements (Map.Bias) := Item;
--         end;

         Node := Map.Nodes (Map.First);
         Element_Index := Map.Bias;

         Map.Bias := Map.Bias - 1;

         Map.Length := Map.Length + 1;

         return;

      end if;

      if Map.First > Map.Nodes'First then

         Map.First := Map.First - 1;

         Map.Nodes (Map.First) := new Node_Type;

--         declare
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            Node.Elements (Elements_Per_Node) := Item;
--         end;

         Node := Map.Nodes (Map.First);
         Element_Index := Elements_Per_Node;

         Map.Bias := Elements_Per_Node - 1;

         Map.Length := Map.Length + 1;

         return;

      end if;

      if Map.Last - Map.First + 1 < Map.Nodes'Length then

         Map.Nodes (Map.First + 1 .. Map.Last + 1) :=
            Map.Nodes (Map.First .. Map.Last);

         Map.Last := Map.Last + 1;

         Map.Nodes (Map.First) := new Node_Type;

--         declare
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            Node.Elements (Elements_Per_Node) := Item;
--         end;

         Node := Map.Nodes (Map.First);
         Element_Index := Elements_Per_Node;

         Map.Bias := Elements_Per_Node - 1;

         Map.Length := Map.Length + 1;

         return;

      end if;

      declare
         N : constant Integer'Base :=
            2 * Map.Nodes'Length;

         Nodes : constant Node_Access_Array_Access :=
            new Node_Access_Array (1 .. N);
      begin
         Nodes (Map.First + 1 .. Map.Last + 1) :=
            Map.Nodes (Map.First .. Map.Last);

         Free (Map.Nodes);

         Map.Nodes := Nodes;

         Map.Last := Map.Last + 1;

         Map.Nodes (Map.First) := new Node_Type;

--         declare
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            Node.Elements (Elements_Per_Node) := Item;
--         end;

         Node := Map.Nodes (Map.First);
         Element_Index := Elements_Per_Node;

         Map.Bias := Elements_Per_Node - 1;

         Map.Length := Map.Length + 1;
      end;

   end Prepend;


   procedure Delete_First (Map : in out Map_Type) is
   begin
      Map.Bias := Map.Bias + 1;

      if Map.Bias = Elements_Per_Node then
         Free (Map.Nodes (Map.First));
         Map.First := Map.First + 1;
         Map.Bias := 0;
      end if;

      Map.Length := Map.Length - 1;
   end;


   procedure Append
     (Map           : in out Map_Type;
      Element_Index :    out Integer'Base) is
   begin

      if Map.Last < Map.First then

         pragma Assert (Map.Length = 0);

         if Map.Nodes = null then
            Map.Nodes := new Node_Access_Array (1 .. 1); --?
         end if;

         Map.First := 1 + Map.Nodes'Length / 2;
         Map.Last := Map.First;

         Map.Nodes (Map.First) := new Node_Type;

         Map.Bias := Elements_Per_Node / 2;

         --Map.Nodes (Map.First).Elements (1) := Item;
         Element_Index := Map.Bias + 1;

         Map.Length := 1;

         return;

      end if;

      declare
         Length : constant Integer'Base :=
            Map.Bias + Map.Length;

         Nodes_Count : constant Integer'Base :=
            Map.Last - Map.First + 1;

         Count : constant Integer'Base :=
            Length - (Nodes_Count - 1) * Elements_Per_Node;
      begin

         if Count < Elements_Per_Node then

            --Map.Nodes (Map.Last).Elements (Count + 1) := Item;
            Element_Index := Count + 1;

         elsif Map.Last < Map.Nodes'Last then

            Map.Nodes (Map.Last + 1) := new Node_Type;

            Map.Last := Map.Last + 1;

            --Map.Nodes (Map.Last).Elements (1) := Item;
            Element_Index := 1;

         elsif Map.Last - Map.First + 1 < Map.Nodes'Length then

            pragma Assert (Map.First > Map.Nodes'First);

            Map.Nodes (Map.First - 1 .. Map.Last - 1) :=
               Map.Nodes (Map.First .. Map.Last);

            Map.First := Map.First - 1;

            Map.Nodes (Map.Last) := new Node_Type;

            --Map.Nodes (Map.Last).Elements (1) := Item;
            Element_Index := 1;

         else

            declare
               N : constant Integer'Base :=
                  2 * Map.Nodes'Length;

               Nodes : constant Node_Access_Array_Access :=
                  new Node_Access_Array (1 .. N);
            begin
               Nodes (Map.First .. Map.Last) :=
                  Map.Nodes (Map.First .. Map.Last);

               Free (Map.Nodes);

               Map.Nodes := Nodes;

               Map.Last := Map.Last + 1;

               Map.Nodes (Map.Last) := new Node_Type;

               --Map.Nodes (Map.Last).Elements (1) := Item;
               Element_Index := 1;
            end;

         end if;

      end;

      Map.Length := Map.Length + 1;

   end Append;


   procedure Delete_Last (Map : in out Map_Type) is

      Length : constant Positive :=
         Map.Bias + Map.Length;

      Nodes_Count : constant Positive :=
         Map.Last - Map.First + 1;

      Count : constant Positive :=
         Length - (Nodes_Count - 1) * Elements_Per_Node;
   begin
      if Count = 1 then
         Free (Map.Nodes (Map.Last));
         Map.Last := Map.Last - 1;
      end if;

      Map.Length := Map.Length - 1;
   end;


   procedure New_Front (Map : in out Map_Type) is
      pragma Assert (Map.Bias = 0);

      Node : constant Node_Access := new Node_Type;

   begin

      if Map.First > Map.Nodes'First then

         Map.First := Map.First - 1;
         Map.Nodes (Map.First) := Node;

      elsif Map.Last - Map.First + 1 < Map.Nodes'Length then

         Map.Nodes (Map.First .. Map.Last + 1) :=
            Node & Map.Nodes (Map.First .. Map.Last);

         Map.Last := Map.Last + 1;

      else

         declare
            N : constant Integer'Base :=
               2 * Map.Nodes'Length;

            Nodes : constant Node_Access_Array_Access :=
               new Node_Access_Array (1 .. N);
         begin
            Nodes (Map.First .. Map.Last + 1) :=
               Node & Map.Nodes (Map.First .. Map.Last);

            Map.Last := Map.Last + 1;

            Free (Map.Nodes);

            Map.Nodes := Nodes;
         end;

      end if;

      Map.Bias := Elements_Per_Node;

   end New_Front;


   procedure Insert_Front
     (Map           : in out Map_Type;
      Before        : in     Natural;
      Element_Node  :    out Node_Access;
      Element_Index :    out Positive) is

      Index : Natural;
      Count : Natural;
      Last  : Positive;

   begin

      if Map.Nodes = null then

         pragma Assert (Map.Length = 0);

         declare
            subtype Array_Subtype is
               Node_Access_Array (1 .. 1);

            Node : Node_Access := new Node_Type;
         begin
            Map.Nodes := new Array_Subtype'(others => Node);
         exception
            when others =>
               Free (Node);
               raise;
         end;

         Map.First := 1;
         Map.Last := 1;
         Map.Bias := 1 + Elements_Per_Node / 2;

      elsif Map.Bias = 0 then

         New_Front (Map);

      end if;

      Index := Map.Bias + Before;
      Count := Index / Elements_Per_Node;
      Last := Map.First + Count;

      declare
         Node : Node_Type renames Map.Nodes (Map.First).all;
      begin
         if Last = Map.First then

            Node.Elements (Map.Bias .. Index - 1) :=
               Node.Elements (Map.Bias + 1 .. Index);

            Element_Node := Map.Nodes (Map.First);
            Element_Index := Index;

            Map.Bias := Map.Bias - 1;

            Map.Length := Map.Length + 1;

            return;

         end if;

         Node.Elements (Map.Bias .. Elements_Per_Node - 1) :=
            Node.Elements (Map.Bias + 1 .. Elements_Per_Node);
      end;

      Map.Bias := Map.Bias - 1;

      for I in Map.First .. Last - 2 loop

         declare
            Curr : Node_Type renames Map.Nodes (I).all;
            Next : Node_Type renames Map.Nodes (I + 1).all;
         begin
            Curr.Elements (Elements_Per_Node) :=
               Next.Elements (1);

            Next.Elements (1 .. Elements_Per_Node - 1) :=
               Next.Elements (2 .. Elements_Per_Node);
         end;

      end loop;

      declare
         Curr : Node_Type renames Map.Nodes (Last - 1).all;
      begin
         if Index mod Elements_Per_Node = 0 then

            --Curr.Elements (Elements_Per_Node) := New_Item;
            Element_Node := Map.Nodes (Last - 1);
            Element_Index := Elements_Per_Node;

         else
            declare
               Next : Node_Type renames Map.Nodes (Last).all;

               J : constant Positive :=
                  Index - Count * Elements_Per_Node;
            begin
               Curr.Elements (Elements_Per_Node) :=
                  Next.Elements (1);

               Next.Elements (1 .. J - 1) :=
                  Next.Elements (2 .. J);

               Element_Node := Map.Nodes (Last);
               Element_Index := J;
            end;
         end if;
      end;

      Map.Length := Map.Length + 1;

   end Insert_Front;


   procedure New_Back (Map : in out Map_Type) is

      Node : constant Node_Access := new Node_Type;
   begin
      if Map.Last < Map.Nodes'Last then

         Map.Last := Map.Last + 1;
         Map.Nodes (Map.Last) := Node;

      elsif Map.Last - Map.First + 1 < Map.Nodes'Length then

         Map.Nodes (Map.First - 1 .. Map.Last) :=
            Map.Nodes (Map.First .. Map.Last) & Node;

      else

         declare
            N : constant Positive := 2 * Map.Nodes'Length;

            Nodes : constant Node_Access_Array_Access :=
               new Node_Access_Array (1 .. N);
         begin
            Map.Last := Map.Last + 1;

            Nodes (Map.First .. Map.Last) :=
               Map.Nodes (Map.First .. Map.Last - 1) & Node;

            Free (Map.Nodes);

            Map.Nodes := Nodes;
         end;

      end if;
   end New_Back;


   procedure Insert_Back
     (Map           : in out Map_Type;
      Before        : in     Natural;
      Element_Node  :    out Node_Access;
      Element_Index :    out Positive) is

      Index : constant Natural :=
         Map.Bias + Before;

      Last : constant Positive :=
         Map.First + Index / Elements_Per_Node;

      Length : constant Positive :=
         Map.Bias + Map.Length;

   begin

      if Length rem Elements_Per_Node = 0 then
         New_Back (Map);
      end if;

      if Last = Map.Last then
         declare
            Node : Node_Type renames Map.Nodes (Last).all;

            Offset : constant Natural :=
               (Map.Last - Map.First) * Elements_Per_Node;

            F : constant Positive := Index + 1 - Offset;
            L : constant Natural := Length - Offset;
         begin
            Element_Node := Map.Nodes (Last);
            Element_Index := F;

            Node.Elements (F + 1 .. L + 1) :=
               Node.Elements (F .. L);
         end;

         Map.Length := Map.Length + 1;

         return;
      end if;

      declare
         Offset : constant Natural :=
            (Map.Last - Map.First) * Elements_Per_Node;

         Count : constant Natural :=
            Length - Offset;

         Node : Node_Type renames Map.Nodes (Map.Last).all;
      begin
         Node.Elements (2 .. 1 + Count) :=
            Node.Elements (1 .. Count);
      end;

      for I in reverse Last + 2 .. Map.Last loop
         declare
            Curr : Node_Type renames Map.Nodes (I).all;
            Prev : Node_Type renames Map.Nodes (I - 1).all;
         begin
            Curr.Elements (1) :=
               Prev.Elements (Elements_Per_Node);

            Prev.Elements (2 .. Elements_Per_Node) :=
               Prev.Elements (1 .. Elements_Per_Node - 1);
         end;
      end loop;

      declare
         Offset : constant Natural :=
            (Last - Map.First) * Elements_Per_Node;

         Curr : Node_Type renames Map.Nodes (Last + 1).all;
         Prev : Node_Type renames Map.Nodes (Last).all;

         F : constant Positive := Index + 1 - Offset;
      begin
         Curr.Elements (1) :=
            Prev.Elements (Elements_Per_Node);

         Element_Node := Map.Nodes (Last);
         Element_Index := F;

         Prev.Elements (F + 1 .. Elements_Per_Node) :=
            Prev.Elements (F .. Elements_Per_Node - 1);
      end;

      Map.Length := Map.Length + 1;

   end Insert_Back;


   procedure Insert
     (Map      : in out Map_Type;
      Before   : in     Natural;
      New_Item : in     Element_Type) is

      Element_Node : Node_Access;
      Element_Index : Positive;
   begin
      if Before <= Map.Length / 2 then
         Insert_Front (Map, Before, Element_Node, Element_Index);
      else
         Insert_Back (Map, Before, Element_Node, Element_Index);
      end if;

      Element_Node.Elements (Element_Index) := New_Item;
   end;



   procedure Insert_Range_Back_Onto_Last_Node
     (Map      : in out Map_Type;
      Before   : in     Integer'Base;
      Length   : in     Natural) is

      Index : constant Natural :=
         Map.Bias + Before;

      Node_Index : constant Positive :=
         Map.First + Index / Elements_Per_Node;

   begin

      if Node_Index = Map.Last then

         declare
            Node : Node_Type renames Map.Nodes (Map.Last).all;

            Offset : constant Natural :=
               (Map.Last - Map.First) * Elements_Per_Node;

            E : Element_Array renames Node.Elements;

            Src_First : constant Positive :=
               1 + Index - Offset;

            Length_Index : constant Natural :=
               Map.Bias + Map.Length;

            Src_Last : constant Positive :=
               Length_Index - Offset;
         begin
            E  (Src_First + Length .. Src_Last + Length) :=
               E (Src_First .. Src_Last);
         end;

         return;

      end if;

      declare
         Node : Node_Type renames Map.Nodes (Map.Last).all;

         Offset : constant Natural :=
            (Map.Last - Map.First) * Elements_Per_Node;

         E : Element_Array renames Node.Elements;

         Length_Index : constant Natural :=
            Map.Bias + Map.Length;

         Src_Last : constant Positive :=
            Length_Index - Offset;
      begin
         E (1 + Length .. Src_Last + Length) :=
            E (1 .. Src_Last);
      end;

      for I in reverse Node_Index + 2 .. Map.Last loop
         declare
            Curr : Node_Type renames Map.Nodes (I).all;
            C : Element_Array renames Curr.Elements;

            Prev : Node_Type renames Map.Nodes (I - 1).all;
            P : Element_Array renames Prev.Elements;
         begin
            C (1 .. Length) :=
               P (Elements_Per_Node - Length + 1 .. Elements_Per_Node);

            P (1 + Length .. Elements_Per_Node) :=
               P (1 .. Elements_Per_Node - Length);
         end;
      end loop;

      declare
         Curr : Node_Type renames Map.Nodes (Node_Index + 1).all;
         C : Element_Array renames Curr.Elements;

         Prev : Node_Type renames Map.Nodes (Node_Index).all;
         P : Element_Array renames Prev.Elements;

         Offset : constant Natural :=
            (Node_Index - Map.First) * Elements_Per_Node;

         I : constant Positive := 1 + Index - Offset;
      begin
         C (1 .. Length) :=
            P (Elements_Per_Node - Length + 1 .. Elements_Per_Node);

         P (I + Length ..  Elements_Per_Node) :=
            P (I .. Elements_Per_Node - Length);
      end;

   end Insert_Range_Back_Onto_Last_Node;


   procedure Insert_Range_Back_Onto_Last_Nodes
     (Map    : in out Map_Type;
      Before : in     Natural;
      Length : in     Positive) is

      New_Length : constant Positive :=
         Map.Bias + Map.Length + Length;

      New_Node_Count : constant Positive :=
         (New_Length + Elements_Per_Node - 1) / Elements_Per_Node;

      Node_Count : constant Positive :=
         Map.Last - Map.First + 1;

      pragma Assert (New_Node_Count > Node_Count);

   begin

      if Map.Nodes'Length < New_Node_Count then

         declare
            Nodes : constant Node_Access_Array_Access :=
               new Node_Access_Array (1 .. New_Node_Count);
         begin
            Nodes (1 .. Node_Count) :=
               Map.Nodes (Map.First .. Map.Last);

            Free (Map.Nodes);

            Map.Nodes := Nodes;

            Map.First := 1;

            Map.Last := New_Node_Count;

            Map.Nodes (Map.First + Node_Count .. Map.Last) :=
               (others => new Node_Type);
         end;

      elsif Map.First + New_Node_Count - 1 > Map.Nodes'Last then

         Map.Nodes (1 .. Node_Count) :=
            Map.Nodes (Map.First .. Map.Last);

         Map.First := 1;

         Map.Last := New_Node_Count;

         Map.Nodes (Map.First + Node_Count .. Map.Last) :=
            (others => new Node_Type);

      else

         Map.Nodes (Map.Last + 1 .. Map.First + New_Node_Count - 1) :=
            (others => new Node_Type);

         Map.Last := Map.First + New_Node_Count - 1;

      end if;

      declare

         SI : constant Natural := Before;
         SJ : constant Natural := Map.Length - 1;

         DJ : Natural := SJ + Length;

         S_NI : Positive;
         S_EI : Positive;

         D_NI : Positive;
         D_EI : Positive;

      begin

         for I in reverse SI .. SJ loop

            Element (Map, I, S_NI, S_EI);
            Element (Map, DJ, D_NI, D_EI);

            declare
               SN : constant Node_Access := Map.Nodes (S_NI);
               DN : constant Node_Access := Map.Nodes (D_NI);

               SA : Element_Array renames SN.Elements;
               DA : Element_Array renames DN.Elements;

               SE : Element_Type renames SA (S_EI);
               DE : Element_Type renames DA (D_EI);
            begin
               DE := SE;
            end;

            DJ := DJ - 1;

         end loop;

      end;

   end Insert_Range_Back_Onto_Last_Nodes;

   procedure Insert_Range_Back
     (Map      : in out Map_Type;
      Before   : in     Integer'Base;
      Length   : in     Positive) is

      Node_Count : constant Positive :=
         Map.Last - Map.First + 1;

      Element_Count : constant Natural :=
         Map.Bias + Map.Length;

      Available_Count : constant Natural :=
         Node_Count * Elements_Per_Node - Element_Count;
   begin
      if Available_Count >= Length then
         Insert_Range_Back_Onto_Last_Node (Map, Before, Length);
      else
         Insert_Range_Back_Onto_Last_Nodes (Map, Before, Length);
      end if;
   end Insert_Range_Back;


   procedure Insert_Range_Empty
     (Map    : in out Map_Type;
      Length : in     Positive) is

      pragma Assert (Map.Length = 0);

      Node_Count : Positive;

   begin

      Map.Bias := Elements_Per_Node / 2;

      Node_Count :=
        (Map.Bias + Length + Elements_Per_Node - 1) /
         Elements_Per_Node;

      if Map.Nodes = null then

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

      elsif Map.Nodes'Length < Node_Count then

         Free (Map.Nodes);

         Map.Nodes := new Node_Access_Array (1 .. Node_Count);

      end if;

      Map.First := Map.Nodes'First;

      Map.Last := Map.First + Node_Count - 1;

      Map.Nodes (Map.First .. Map.Last) := (others => new Node_Type);

   end Insert_Range_Empty;


   procedure Insert_Range_Front_Onto_First_Node
     (Map    : in out Map_Type;
      Before : in     Natural;
      Length : in     Positive) is

      Bias : constant Natural := Map.Bias - Length;

      Dst_Index : Natural := 0;

      procedure Copy (Src : Element_Type) is

         NI : constant Positive :=
            Map.First + (Bias + Dst_Index) / Elements_Per_Node;

         N : Node_Type renames Map.Nodes (NI).all;

         E : Element_Array renames N.Elements;

         EI : constant Positive :=
            1 + (Bias + Dst_Index) rem Elements_Per_Node;

         Dst : Element_Type renames E (EI);
      begin
         Dst := Src;
         Dst_Index := Dst_Index + 1;
      end;

      procedure Process
        (Node        : access Node_Type;
         First, Last : Positive) is
      begin
         for I in First .. Last loop
            Copy (Src => Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Generic_Iteration (Process);

   begin

      Iterate (Map, First => 0, Last => Before - 1);

      Map.Bias := Bias;

   end Insert_Range_Front_Onto_First_Node;


   procedure Insert_Range_Front_Onto_First_Nodes
     (Map    : in out Map_Type;
      Before : in     Natural;
      Length : in     Positive) is

      N : constant Positive := Length - Map.Bias;

      M : constant Positive :=
         (N + Elements_Per_Node - 1) / Elements_Per_Node;

      Node_Count : constant Positive :=
         Map.Last - Map.First + 1;

      New_Node_Count : constant Positive :=
         Node_Count + M;

   begin

      if New_Node_Count > Map.Nodes'Length then

         declare
            Nodes : constant Node_Access_Array_Access :=
               new Node_Access_Array (1 .. New_Node_Count);
         begin
            Nodes (1 .. M) := (others => new Node_Type);

            Nodes (M + 1 .. Nodes'Last) :=
               Map.Nodes (Map.First .. Map.Last);

            Map.First := M + 1;
            Map.Last := Nodes'Last;

            Free (Map.Nodes);

            Map.Nodes := Nodes;
         end;

      elsif Map.First <= M then

         Map.Nodes (M + 1 .. New_Node_Count) :=
            Map.Nodes (Map.First .. Map.Last);

         Map.Nodes (1 .. M) := (others => new Node_Type);

         Map.First := M + 1;
         Map.Last := New_Node_Count;

      else

         Map.Nodes (Map.First - M .. Map.First - 1) :=
            (others => new Node_Type);

      end if;

      declare
         Bias : constant Natural :=
            M * Elements_Per_Node - N;

         First : constant Positive :=
            Map.First - M;

         Dst_Index : Natural := 0;

         procedure Copy (Src : Element_Type) is

            NI : constant Positive :=
               First + (Bias + Dst_Index) / Elements_Per_Node;

            N : Node_Type renames Map.Nodes (NI).all;

            E : Element_Array renames N.Elements;

            EI : constant Positive :=
               1 + (Bias + Dst_Index) rem Elements_Per_Node;

            Dst : Element_Type renames E (EI);

         begin

            Dst := Src;

            Dst_Index := Dst_Index + 1;

         end Copy;

         procedure Process
           (Node        : access Node_Type;
            First, Last : Positive) is
         begin
            for I in First .. Last loop
               Copy (Src => Node.Elements (I));
            end loop;
         end;

         procedure Iterate is
            new Generic_Iteration (Process);
      begin
         Iterate (Map, First => 0, Last => Before - 1);

         Map.Bias := Bias;

         Map.First := First;
      end;

   end Insert_Range_Front_Onto_First_Nodes;


   procedure Insert_Range_Front
     (Map    : in out Map_Type;
      Before : in     Natural;
      Length : in     Positive) is
   begin

      if Length <= Map.Bias then
         Insert_Range_Front_Onto_First_Node (Map, Before, Length);
      else
         Insert_Range_Front_Onto_First_Nodes (Map, Before, Length);
      end if;

   end Insert_Range_Front;


   procedure Insert_Range
     (Map      : in out Map_Type;
      Before   : in     Integer'Base;
      Length   : in     Natural) is

   begin

      if Length = 0 then

         null;

      elsif Map.Nodes = null
        or else Map.Last < Map.First
      then

         Insert_Range_Empty (Map, Length);

      elsif Before <= Map.Length / 2 then

         Insert_Range_Front (Map, Before, Length);

      else

         Insert_Range_Back (Map, Before, Length);

      end if;

      Map.Length := Map.Length + Length;

   end Insert_Range;


   procedure Delete_Back
     (Map   : in out Map_Type;
      Index : in     Natural) is

      procedure Post_Delete is
      begin
         Map.Length := Map.Length - 1;

         declare
            Length : constant Natural :=
               Map.Bias + Map.Length;
         begin
            if Length rem Elements_Per_Node = 0 then
               Free (Map.Nodes (Map.Last));
               Map.Last := Map.Last - 1;
            end if;
         end;
      end;

      I : constant Natural := Map.Bias + Index;

      J : constant Positive :=
         Map.First + I / Elements_Per_Node;

   begin -- Delete_Back

      if J = Map.Last then
         declare
            Length : constant Natural := Map.Bias + Map.Length;

            Offset : constant Natural :=
               (J - Map.First) * Elements_Per_Node;

            Node : Node_Type renames Map.Nodes (Map.Last).all;

            E : Element_Array renames Node.Elements;
         begin
            E (I + 1 - Offset .. Length - 1 - Offset) :=
               E (I + 2 - Offset .. Length - Offset);
         end;

         Post_Delete;

         return;
      end if;

      declare
         Offset : constant Natural :=
            (J - Map.First) * Elements_Per_Node;

         Node : Node_Type renames Map.Nodes (J).all;

         E : Element_Array renames Node.Elements;
      begin
         E (I + 1 - Offset .. Elements_Per_Node - 1) :=
            E (I + 2 - Offset .. Elements_Per_Node);
      end;

      for K in J .. Map.Last - 2 loop
         declare
            Curr : Node_Type renames Map.Nodes (K).all;
            Next : Node_Type renames Map.Nodes (K + 1).all;
         begin
            Curr.Elements (Elements_Per_Node) :=
               Next.Elements (1);

            Next.Elements (1 .. Elements_Per_Node - 1) :=
               Next.Elements (2 .. Elements_Per_Node);
         end;
      end loop;

      declare
         Curr : Node_Type renames Map.Nodes (Map.Last - 1).all;
         Next : Node_Type renames Map.Nodes (Map.Last).all;

         Length : constant Natural :=
            Map.Bias + Map.Length;

         Offset : constant Natural :=
            (Map.Last - Map.First) * Elements_Per_Node;
      begin
         Curr.Elements (Elements_Per_Node) :=
            Next.Elements (1);

         Next.Elements (1 .. Length - 1 - Offset) :=
            Next.Elements (2 .. Length - Offset);
      end;

      Post_Delete;

   end Delete_Back;


   procedure Delete_Front
     (Map   : in out Map_Type;
      Index : in     Natural) is

      procedure Post_Delete is
      begin
         Map.Length := Map.Length - 1;

         Map.Bias := Map.Bias + 1;

         if Map.Bias = Elements_Per_Node then
            Free (Map.Nodes (Map.First));
            Map.First := Map.First + 1;
            Map.Bias := 0;
         end if;
      end;

      I : constant Natural := Map.Bias + Index;

      J : constant Positive :=
         Map.First + I / Elements_Per_Node;

   begin

      if J = Map.First then
         declare
            Node : Node_Type renames Map.Nodes (J).all;
            E : Element_Array renames Node.Elements;
         begin
            E (Map.Bias + 2 .. I + 1) := E (Map.Bias + 1 .. I);
         end;

         Post_Delete;

         return;
      end if;

      declare
         Node : Node_Type renames Map.Nodes (J).all;

         E : Element_Array renames Node.Elements;

         Offset : constant Positive :=
            (J - Map.First) * Elements_Per_Node;
      begin
         E (2 .. I + 1 - Offset) := E (1 .. I - Offset);
      end;

      for K in reverse Map.First + 2 .. J loop
         declare
            Curr : Node_Type renames Map.Nodes (K).all;

            Prev : Node_Type renames Map.Nodes (K - 1).all;
            E : Element_Array renames Prev.Elements;
         begin
            Curr.Elements (1) := E (Elements_Per_Node);

            E (2 .. Elements_Per_Node) :=
               E (1 .. Elements_Per_Node - 1);
         end;
      end loop;

      declare
         Curr : Node_Type renames Map.Nodes (Map.First + 1).all;

         Prev : Node_Type renames Map.Nodes (Map.First).all;
         E : Element_Array renames Prev.Elements;
      begin
         Curr.Elements (1) := E (Elements_Per_Node);

         E (Map.Bias + 2 .. Elements_Per_Node) :=
            E (Map.Bias + 1 .. Elements_Per_Node - 1);
      end;

      Post_Delete;

   end Delete_Front;


   procedure Delete
     (Map   : in out Map_Type;
      Index : in     Natural) is
   begin
      if Index <= Map.Length / 2 then
         Delete_Front (Map, Index);
      else
         Delete_Back (Map, Index);
      end if;
   end;


   procedure Delete_Range_Front
     (Map   : in out Map_Type;
      First : in     Integer'Base;
      Last  : in     Integer'Base) is

      Dst_Index : Integer'Base := Last;

      procedure Process (Src : Element_Type) is
         Dst_NI : Positive;
         Dst_EI : Positive;
      begin
         Element (Map, Dst_Index, Dst_NI, Dst_EI);
         Map.Nodes (Dst_NI).Elements (Dst_EI) := Src;
         Dst_Index := Dst_Index - 1;
      end;

      procedure Process
        (Node              : access Node_Type;
         First_EI, Last_EI : Positive) is
      begin
         for I in reverse First_EI .. Last_EI loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Generic_Reverse_Iteration (Process);

      Length : constant Positive := Last - First + 1;
   begin
      Iterate (Map, First => 0, Last => First - 1);

      Map.Bias := Map.Bias + Length;

      while Map.Bias >= Elements_Per_Node loop
         Free (Map.Nodes (Map.First));
         Map.First := Map.First + 1;
         Map.Bias := Map.Bias - Elements_Per_Node;
      end loop;

      Map.Length := Map.Length - Length;
   end;


   procedure Delete_Range_Back
     (Map   : in out Map_Type;
      First : in     Integer'Base;
      Last  : in     Integer'Base) is

      Dst_Index : Integer'Base := First;

      procedure Process (Src : Element_Type) is
         Dst_NI : Positive;
         Dst_EI : Positive;
      begin
         Element (Map, Dst_Index, Dst_NI, Dst_EI);
         Map.Nodes (Dst_NI).Elements (Dst_EI) := Src;
         Dst_Index := Dst_Index + 1;
      end;

      procedure Process
        (Node              : access Node_Type;
         First_EI, Last_EI : Positive) is
      begin
         for I in First_EI .. Last_EI loop
            Process (Node.Elements (I));
         end loop;
      end;

      procedure Iterate is
         new Generic_Iteration (Process);

      Length : constant Positive := Last - First + 1;
   begin
      Iterate (Map, First => Last + 1, Last => Map.Length - 1);

      Map.Length := Map.Length - Length;

      declare
         Index : constant Natural := Integer'Max (Map.Length - 1, 0);

         Node_Index : constant Positive :=
            Map.First + (Map.Bias + Index) / Elements_Per_Node;
      begin
         for I in Node_Index + 1 .. Map.Last loop
            Free (Map.Nodes (I));
         end loop;

         Map.Last := Node_Index;
      end;
   end Delete_Range_Back;


   procedure Delete
     (Map         : in out Map_Type;
      First, Last : in     Integer'Base) is
   begin
      if Last < First then
         null;
      elsif First <= Map.Length / 2 then
         Delete_Range_Front (Map, First, Last);
      else
         Delete_Range_Back (Map, First, Last);
      end if;
   end;


   function Element
     (Map   : Map_Type;
      Index : Natural) return Element_Type is

      Node_Index : constant Integer'Base :=
         Map.First + (Map.Bias + Index) / Elements_Per_Node;

      Node : Node_Type renames Map.Nodes (Node_Index).all;

      Element_Index : constant Integer'Base :=
         1 + (Map.Bias + Index) rem Elements_Per_Node;

      Result : Element_Type renames Node.Elements (Element_Index);
   begin
      return Result;
   end;


   procedure Element
     (Map           : in     Map_Type;
      Index         : in     Natural;
      Node_Index    :    out Positive;
      Element_Index :    out Positive) is
   begin
      Node_Index := Map.First + (Map.Bias + Index) / Elements_Per_Node;
      Element_Index := 1 + (Map.Bias + Index) rem Elements_Per_Node;
   end;


   procedure Replace_Element
     (Map   : in Map_Type;
      Index : in Natural;
      Item  : in Element_Type) is

      Node_Index    : Positive;
      Element_Index : Positive;
   begin
      Element (Map, Index, Node_Index, Element_Index);

      declare
         Node : Node_Type renames Map.Nodes (Node_Index).all;
      begin
         Node.Elements (Element_Index) := Item;
      end;
   end;


   procedure Generic_Iteration
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) is
   begin

      if First > Last
        or else Map.First > Map.Last
      then
         return;
      end if;

--      if Map.First = Map.Last then
--         declare
--            First_Element : constant Positive :=
--               Map.Bias + 1 + First;
--
--            Last_Element : constant Natural :=
--               First_Element + Last - First;
--         begin
--            Process
--              (Map.Nodes (Map.First),
--               First_Element,
--               Last_Element);
--         end;
--
--         return;
--      end if;

      declare
         First_NI : Positive;
         First_EI : Positive;

         Last_NI : Positive;
         Last_EI : Positive;
      begin
         Element (Map, First, First_NI, First_EI);
         Element (Map, Last, Last_NI, Last_EI);

         if First_NI = Last_NI then

            Process (Map.Nodes (First_NI), First_EI, Last_EI);

         else

            Process (Map.Nodes (First_NI), First_EI, Elements_Per_Node);

            for I in First_NI + 1 .. Last_NI - 1 loop
               Process (Map.Nodes (I), 1, Elements_Per_Node);
            end loop;

            Process (Map.Nodes (Last_NI), 1, Last_EI);

         end if;
      end;

   end Generic_Iteration;


   procedure Generic_Reverse_Iteration
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) is
   begin

      if First > Last
        or else Map.First > Map.Last
      then
         return;
      end if;

--      if Map.First = Map.Last then
--         declare
--            First_Element : constant Positive :=
--               Map.Bias + 1 + First;
--
--            Last_Element : constant Natural :=
--               First_Element + Last - First;
--         begin
--            Process
--              (Map.Nodes (Map.First),
--               First_Element,
--               Last_Element);
--         end;
--
--         return;
--      end if;

      declare
         First_NI : Positive;
         First_EI : Positive;

         Last_NI : Positive;
         Last_EI : Positive;
      begin
         Element (Map, First, First_NI, First_EI);
         Element (Map, Last, Last_NI, Last_EI);

         if First_NI = Last_NI then

            Process (Map.Nodes (First_NI), First_EI, Last_EI);

         else

            Process (Map.Nodes (Last_NI), 1, Last_EI);

            for I in reverse First_NI + 1 .. Last_NI - 1 loop
               Process (Map.Nodes (I), 1, Elements_Per_Node);
            end loop;

            Process (Map.Nodes (First_NI), First_EI, Elements_Per_Node);

         end if;
      end;

   end Generic_Reverse_Iteration;


   function Generic_Find
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) return Integer'Base is

      function Index
        (NI : Positive;
         EI : Positive) return Integer'Base is

         Node_Count : constant Natural := NI - Map.First;

         Offset : constant Natural := Node_Count * Elements_Per_Node;

         Result : constant Natural :=
            Offset + EI - Map.Bias - 1;
      begin
         return Result;
      end;

   begin

      if First > Last
        or else Map.First > Map.Last
      then
         return Map.Length;
      end if;

--      if Map.First = Map.Last then
--         declare
--            First_Element : constant Positive :=
--               Map.Bias + 1 + First;
--
--            Last_Element : constant Natural :=
--               First_Element + Last - First;
--
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            for I in First_Element .. Last_Element loop
--               if Predicate (Node.Elements (I)) then
--                  return I - Map.Bias - 1;
--               end if;
--            end loop;
--
--            return Map.Length;
--         end;
--      end if;

      declare
         First_NI : Positive;
         First_EI : Positive;

         Last_NI : Positive;
         Last_EI : Positive;
      begin
         Element (Map, First, First_NI, First_EI);
         Element (Map, Last, Last_NI, Last_EI);

         if First_NI = Last_NI then

            declare
               Node : Node_Type renames Map.Nodes (First_NI).all;
            begin
               for I in First_EI .. Last_EI loop
                  if Predicate (Node.Elements (I)) then
                     return Index (First_NI, I);
                  end if;
               end loop;
            end;

            return Map.Length;

         end if;

         declare
            Node : Node_Type renames Map.Nodes (First_NI).all;
         begin
            for I in First_EI .. Elements_Per_Node loop
               if Predicate (Node.Elements (I)) then
                  return Index (First_NI, I);
               end if;
            end loop;
         end;

         for I in First_NI + 1 .. Last_NI - 1 loop
            declare
               Node : Node_Type renames Map.Nodes (I).all;
            begin
               for J in 1 .. Elements_Per_Node loop
                  if Predicate (Node.Elements (J)) then
                     return Index (I, J);
                  end if;
               end loop;
            end;
         end loop;

         declare
            Node : Node_Type renames Map.Nodes (Last_NI).all;
         begin
            for I in 1 .. Last_EI loop
               if Predicate (Node.Elements (I)) then
                  return Index (Last_NI, I);
               end if;
            end loop;
         end;

         return Map.Length;
      end;

   end Generic_Find;


   function Generic_Reverse_Find
     (Map   : Map_Type;
      First : Integer'Base;
      Last  : Integer'Base) return Integer'Base is

      function Index
        (NI : Positive;
         EI : Positive) return Integer'Base is

         Node_Count : constant Natural := NI - Map.First;

         Offset : constant Natural := Node_Count * Elements_Per_Node;

         Result : constant Natural :=
            Offset + EI - Map.Bias - 1;
      begin
         return Result;
      end;

   begin

      if First > Last
        or else Map.First > Map.Last
      then
         return Map.Length;
      end if;

--      if Map.First = Map.Last then
--         declare
--            First_Element : constant Positive :=
--               Map.Bias + 1 + First;
--
--            Last_Element : constant Natural :=
--               First_Element + Last - First;
--
--            Node : Node_Type renames Map.Nodes (Map.First).all;
--         begin
--            for I in reverse First_Element .. Last_Element loop
--               if Predicate (Node.Elements (I)) then
--                  return I - Map.Bias - 1;
--               end if;
--            end loop;
--
--            return Map.Length;
--         end;
--      end if;

      declare
         First_NI : Positive;
         First_EI : Positive;

         Last_NI : Positive;
         Last_EI : Positive;
      begin
         Element (Map, First, First_NI, First_EI);
         Element (Map, Last, Last_NI, Last_EI);

         if First_NI = Last_NI then

            declare
               Node : Node_Type renames Map.Nodes (First_NI).all;
            begin
               for I in reverse First_EI .. Last_EI loop
                  if Predicate (Node.Elements (I)) then
                     return Index (First_NI, I);
                  end if;
               end loop;
            end;

            return Map.Length;

         end if;

         declare
            Node : Node_Type renames Map.Nodes (Last_NI).all;
         begin
            for I in reverse 1 .. Last_EI loop
               if Predicate (Node.Elements (I)) then
                  return Index (Last_NI, I);
               end if;
            end loop;
         end;

         for I in reverse First_NI + 1 .. Last_NI - 1 loop
            declare
               Node : Node_Type renames Map.Nodes (I).all;
            begin
               for J in reverse 1 .. Elements_Per_Node loop
                  if Predicate (Node.Elements (J)) then
                     return Index (I, J);
                  end if;
               end loop;
            end;
         end loop;

         declare
            Node : Node_Type renames Map.Nodes (First_NI).all;
         begin
            for I in reverse First_EI .. Elements_Per_Node loop
               if Predicate (Node.Elements (I)) then
                  return Index (First_NI, I);
               end if;
            end loop;
         end;

         return Map.Length;
      end;

   end Generic_Reverse_Find;


end Charles.Deque_Types;
