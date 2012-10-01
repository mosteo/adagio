------------------------------------------------------------------------------
--                                                                          --
--                     CHARLES CONTAINER LIBRARY                            --
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
with System;

with Charles.Algorithms.Generic_Lexicographical_Compare;

package body Charles.Vectors.Limited_Unbounded is

   use type System.Address;

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Array_Type,
                                      Element_Array_Access);


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


   procedure Finalize (Container : in out Container_Type) is
   begin
      Free (Container.Elements);
   exception
      when others =>
         Container.Elements := null;
         Container.Last := Last_Subtype'First;
         raise;
   end;


   function Generic_Equal
     (Left, Right : Container_Type) return Boolean is
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


   function To_Access
      (Container : Container_Type) return Element_Array_Access is
   begin
      return Container.Elements;
   end;


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


   procedure Swap (Left, Right : in out Container_Type) is

      L_EA : constant Element_Array_Access := Left.Elements;
      L_Last : constant Last_Subtype := Left.Last;

   begin

      Left.Elements := Right.Elements;
      Left.Last := Right.Last;

      Right.Elements := L_EA;
      Right.Last := L_Last;

   end Swap;


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
            new Element_Array_Type (Index_Type'First .. Last);
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


   procedure Append
     (Container : in out Container_Type) is

      subtype Size_Subtype is Index_Type'Base range
         Index_Type'First .. Container.Elements'Last;

      Last : constant Size_Subtype := Index_Type'Succ (Container.Last);
   begin
      Container.Last := Last;
   end;


   function Append
     (Container : access Container_Type) return Index_Type is
   begin
      Append (Container.all);
      return Container.Last;
   end;


   function Generic_Append
     (Container : access Container_Type) return Element_Access is
   begin
      Append (Container.all);
      return Container.Elements (Container.Last)'Access;
   end;


   procedure Append_N
     (Container : in out Container_Type;
      Count     : in     Natural) is
   begin
      if Count = 0 then
         return;
      end if;

      declare
         subtype Size_Subtype is Index_Type'Base range
            Index_Type'First .. Container.Elements'Last;

         Last : constant Size_Subtype :=
            To_Index (To_Integer (Container.Last) + Count);
      begin
         Container.Last := Last;
      end;
   end Append_N;


   procedure Delete_Last (Container : in out Container_Type) is
   begin
      Container.Last := Index_Type'Pred (Container.Last);
   end;


   procedure Delete_Last_N
     (Container : in out Container_Type;
      Count     : in     Natural) is
   begin
      Container.Last := To_Index (To_Integer (Container.Last) - Count);
   end;



   function Size (Container : Container_Type) return Natural is
   begin
      if Container.Elements = null then
         return 0;
      else
         return Container.Elements'Length;
      end if;
   end Size;


   procedure Resize
     (Container : in out Container_Type;
      Size      : in     Natural) is

      Last : constant Last_Subtype := To_Index (Size);

      subtype Range_Subtype is Index_Type'Base range
         Index_Type'First .. Last;

      subtype Array_Subtype is
         Element_Array_Type (Range_Subtype);
   begin
      if Container.Elements = null then
         pragma Assert (Container.Last = Last_Subtype'First);

         Container.Elements := new Array_Subtype;
         return;
      end if;

      if Container.Last > Last_Subtype'First then
         return;
      end if;

      if Size <= Container.Elements'Length then
         return;
      end if;

      begin
         Free (Container.Elements);
      exception
         when others =>
            Container.Elements := null;
            raise;
      end;

      Container.Elements := new Array_Subtype;
   end Resize;


   function First
     (Container : Container_Type) return Index_Type is
   begin
      return Index_Type'First;
   end;


   function Front
     (Container : Container_Type) return Index_Type'Base is
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

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      return Container.Elements (T'(Index));
   end;


   function Generic_Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Access is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;
   begin
      return Container.Elements (T'(Index))'Access;
   end;




   procedure Generic_Swap
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type) is

      EA : constant Element_Array_Access := Container.Elements;

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      E : Element_Type renames EA (T'(Index));
   begin
      Swap (E, Item);
   end;




   procedure Generic_Swap_Element
     (Container   : in Container_Type;
      Left, Right : in Index_Type) is

      subtype T is Index_Type'Base range
         Index_Type'First .. Container.Last;

      LI : constant T := Left;
      RI : constant T := Right;

      EA : Element_Array_Type renames Container.Elements.all;
   begin
      Swap (EA (LI), EA (RI));
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
   end Generic_Modify_Elements;



   procedure Generic_Access_Elements
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
   end Generic_Reverse_Modify_Elements;




   procedure Generic_Reverse_Access_Elements
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


end Charles.Vectors.Limited_Unbounded;
