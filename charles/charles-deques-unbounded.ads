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

with Charles.Deque_Types;
pragma Elaborate_All (Charles.Deque_Types);

with Ada.Finalization;

generic

   type Index_Type is (<>);

   type Element_Type is private;

   with function "=" (L, R : Element_Type) return Boolean is <>;

package Charles.Deques.Unbounded is

   pragma Preelaborate;


   subtype Index_Subtype is Index_Type;

   subtype Element_Subtype is Element_Type;

   function Compare_Elements (L, R : Element_Type)
      return Boolean renames "=";


   type Container_Type is private;



   function "=" (Left, Right : Container_Type) return Boolean;

   function Length (Container : Container_Type) return Natural;

   function Is_Empty (Container : Container_Type) return Boolean;

   procedure Clear (Container : in out Container_Type);

   procedure Swap (Left, Right : in out Container_Type);


   function To_Container
      (Length : Natural) return Container_Type;

   function To_Container
     (Length : Natural;
      Item   : Element_Type) return Container_Type;

   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural);

   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural;
      Item   : in     Element_Type);

   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type);


   procedure Prepend
     (Container : in out Container_Type);

   procedure Prepend
     (Container : in out Container_Type;
      New_Item  : in     Element_Type);

   procedure Delete_First (Container : in out Container_Type);


   procedure Append
     (Container : in out Container_Type);

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Element_Type);

   procedure Delete_Last (Container : in out Container_Type);



   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      New_Item  : in     Element_Type);

   procedure Insert_Range
     (Container : in out Container_Type;
      Before    : in     Index_Type'Base;
      Length    : in     Natural);

   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Index_Type);

   procedure Delete
     (Container : in out Container_Type;
      First     : in     Index_Type;
      Back      : in     Index_Type'Base);


   function Front (Container : Container_Type) return Index_Type'Base;

   function First (Container : Container_Type) return Index_Type;

   function Last (Container : Container_Type) return Index_Type'Base;

   function Back (Container : Container_Type) return Index_Type'Base;

   function Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element
     (Container : Container_Type;
      Index     : Index_Type) return Element_Access;

   procedure Replace_Element
     (Container : Container_Type;
      Index     : Index_Type;
      By        : Element_Type);

--   procedure Assign
--     (Target : in out Element_Type;
--      Source : in     Container_Type;
--      Index  : in     Index_Type);
   procedure Copy
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      :    out Element_Type);

   procedure Swap
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type);

   generic
      with procedure Swap (L, R : in out Element_Type) is <>;
   procedure Generic_Swap
     (Container : in     Container_Type;
      Index     : in     Index_Type;
      Item      : in out Element_Type);

   procedure Swap_Element
     (Container   : in Container_Type;
      Left, Right : in Index_Type);

   generic
      with procedure Swap
        (L, R : in out Element_Type) is <>;
   procedure Generic_Swap_Element
     (Container   : in Container_Type;
      Left, Right : in Index_Type);

   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Select_Element
     (Container : Container_Type;
      Index     : Index_Type);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Element
     (Container : Container_Type;
      Index     : Index_Type);

   generic
      with procedure Process (Element : access Element_Type) is <>;
   procedure Generic_Access_Element
     (Container : Container_Type;
      Index     : Index_Type);

   generic
      with procedure Process
        (Container : Container_Type;
         Index     : Index_Type) is <>;
   procedure Generic_Iteration
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);

   generic
      with procedure Process
        (Container : Container_Type;
         Index     : Index_Type) is <>;
   procedure Generic_Reverse_Iteration
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);


   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Select_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Modify_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);

   generic
      with procedure Process (Element : access Element_Type) is <>;
   procedure Generic_Access_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);


   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Reverse_Select_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Reverse_Modify_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);

   generic
      with procedure Process (Element : access Element_Type) is <>;
   procedure Generic_Reverse_Access_Elements
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base);


   function Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base;

   function Find
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base;

   generic
      with function Predicate (Element : Element_Type)
         return Boolean is <>;
   function Generic_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) return Index_Type'Base;

   function Reverse_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base;
      Item      : Element_Type) return Index_Type'Base;

   function Reverse_Find
     (Container : Container_Type;
      Item      : Element_Type) return Index_Type'Base;

   generic
      with function Predicate (Element : Element_Type)
         return Boolean is <>;
   function Generic_Reverse_Find
     (Container : Container_Type;
      First     : Index_Type;
      Back      : Index_Type'Base) return Index_Type'Base;

private

   Elements_Per_Node : constant := 8; --?

   package Rep_Types is
      new Charles.Deque_Types (Element_Type,
                               Elements_Per_Node,
                               "=" => "=");

   type Container_Type is
      new Ada.Finalization.Controlled with record
         Map : aliased Rep_Types.Map_Type;
      end record;

   procedure Initialize (Container : in out Container_Type);

   procedure Adjust (Container : in out Container_Type);

   procedure Finalize (Container : in out Container_Type);


end Charles.Deques.Unbounded;
