------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--                   Charles.Strings.Bounded (spec)                         --
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
package Charles.Strings.Bounded is

   pragma Pure;
   
   
   type Container_Type (Size : Natural) is 
      limited record
         Elements : aliased String (1 .. Size);
         Last     : Natural := 0;
      end record;
      
   
   function "=" (Left, Right : Container_Type) return Boolean;
   
   function "=" 
      (Left  : Container_Type;
       Right : String) return Boolean;

   function "=" 
      (Left  : String;
       Right : Container_Type) return Boolean;

   function "<" (Left, Right : Container_Type) return Boolean;
   
   function "<" 
     (Left  : Container_Type;
      Right : String) return Boolean;

   function "<" 
     (Left  : String;
      Right : Container_Type) return Boolean;

   function "<=" (Left, Right : Container_Type) return Boolean;
   
   function "<=" 
     (Left  : Container_Type;
      Right : String) return Boolean;

   function "<=" 
     (Left  : String;
      Right : Container_Type) return Boolean;
      
   function ">=" (Left, Right : Container_Type) return Boolean;
   
   function ">=" 
     (Left  : Container_Type;
      Right : String) return Boolean;

   function ">=" 
     (Left  : String;
      Right : Container_Type) return Boolean;
      
      
   function Length (Container : Container_Type) return Natural;
   
   function Is_Empty (Container : Container_Type) return Boolean;
   
   procedure Clear (Container : in out Container_Type);
         

   procedure Assign
     (Target : in out Container_Type;
      Length : in     Natural);
      
   procedure Assign
     (Target : in out Container_Type;
      Source : in     Character;
      Count  : in     Natural := 1);

   procedure Assign
     (Target : in out Container_Type;
      Source : in     String);

   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type);

   procedure Assign
     (Target : in out Container_Type;
      Source : in     Container_Type;
      Index  : in     Positive;
      Length : in     Natural);

   procedure Assign_Slice
     (Target : in out Container_Type;
      Source : in     Container_Type;
      Low    : in     Positive;
      High   : in     Natural);

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      Count     : in     Natural);

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Character;
      Count     : in     Natural := 1);

   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     String);
     
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Container_Type);
      
   procedure Insert
     (Container : in out Container_Type;
      Before    : in     Positive;
      New_Item  : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural);
      
   procedure Delete
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural);
                  
   procedure Delete_Slice
     (Container : in out Container_Type;
      From      : in     Positive;
      Through   : in     Natural);

   function Size (Container : Container_Type) return Natural;
   
--   procedure Resize
--     (Container : in out Container_Type;
--      Size      : in     Natural);
      
   function Front 
     (Container : Container_Type) return Natural;

   function First 
     (Container : Container_Type) return Positive;

   function Last
     (Container : Container_Type) return Natural;

   function Back 
     (Container : Container_Type) return Positive;

   function Element 
     (Container : Container_Type;
      Index     : Positive) return Character;
      
   procedure Replace_Element
     (Container : in Container_Type;
      Index     : in Positive;
      By        : in Character);
      
   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     String);

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Character;
      Count     : in     Natural := 1);

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Container_Type);      

   procedure Replace
     (Container : in out Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      By        : in     Container_Type;
      Position  : in     Positive;
      Count     : in     Natural);

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     String);

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Character;
      Count     : in     Natural := 1);

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Container_Type);

   procedure Replace_Slice
     (Container : in out Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      By        : in     Container_Type;
      Position  : in     Positive;
      Count     : in     Natural);

   function To_String
     (Container : Container_Type) return String;
     
   function To_String
     (Container : Container_Type;
      Index     : Positive;
      Length    : Natural) return String;
      
   function Slice
     (Container : Container_Type;
      Low       : Positive;
      High      : Natural) return String;      
      
   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Character;
      Count     : in     Natural := 1);

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     String);

   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Container_Type);
      
   procedure Append
     (Container : in out Container_Type;
      New_Item  : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural);

   procedure Append_Slice
     (Container : in out Container_Type;
      New_Item  : in     Container_Type;
      Low       : in     Positive;
      High      : in     Natural);
      
      
   procedure Copy
     (Container : in     Container_Type;
      Item      :    out String;
      Last      :    out Natural);

   procedure Copy
     (Container : in     Container_Type;
      Index     : in     Positive;
      Length    : in     Natural;
      Item      :    out String;
      Last      :    out Natural);
      
   procedure Copy_Slice
     (Container : in     Container_Type;
      Low       : in     Positive;
      High      : in     Natural;
      Item      :    out String;
      Last      :    out Natural);

   function Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural;
      
   function Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : String) return Natural;
      
   function Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : Container_Type) return Natural;

   function Reverse_Find 
     (Container : Container_Type;
      Index     : Positive;
      Item      : Character) return Natural;
      
   function Reverse_Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : String) return Natural;
      
   function Reverse_Find
     (Container : Container_Type;
      Index     : Positive;
      Item      : Container_Type) return Natural;
      

end Charles.Strings.Bounded;

         
