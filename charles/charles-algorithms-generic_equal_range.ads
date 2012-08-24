generic

   type Iterator_Type is private;
   
   type Element_Type (<>) is limited private;
   
   with function Is_Less
     (Iterator : Iterator_Type;
      Item     : Element_Type) return Boolean is <>;

   with function Is_Less
     (Item     : Element_Type;
      Iterator : Iterator_Type) return Boolean is <>;
   
   with function Succ (Iterator : Iterator_Type) 
      return Iterator_Type is <>;
   
   with function Distance (First, Back : Iterator_Type) 
      return Integer'Base is <>;
      
   with procedure Advance 
     (Iterator : in out Iterator_Type;
      Distance : in     Integer'Base) is <>;

procedure Charles.Algorithms.Generic_Equal_Range
  (First, Back               : in     Iterator_Type;
   Item                      : in     Element_Type;
   Result_First, Result_Back :    out Iterator_Type);
   
