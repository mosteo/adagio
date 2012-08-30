generic

   type Iterator_Type is private;
   
   --with procedure Debug (Iter : Iterator_Type; Text : String);
   --with procedure Debug (Text : String);
   
   with function Is_Less (L, R : Iterator_Type) return Boolean is <>;
   
   with procedure Swap (L, R : Iterator_Type) is <>;
 
   with function "+" (L : Iterator_Type; R : Integer'Base) 
      return Iterator_Type is <>;
      
   with function "-" (L : Iterator_Type; R : Integer'Base)
      return Iterator_Type is <>;

   with function "-" (L, R : Iterator_Type)
      return Integer'Base is <>;
      
   with function "<" (L, R : Iterator_Type)
      return Boolean is <>;
      
   with function "=" (L, R : Iterator_Type)
      return Boolean is <>;

procedure Charles.Algorithms.Generic_Quicksort
  (First : in Iterator_Type;
   Back  : in Iterator_Type);
   
  