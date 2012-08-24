generic

   type Iterator_Type is private;
   
   type Element_Type (<>) is limited private;

   with function Is_Less 
     (L : Iterator_Type; 
      R : Element_Type) return Boolean is <>;
      
   with function Is_Less
     (L : Element_Type;
      R : Iterator_Type) return Boolean is <>;
      
   with function Succ (Iterator : Iterator_Type) 
      return Iterator_Type is <>;

   with function Distance (First, Back : Iterator_Type)
      return Integer'Base is <>;
      
   with procedure Advance 
     (Iterator : in out Iterator_Type;
      Distance : in     Integer'Base) is <>;
            
   with function "=" (L, R : Iterator_Type)
      return Boolean is <>;

function Charles.Algorithms.Generic_Binary_Search
  (First, Back : Iterator_Type;
   Item        : Element_Type) return Iterator_Type;


