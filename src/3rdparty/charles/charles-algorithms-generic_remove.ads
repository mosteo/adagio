generic

   type Iterator_Type is private;
   
   with function Succ (Iterator : Iterator_Type) 
      return Iterator_Type is <>;

   with function Predicate 
     (Iterator : Iterator_Type) return Boolean is <>;
     
   with procedure Assign
     (Target, Source : Iterator_Type) is <>;

function Charles.Algorithms.Generic_Remove
  (First, Back : Iterator_Type) return Iterator_Type;

   