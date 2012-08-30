generic

   type Iterator_Type is private;
   
   with function Is_Less (L, R : Iterator_Type) return Boolean is <>;
   
   with procedure Succ (Iterator : in out Iterator_Type) is <>;
   
   with function "=" (L, R : Iterator_Type) return Boolean is <>;
   
function Charles.Algorithms.Generic_Includes
  (Left_First, Left_Back   : Iterator_Type;
   Right_First, Right_Back : Iterator_Type) return Boolean;
   


