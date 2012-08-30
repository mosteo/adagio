generic

   type Iterator_Type is private;
   
   with procedure Succ (Iterator : in out Iterator_Type) is <>;
   
   with procedure Process (Iterator : Iterator_Type) is <>;
   
   with function Is_Less (L, R : Iterator_Type) return Boolean is <>;
   
   with function "=" (L, R : Iterator_Type) return Boolean is <>;
   
procedure Charles.Algorithms.Generic_Set_Intersection
  (Left_First, Left_Back   : Iterator_Type;
   Right_First, Right_Back : Iterator_Type);
   


