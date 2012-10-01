procedure Charles.Algorithms.Generic_Set_Intersection
  (Left_First, Left_Back   : Iterator_Type;
   Right_First, Right_Back : Iterator_Type) is

   L : Iterator_Type := Left_First;
   R : Iterator_Type := Right_First;
   
begin

   loop
   
      if L = Left_Back then
         return;
      end if;
      
      if R = Right_Back then
         return;
      end if;
   
      if Is_Less (L, R) then

         Succ (L);
      
      elsif Is_Less (R, L) then

         Succ (R);

      else

         Process (L);

         Succ (L);
         Succ (R);

      end if;         
         
   end loop;   
   
end Charles.Algorithms.Generic_Set_Intersection;

