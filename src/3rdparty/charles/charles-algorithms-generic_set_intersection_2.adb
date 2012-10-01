procedure Charles.Algorithms.Generic_Set_Intersection_2
  (Left_First, Left_Back   : Left_Type;
   Right_First, Right_Back : Right_Type) is

   L : Left_Type := Left_First;
   R : Right_Type := Right_First;
   
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
   
end Charles.Algorithms.Generic_Set_Intersection_2;

