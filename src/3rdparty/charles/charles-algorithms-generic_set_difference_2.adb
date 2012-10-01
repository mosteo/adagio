procedure Charles.Algorithms.Generic_Set_Difference_2
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
      
         while L /= Left_Back loop
            Process (L);
            Succ (L);
         end loop;
         
         return;
         
      end if;
            
      if Is_Less (L, R) then

         Process (L);
         Succ (L);
      
      elsif Is_Less (R, L) then

         Succ (R);

      else

         Succ (L);
         Succ (R);

      end if;         
         
   end loop;   
   
end Charles.Algorithms.Generic_Set_Difference_2;

