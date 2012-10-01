procedure Charles.Algorithms.Generic_Set_Symmetric_Difference
  (Left_First, Left_Back   : Iterator_Type;
   Right_First, Right_Back : Iterator_Type) is

   L : Iterator_Type := Left_First;
   R : Iterator_Type := Right_First;
   
begin

   loop
   
      if L = Left_Back then
      
         while R /= Right_Back loop
            Process (R);
            Succ (R);
         end loop;
         
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

         Process (R);
         Succ (R);

      else

         Succ (L);
         Succ (R);

      end if;         
         
   end loop;   
   
end Charles.Algorithms.Generic_Set_Symmetric_Difference;

