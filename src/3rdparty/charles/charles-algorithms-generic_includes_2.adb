function Charles.Algorithms.Generic_Includes_2
  (Left_First, Left_Back   : Left_Type;
   Right_First, Right_Back : Right_Type) return Boolean is

   L : Left_Type := Left_First;
   R : Right_Type := Right_First;
   
begin

   loop
   
      if L = Left_Back then
         return R = Right_Back;
      end if;
      
      if R = Right_Back then
         return True;
      end if;
            
      if Is_Less (R, L) then
         return False;
      end if;

      if Is_Less (L, R) then
         Succ (L);
      else
         Succ (L);
         Succ (R);
      end if;         
         
   end loop;   
   
end Charles.Algorithms.Generic_Includes_2;

