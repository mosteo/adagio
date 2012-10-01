function Charles.Algorithms.Generic_Includes
  (Left_First, Left_Back   : Iterator_Type;
   Right_First, Right_Back : Iterator_Type) return Boolean is

   L : Iterator_Type := Left_First;
   R : Iterator_Type := Right_First;
   
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
   
end Charles.Algorithms.Generic_Includes;

