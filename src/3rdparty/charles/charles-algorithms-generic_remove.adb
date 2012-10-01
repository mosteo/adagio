function Charles.Algorithms.Generic_Remove
  (First, Back : Iterator_Type) return Iterator_Type is
  
   I : Iterator_Type := First;
   J : Iterator_Type := I;
   
begin

   loop
      
      loop
         if J = Back then
            return I;
         end if;

         exit when not Predicate (J);
         
         J := Succ (J);
      end loop;
      
      Assign (Target => I, Source => J);
      
      I := Succ (I);
      J := Succ (J);
      
   end loop;
   
end Charles.Algorithms.Generic_Remove;

            


   
