function Charles.Algorithms.Generic_Upper_Bound
  (First, Back : Iterator_Type;
   Item        : Element_Type) return Iterator_Type is
   
   I : Integer'Base := Distance (First, Back);
   J : Integer'Base;
   
   Iterator : Iterator_Type := First;
   Middle : Iterator_Type;

begin

   while I > 0 loop
   
      J := I / 2;
      
      Middle := Iterator;

      Advance (Middle, Distance => J);

      if not Is_Less (Item, Middle) then

         Iterator := Succ (Middle);

         I := I - J - 1;

      else

         I := J;

      end if;         
      
   end loop;   

   return Iterator;

end Charles.Algorithms.Generic_Upper_Bound;

