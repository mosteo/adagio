with Charles.Algorithms.Generic_Lower_Bound;
with Charles.Algorithms.Generic_Upper_Bound;

procedure Charles.Algorithms.Generic_Equal_Range
  (First, Back               : in     Iterator_Type;
   Item                      : in     Element_Type;
   Result_First, Result_Back :    out Iterator_Type) is
   
   I : Integer'Base := Distance (First, Back);
   J : Integer'Base;
   
   Iterator : Iterator_Type := First;
   Middle   : Iterator_Type;

begin

   while I > 0 loop
   
      J := I / 2;
      
      Middle := Iterator;

      Advance (Middle, Distance => J);

      if Is_Less (Middle, Item) then

         Iterator := Succ (Middle);

         I := I - J - 1;

      elsif Is_Less (Item, Middle) then
      
         I := J;
      
      else
      
         declare
            function Lower_Bound is 
               new Generic_Lower_Bound 
                 (Iterator_Type,
                  Element_Type);
         begin
            Result_First := Lower_Bound (Iterator, Middle, Item);
         end;
         
         Advance (Iterator, Distance => I);
         
         declare               
            function Upper_Bound is 
               new Generic_Upper_Bound
                 (Iterator_Type,
                  Element_Type);
         begin
            Result_Back := Upper_Bound (Succ (Middle), Iterator, Item);
         end;
         
         return;
         
      end if;
                     
   end loop;   

   Result_First := First;
   Result_Back := First;

end Charles.Algorithms.Generic_Equal_Range;

