with Charles.Algorithms.Generic_Lower_Bound;

function Charles.Algorithms.Generic_Binary_Search 
  (First, Back : Iterator_Type;
   Item        : Element_Type) return Iterator_Type is
   
   function Lower_Bound is 
      new Generic_Lower_Bound 
        (Iterator_Type,
         Element_Type);
         
   I : constant Iterator_Type := Lower_Bound (First, Back, Item);

begin

   if I = Back then
      return Back;
   end if;

   if not Is_Less (Item, I) then
      return I;
   end if;
   
   return Back;

end Charles.Algorithms.Generic_Binary_Search;
