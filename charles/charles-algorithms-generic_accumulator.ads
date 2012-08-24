generic

   type Element_Type (<>) is private;
   
   Result : in out Element_Type'Base;
   
   with function "+" (L, R : Element_Type'Base)
      return Element_Type'Base is <>;
   
procedure Charles.Algorithms.Generic_Accumulator (Element : in Element_Type);
