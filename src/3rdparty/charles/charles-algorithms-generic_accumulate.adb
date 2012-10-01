function Charles.Algorithms.Generic_Accumulate
  (First, Back   : Iterator_Type;
   Initial_Value : Element_Type'Base) return Element_Type'Base is

   Iterator : Iterator_Type := First;
   
   Result : Element_Type'Base := Initial_Value;
   
begin

   while Iterator /= Back loop
      Result := Result + Element (Iterator);
      Iterator := Succ (Iterator);
   end loop;
   
   return Result;
   
end Charles.Algorithms.Generic_Accumulate;

     

