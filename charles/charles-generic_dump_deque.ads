with Charles.Deque_Types;

generic

   with package Maps is 
      new Charles.Deque_Types (<>);
      
   with function Image (Element : Maps.Element_Type)
      return String;
      
procedure Charles.Generic_Dump_Deque (Map : Maps.Map_Type);
