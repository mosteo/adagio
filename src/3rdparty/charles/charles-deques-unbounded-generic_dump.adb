with Charles.Generic_Dump_Deque;

procedure Charles.Deques.Unbounded.Generic_Dump
  (Container : in Container_Type) is
  
   procedure Dump is
      new Charles.Generic_Dump_Deque (Rep_Types, Image);
      
begin

   Dump (Container.Map);
   
end;

  