with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Charles.Generic_Dump_Deque (Map : Maps.Map_Type) is

   I : Integer;

   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use type Maps.Node_Access_Array_Access;

begin
   
   Put ("elements_per_node=");
   Put (Maps.Elements_Per_Node, Width => 0);
   New_Line;

   Put ("map.length=");
   Put (Map.Length, Width => 0);
   New_Line;
   
   Put ("map.bias=");
   Put (Map.Bias, Width => 0);
   New_Line;
   
   Put ("map.first=");
   Put (Map.First, Width => 0);
   New_Line;
   
   Put ("map.last=");
   Put (Map.Last, Width => 0);
   New_Line;

   if Map.Nodes = null then
      Put_Line ("map.nodes=null");
      return;
   end if;
   
   Put ("map.nodes'length=");
   Put (Map.Nodes'Length, Width => 0);
   New_Line;
   
   if Map.Last < Map.First then
      Put_Line ("<no nodes>");
      return;
   end if;
   
   if Map.Length = 0 then
      Put_Line ("<no elements>");
      return;
   end if;
   
   I := Map.First;

   declare
      Node : Maps.Node_Type renames Map.Nodes (Map.First).all;
      J : constant Integer'Base := Map.Bias + 1;
      K : Integer'Base;
   begin
      if Map.Length <= Maps.Elements_Per_Node - Map.Bias then
         K := J + Map.Length - 1;
      else
         K := Node.Elements'Last;
      end if;
      
      Put (J, Width => 0);
      Put ("..");
      Put (K, Width => 0);
      Put (": ");
      
      for Index in J .. K loop
         Put (Image (Node.Elements (Index)));
         Put (' ');
      end loop;
      
      New_Line;
   end;
   
   I := I + 1;
   
   if I > Map.Last then
      return;
   end if;
   
   for Index in I .. Map.Last - 1 loop
      declare
         Node : Maps.Node_Type renames Map.Nodes (Index).all;
      begin
         for X in Node.Elements'Range loop
            Put (Image (Node.Elements (X)));
            Put (' ');
         end loop;
         
         New_Line;
      end;
   end loop;
         
   declare
      Node : Maps.Node_Type renames Map.Nodes (Map.Last).all;

      Length : constant Integer'Base :=
         Map.Bias + Map.Length;
      
      Nodes_Count : constant Integer'Base :=
         Map.Last - Map.First + 1;
         
      Count : constant Integer'Base := 
         Length - (Nodes_Count - 1) * Maps.Elements_Per_Node;
   begin
      Put (Integer'(1), Width => 0);
      Put ("..");
      Put (Count, Width => 0);
      Put (": ");
      
      for X in 1 .. Count loop
         Put (Image (Node.Elements (X)));
         Put (' ');
      end loop;
      
      New_Line;
   end;
   
end Charles.Generic_Dump_Deque;



