with Adagio.Os.Memory;
with Adagio.Os.Memory_Stats;
use  Adagio.Os;

with Agpl.Bmp;
with Agpl.Graph;
with Agpl.Trace;     use Agpl.Trace;
use  Agpl;

with Ada.Text_Io;    use Ada.Text_Io;

procedure T001_GraphMem is
   G : Graph.Object (1, 100);
begin
   loop
      G.Add_Sample (1, 100.0);
      declare
         B : constant Agpl.Bmp.Object := Graph.Get_Bmp (G, 100);
      begin
         null;
      end;
      Put_Line ("Memory:" & Memory.Heap_Usage'Img &
                " Cached:" & Memory_Stats.Cached_Heap_Usage'Img);
   end loop;
exception
   when E : others =>
      Put_Line ("Exception: " & Report (E));
end T001_GraphMem;
