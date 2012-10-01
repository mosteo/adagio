with Agpl.Streams.Circular_Unbounded;
use  Agpl;

with Ada.Exceptions;
use  Ada;

with Text_IO;

procedure Test is
   S : Streams.Circular_Unbounded.Stream_Type (
      Max_Memory_Usage => 10240,
      Initial_Size     => 16,
      Grow_Factor      => 100);
   use type Streams.Stream_Element_Offset;
begin
   for I in 1 .. 1000 loop
      Text_Io.Put_Line (I'Img);
      declare
         Str : Streams.Stream_Element_Array (1 .. Streams.Stream_Element_Offset (I)) := (others => 0);
         Lst : Streams.Stream_Element_Offset;
      begin
         Streams.Circular_Unbounded.Write (S, Str);
         Streams.Circular_Unbounded.Read  (S, Str (1 .. Str'Last / 2), Lst);
         pragma Assert (Lst = Str'Last / 2);
      end;
   end loop;
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
