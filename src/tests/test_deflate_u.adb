with Agpl.Conversions;
with Agpl.Streams.Deflate_Unbounded;
with Agpl.Streams.File;
use  Agpl;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
use  Ada;

with Text_IO;

procedure Test is
   package Deflate renames Agpl.Streams.Deflate_Unbounded;
   use type Agpl.Streams.Stream_Element_Count;

   F : Ada.Streams.Stream_IO.File_Type;
   S : Agpl.Streams.File.Stream_Type;
   Z : Deflate.Stream_Type (
      S'Unrestricted_Access,
      21024,
      1,
      100);
   
   Iters : constant Natural := 10000;

begin
   Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   for I in 1 .. Iters loop
      String'Output (Z'Unrestricted_Access, "Abracadabra");
   end loop;
   while not Deflate.Everything_Written (Z) loop
      Text_IO.Put (".");
      Deflate.Flush (Z);
      Text_IO.Put ("x");
   end loop;
   Text_IO.New_Line;
   Deflate.Close (Z);
   Ada.Streams.Stream_IO.Close (F);

   Text_Io.Put_Line ("Writing finished with ratio " & 
      Conversions.To_String (Deflate.Get_Write_Ratio (Z), 4));

   Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   Deflate.Reset (Z);
   for I in 1 .. Iters loop
      declare
         Str : String := String'Input (Z'Unrestricted_Access);
      begin
         -- Text_IO.Put_Line ("Read: " & Str);
         null;
      end;
   end loop;
   Deflate.Close (Z);
   Ada.Streams.Stream_IO.Close (F);
   Text_Io.Put_Line ("Reading finished with ratio " & 
      Conversions.To_String (Deflate.Get_Read_Ratio (Z), 4));
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
