with Agpl.Conversions;
with Agpl.Streams.Deflate;
with Agpl.Streams.File;
use  Agpl;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
use  Ada;

with Text_IO;

procedure Test is
   F : Ada.Streams.Stream_IO.File_Type;
   S : Agpl.Streams.File.Stream_Type;
   Z : Agpl.Streams.Deflate.Stream_Type;
   
   Iters : constant Natural := 10000;

   function Avail (This : access Ada.Streams.Root_Stream_Type'Class) return Natural is
   begin
      return Agpl.Streams.File.Available_Read (
         Agpl.Streams.File.Stream_Type (This.all));
   end Avail;

begin
   Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   Agpl.Streams.Deflate.Create (Z, S'Unrestricted_Access,
      Agpl.Streams.Deflate.Never_Available'Access,
      Agpl.Streams.Deflate.Always_Available'Access, 
      16 * 1024 * 1024, 50);
   if not Agpl.Streams.Deflate.Ready_For_Writing (Z) then
      Text_Io.Put_Line ("Unwritable just opened!");
   end if;
   for I in 1 .. Iters loop
      String'Output (Z'Unrestricted_Access, "Abracadabra" & I'Img);
      while not Agpl.Streams.Deflate.Ready_For_Writing (Z) loop
         Text_IO.Put (".");
         Agpl.Streams.Deflate.Hard_Flush (Z);
         Text_IO.Put ("x");
      end loop;
   end loop;
   while not Agpl.Streams.Deflate.Everything_Written (Z) loop
      Text_IO.Put (".");
      Agpl.Streams.Deflate.Hard_Flush (Z);
      Text_IO.Put ("x");
   end loop;
   Text_IO.New_Line;
   Agpl.Streams.Deflate.Close (Z);
   Ada.Streams.Stream_IO.Close (F);

   Text_Io.Put_Line ("Writing finished with ratio " & 
      Conversions.To_String (Agpl.Streams.Deflate.Get_Write_Ratio (Z), 4));

   Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   Agpl.Streams.Deflate.Create (Z, S'Unrestricted_Access,
      Avail'Unrestricted_Access,
      Agpl.Streams.Deflate.Never_Available'Access,
      102400, 90);
   for I in 1 .. Iters loop
      declare
         Str : String := String'Input (Z'Unrestricted_Access);
      begin
         Text_IO.Put_Line ("Read: " & Str);
         null;
      end;
   end loop;
   Agpl.Streams.Deflate.Close (Z);
   Ada.Streams.Stream_IO.Close (F);
   Text_Io.Put_Line ("Reading finished with ratio " & 
      Conversions.To_String (Agpl.Streams.Deflate.Get_Read_Ratio (Z), 4));
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
