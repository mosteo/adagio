with Agpl.Conversions;
with Agpl.Bandwidth_Throttle;
with Agpl.Streams.Bandwidth_Throttle;
with Agpl.Streams.File;
with Agpl.Types;
use  Agpl;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
use  Ada;

with Text_IO;

procedure Test is
   F : Ada.Streams.Stream_IO.File_Type;
   S : aliased Agpl.Streams.File.Stream_Type;
   BW : aliased Agpl.Bandwidth_Throttle.Object (7, 500);
   T : aliased Agpl.Streams.Bandwidth_Throttle.Stream_Type (S'Access, BW'Access, BW'Access);
   Data : String (1 .. 80);
   Buf  : Ada.Streams.Stream_Element_Array (1 .. 1000);
   Last : Ada.Streams.Stream_Element_Offset;

   use type Ada.Streams.Stream_Element_Offset;

   package BWT renames Agpl.Streams.Bandwidth_Throttle;
begin
   Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   String'Output (T'Access, Data);
   while BWT.Get_Buffered_Count (T) > 0 loop
      Text_Io.Put_Line ("Buffered:" & Ada.Streams.Stream_Element_Count'Image (
         BWT.Get_Buffered_Count (T)));
      BWT.Flush (T);
      delay 0.5;
      Text_Io.Put_Line ("Speed:" & Agpl.Types.Data_Rate'Image (BWT.Get_Current_Write_Rate (T'Access)));
   end loop;
   Text_Io.Put_Line ("Global_Speed:" & Agpl.Types.Data_Rate'Image (BWT.Get_Global_Write_Rate (T)));

   Ada.Streams.Stream_IO.Close (F);

   Text_Io.Put_Line ("Writing finished");

   Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, "test.txt");
   Agpl.Streams.File.Create (S, F);
   BWT.Reset (T);

   loop
      BWT.Read (T, Buf, Last);
      Text_Io.Put_Line ("Speed:" & Agpl.Types.Data_Rate'Image (BWT.Get_Current_Read_Rate (T'Access)));
      Text_Io.Put_Line ("Read:" & Natural'Image (Integer (Last) - Integer (Buf'First) + 1));
      exit when Agpl.Streams.File.Available_Read (S) = Natural'(0);
      delay 0.5;
   end loop;
   Text_Io.Put_Line ("Global_Speed:" & Agpl.Types.Data_Rate'Image (BWT.Get_Global_Read_Rate (T)));
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
