-- Test program for PragmARC.Scalar_Wrapping

with Scalar_Test_Help;
use Scalar_Test_Help;

with Ada.Text_IO;
use Ada.Text_IO;
procedure Scalar_Test is
   use Wrapped_Positive;

   Value : Wrapped_Scalar;
   List  : Positive_List.Handle;
   Pos   : Positive_List.Position;
   Dummy : Positive_List.Context_Data;
begin -- Scalar_Test
   Fill : for I in 1 .. 10 loop
      List.Append (Item => (Value => I), After => List.Last, New_Pos => Pos);
   end loop Fill;

   List.Iterate (Action => Print_One'access, Context => Dummy);

   Pos := List.Next (List.Next (List.First) );
   Value := List.Get (Pos);
   Put_Line (Item => Positive'Image (Value.Value) );
end Scalar_Test;