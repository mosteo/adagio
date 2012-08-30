with Hungarian_Solver;

with Ada.Text_Io; use Ada.Text_Io;

procedure Hungarian_Test is
   package Solver is new Hungarian_Solver.Solver (4, 3);
   use Solver;

   P :          Problem;
   C : constant Cost_Matrix :=
         ((100.0, 1.0, 1.0),
          (100.0, 2.0, 2.0),
          (1.0,   0.0, 0.0),
          (0.0,   2.0, 0.0));
begin
   Put_Line ("Creating...");
   P.Create (C);
   Put_Line ("Printing costs...");
   P.Print_Cost_Matrix;
   Put_Line ("Solving...");
   P.Solve;
   Put_Line ("Printing solution...");
   P.Print_Assignment;
   declare
      S : constant Solution_Array := P.Solution;
   begin
      for I in S'Range loop
         Put (S (I)'Img);
      end loop;
      New_Line;
   end;
   Put_Line ("Done");
end Hungarian_test;
