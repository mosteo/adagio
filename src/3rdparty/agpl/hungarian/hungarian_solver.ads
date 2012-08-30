with Ada.Finalization;

with Interfaces.C;

package Hungarian_Solver is

   pragma Preelaborate;

   use type Interfaces.C.Int;

   type Modes is private;

   Minimize_Cost : constant Modes;
   Maximize_Util : constant Modes;

   type Costs is
   delta 0.01
   range 0.0 .. Float (Interfaces.C.Int'Last / 100);
   --  To ensure it will fit in a C.Int which is used internally.

   generic
      Workers : Natural;
      Jobs    : Natural;
   package Solver is

      subtype Costs is Hungarian_Solver.Costs;

      type Size is new Natural range 1 .. Natural'Max (Workers, Jobs);

      type Problem is tagged limited private;

      type Job_Sol_Index is new Natural       range 0 .. Jobs;
      subtype  Job_Index is     Job_Sol_Index range 1 .. Job_Sol_Index'Last;

      type Worker_Index is new Positive range 1 .. Workers;

      type Cost_Matrix is array (Worker_Index'Range,
                                 Job_Index'Range) of Costs;

      type Solution_Array is array (Worker_Index'Range) of Job_Sol_Index;

      procedure Create (This  : out Problem;
                        Costs :     Cost_Matrix;
                        Mode  :     Modes := Minimize_Cost);

      procedure Solve (This : in out Problem);

      function  Solution (This : Problem) return Solution_Array;

      procedure Print_Cost_Matrix (This : Problem);
      procedure Print_Assignment  (This : Problem);
      procedure Print_Status      (This : Problem);

   private

      use Ada;
      use Interfaces;

      type C_Cost is new C.Int;

      type C_Cost_Row is array (Job_Index'Range) of aliased C_Cost;
      pragma Convention (C, C_Cost_Row);

      type C_Cost_Row_Access is access all C_Cost_Row;
      pragma Convention (C, C_Cost_Row_Access);
--      pragma No_Strict_Aliasing (C_Job_Array_Access);

      type C_Cost_Row_Column is
        array (Worker_Index'Range) of aliased C_Cost_Row_Access;
      pragma Convention (C, C_Cost_Row_Column);

      type C_Cost_Row_Column_Access is access all C_Cost_Row_Column;
      pragma Convention (C, C_Cost_Row_Column_Access);
--      pragma No_Strict_Aliasing (C_Cost_Matrix_Access);

      type C_Cost_Rows is array (Worker_Index'Range) of aliased C_Cost_Row;

      type C_Cost_Matrix is new Finalization.Controlled with record
         Rows      :         C_Cost_Rows;
         Rows_Ptrs : aliased C_Cost_Row_Column;
         This      :         C_Cost_Row_Column_Access;
      end record;
      procedure Initialize (X : in out C_Cost_Matrix);
      procedure Adjust     (X : in out C_Cost_Matrix) renames Initialize;

      type C_Size_Row is array (Size) of aliased C.Int;
      pragma Convention (C, C_Size_Row);

      type C_Size_Row_Access is access all C_Size_Row;
      pragma Convention (C, C_Size_Row_Access);

      type C_Size_Row_Column is array (Size) of aliased C_Size_Row_Access;
      pragma Convention (C, C_Size_Row_Column);

      type C_Size_Row_Column_Access is access all C_Size_Row_Column;
      pragma Convention (C, C_Size_Row_Column_Access);

      type C_Size_Rows is array (Size) of aliased C_Size_Row;

      type C_Size_Matrix is new Finalization.Controlled with record
         Rows      :         C_Size_Rows;
         Rows_Ptrs : aliased C_Size_Row_Column;
         This      :         C_Size_Row_Column_Access;
      end record;
      procedure Initialize (X : in out C_Size_Matrix);
      procedure Adjust     (X : in out C_Size_Matrix) renames Initialize;

      type C_Problem is record
         Num_Rows   : C.Int;
         Num_Cols   : C.Int;
         Cost       : C_Size_Row_Column_Access;
         Assignment : C_Size_Row_Column_Access;
      end record;
      pragma Convention (C, C_Problem);

      --     Typedef Struct {
      --       Int Num_Rows;
      --       Int Num_Cols;
      --       Int ** Cost;
      --       Int ** Assignment;
      --     } Hungarian_Problem_T;

      type Problem is new Ada.Finalization.Limited_Controlled with record
         Size  : C.Int;
         Inner : C_Problem;
      end record;
      procedure Finalize (This : in out Problem);

      function "+" (X : Costs)       return C_Cost; pragma Inline ("+");
      function "+" (X : C_Cost)      return Costs; pragma Inline ("+");
      function "+" (X : Cost_Matrix) return C_Cost_Matrix;

   end Solver;

private

   use Interfaces;

   type Modes is new C.Int;
   Minimize_Cost : constant Modes := 0;
   Maximize_Util : constant Modes := 1;

end Hungarian_Solver;
