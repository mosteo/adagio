package body Hungarian_Solver is

--  int hungarian_init(hungarian_problem_t* p,
--   int** cost_matrix,
--   int rows,
--   int cols,
--   int mode);
--
--  /** Free the memory allocated by init. **/
--  void hungarian_free(hungarian_problem_t* p);
--
--  /** This method computes the optimal assignment. **/
--  void hungarian_solve(hungarian_problem_t* p);
--
--  /** Print the computed optimal assignment. **/
--  void hungarian_print_assignment(hungarian_problem_t* p);
--
--  /** Print the cost matrix. **/
--  void hungarian_print_costmatrix(hungarian_problem_t* p);
--
--  /** Print cost matrix and assignment matrix. **/
--  void hungarian_print_status(hungarian_problem_t* p);

   package body Solver is

      ------------
      -- Create --
      ------------

      procedure Create (This  : out Problem;
                        Costs :     Cost_Matrix;
                        Mode  :     Modes := Minimize_Cost)
      is
         procedure C_Init (Result :    out C.Int;
                           P      : in out C_Problem;
                           Co     :        C_Cost_Row_Column_Access;
                           Rows   :        C.Int;
                           Cols   :        C.Int;
                           Mode   :        C.Int);
         pragma Import (C, C_Init, "hungarian_init");
         pragma Import_Valued_Procedure (C_Init);
         C_Costs : constant C_Cost_Matrix := +Costs;
      begin
         C_Init (This.Size,
                 This.Inner,
                 C_Costs.This,
                 C.Int (Costs'Last (1)),
                 C.Int (Costs'Last (2)),
                 C.Int (Mode));
      end Create;

      -----------
      -- Solve --
      -----------

      procedure Solve (This : in out Problem) is
         procedure C_Solve (P : C_Problem);
         pragma Import (C, C_Solve, "hungarian_solve");
      begin
         C_Solve (This.Inner);
      end Solve;

      --------------
      -- Solution --
      --------------

      function Solution (This : Problem) return Solution_Array is
         Sol : Solution_Array := (others => 0);
         A   : C_Size_Row_Column renames This.Inner.Assignment.all;
      begin
         if Workers = 0 or else Jobs = 0 then
            return Sol;
         end if;

         for I in 1 .. Size (Workers) loop
            for J in 1 .. Size (Jobs) loop
               if A (I)(J) /= 0 then
                  Sol (Worker_Index (I)) := Job_Sol_Index (J);
               end if;
            end loop;
         end loop;

         return Sol;
      end Solution;

      -----------------------
      -- Print_Cost_Matrix --
      -----------------------

      procedure Print_Cost_Matrix (This : Problem) is
         procedure C_Print (P : C_Problem);
         pragma Import (C, C_Print, "hungarian_print_costmatrix");
      begin
         C_Print (This.Inner);
      end Print_Cost_Matrix;

      ----------------------
      -- Print_Assignment --
      ----------------------

      procedure Print_Assignment (This : Problem) is
         procedure C_Print (P : C_Problem);
         pragma Import (C, C_Print, "hungarian_print_assignment");
      begin
         C_Print (This.Inner);
      end Print_Assignment;

      ------------------
      -- Print_Status --
      ------------------

      procedure Print_Status (This : Problem) is
         procedure C_Print (P : C_Problem);
         pragma Import (C, C_Print, "hungarian_print_status");
      begin
         C_Print (This.Inner);
      end Print_Status;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (This : in out Problem) is
         procedure C_Free (P : C_Problem);
         pragma Import (C, C_Free, "hungarian_free");
      begin
         C_Free (This.Inner);
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (X : in out C_Cost_Matrix) is
      begin
         for I in X.Rows_Ptrs'Range loop
            X.Rows_Ptrs (I) := X.Rows (I)'Unchecked_Access;
         end loop;
         X.This := X.Rows_Ptrs'Unchecked_Access;
      end Initialize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (X : in out C_Size_Matrix) is
      begin
         for I in X.Rows_Ptrs'Range loop
            X.Rows_Ptrs (I) := X.Rows (I)'Unchecked_Access;
         end loop;
         X.This := X.Rows_Ptrs'Unchecked_Access;
      end Initialize;

      ---------
      -- "+" --
      ---------

      function "+" (X : Costs) return C_Cost is
      begin
         return C_Cost (X * 100.0);
      end "+";

      function "+" (X : C_Cost) return Costs is
      begin
         return Costs (X) / 100.0;
      end "+";

      function "+" (X : Cost_Matrix) return C_Cost_Matrix is
         Cm : C_Cost_Matrix;
      begin
         for I in X'Range loop
            for J in X'Range (2) loop
               Cm.This (I)(J) := + X (I, J);
            end loop;
         end loop;
         return Cm;
      end "+";

   end Solver;

end Hungarian_Solver;
