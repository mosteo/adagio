
--  $Id: expressions_evaluator.adb,v 1.2 2003/10/08 23:17:33 Jano Exp $

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

package body Expressions_Evaluator is

   use Ada;
   use Strings.Unbounded;
   use Strings.Fixed;
   use Strings;
   use Exceptions;

   Generator : Numerics.Float_Random.Generator;

   type Parent_Scan is (Opening, Closing);

   ----------
   -- Math --
   ----------

   package Math is new
     Ada.Numerics.Generic_Elementary_Functions (Values);

   ---------------
   -- Factorial --
   ---------------

   function Factorial (V : in Values)
                       return Values
   is
      F : Positive;
      R : Values := 1.0;
   begin
      if V < 0.0 then
         Raise_Exception (Argument_Error'Identity,
                          "factorial for negative value");
      elsif V = 0.0 then
         return 1.0;
      else
         F := Positive (V);
         for I in Positive range 2 .. F loop
            R := R * Values (I);
         end loop;
      end if;
      return R;
   end Factorial;

   ------------
   -- Create --
   ------------

   function Create (Expression : in String)
                    return Expressions
   is
      E : Expressions;

      ---------------------
      -- Cut_Expressions --
      ---------------------

      procedure Cut_Expressions (E : in out Expressions) is

         S : String := To_String (E.Expression_String);
         First, Last : Natural;

         ---------------------
         -- Matching_Parent --
         ---------------------

         function Matching_Parent (E     : in String;
                                   P     : in Positive;
                                   Which : in Parent_Scan := Closing)
                                   return Positive
         is
            PP           : Positive := P;
            Parent_Level : Integer  := 0;
            Parent       : Character;
         begin
            case Which is
               when Closing =>
                  Parent   := ')';
               when Opening =>
                  Parent   := '(';
            end case;

            loop
               if E (PP) = '(' then
                  Parent_Level := Parent_Level + 1;
               elsif E (PP) = ')' then
                  Parent_Level := Parent_Level - 1;
               end if;
               exit when Parent_Level = 0 and then E (PP) = Parent;

               case Which is
                  when Closing =>
                     PP := PP + 1;
                     exit when PP > E'Last;
                  when Opening =>
                     PP := PP - 1;
                     exit when PP < E'First;
               end case;
            end loop;

            case Which is
               when Closing =>
                  if PP > E'Last then
                     Raise_Exception (Syntax_Error'Identity,
                                      "no matching parenthesis for " &
                                      Positive'Image (P));
                  else
                     return PP;
                  end if;
               when Opening =>
                  if PP < E'First then
                     Raise_Exception (Syntax_Error'Identity,
                                      "no matching parenthesis for " &
                                      Positive'Image (P));
                  else
                     return PP;
                  end if;
            end case;
         end Matching_Parent;

         --------------------
         -- Parse_Equation --
         --------------------

         procedure Parse_Equation (E : in out Expressions;
                                   S : in     String)
         is
            package Variable_IO is
              new Ada.Text_IO.Enumeration_IO (Variables);

            ----------------------
            -- Parse_Expression --
            ----------------------

            function Parse_Expression (E : in String)
                                       return Node_Access
            is

               ------------
               -- Parser --
               ------------

               package Parser is

                  type Token_Type is (Var, Val, Operator, Parent, Rand, Void);

                  procedure Read_Token (S : in     String;
                                        T :    out Token_Type;
                                        P :    out Positive);

                  Last_Var      : Variables;
                  Last_Value    : Values;
                  Last_Operator : Unary_Operators;

               end Parser;

               package body Parser is

                  ----------------
                  -- Skip_Space --
                  ----------------

                  procedure Skip_Space (S : in     String;
                                        P : in out Positive) is
                  begin
                     loop
                        exit when S (P) /= ' ';
                        P := P + 1;
                        exit when P > S'Last;
                     end loop;
                  end Skip_Space;

                  ----------------
                  -- Read_Token --
                  ----------------

                  procedure Read_Token (S : in     String;
                                        T :    out Token_Type;
                                        P :    out Positive)
                  is
                     -----------------
                     -- Read_Number --
                     -----------------

                     procedure Read_Number (S : in     String;
                                            P : in out Positive)
                     is
                        First, Last : Positive;
                        Tmp         : Natural;
                     begin
                        Find_Token (S (P .. S'Last),
                                    Maps.To_Set ("0123456789."),
                                    Inside,
                                    First,
                                    Last);

                        if Index (S (First .. Last), ".") = 0 then
                           --  an integer
                           declare
                              V : Integer;
                           begin
                              Integer_Text_IO.Get (S (First .. Last), V, Tmp);
                              Last_Value := Values (V);
                           end;
                        else
                           --  a float
                           declare
                           V : Float;
                           begin
                              Float_Text_IO.Get (S (First .. Last), V, Tmp);
                              Last_Value := Values (V);
                           end;
                        end if;

                        P := Last + 1;

                        if P <= S'Last and then S (P) = '!' then
                           Last_Value := Factorial (Last_Value);
                           P := P + 1;
                        end if;

                     end Read_Number;

                     -----------------
                     -- Read_Symbol --
                     -----------------

                     procedure Read_Symbol (S : in     String;
                                            T :    out Token_Type;
                                            P : in out Positive)
                     is
                        First, Last : Positive;
                     begin
                        Find_Token (S (P .. S'Last),
                                    Maps.Constants.Lower_Set,
                                    Inside,
                                    First,
                                    Last);

                        if First = Last then --  variable
                           T := Var;
                           Last_Var := Variables'Val
                             (Character'Pos (S (First)) -
                              Character'Pos ('a') +
                              Variables'Pos (A));
                        elsif S (First .. Last) = "abs" then
                           T := Operator;
                           Last_Operator := VABS;
                        elsif S (First .. Last) = "sin" then
                           T := Operator;
                           Last_Operator := SIN;
                        elsif S (First .. Last) = "cos" then
                           T := Operator;
                           Last_Operator := COS;
                        elsif S (First .. Last) = "tan" then
                           T := Operator;
                           Last_Operator := TAN;
                        elsif S (First .. Last) = "cot" then
                           T := Operator;
                           Last_Operator := COT;
                        elsif S (First .. Last) = "arcsin" then
                           T := Operator;
                           Last_Operator := SIN;
                        elsif S (First .. Last) = "arccos" then
                           T := Operator;
                           Last_Operator := COS;
                        elsif S (First .. Last) = "arctan" then
                           T := Operator;
                           Last_Operator := TAN;
                        elsif S (First .. Last) = "arccot" then
                           T := Operator;
                           Last_Operator := COT;
                        elsif S (First .. Last) = "sinh" then
                           T := Operator;
                           Last_Operator := SIN;
                        elsif S (First .. Last) = "cosh" then
                           T := Operator;
                           Last_Operator := COS;
                        elsif S (First .. Last) = "tanh" then
                           T := Operator;
                           Last_Operator := TAN;
                        elsif S (First .. Last) = "coth" then
                           T := Operator;
                           Last_Operator := COT;
                        elsif S (First .. Last) = "arcsinh" then
                           T := Operator;
                           Last_Operator := SIN;
                        elsif S (First .. Last) = "arccosh" then
                           T := Operator;
                           Last_Operator := COS;
                        elsif S (First .. Last) = "arctanh" then
                           T := Operator;
                           Last_Operator := TAN;
                        elsif S (First .. Last) = "arccoth" then
                           T := Operator;
                           Last_Operator := COT;
                        elsif S (First .. Last) = "sqrt" then
                           T := Operator;
                           Last_Operator := SQRT;
                        elsif S (First .. Last) = "exp" then
                           T := Operator;
                           Last_Operator := EXP;
                        elsif S (First .. Last) = "log" then
                           T := Operator;
                           Last_Operator := LOG;
                        elsif S (First .. Last) = "ln" then
                           T := Operator;
                           Last_Operator := LN;
                        else
                           Raise_Exception (Syntax_Error'Identity,
                                            "unknown operator " &
                                            S (First .. Last));
                        end if;

                        P := Last + 1;

                     end Read_Symbol;

                  begin -- Read_Token
                     P := S'First;

                     Skip_Space (S, P);

                     --  no more token
                     if P > S'Last then
                        T := Void;
                        return;
                     end if;

                     case S (P) is

                        when 'a' .. 'z' => --  a variable or function
                           Read_Symbol (S, T, P);

                        when '0' .. '9' => --  a number
                           Read_Number (S, P);
                           T := Val;

                        when '#' =>
                           T := Rand;
                           P := P + 1;

                        when '+' =>
                           Last_Operator := '+';
                           T := Operator;
                           P := P + 1;

                        when '-' =>
                           Last_Operator := '-';
                           T := Operator;
                           P := P + 1;

                        when '!' =>
                           Last_Operator := FACTORIAL;
                           T := Operator;
                           P := P + 1;

                        when '(' =>
                           T := Parent;
                           P := P + 1;

                        when others =>
                           Raise_Exception (Syntax_Error'Identity,
                                            "error for " & S (P));
                     end case;

                  end Read_Token;

               end Parser;

               -------------------
               -- Find_Operator --
               -------------------

               procedure Find_Operator (S      : in     String;
                                        Op_Pos :    out Natural;
                                        Op     :    out Binary_Operators)
               is

                  P : Natural;

                  ---------------------
                  -- Is_Operator_Add --
                  ---------------------

                  function Is_Operator_Add (C : in Character) return Boolean is
                  begin
                     case C is
                        when '+' | '-' =>
                           return True;
                        when others =>
                           return False;
                     end case;
                  end Is_Operator_Add;

                  ---------------------
                  -- Is_Operator_Mul --
                  ---------------------

                  function Is_Operator_Mul (C : in Character) return Boolean is
                  begin
                     case C is
                        when '/' | '*' =>
                           return True;
                        when others =>
                           return False;
                     end case;
                  end Is_Operator_Mul;

                  ---------------------
                  -- Is_Operator_Exp --
                  ---------------------

                  function Is_Operator_Exp (C : in Character) return Boolean is
                  begin
                     case C is
                        when '^' =>
                           return True;
                        when others =>
                           return False;
                     end case;
                  end Is_Operator_Exp;

                  ------------------
                  -- Set_Operator --
                  ------------------

                  procedure Set_Operator (P : in Positive) is
                  begin
                     Op_Pos := P;
                     case S (P) is
                        when '+' =>
                           Op := '+';
                        when '-' =>
                           Op := '-';
                        when '*' =>
                           Op := '*';
                        when '/' =>
                           Op := '/';
                        when '^' =>
                           Op := '^';
                        when others =>
                           null;
                     end case;
                  end Set_Operator;

                  type Operator_Function is
                     access function (C : in Character) return Boolean;

                  -------------------
                  -- Scan_Operator --
                  -------------------

                  function Scan_Operator (Op_Function : in Operator_Function;
                                          S           : in String)
                                          return Natural
                  is
                     Parent_Level : Natural  := 0;
                     Pos          : Positive := S'First;
                  begin
                     loop
                        if S (Pos) = '(' then
                                       Parent_Level := Parent_Level + 1;
                        elsif S (Pos) = ')' then
                           Parent_Level := Parent_Level - 1;
                        end if;
                        exit when Parent_Level = 0 and Op_Function (S (Pos));
                        Pos := Pos + 1;
                        exit when Pos > S'Last;
                     end loop;

                     if Pos > S'Last then
                        return 0;
                     else
                        return Pos;
                     end if;
                  end Scan_Operator;

               begin -- Find_Operator
                  P := Scan_Operator (Is_Operator_Add'Access, S);

                  if P = 0 then
                     P := Scan_Operator (Is_Operator_Mul'Access, S);
                     if P = 0 then
                        P := Scan_Operator (Is_Operator_Exp'Access, S);
                        if P = 0 then
                           Op_Pos := 0;
                           Op     := '+';
                        else
                           Set_Operator (P);
                        end if;
                     else
                        Set_Operator (P);
                     end if;
                  else
                     Set_Operator (P);
                  end if;

               end Find_Operator;

               T      : Parser.Token_Type;
               P      : Positive := 1;
               Op_Pos : Natural;
               B_Op   : Binary_Operators;

            begin -- Parse_Expression
               Parser.Read_Token (E, T, P);

               case T is

                  when Parser.Var =>
                     Find_Operator (E, Op_Pos, B_Op);
                     if Op_Pos = 0 then
                        return new Node'(Variable, Parser.Last_Var);
                     else
                        return new Node'
                          (Binary_Operator,
                           B_Op,
                           Left  =>
                             Parse_Expression (E (E'First .. Op_Pos - 1)),
                           Right =>
                             Parse_Expression (E (Op_Pos + 1 .. E'Last)));
                     end if;

                  when Parser.Val =>
                     Find_Operator (E, Op_Pos, B_Op);
                     if Op_Pos = 0 then
                        return new Node'(Value, Parser.Last_Value);
                     else
                        return new Node'
                          (Binary_Operator,
                           B_Op,
                           Left  =>
                             Parse_Expression (E (E'First .. Op_Pos - 1)),
                           Right =>
                             Parse_Expression (E (Op_Pos + 1 .. E'Last)));
                     end if;

                  when Parser.Rand =>
                     Find_Operator (E, Op_Pos, B_Op);
                     if Op_Pos = 0 then
                        return new Node'(Node_Type => Random);
                     else
                        return new Node'
                          (Binary_Operator,
                           B_Op,
                           Left  =>
                             Parse_Expression (E (E'First .. Op_Pos - 1)),
                           Right =>
                             Parse_Expression (E (Op_Pos + 1 .. E'Last)));
                     end if;

                  when Parser.Operator =>
                     Find_Operator (E (P .. E'Last), Op_Pos, B_Op);
                     if Op_Pos = 0 then
                        return new Node'
                          (Unary_Operator,
                           Parser.Last_Operator,
                           Next => Parse_Expression (E (P .. E'Last)));
                     else
                        return new Node'
                          (Binary_Operator,
                           B_Op,
                           Left  =>
                             Parse_Expression (E (E'First .. Op_Pos - 1)),
                           Right =>
                             Parse_Expression (E (Op_Pos + 1 .. E'Last)));
                     end if;

                  when Parser.Parent =>
                     Find_Operator (E, Op_Pos, B_Op);
                     if Op_Pos = 0 then
                        declare
                           P2 : Positive;
                        begin
                           P2 := Matching_Parent (E, P - 1);
                           return Parse_Expression (E (P .. P2 - 1));
                        end;
                     else
                        return new Node'
                        (Binary_Operator,
                         B_Op,
                         Left  => Parse_Expression (E (E'First .. Op_Pos - 1)),
                         Right => Parse_Expression (E (Op_Pos + 1 .. E'Last)));
                     end if;

                  when Parser.Void =>
                     pragma Warnings (Off);
                     null;
                     pragma Warnings (On);

               end case;

            end Parse_Expression;

            Var : Variables;
            I   : Natural;

         begin -- Parse_Equation
            I := Index (S, "=");

            if I = 0 then
               Raise_Exception (Syntax_Error'Identity,
                                "not an equation (missing =) in " & S);
            else
               declare
                  Last : Positive;
               begin
                  Variable_IO.Get (S (S'First .. I - 1), Var, Last);
               exception
                  when others =>
                     Raise_Exception (Syntax_Error'Identity,
                                      "left value is not a variable in " & S);
               end;
            end if;

            if I = S'Last then
               Raise_Exception (Syntax_Error'Identity,
                                "No expression to parse in " & S);
            end if;

            E.Nodes (Var).Node := Parse_Expression (S (I + 1 .. S'Last));
         end Parse_Equation;

      begin -- Cut_Expressions
         First := S'First;
         Last  := S'Last;

         --  make the factorial operator for Variables and expressions
         --  a prefix.
         for I in S'Range loop
            if S (I) = '!' then
               case S (I - 1) is
                  when 'a' .. 'z' =>  --  a variable
                     S (I)     := S (I - 1);
                     S (I - 1) := '!';
                  when ')' =>         --  an expression
                     declare
                        P : Positive;
                     begin
                        P := Matching_Parent (S, I - 1, Opening);
                        S (P + 1 .. I) := S (P .. I - 1);
                        S (P) := '!';
                     end;
                  when others =>
                     null;
               end case;
            end if;
         end loop;

         loop
            Last  := Index (S (First .. S'Last), ";");
            if Last = 0 then
               Parse_Equation (E, S (First .. S'Last));
               exit;
            else
               Parse_Equation (E, S (First .. Last - 1));
               First := Last + 1;
               exit when First > S'Last;
            end if;
         end loop;
      end Cut_Expressions;

   begin -- Create
      E.Expression_String := To_Unbounded_String
        (Translate (Expression, Maps.Constants.Lower_Case_Map));
      Cut_Expressions (E);

      --  initialize some variables
      E.Nodes (Expressions_Evaluator.E).Node := new Node'(Value, Numerics.e);
      E.Nodes (Expressions_Evaluator.P).Node := new Node'(Value, Numerics.Pi);

      return E;
   end Create;

   ------------------
   -- Destroy_Tree --
   ------------------

   procedure Destroy_Tree (Root : in out Node_Access) is

      procedure Free is
        new Unchecked_Deallocation (Node, Node_Access);

   begin
      if Root /= null then

         case Root.Node_Type is

            when Expressions_Evaluator.Variable | Value | Random =>
               null;
               Free (Root);

            when Unary_Operator =>
               Destroy_Tree (Root.Next);
               Free (Root);

            when Binary_Operator =>
               Destroy_Tree (Root.Left);
               Destroy_Tree (Root.Right);
               Free (Root);

         end case;
      end if;
   end Destroy_Tree;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Expression : in out Expressions) is
   begin
      for V in Variables loop
         Destroy_Tree (Expression.Nodes (V).Node);
      end loop;
      Expression.Expression_String := Strings.Unbounded.Null_Unbounded_String;
   end Destroy;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expression : in Expressions;
                      Variable   : in Variables)
                      return Values
   is

      Have_Been_Evaluated : array (Variables) of Boolean :=
         (E => true, P => true, others => False);

      ----------
      -- Eval --
      ----------

      function Eval (Node : in Node_Access)
                     return Values
      is
         V : Values;
      begin
         if Node = null then
            Raise_Exception (Not_Set'Identity,
                             "a variable is not set");
         end if;

         case Node.Node_Type is

            when Expressions_Evaluator.Variable =>
               if Have_Been_Evaluated (Node.Var) then
                  Raise_Exception (Recursive_Error'Identity,
                                   "infinite recursion detected.");
               else
                  Have_Been_Evaluated (Node.Var) := True;
                  V := Eval (Expression.Nodes (Node.Var).Node);
                  Have_Been_Evaluated (Node.Var) := False;
                  return V;
               end if;

            when Value =>
               return Node.V;

            when Unary_Operator =>
               case Node.U_Operator is
                  when '-' =>
                     return -Eval (Node.Next);
                  when '+' =>
                     return Eval (Node.Next);
                  when FACTORIAL =>
                     return Factorial (Eval (Node.Next));
                  when VABS =>
                     return abs (Eval (Node.Next));
                  when SIN =>
                     return Math.Sin (Eval (Node.Next));
                  when COS =>
                     return Math.Cos (Eval (Node.Next));
                  when TAN =>
                     return Math.Tan (Eval (Node.Next));
                  when COT =>
                     return Math.Cot (Eval (Node.Next));
                  when ARCSIN =>
                     return Math.Sin (Eval (Node.Next));
                  when ARCCOS =>
                     return Math.Cos (Eval (Node.Next));
                  when ARCTAN =>
                     return Math.Tan (Eval (Node.Next));
                  when ARCCOT =>
                     return Math.Cot (Eval (Node.Next));
                  when SINH =>
                     return Math.Sin (Eval (Node.Next));
                  when COSH =>
                     return Math.Cos (Eval (Node.Next));
                  when TANH =>
                     return Math.Tan (Eval (Node.Next));
                  when COTH =>
                     return Math.Cot (Eval (Node.Next));
                  when ARCSINH =>
                     return Math.Sin (Eval (Node.Next));
                  when ARCCOSH =>
                     return Math.Cos (Eval (Node.Next));
                  when ARCTANH =>
                     return Math.Tan (Eval (Node.Next));
                  when ARCCOTH =>
                     return Math.Cot (Eval (Node.Next));
                  when SQRT =>
                     return Math.Sqrt (Eval (Node.Next));
                  when EXP =>
                     return Math.Exp (Eval (Node.Next));
                  when LOG =>
                     return Math.Log (Eval (Node.Next), Base => 10.0);
                  when LN =>
                     return Math.Log (Eval (Node.Next));
               end case;

            when Binary_Operator =>
               case Node.B_Operator is
                  when '+' =>
                     return Eval (Node.Left) + Eval (Node.Right);
                  when '-' =>
                     return Eval (Node.Left) - Eval (Node.Right);
                  when '*' =>
                     return Eval (Node.Left) * Eval (Node.Right);
                  when '/' =>
                     return Eval (Node.Left) / Eval (Node.Right);
                  when '^' =>
                     declare
                        use Math;
                     begin
                        return Eval (Node.Left) ** Eval (Node.Right);
                     end;
               end case;

            when Random =>
               return Values (Numerics.Float_Random.Random (Generator));

         end case;
      end Eval;

   begin
      return Eval (Expression.Nodes (Variable).Node);
   end Evaluate;

   ---------
   -- Set --
   ---------

   procedure Set (Expression : in out Expressions;
                  Variable   : in     Variables;
                  Value      : in     Values) is
   begin
      Expression.Nodes (Variable).Node
        := new Node'(Expressions_Evaluator.Value, Value);
   end Set;

   -----------
   -- UnSet --
   -----------

   procedure UnSet (Expression : in out Expressions;
                    Variable   : in     Variables) is
   begin
      Destroy_Tree (Expression.Nodes (Variable).Node);
   end UnSet;

end Expressions_Evaluator;
