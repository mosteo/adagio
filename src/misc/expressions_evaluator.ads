
--  -----------------------------------------------------------------------  --
--
--  Author  : Pascal Obry
--  E-Mail  : pascal_obry@csi.com
--
--  -----------------------------------------------------------------------  --
--       Module Name : Expressions_Evaluator
--         File name : expressions_evaluator.ads
--
--       Created by  : Pascal Obry
--               on  : Sat Feb 01 13:50:16 1997
--
--  ======================================== I D E N T I F I C A T I O N ==  --
--
--  Description
--     Ce package permet d'evaluer des expressions mathematiques.
--     L'expression est donnees sous forme de chaine de caractere.
--
--  Mots-cles
--     Math, Expression, Evaluator
--
--  Caracterisation
--     Unite    : Paquetage
--     Genre    : Type de donnee abstrait
--     Liaisons : Independant
--
--  Disponibilite
--     Systemes de compilation
--        GNAT, SPARC Solaris 2.4
--        GNAT, Windows-NT
--     Access
--        Sources
--
--  Historique
--
--  ======================================== S P E C I F I C A T I O N S ==  --
--
--  Elements generiques et ajustement de comportement
--     (Unite non generique)
--
--  Elements principaux
--     Create
--        permet de creer un objet Expressions.
--     Evaluate
--        retourne la valeur d'une variable de l'expression.
--
--  Elements annexes
--     Set
--        positionne une valeur pour une variable.
--     Unset
--        efface la valeur d'une variable.
--
--  ====================================== I M P L E M E N T A T I O N S ==  --
--
--  Elaboration
--     (neant - pas de pragma d'elaboration necessaire)
--
--  Algorithme
--     l'algorithme utilise est recursif.
--
--  Elements sensibles utilises
--     (neant)
--
--  Performances
--     (neant)
--
--  Autres informations
--
--     La variable P est initialisee a Pi.
--     La variable E est initialisee avec la constante e.
--     La variable speciale # represente un nombre aleatoire entre 0 et 1.
--
--     exemple d'utilisation.
--
--      E := Create ("a = 9; b = a! + 2; c = #; f = (2 * a) - b; z = sin (a)");
--      V := Evaluate (E, F);
--      (ici V = -362864)
--
--      V := Evaluate (E, Z);
--      (ici V = 0.412118)
--
--  =======================================================================  --
--



with Ada.Numerics;
with Ada.Strings.Unbounded;

generic

   type Values is digits <>;

package Expressions_Evaluator is

   Not_Set         : exception;
   Syntax_Error    : exception;
   Recursive_Error : exception;
   Argument_Error  : exception renames Ada.Numerics.Argument_Error;

   type Expressions is private;

   type Variables is (A, B, C, D, E, F, G, H, I, J, K, L, M,
                      N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

   function Create (Expression : in String)
                    return Expressions;
   --  variable E is set to constant E.
   --  variable P is set to PI.
   procedure Destroy (Expression : in out Expressions);

   function Evaluate (Expression : in Expressions;
                      Variable   : in Variables)
                      return Values;

   procedure Set (Expression : in out Expressions;
                  Variable   : in     Variables;
                  Value      : in     Values);

   procedure UnSet (Expression : in out Expressions;
                    Variable   : in     Variables);

private

   type Unary_Operators is ('-', '+', VABS,
                            SIN, COS, TAN, COT,
                            ARCSIN, ARCCOS, ARCTAN, ARCCOT,
                            SINH, COSH, TANH, COTH,
                            ARCSINH, ARCCOSH, ARCTANH, ARCCOTH,
                            SQRT, EXP, LOG, LN, FACTORIAL);

   type Binary_Operators is ('-', '+', '*', '/', '^');

   type Node_Types is (Unary_Operator, Binary_Operator,
                       Value, Variable, Random);

   type Node;

   type Node_Access is access Node;

   type Node (Node_Type : Node_Types := Value) is
      record
         case Node_Type is
            when Unary_Operator =>
               U_Operator : Unary_Operators;
               Next       : Node_Access;

            when Binary_Operator =>
               B_Operator  : Binary_Operators;
               Left, Right : Node_Access;

            when Value =>
               V : Values;

            when Variable =>
               Var : Variables;

            when Random =>
               null;
         end case;
      end record;

   type Root_Node is
      record
         Node       : Node_Access := null;
         Last_Value : Values      := 0.0;
      end record;

   type Set_Of_Node is array (Variables) of Root_Node;

   type Expressions is
      record
         Nodes             : Set_Of_Node;
         Expression_String : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Expressions_Evaluator;
