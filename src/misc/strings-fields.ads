package Strings.Fields is

   -- Compile this package with
   --     gcc -c -O3 -gnatN
   -- to get max speed

   function Select_Field (
      Item            : String;
      Field_No        : Integer;
      Field_Separator : Character) return String;

   --  Returns a string that represents the nth string in the field.
   --  The 'first of the return string is always set to one
   --  Fields are separated by the supplied character
   --  E.g.
   --
   --     Field ("cat:dog:mouse", 3, ':')
   --
   --  would result in the string
   --
   --     "mouse"
   --
   --  A -ve value indicates searching from the RHS.
   --
   --  E.g.
   --
   --     Field ("cat:dog:mouse", -1, ':')
   --
   --  would result in the string
   --     "mouse"
   --
   --  To process each field in a string you can do the following
   --  (presuming Line (1..Last) has the string you want to process)
   -- 
   --  i := 1;
   --  loop
   --     declare
   --        Item : constant String := Field (Line (1..Last), i, ':');
   --     begin
   --        exit when Item = ""; -- no more fields (assumes all fields
   --                             -- have values!
   --        -- process item...
   --        I := I + 1;
   --     end;
   --  end loop;


   function Count_Fields (
      Item            : String;
      Field_Separator : Character) return Natural;

   --  returns the # of fields separated by the field_separator
   --  character
   --  E.g.
   --
   --    Count_Fields ("cat:dog:mouse", ':')  -> 3
   --    Count_Fields ("cat:dog:mouse", 'o')  -> 3
   --    Count_Fields ("cat:dog:mouse", ' ')  -> 1
   --    Count_Fields ("", ' ')  -> 0

   function Select_Field (
      Item     : String; 
      Field_No : Integer) return String;

   --  Returns a string that represents the nth string in the field.
   --  The 'first of the return string is always set to one
   --  Differs from the other 'field' function in that it considers
   --  fields to be separated by multiple white space characters
   --  As above -ve field numbers signify searching from the RHS
   --  E.g.
   --
   --      Field ("cat    dog  mouse", 3)
   --
   --  would result in the string
   --
   --      "mouse"
   --  Note that this is _not_ the same as
   --
   --      Field ("cat    dog  mouse", 3, ' ')
   --
   --  which sees _each_ space as separating (many empty) fields



   function Select_Fields (
      Line   : String;
      Format : String) return String;

   --  Construct a new string from the string line, by applying the
   --  fields according to the format specifier. Fields are considerd
   --  to be separated by spaces.
   --  As above -ve field numbers signify searching from the RHS
   --  E.g.
   --
   --    Select_Fields ("cat dog mouse", "3 2")
   --
   --  would result in the string
   --
   --    "mouse dog"


   function Build_String (
      Line   : String;
      Format : String) return String;

   Format_Error : Exception;

   --  Construct a new string from Line, by applying the fields
   --  according to the format specifier. The fields in line are
   --  separated by whitespace (spaces and tabs).
   --
   --  Format allows for $1, $2..$n, $-1, $-2..$-n to select a field
   --  from Line
   --  $* represents all the fields.
   --  $$ represents the characer '$'
   --  Other characters in the format string are copied verbatim into
   --  the output string
   --
   --  Format_Error is raised if
   --     the character following a '$' is not one of the above
   --     '$' is the last chacter in the string
   --     the number following '$' is too long to be converted to an integer.
   --
   --  E.g.
   --     Build_String ("cat mouse dog", "The $1 ate the $2")
   --
   --  would result in the string..
   --
   --     "The cat ate the mouse"
   --
   --  E.g.
   --     Build_String ("cat mouse dog", "$3:$2:$1")
   --
   --  would result in the string..
   --
   --     "dog:mouse:cat"
   --
   --  E.g.
   --     Build_String ("cat mouse dog", "the $1 ate the $");
   --
   --  would result in a Format_Exception error.

end Strings.Fields;
