with
Ada.Strings.Unbounded;

use
Ada.Strings.Unbounded;

package body Strings.Fields is

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Item : Character) return Boolean is
   begin
      return Item = ' ' or else Item = Ascii.HT;
   end;


   ------------------
   -- Select_Field --
   ------------------

   --  Returns a string that represents the nth string in the field.
   --  The 'first of the return string is always set to one
   --

    function Select_Field (
      Item            : String;
      Field_No        : Integer;
      Field_Separator : Character) return String
   is
      First : constant Integer := Item'first;
      Last  : constant Integer := Item'Last;

      Start  : Natural;
      Finish : Natural;


      ----------------------
      -- Search_Backwards --
      ----------------------

      procedure Search_Backwards (
         Start    : in out Natural;
         Finish   : in out Natural;
         Field_No : in     Natural)
      is
         Field : Natural := 1;
      begin
         --  find the start of the field
         --  Post condition : Start points at the char following
         --  the n-1th sep. char, or past the last char in the string

         loop
            exit when Finish < First or else Field = Field_No;

            if Item (Finish) = Field_Separator then
               Field := Field + 1;
            end if;

            Finish := Finish - 1;
         end loop;

         --  Find the end of the field

         --  Finish points at the end of the appropriate bit
         Start := Finish;
         loop
            exit when Start < First or else
                      Item (Start) = Field_Separator;

            Start := Start - 1;
         end loop;

         Start := Start + 1;
      end Search_Backwards;


      ---------------------
      -- Search_Forwards --
      ---------------------

      procedure Search_Forwards (
         Start    : in out Natural;
         Finish   : in out Natural;
         Field_No : in     Natural)
      is
         Field : Natural := 1;
      begin
         --  find the start of the field
         --  Post condition : Start points at the char following
         --  the n-1th sep. char, or past the last char in the string

         loop
            exit when Start > Last or else Field = Field_No;

            if Item (Start) = Field_Separator then
               Field := Field + 1;
            end if;

            Start := Start + 1;
         end loop;

         --  Find the end of the field

         --  Start points at the start of the appropriate bit
         Finish := Start;
         loop
            exit when Finish > Last or else
                      Item (Finish) = Field_Separator;

            Finish := Finish + 1;
         end loop;

         Finish := Finish - 1;
      end Search_Forwards;

   begin

      if Field_No > 0 then
         Start  := First;
         Finish := First;
         Search_Forwards (Start, Finish, Field_No);

      elsif Field_No < 0 then
         Start  := Last;
         Finish := Last;
         Search_Backwards (Start, Finish, abs Field_No);

      else
         raise Constraint_Error;

      end if;


      --  Make a subtype conversion to a string with diff.
      --  bounds. Forces the 'first to be 1, which makes life
      --  simipler for the caller

      declare
         subtype Slide is String (1..Finish - Start + 1);
      begin
         return Slide (Item (Start..Finish));
      end;

   end Select_Field;


   ------------------
   -- Count_Fields --
   ------------------

   function Count_Fields (
      Item            : String;
      Field_Separator : Character) return Natural
   is
      Count : Positive;
   begin
      if Item'Length = 0 then
         return 0;

      else
         Count := 1;
         for i in Item'Range loop
            if Item (i) = Field_Separator then
               Count := Count + 1;
            end if;
         end loop;
      end if;
      return Count;

   end Count_Fields;


   ------------------
   -- Select_Field --
   ------------------

   function Select_Field (
      Item     : String;
      Field_No : Integer) return String

   is

      First : constant Integer := Item'first;
      Last  : constant Integer := Item'Last;

      Start  : Natural;
      Finish : Natural;


      --------------------
      -- Search_Forward --
      --------------------

      procedure Search_Forwards (
         Start    : in out Natural;
         Finish   : in out Natural;
         Field_No : in    Positive)
     is

         Field : Natural := 0;

         ----------------
         -- Skip_Space --
         ----------------

         -- Postcondition : Ptr points at a non space char, or beyond the
         --                 end of the array
         procedure Skip_Space (Ptr : in out Positive) is
         begin
            while (Ptr <= Last) and then (Is_Space (Item (Ptr))) loop
               Ptr := Ptr + 1;
            end loop;
         end;
         pragma Inline (Skip_Space);

         --------------------
         -- Skip_Non_Space --
         --------------------

         procedure Skip_Non_Space (Ptr : in out Positive) is
         begin
            while Ptr <= Last and then not Is_Space (Item (Ptr)) loop
               Ptr := Ptr + 1;
            end loop;
         end;
         pragma Inline (Skip_Non_Space);

      begin
         loop
            Skip_Space (Start);
            Field := Field + 1;
            exit when Start > Last or else Field = Field_No;

            Skip_Non_Space (Start);
         end loop;

         Finish := Start;
         Skip_Non_Space (Finish);
         -- Finish will point one beyond the end, or at the end of
         -- the list. (how can we tell!)
         Finish := Finish - 1;

      end Search_Forwards;


      ----------------------
      -- Search_Backwards --
      ----------------------

      procedure Search_Backwards (
         Start    : in out Natural;
         Finish   : in out Natural;
         Field_No : in    Positive)
     is

         Field : Natural := 0;

         ----------------
         -- Skip_Space --
         ----------------

         -- Postcondition : Ptr points at a non space char, or before the
         --                 start of the array
         procedure Skip_Space (Ptr : in out Natural) is
         begin
            while (Ptr >= First) and then (Is_Space (Item (Ptr))) loop
               Ptr := Ptr - 1;
            end loop;
         end;
         pragma Inline (Skip_Space);

         --------------------
         -- Skip_Non_Space --
         --------------------

         procedure Skip_Non_Space (Ptr : in out Natural) is
         begin
            while Ptr >= First and then not Is_Space (Item (Ptr)) loop
               Ptr := Ptr - 1;
            end loop;
         end;
         pragma Inline (Skip_Non_Space);

      begin
         loop
            Skip_Space (Finish);
            Field := Field + 1;
            exit when Finish < First or else Field = Field_No;

            Skip_Non_Space (Finish);
         end loop;

         Start := Finish;
         Skip_Non_Space (Start);

         -- Start will point one before the start
         Start := Start + 1;

      end Search_Backwards;

   begin

      if Field_No > 0 then
         Start  := First;
         Finish := First;
         Search_Forwards (Start, Finish, Field_No);

      elsif Field_No < 0 then
         Start  := Last;
         Finish := Last;
         Search_Backwards (Start, Finish, abs Field_No);

      else
         raise Constraint_Error;
      end if;


      --  Make a subtype conversion to a string with diff.
      --  bounds. Forces the 'first to be 1, which makes life
      --  simipler for the caller

      declare
         subtype Slide is String (1..Finish - Start + 1);
      begin
         return Slide (Item (Start..Finish));
      end;

   end Select_Field;



   -------------------
   -- Select_Fields --
   -------------------

   function Select_Fields (
      Line   : String;
      Format : String) return String is

      Result : Unbounded_String;

      Count  : Positive := 1;
   begin
      -- extract each number from the format string.
      -- then use that to build up a the resulting string

      loop
         declare
            Selected_Field_String : constant String := Select_Field (Format, Count);
            Selected_Field : Positive;
            -- if this raises a constraint error, then the user has to
            -- deal with it.
         begin
            -- we have run out of format numbers, so our job is done

            exit when Selected_Field_String = "";

            Selected_Field := Integer'Value (Selected_Field_String);

            -- if this raises a constraint error, then the user has to
            -- deal with it.


            if Count /= 1 then
               Append (Result, ' ');
            end if;
            Append (Result, Select_Field (Line, Selected_Field));
            Count := Count + 1;
         end;
      end loop;



      return To_String (Result);
   end Select_Fields;


   ------------------
   -- Build_String --
   ------------------

   --  Format effector allows for $1, $2..$n to select a field.
   --  $* represents all the fields.
   --  $$ represents the characer '$'
   --  Other characters in the format string are copied verbatim into
   --  the output string
   --  E.g.
   --     Build_String ("cat mouse dog", "The $1 ate the $2")
   --  would result in the string..
   --     "The cat ate the mouse"
   --
   --  Written with global variables and pragma Inline to ensure
   --  that the code is as fast as it can get. As this routine
   --  is likely to sit inside loops (typically through processing
   --  a file) this is quite important.


   Format_Char : constant Character := '$';
   Entire_Line : constant Character := '*';

   function Build_String (
      Line   : String;
      Format : String) return String
   is

      First  : constant Natural := Format'First;
      Last   : constant Natural := Format'Last;
      Result : Unbounded_String;

      Start  : Positive := First;
      Count  : Positive := Start;

      ----------------
      -- Skip_Chars --
      ----------------

      procedure Skip_Chars is
      begin
         loop
            exit when         Count > Last
                      or else Format (Count) = Format_Char;

            Count := Count + 1;
         end loop;
      end Skip_Chars;
      pragma Inline (Skip_Chars);

      --------------
      -- Optional --
      --------------

      procedure Optional (Char : Character) is
      begin
          if Format (Count) = Char then
             Count := Count + 1;
          end if;
      end;
      pragma Inline (Optional);

      -----------
      -- Digit --
      -----------

      procedure Digit (Ok : out Boolean) is
      begin
         Ok := Format (Count) in '0'..'9';
         if Ok then
            Count := Count + 1;
         end if;
      end Digit;
      pragma Inline (Digit);


      ---------
      -- Int --
      ---------

      procedure Int (Ok : out Boolean) is
         Digit_Char_Found : Boolean;
      begin
         Optional ('-');
         Digit (Ok);
         loop
            exit when Count > Last;

            Digit (Digit_Char_Found);

            exit when not Digit_Char_Found;
         end loop;
      end;
      pragma Inline (Int);

   begin

      loop
         --  Grab characters up to the first format character
         Start := Count;
         Skip_Chars;

         Append (Result, Format (Start..Count - 1));

         -- either we are at the end of the line, or we
         -- have found a Format_Char

         exit when Count > Last;


         if Count = Last then
            raise Format_Error;

         else
            -- Consume the Format_Char
            Count := Count + 1;

            case Format (Count) is
               when Format_Char =>
                  -- $$, append the '$'
                  Append (Result, Format_Char);
                  Count := Count + 1;

               when Entire_Line =>
                  Append (Result, Line);
                  -- consume the '*' character
                  Count := Count + 1;

               when others =>
                  -- should be a number. Won't _just_ check for a positive,
                  -- as we expect to eventually have -ve values as well
                  -- (which will represent fields counted from the right
                  -- of the string

                  declare
                     Selected_Field : Integer;
                     Ok : Boolean;
                  begin
                     Start := Count;
                     Int (Ok);

                     if not Ok then
                        raise Format_Error;
                     end if;

                     Selected_Field := Integer'Value (Format (Start..Count - 1));
                     -- Although the digits were checked (int (ok)), a large
                     -- # will still result in o/flow.

                     Append (Result, Select_Field (Line, Selected_Field));
                  exception
                     when others =>
                        raise Format_Error;
                  end;

            end case;
         end if;
      end loop;

      return To_String (Result);
   end Build_String;


end Strings.Fields;
