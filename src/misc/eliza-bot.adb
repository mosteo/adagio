-- Bush page: http://www.pegasoft.ca/docs/discus/index_bush.html
-- Eliza chatterbot
--
-- Original author: Joseph Weizenbaum
-- Translated from Bush to Ada by Alejandro Mosteo (public@mosteo.com)

with Eliza.Phrases; 
use  Eliza.Phrases;

with Strings.Fields;
use  Strings.Fields;

with Ada.Numerics.Float_random;
use  Ada.Numerics.Float_random;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

use  Ada;

package body Eliza.Bot is

   function U (This : in String) return Unbounded_string 
      renames To_unbounded_string;
   function S (This : in Unbounded_String) return String 
      renames To_string;

   subtype ustring is Unbounded_string;
   type ustring_array is array (Positive range <>) of ustring;

   Byes : Ustring_array := (
      U ("BYE!"),
      U ("SEE YOU..."),
      U ("BYEBYE"),
      u ("GOOD BYE"));

   Repeats : Ustring_array := (
      U ("AGAIN?"),
      U ("I UNDERSTAND IT THE FIRST TIME"),
      U ("AHA"),
      U ("OK, OK"),
      U ("I SEE"),
      U ("CORRECT"),
      U ("OK"));

   function Field (
      str       : in UString; 
      Delimiter : in Character; 
      Pos       : in Positive) return UString is
   begin
      return U (Select_field (S (str), Pos, Delimiter));
   end Field;

   ------------------------------------------------------------------------
   -- Choose                                                             --
   ------------------------------------------------------------------------
   -- Selects a string from a ustring array
   function Choose (From : in Ustring_array) return String is
      Rand : Generator;
   begin
      Reset (Rand);
      return S (From (
         Positive (
            Float'Floor (Random (Rand) * Float (From'Length) + 1.0))));
   exception
      when others =>
         return S (From (From'Last));
   end Choose;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   -- Prepares internals.
   procedure Initialize (This : in out Object) is
   begin
      Reset (This.Rand);
   end Initialize;

   ------------------------------------------------------------------------
   -- Get_greeting                                                       --
   ------------------------------------------------------------------------
   -- Get a random greeting phrase
   function Get_greeting (This : in Object) return String is
      pragma Unreferenced (This);
      Greets : Ustring_array := (
         U ("HELLO. HOW ARE YOU?"),
         U ("HI. HOW ARE YOU?"),
         U ("NICE TO MEET YOU. HOW ARE YOU DOING?"));
   begin
      return Choose (Greets);
   end Get_greeting;

   ------------------------------------------------------------------------
   -- Is_done                                                            --
   ------------------------------------------------------------------------
   -- Returns true if the conversation is after a bye.
   function Is_done (This : in Object) return Boolean is
   begin
      return This.Done;
   end Is_done;

   ------------------------------------------------------------------------
   -- Get_response                                                       --
   ------------------------------------------------------------------------
   -- Gets the next phrase from Eliza.
   -- You must supply your answer to the previous statement.
   function Get_response (This : access Object; Phrase : in String)
      return String 
   is
      i    : Ustring := U (Phrase);
   begin
      i := U (" ") & i & U (" ");

      -- clean up input

      declare
        c : character;
        new_i : ustring := U ("");
        last_was_space : boolean := false;
      begin
        for l in 1..length( i ) loop
          c := element( i, l );
          if c = ' ' then
             if last_was_space then
                null;
             else
                new_i := new_i & c;
                last_was_space := true;
             end if;
          else
             last_was_space := false;
             if c >= 'a' and c <= 'z' then
                new_i := new_i & character'val(  character'pos( c ) - 32 );
             elsif c >= '0' and c <= '9' then
                new_i := new_i & c;
             elsif c >= 'A' and c <= 'Z' then
                new_i := new_i & c;
             end if;
          end if;
        end loop;
        i := new_i;
      end;

      -- test for the basics

      if i = This.Prev then
         return Choose (Repeats);
      elsif i = " BYE " or 
            i = " GOOD BYE " or 
            i = " GOODBYE " or 
            i = " BYEBYE " or
            i = " SEEYOU " or
            i = " SEE YOU " or
            i = " NIGHT " or
            i = " DAY " or
            i = " GNIGHT " or
            i = " GDAY "
      then
         This.Done := true;
         return Choose (Byes);
      else
         This.Prev := i;

         -- look for keyword(s)

         declare
            remains : ustring := U ("");

            eliza_reply : ustring := U ("");
            testword    : ustring := U ("");
            response_pos: natural := 0;
         begin

            -- look for single keywords

            declare
              k : positive; -- keyword in the user's input
              keyword : ustring;
            begin
              k := 2; -- skip first null "field"
              loop
                testword := U (select_field( S (i), k, ' ' ));
                exit when testword = U ("");
                -- not 100% since doesn't take into account leading delimiter
                -- since first single keyword has no leading delimiter
           if index( single_keywords, S (testword & delimiter) ) > 0 then
                   for sk in 1..num_single loop
                       keyword := U (select_field( S (single_keywords), sk, delimiter ));
                       if keyword = testword then
                     response_pos := natural'value( select_field( S (single_keywords), sk+1, delimiter ) );
                          exit;
                       end if;
                   end loop;
                end if;
                exit when response_pos > 0;
           k := k + 1;
              end loop;
            end;

            -- no match? look for multiple keywords

            if response_pos = 0 then
               declare
                 k  : positive;
               begin
                 k := 1;
                 while k < positive( num_multi ) loop
                testword := field( multi_keywords, delimiter, k ); 
                if index( i, S (' ' & testword & ' ') ) > 0 then
                   response_pos := natural'value( S (field( multi_keywords, delimiter, k+1 ) ));
                   exit;
                     end if;
                k := k+1;
                 end loop;
               end;
            end if;

            -- still no match? use fallback responses else get remainder
            -- of user input after the keyword(s)

            if response_pos = 0 then
          testword := U (nokey_marker);
          response_pos := natural'value( S (field( single_keywords, delimiter, num_single ) ));
            else
               remains := U (slice( i, positive( index( i, S (testword) ) +
                  length( testword ) ), length( i ) ));
            end if;

            -- rewrite the remainder of the input

            declare
              c : natural;
              p : positive;
            begin
              c := index( remains, " ARE " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+4, " AM+ " );
              end if;
              c := index( remains, " AM " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " ARE+ " );
              end if;
              c := index( remains, " WERE " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+5, " WAS+ " );
              end if;
              c := index( remains, " WAS " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+4, " WERE+ " );
              end if;
              c := index( remains, " YOU " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+4, " I+ " );
              end if;
              c := index( remains, " I " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+2, " YOU+ " );
              end if;
              c := index( remains, " YOUR " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+5, " MY+ " );
              end if;
              c := index( remains, " MY " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " YOUR+ " );
              end if;
              c := index( remains, " IVE " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+4, " YOUVE+ " );
              end if;
              c := index( remains, " YOUVE " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+6, " IVE+ " );
              end if;
              c := index( remains, " IM " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " YOURE+ " );
              end if;
              c := index( remains, " ME " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " YOU+ " );
              end if;
              c := index( remains, " US " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " YOU+ " );
              end if;
              c := index( remains, " WE " );
              if c > 0 then
                 p := positive( c );
                 remains := replace_slice( remains, p, c+3, " YOU+ " );
              end if;
              loop
                 c := index( remains, "+" );
                 exit when c = 0;
                 remains := delete( remains, positive( c ), c );
              end loop;
              if tail( remains, 3 ) = " I " then
                 remains := U (slice( remains, 1,
                    length( remains ) - 2 ))
                    & "ME ";
         end if;
            end;
      --put_line( "Remains (after conjugation): " & remains );

          -- attach reply

            declare
              last_pos    : natural := 0;
              reply_cnt   : natural := 0;
              reply       : natural := 0;
              ch          : character;
              response    : ustring;
            begin

              -- count replies

              last_pos := response_pos+1;
              loop
                 response := field( responses, delimiter, last_pos );
                 exit when response = "";
                 last_pos := last_pos+1;
              end loop;
              reply_cnt := last_pos - response_pos;

         reply :=  Natural (Float'Floor (random (This.rand) * float (reply_cnt)));
              eliza_reply := field( responses, delimiter,
                response_pos + reply );

              ch := element( eliza_reply, positive( length( eliza_reply ) ) );
              if ch = '*' then
                 eliza_reply := head( eliza_reply,
                   length( eliza_reply )-1 ) & remains;
              end if;
            end;
            return S (eliza_reply);
         end;
      end if;
   end Get_response;

end Eliza.Bot;

