-- Bush page: http://www.pegasoft.ca/docs/discus/index_bush.html
-- Eliza chatterbot
--
-- Original author: Joseph Weizenbaum
-- Translated from Bush to Ada by Alejandro Mosteo (public@mosteo.com)
with Ada.Strings.Unbounded;

package Eliza.Phrases is

   pragma Elaborate_body;

   subtype ustring is Ada.Strings.Unbounded.Unbounded_string;

   -- Keywords
   --
   -- Keywords and responses are loaded into the keywords
   -- variable.  Keywords are prefixed with the keyword_tag
   -- character, responses with the response_tag character.

   delimiter    : constant character := '~';
   nokey_marker : constant string := delimiter & "NOKEYFOUND";

   -- keyword lists are pairs of a keyword and first reponse field
   -- reponses are lists of responses terminated with an empty field
   -- These vars are initialized during body elaboration.

   single_keywords : ustring;
   num_single      : natural := 0;
   multi_keywords  : ustring;
   num_multi       : natural := 0;
   responses       : ustring;
   num_responses   : natural := 0;

end Eliza.Phrases;
