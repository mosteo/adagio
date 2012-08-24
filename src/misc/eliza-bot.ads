-- Eliza
--
-- Original author: Joseph Weizenbaum
-- Translated from Bush to Ada by Alejandro Mosteo (public@mosteo.com)
-- Packed as an object by Alejandro Mosteo (public@mosteo.com)

with Ada.Finalization;
with Ada.Numerics.Float_random;
with Ada.Strings.Unbounded;
use  Ada;

package Eliza.Bot is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- Encapsulates an Eliza session.
   type Object is new Finalization.Limited_Controlled with private;

   ------------------------------------------------------------------------
   -- Get_greeting                                                       --
   ------------------------------------------------------------------------
   -- Get a random greeting phrase
   function Get_greeting (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_response                                                       --
   ------------------------------------------------------------------------
   -- Gets the next phrase from Eliza.
   -- You must supply your answer to the previous statement.
   function Get_response (This : access Object; Phrase : in String)
      return String;

   ------------------------------------------------------------------------
   -- Is_done                                                            --
   ------------------------------------------------------------------------
   -- Returns true if the conversation is after a bye.
   function Is_done (This : in Object) return Boolean;

private

   type Object is new Finalization.Limited_Controlled with record
      Rand : Ada.Numerics.Float_random.Generator;
      Prev : Strings.Unbounded.Unbounded_string; -- Last thing said by user.
      Done : Boolean := false;
   end record;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   -- Prepares internals.
   procedure Initialize (This : in out Object);

end Eliza.Bot;
