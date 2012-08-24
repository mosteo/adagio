with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

function Charles.Hash_Unbounded_String 
   (Key : Unbounded_String) return Integer'Base;

pragma Preelaborate (Charles.Hash_Unbounded_String);
