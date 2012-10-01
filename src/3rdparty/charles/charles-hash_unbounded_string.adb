--source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

with Ada.Unchecked_Conversion;

function Charles.Hash_Unbounded_String 
  (Key : Unbounded_String) return Integer'Base is

   type Uns is mod 2 ** 32;

   function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
   pragma Import (Intrinsic, Rotate_Left);

   Tmp : Uns := 0;

   function To_Integer is 
      new Ada.Unchecked_Conversion (Uns, Integer'Base);

begin

   for J in 1 .. Length (Key) loop
      Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Element (Key, J));
   end loop;

--   return Header_Num'First +
--            Header_Num'Base (Tmp mod Header_Num'Range_Length);

   return To_Integer (Tmp);

end Charles.Hash_Unbounded_String;

