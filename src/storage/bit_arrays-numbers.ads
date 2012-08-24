-- Simple functions for integers:
with Interfaces;

package Bit_arrays.Numbers is

   -- Always Big Endian; Size specifies the number of significant bits in n
   -- Undefined results if n < 0
   function To_bit_array(
      n: Long_long_integer; Size: Positive) return Bit_array;
   function To_bit_array(
      n: Integer; Size: Positive) return Bit_array;

   -- Returns a number:
   function To_number_LE(b: Bit_array) return integer;
   function To_number_BE(b: Bit_array) return integer;

private

   subtype Mod_aux is Interfaces.Unsigned_64;

end Bit_arrays.Numbers;
