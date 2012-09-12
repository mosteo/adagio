-- Modular types:
-- In this package, little endian and big endian is used like this:
-- type Size4 is mod 2 ** 4;

-- Size4'(1) big endian <--> Bit_array'(false, false, false, true)
-- Size4'(1) little endian <--> Bit_array'(true, false, false, false)

-- See private part (!) for maximum allowed modulus.

With Interfaces;
Use Interfaces;

generic
   type Num is Mod <>;
package Bit_arrays.Modular is

   -- Convert to bit_array (little endian):
   function To_bit_array_LE(n: Num) return Bit_array;

   -- Convert to bit_array (big endian):
   function To_bit_array_BE(n: Num) return Bit_array;

   -- Function to append bits (little endian):
   function Append_LE(b: Bit_array; n: Num) return Bit_array;

   -- Function to append bits (big endian):
   function Append_BE(b: Bit_array; n: Num) return Bit_array;

   -- Convert the array to number (little endian):
   function To_number_LE(b: Bit_array) return Num;

   -- Convert the array to number (big endian):
   function To_number_BE(b: Bit_array) return Num;

private

   -- Maximum size for values to add/extract:
   subtype Mod_aux is Interfaces.Unsigned_64;

end Bit_arrays.Modular;
