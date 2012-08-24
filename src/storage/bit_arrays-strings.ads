package Bit_arrays.Strings is

   -- Creation:
   -- A string. Their bytes will be used
   function To_bit_array(s: String) return Bit_array;

   -- Reconstructs a string:
   function To_string(b: Bit_array) return String;

   -- String with binary representation (i.e "100100010110101"):
   function To_binary_string(b: Bit_array) return String;

end Bit_arrays.Strings;
