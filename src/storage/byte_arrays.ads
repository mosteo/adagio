with Interfaces;

package Byte_arrays is

   subtype Byte is Interfaces.Unsigned_8;

   type Byte_array is array (integer range <>) of Byte;
   pragma Pack(Byte_array);

end Byte_arrays;
