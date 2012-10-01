with Bit_arrays.Modular;

package body Bit_arrays.Strings is

   type Byte is mod 2 ** 8;

   package Byte_io is new Bit_arrays.Modular(Byte); 

   -- Creation:
   -- A string. Their bytes will be used
   function To_bit_array(s: String) return Bit_array is
      Result: Bit_array(1..s'length * 8);
      Pos: integer:= Result'first;
   begin
      for i in s'range loop
         Result(Pos..Pos + 7):= Byte_io.To_bit_array_BE(character'pos(s(i)));
         Pos := Pos + 8;
      end loop;
      return Result;
   end To_bit_array;

   -- Reconstructs a string:
   function To_string(b: Bit_array) return String is
      s: String(1..b'length / 8);
      Pos: integer:= b'first;
   begin
      if (b'length mod 8) /= 0 then
         raise Constraint_error;
      end if;
      for i in s'range loop
         s(i):= Character'Val(Byte_io.To_number_BE(b(Pos..Pos + 7)));
         Pos := Pos + 8;
      end loop;
      return s;
   end To_string;

   -- String with binary representation (i.e "100100010110101"):
   function To_binary_string(b: Bit_array) return String is
      s: String(1..b'length);
      rep: constant array(boolean) of Character:= (true => '1', false => '0');
   begin
      for i in b'range loop
         s(s'first + i - b'first):= rep(b(i));
      end loop;
      return s;
   end To_binary_string;

end Bit_arrays.Strings;
