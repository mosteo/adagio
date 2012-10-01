-- Simple functions for integers:

with Bit_arrays.Modular;

package body Bit_arrays.Numbers is

   use Interfaces;

   function To_bit_array(
      n: Long_long_integer; Size: Positive) return Bit_array is
      Result: Bit_array(1..Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      for i in 1..Size loop
         Result(i):= (Shift_right(Aux, Size - i) and 1) = 1; 
      end loop;
      Return Result;
   end To_bit_array;

   function To_bit_array(n: Integer; Size: Positive) return Bit_array is
      Result: Bit_array(1..Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      for i in 1..Size loop
         Result(i):= (Shift_right(Aux, Size - i) and 1) = 1; 
      end loop;
      Return Result;
   end To_bit_array;

   To_num: constant array(boolean) of Mod_aux:= (True => 1, False => 0);

   -- Returns a number:
   function To_number_LE(b: Bit_array) return integer is
      Result: Mod_aux:= 0;
   begin
      for i in b'range loop
         Result:= Result or Shift_left(To_num(b(i)), i - b'first);
      end loop;
      return integer(Result);
   end To_number_LE;

   function To_number_BE(b: Bit_array) return integer is
   Result: Mod_aux:= 0;
   begin
      for i in b'range loop
         Result:= Shift_left(Result, 1) or To_num(b(i));
      end loop;
      return integer(result);
   end To_number_BE;

end Bit_arrays.Numbers;
