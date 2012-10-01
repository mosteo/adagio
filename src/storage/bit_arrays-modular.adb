package body Bit_arrays.Modular is

   use Interfaces;

   function To_bit_array_LE(n: Num) return Bit_array is
      Result: Bit_array(1..Num'Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      for i in 1..Num'Size loop
         Result(i):= (Shift_right(Aux, i - 1) and 1) = 1; 
      end loop;
      Return Result;
   end To_bit_array_LE;

   function To_bit_array_BE(n: Num) return Bit_array is
      Result: Bit_array(1..Num'Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      for i in 1..Num'Size loop
         Result(i):= (Shift_right(Aux, Num'Size - i) and 1) = 1; 
      end loop;
      Return Result;
   end To_bit_array_BE;

   function Append_LE(b: Bit_array; n: Num) return Bit_array is
      Result: Bit_array(b'first..b'last + Num'Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      Result(b'first..b'last):= b;
      for i in 1..Num'Size loop
         Result(b'last + i):= (Shift_right(Aux, i - 1) and 1) = 1; 
      end loop;
      Return Result;
   end Append_LE;

   function Append_BE(b: Bit_array; n: Num) return Bit_array is
      Result: Bit_array(b'first..b'last + Num'Size);
      Aux: Mod_aux:= Mod_aux(n);
   begin
      Result(b'first..b'last):= b;
      for i in 1..Num'Size loop
         Result(b'last + i):= (Shift_right(Aux, Num'Size - i) and 1) = 1; 
      end loop;
      Return Result;
   end Append_BE;

   To_num: constant array(boolean) of Mod_aux:= (True => 1, False => 0);

   function To_number_LE(b: Bit_array) return Num is
      Result: Mod_aux:= 0;
   begin
      for i in b'range loop
         Result:= Result or Shift_left(To_num(b(i)), i - b'first);
      end loop;
      return Num(Result);
   end To_number_LE;

   function To_number_BE(b: Bit_array) return Num is
      Result: Mod_aux:= 0;
   begin
      for i in b'range loop
         Result:= Shift_left(Result, 1) or To_num(b(i));
      end loop;
      return Num(result);
   end To_number_BE;

end Bit_arrays.Modular;
