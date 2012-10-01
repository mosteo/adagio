
Pragma Warnings( Off );
with
Bit_arrays,
Bit_arrays.Modular,
Bit_arrays.Numbers,
Bit_arrays.Strings;
Pragma Warnings( On );


package body Sha1 is

   use Interfaces;

   function "<" (Left, Right: Digest) return boolean is
   begin
      for i in Left'range loop
         if Left(i) /= Right(i) then
            return Left(i) < Right(i);
         end if;
      end loop;
      return false;
   end "<";

   type Mod32 is mod 2 ** 5;

   package Word_bit is new Bit_arrays.Modular(Word);
   package Byte_bit is new Bit_arrays.Modular(Byte);
   package Mod32_bit is new Bit_arrays.Modular(Mod32);
   package ML_bit is new Bit_arrays.Modular(Message_length);

   function f(t: t_range; B, C, D: Word) return Word is
   begin
      case t is
         when 0..19 =>
            return f00_19(B, C, D);
         when 20..39 =>
            return f20_39(B, C, D);
         when 40..59 =>
            return f40_59(B, C, D);
         when 60..79 =>
            return f60_79(B, C, D);
      end case;
   end f;

   function f00_19(B, C, D: Word) return Word is
   begin
      return (B and C) or ((not B) and D);
   end f00_19;

   function f20_39(B, C, D: Word) return Word is
   begin
      return B xor C xor D;
   end f20_39;

   function f40_59(B, C, D: Word) return Word is
   begin
      return (B and C) or (B and D) or (C and D);
   end f40_59;

   function f60_79(B, C, D: Word) return Word is
   begin
      return B xor C xor D;
   end f60_79;

    -- Processing method 1 in rfc3174:
   procedure Method1(Mi: Block; H: in out Digest) is
      A, B, C, D, E, Temp: Word;
      W: Word_array(t_range);
   begin
      W(0..15):= Mi;
      for t in 16..79 loop
         W(t):= S(W(t - 3) xor W(t - 8) xor W(t - 14) xor W(t - 16), 1);
      end loop;
      A:= H(0);
      B:= H(1);
      C:= H(2);
      D:= H(3);
      E:= H(4);
      for t in t_range loop
         Temp:= S(A, 5) + f(t, B, C, D) + E + W(t) + K(t);
         E:= D;
         D:= C;
         C:= S(B, 30);
         B:= A;
         A:= Temp;
      end loop;
      H(0):= H(0) + A;
      H(1):= H(1) + B;
      H(2):= H(2) + C;
      H(3):= H(3) + D;
      H(4):= H(4) + E;
   end Method1;

    -- Processing method 2 in rfc3174:
   procedure Method2(Mi: Block; H: in out Digest) is
      A, B, C, D, E, Temp: Word;
      Mask: Word:= 16#0f#;
      index: Word;
      W: Word_array(0..15);
   begin
      W(0..15):= Mi;
      A:= H(0);
      B:= H(1);
      C:= H(2);
      D:= H(3);
      E:= H(4);
      for t in t_range loop
         index:= Word(t) and Mask;
         if t >= 16 then
            W(integer(index)):=
               S(W(integer((index + 13) and Mask)) xor
                 W(integer((index + 8) and Mask)) xor
                 W(integer((index + 2) and Mask)) xor
                 W(integer(index)), 1);
         end if;
         Temp:= S(A, 5) + f(t, B, C, D) + E + W(integer(index)) + K(t);
         E:= D;
         D:= C;
         C:= S(B, 30);
         B:= A;
         A:= Temp;
      end loop;
      H(0):= H(0) + A;
      H(1):= H(1) + B;
      H(2):= H(2) + C;
      H(3):= H(3) + D;
      H(4):= H(4) + E;
   end Method2;

   -- Auxiliary conversion function:
   function To_block(B: Byte_array) return Block is
      Result: Block;
      Pos: Integer:= B'first;
   begin
      if B'length /= 64 then
         raise Constraint_error;
      end if;
      for i in Result'range loop
         Result(i):=
            Shift_left(Word(B(Pos + 0)), 24) or
            Shift_left(Word(B(Pos + 1)), 16) or
            Shift_left(Word(B(Pos + 2)), 8) or
            Shift_left(Word(B(Pos + 3)), 0);
         Pos:= Pos + 4;
      end loop;
      return Result;
   end To_block;

   function To_block(b: Bit_array) return Block is
      Result: Block;
      Pos: Natural:= b'first;
   begin
      if b'length /= 512 then
         raise Constraint_error;
      end if;
      for i in Result'range loop
         Result(i):= Word_bit.To_number_BE(b(Pos..Pos + 31));
         Pos:= Pos + 32;
      end loop;
      return Result;
   end To_block;

   -- Data supplying:
   procedure Feed
         (C: in out Context; B: Byte_array; Count_size: boolean:= true)
   is
      Byte_aux       : Bit_array(1..8);
      Original_size  : constant Message_length:= C.Length;
      B_pos          : Integer := B'First;
      Span           : Integer;
   begin
      if Count_size then
         C.Length:= C.Length + Message_length(B'length * Byte'size);
         -- If it wraps, error:
         if C.Length < Original_size then
            raise Constraint_error;
         end if;
      end if;
      case C.Kind is
         when Byte_context =>
            while B_pos <= B'Last loop
               Span := Integer'Min (
                  B'Last - B_pos, C.Byte_data'Last - C.Pos);
               C.Byte_data (C.Pos .. C.Pos + Span) :=
                  B (B_pos .. B_pos + Span);
               B_pos := B_pos + Span + 1;
               C.Pos := C.Pos + Span + 1;
               if C.Pos = C.Byte_data'length then
                  -- Process the filled block
                  case C.Implementation is
                     when Time_efficient =>
                        Method1(To_block(C.Byte_data), C.H);
                     when Space_efficient =>
                        Method2(To_block(C.Byte_data), C.H);
                  end case;
                  C.Pos:= 0;
               end if;
            end loop;
         when Bit_context =>
            for i in B'range loop
               Byte_aux:= Byte_bit.To_bit_array_BE(B(i));
               for j in Byte_aux'range loop
                  C.Bit_data(C.Pos):= Byte_aux(j);
                  C.Pos:= C.Pos + 1;
                  if C.Pos = C.Byte_data'length then
                     -- Process the filled block
                     case C.Implementation is
                        when Time_efficient =>
                           Method1(To_block(C.Bit_data), C.H);
                        when Space_efficient =>
                           Method2(To_block(C.Bit_data), C.H);
                     end case;
                     C.Pos:= 0;
                  end if;
               end loop;
            end loop;
      end case;
   end;

   procedure Feed(C: in out Context; b: Bit_array; Count_size: Boolean:= true)
   is
      Original_size: constant Message_length:= C.Length;
   begin
      if Count_size then
         C.Length:= C.Length + b'length;
         -- If it wraps, error:
         if C.Length < Original_size then
            raise Constraint_error;
         end if;
      end if;
      case C.Kind is
         when Byte_context =>
            -- Only 8-multiple bit sequences allowed:
            if b'length mod 8 /= 0 then
               raise Constraint_error;
            end if;
            -- Convert to bytes and feed them:
            for i in 0..b'length / 8 - 1 loop
               Feed(
                  C,
                  Byte_array'(1 => Byte_bit.To_number_BE(b(i * 8..i * 8 + 7))),
                  Count_size => false);
            end loop;
         when Bit_context =>
            for i in b'range loop
               C.Bit_data(C.Pos):= b(i);
               C.Pos:= C.Pos + 1;
               if C.Pos = C.Bit_data'length then
                  -- Process the filled block
                  case C.Implementation is
                     when Time_efficient =>
                        Method1(To_block(C.Bit_data), C.H);
                     when Space_efficient =>
                        Method2(To_block(C.Bit_data), C.H);
                  end case;
                  C.Pos:= 0;
               end if;
            end loop;
      end case;
   end;

   -- Final results:
   function Get_SHA1(C: Context) return Digest is
      procedure Pad(C: in out Context) is
         Void: Natural;
      begin
         case C.Kind is
            when Byte_context =>
               Feed(C, Byte_array'(1 => 16#80#), Count_size => false);
               if C.Pos + 8 > C.Byte_data'length then
                  -- Not enough room for padding
                  Void:= C.Byte_data'length +
                        (C.Byte_data'last - C.Pos + 1) - 8;
               else
                  -- Enough room for padding:
                  Void:= (C.Byte_data'last - C.Pos + 1) - 8;
               end if;
               if Void > 0 then
                  declare
                     Zeroes: Byte_array(1..Void):= (others => 0);
                  begin
                     Feed(C, Zeroes, Count_size => false);
                  end;
               end if;
               if C.Pos + 8 /= C.Byte_data'length then
                  raise Constraint_error;
               end if;
               -- Feed the length:
               for i in reverse 0..7 loop
                  Feed(C, Byte_array'
                    (1 => Byte(Shift_right(C.Length, i * 8) and 16#ff#)),
                     Count_size => false);
               end loop;
            when Bit_context =>
               Feed(C, Bit_array'(1 => true), Count_size => false);
               if C.Pos + 64 > C.Bit_data'length then
                  -- Not enough room for padding
                  Void:= C.Bit_data'length +
                        (C.Bit_data'last - C.Pos + 1) - 64;
               else
                  -- Enough room for padding:
                  Void:= (C.Bit_data'last - C.Pos + 1) - 64;
               end if;
               if Void > 0 then
                  declare
                     Zeroes: Bit_array(1..Void):= (others => false);
                  begin
                     Feed(C, Zeroes, Count_size => false);
                  end;
               end if;
               if C.Pos + 64 /= C.Bit_data'length then
                  raise Constraint_error;
               end if;
               -- Feed the length:
               Feed(C, ML_bit.To_bit_array_BE(C.Length), Count_size => false);
         end case;
         C.Padded:= true;
      end Pad;
      Caux: Context:= C;
   begin
      if not Caux.Padded then
         Pad(Caux);
      end if;
      return Caux.H;
   end;

   -- functions to easily display Digests:
   function To_Hex(D: Digest) return String is
      s: String(1..40);
      Pos: Natural:= s'first;
      h: constant Array(0..15) of Character:= "0123456789ABCDEF";
   begin
      for i in D'range loop
         for j in reverse 0..3 loop
           s(Pos):= h(integer(Shift_right(D(i), j * 8 + 4) and 16#f#));
           s(Pos + 1):= h(integer(Shift_right(D(i), j * 8) and 16#f#));
           Pos:= Pos + 2;
         end loop;
      end loop;
      return s;
   end To_Hex;

   function To_Base32(D: Digest) return String is
      use Bit_arrays;
      Base32: constant array(0..31) of Character:= "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
      Raw: Bit_array(1..160);
      Pos: Natural:= Raw'First;
      s: String(1..32);
   begin
      -- Init the bit array:
      for i in D'range loop
         for j in reverse 0..3 loop
            Raw(Pos..Pos + 7):= Byte_bit.To_bit_array_BE
              (Byte(Interfaces.Rotate_right(D(i), j * 8) and 16#ff#));
            Pos:= Pos + 8;
         end loop;
      end loop;
      -- Churn the base32 representation:
      Pos:= Raw'First;
      for i in 1..32 loop
         s(i):= Base32(Bit_arrays.Numbers.To_number_BE(Raw(pos..pos + 4)));
         Pos:= Pos + 5;
      end loop;
      return s;
   end To_Base32;

   -- And their converse
   hex_values : constant array (Character) of Interfaces.Unsigned_32:= (
      '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7 , '8' => 8, '9' => 9,
      'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15,
      'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15, others => 0);
   function From_Hex (S : in String) return Digest is
      D   : Digest;
      Pos : Natural := S'First;
   begin
      for i in D'range loop
         D (i) := 0;
         for j in reverse 0..3 loop
           D (i) := D (i) or Shift_Left (Hex_Values (S (Pos)), j * 8 + 4);
           D (i) := D (i) or Shift_Left (Hex_Values (S (Pos + 1)), j * 8);
           Pos:= Pos + 2;

           --Inverse:
           --s(Pos):= h(integer(Shift_right(D(i), j * 8 + 4) and 16#f#));
           --s(Pos + 1):= h(integer(Shift_right(D(i), j * 8) and 16#f#));
         end loop;
      end loop;
      return D;
   end From_Hex;

   --Base32: constant array(0..31) of Character:= "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
   Base32_Values : constant array (Character) of Mod32 := (
      'A' => 0, 'B' => 1, 'C' => 2, 'D' => 3, 'E' => 4, 'F' => 5, 'G' => 6, 'H' => 7, 'I' => 8,
      'J' => 9, 'K' => 10, 'L' => 11, 'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16,
      'R' => 17, 'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23, 'Y' => 24,
      'Z' => 25, '2' => 26, '3' => 27, '4' => 28, '5' => 29, '6' => 30, '7' => 31, others => 0);
   function From_Base32 (S : in String) return Digest is
      Raw: Bit_array(1..160);
      D  : Digest;
      Pos: Natural := Raw'First;
   begin
      -- Init the bit array
      for I in S'Range loop
         Raw (Pos .. Pos + 4) := Mod32_Bit.To_Bit_Array_BE (Base32_Values (S (I)));
         Pos := Pos + 5;
      end loop;
      -- Extract the Digest
      Pos := Raw'First;
      for i in D'range loop
         D (I) := 0;
         for j in reverse 0..3 loop
            D (i) := D (i) or Rotate_Left (Word (Byte_Bit.To_Number_BE (Raw (Pos .. Pos + 7))), j * 8);
            Pos:= Pos + 8;
         end loop;
      end loop;
      return D;
   end From_Base32;

   -- conversion to binary string:
   function To_char_array (D : in Digest) return String is
      Result : String (1 .. 20);
      use Interfaces;
   begin
      for N in D'range loop
         declare
            S : String (1 .. 4);
         begin
            for M in S'Range loop
               S (M) := Character'Val (
                  Integer (Shift_right (D (N), 8 * (4 - M)) and 16#ff#));
            end loop;
            Result (N * 4 + 1 .. N * 4 + 4) := S;
         end;
      end loop;
      return Result;
   end To_char_array;

   function From_char_array (S : in String) return Digest is
      D : Digest;
      use Interfaces;
      Pos : Natural := S'First;
   begin
      for N in D'range loop
         declare
            Chunk : String (1 .. 4) := S (Pos .. Pos + 3);
         begin
            Pos := Pos + 4;
            D (N) := 0;
            for M in Chunk'Range loop
               D (N) := Shift_left (D (N), 8) or
                  Word (Character'Pos (Chunk (M)));
            end loop;
         end;
      end loop;
      return D;
   end From_char_array;

end Sha1;
