-- Basic core for SHA1 calculation.
-- Child packages will be added for specific sources of bits,
--    as streams, strings, bit arrays or others.

-- See RFC3174 for details.

with Bit_arrays;    use Bit_arrays;
with Interfaces;
   
package Sha1 is

   Unimplemented: Exception;

   type Digest is private;

   function "<" (Left, Right: Digest) return boolean;

   -- Empty digest:
   Null_digest: constant Digest;

   type Method is (Time_efficient, Space_efficient);

   -- Message length is measured in bits:
   subtype Message_length is Interfaces.Unsigned_64; -- eight bytes

   -- functions to easily display digests:
   function To_Hex(D: Digest) return String; 
   function To_Base32(D: Digest) return String; 

   -- And their converse
   function From_Hex (S : in String) return Digest; -- UNTESTED, MAY BE ERRONEOUS
   function From_Base32 (S : in String) return Digest; -- TESTED, SAFE

   -- conversion to/from binary string:
   -- Maintains the word in big endian convention.
   function To_char_array (D : in Digest) return String;
   function From_char_array (S : in String) return Digest;

   subtype Byte is Interfaces.Unsigned_8;
   type Byte_array is array(Natural range <>) of Byte;
   pragma Pack (Byte_array);

private

   subtype Word is Interfaces.Unsigned_32;  -- four bytes

   type Word_array is array(integer range <>) of Word;
   pragma Pack(Word_array);

   type Digest is new Word_array(0..4);
   pragma Pack(Digest);
   Null_digest: constant Digest:= (others => 0);

   subtype Block is Word_array(0..15);
   Block_size: constant integer:= Block'Size / 8;

   type Message is array(integer range <>) of Block;

   subtype t_range is integer range 0..79;

   type Context_kind is (Byte_context, Bit_context);

   type Context
     (Kind: Context_kind:= Byte_context; 
      Implementation: Method:= Time_efficient) is 
   record
      Padded: Boolean:= false;
      Length: Message_length:= 0;
      H: Digest:=
        (16#67452301#,
         16#efcdab89#,
         16#98badcfe#,
         16#10325476#,
         16#c3d2e1f0#); 
      Pos: Natural:= 0;
      case Kind is
         when Byte_context =>
            Byte_data: Byte_array(0..63);
         when Bit_context =>
            Bit_data: Bit_array(0..511);
      end case;
   end record;
   
   K: constant array(t_range) of Word:=
     ( 0..19 => 16#5a827999#,
      20..39 => 16#6ed9eba1#,
      40..59 => 16#8f1bbcdc#,
      60..79 => 16#ca62c1d6#);

   function f00_19(B, C, D: Word) return Word;
   function f20_39(B, C, D: Word) return Word;
   function f40_59(B, C, D: Word) return Word;
   function f60_79(B, C, D: Word) return Word;
   pragma Inline(f00_19, f20_39, f40_59, f60_79);

   function f(t: t_range; B, C, D: Word) return Word;
   pragma Inline(f);

   function S(n: Interfaces.Unsigned_32; Amount: Natural)
      return Interfaces.Unsigned_32
      renames Interfaces.Rotate_left;

   -- Processing method 1 in rfc3174:
   -- Time efficient, space inefficient (364 bytes)
   procedure Method1(Mi: Block; H: in out Digest);

   -- Processing method 2 in rfc3174:
   -- Time inefficient, space efficient (64 bytes)
   procedure Method2(Mi: Block; H: in out Digest);

   -- Auxiliary functions:
   function To_block(B: Byte_array) return Block;
   function To_block(B: Bit_array) return Block;

   -- Data supplying:
   procedure Feed
         (C: in out Context; B: Byte_array; Count_size: boolean:= true);
   procedure Feed
         (C: in out Context; b: Bit_array; Count_size: boolean:= true);

   -- Final results:
   function Get_SHA1(C: Context) return Digest;
   
end Sha1;
