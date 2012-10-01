with Bit_arrays.Modular;
with Bit_arrays.Numbers;

package body Sha1.Bits is

   -- A single bit sequence:
   function Hash
     (b: Bit_array; Implementation: Method:= Time_efficient) return Digest is
     C: Context(Kind => Bit_context, Implementation => Implementation);
   begin
      Feed(C, b);

      return Get_SHA1(C);
   end Hash;

   -- Functions for staggered hashing:
   procedure Feed(HContext: in out Hash_context; b: Bit_array) is
      use type Interfaces.Unsigned_64;
   begin
      Feed(Context(HContext), b);
   end Feed;

   -- Modular types, big endian first (most usual):
   procedure Feed_BE
     (HContext: in out Hash_context; Data: Num) is
     package Num_bit is new Bit_arrays.Modular(Num);
   begin
      Feed(Context(HContext), Num_bit.To_bit_array_BE(Data));
   end Feed_BE;

   -- Modular types, little endian first:
   procedure Feed_LE
     (HContext: in out Hash_context; Data: Num) is
     package Num_bit is new Bit_arrays.Modular(Num);
   begin
      Feed(Context(HContext), Num_bit.To_bit_array_LE(Data));
   end Feed_LE;

   -- Any integer, big endian:
   -- Size means the significative bits in n to feed
   procedure Feed(HContext: in out Hash_context; n: integer; Size: Positive) is
   begin
      Feed(Context(HContext), Bit_arrays.Numbers.To_bit_array(n, Size));
   end Feed;

   -- Retrieve hash once completed:
   -- Note that it can be reused with accumulative supplied data
   -- (i.e., you can hash 'xxxx', supply 'yyyy' and obtain hash('xxxxyyyy')
   function Hash(HContext: Hash_context) return Digest is 
   begin
      return Get_SHA1(Context(HContext));
   end Hash;
   
end Sha1.Bits;
