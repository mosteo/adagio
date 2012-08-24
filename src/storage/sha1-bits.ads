with Bit_arrays;

package Sha1.Bits is

   -- Type for staggered hashing:
   type Hash_context(Implementation: Method:= Time_efficient) is private;

   -- A single bit sequence:
   function Hash
     (b: Bit_array; Implementation: Method:= Time_efficient) return Digest;

   -- Functions for chunk hashing:
   procedure Feed(HContext: in out Hash_context; b: Bit_array);

   -- Any integer, big endian:
   -- Size means the significative bits in n to feed
   procedure Feed(HContext: in out Hash_context; n: integer; Size: Positive);

   -- Modular types, big endian first (most usual):
   generic
      type Num is mod <>;
   procedure Feed_BE(HContext: in out Hash_context; Data: Num);

   -- Modular types, little endian first:
   generic
      type Num is mod <>;
   procedure Feed_LE(HContext: in out Hash_context; Data: Num);

   -- Retrieve hash once completed:
   -- Note that it can be reused with accumulative supplied data
   -- (i.e., you can hash 'xxxx', supply 'yyyy' and obtain hash('xxxxyyyy')
   function Hash(HContext: Hash_context) return Digest;

private

   type Hash_context(Implementation: Method:= Time_efficient) is new 
      Context(Kind => Bit_context, Implementation => Implementation);
   
end Sha1.Bits;
