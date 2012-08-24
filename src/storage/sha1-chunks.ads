with Ada.Streams;
with Bit_arrays;

package Sha1.Chunks is

   -- Type for chunk hashing:
   type Hash_context(Implementation: Method:= Time_efficient) is private;

   -- Any bit sequence:
   procedure Feed(HContext: in out Hash_context; b: Bit_array);

   -- Any string:
   procedure Feed(HContext: in out Hash_context; s: String);

   -- Any stream:
   -- Size in *bytes*
   procedure Feed
     (HContext: in out Hash_context; 
      Stream: Ada.Streams.Root_stream_type'Class;
      Size: Positive);

   -- Regular integer:
   -- Always Big Endian; Size specifies the number of significant bits in n
   -- Undefined results if n < 0
   procedure Feed (HContext: in out Hash_context; n: Integer; Size: Positive);

   -- Modular types, big endian first (most usual):
   generic
      type Num is mod <>;
   procedure Feed_BE(HContext: in out Hash_context; Data: Num; Size: Positive);

   -- Modular types, little endian first:
   generic
      type Num is mod <>;
   procedure Feed_LE(HContext: in out Hash_context; Data: Num; Size: Positive);

   -- Retrieve hash once completed:
   -- Note that it can be reused with accumulative supplied data
   -- (i.e. you can hash 'xxxx', supply 'yyyy' and obtain the hash for xxxyyyy)
   -- It permits also take branchs since Hash_context is not limited.
   function Hash(HContext: Hash_context) return Digest;

private

   type Hash_context(Implementation: Method:= Time_efficient) is new 
      Context(Kind => Bit_context, Implementation => Implementation);
   
end Sha1.Chunks;
