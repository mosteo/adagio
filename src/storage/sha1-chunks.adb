with Bit_arrays.Modular;
with Bit_arrays.Numbers;

package body Sha1.Chunks is

   use type Interfaces.Unsigned_64;

   -- Any bit sequence:
   -- If context is of type byte, The array has to be of 8-multiple size
   procedure Feed(HContext: in out Hash_context; b: Bit_array) is
   begin
      Feed(Context(HContext), b);
   end Feed;

   -- Any string:
   procedure Feed(HContext: in out Hash_context; s: String) is
   begin
      for i in s'range loop
         Feed(Context(HContext), Byte_array'(1 => Character'Pos(s(i))));
      end loop;
   end Feed;

   -- Any stream:
   -- Size in *bytes*
   procedure Feed
     (HContext: in out Hash_context; 
      Stream: Ada.Streams.Root_stream_type'class; 
      Size: Positive) is
      B: Byte_array(1..1);
   begin
      Byte_array'Read(Stream'Unrestricted_access, B);
      Feed(Context(HContext), B);
   end Feed;

   -- Regular integer:
   -- Always Big Endian; Size specifies the number of significant bits in n
   -- Undefined results if n < 0
   procedure Feed(HContext: in out Hash_context; n: Integer; Size: Positive) is
      b: Bit_array:= Bit_arrays.Numbers.To_bit_array(n, Size);
   begin
      Feed(Context(HContext), b);
   end Feed;

   -- Modular types, big endian first (most usual):
   procedure Feed_BE(HContext: in out Hash_context; Data: Num; Size: Positive)is
      package Num_bit is new Bit_arrays.Modular(Num);
      b: Bit_array:= Num_bit.To_bit_array_BE(Data);
   begin
      Feed(Context(HContext), b);
   end Feed_BE;

   -- Modular types, little endian first:
   procedure Feed_LE(HContext: in out Hash_context; Data: Num; Size: Positive)is
      package Num_bit is new Bit_arrays.Modular(Num);
      b: Bit_array:= Num_bit.To_bit_array_LE(Data);
   begin
      Feed(Context(HContext), b);
   end Feed_LE;

   -- Retrieve hash once completed:
   -- Note that it can be reused with accumulative supplied data
   -- (i.e. you can hash 'xxxx', supply 'yyyy' and obtain the hash for xxxyyyy)
   -- It permits also take branchs since Hash_context is not limited.
   function Hash(HContext: Hash_context) return Digest is
   begin
      return Get_SHA1(Context(HContext));
   end Hash;

end Sha1.Chunks;
