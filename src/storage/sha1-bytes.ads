package Sha1.Bytes is

   -- Type for chunk hashing:
   type Hash_context(Implementation: Method:= Time_efficient) is private;

   procedure Feed (
      HContext: in out Hash_context; 
      Bytes :   in     Byte_array);

   -- It permits also take branchs since Hash_context is not limited.
   function Hash (HContext: Hash_context) return Digest;

private

   type Hash_context(Implementation: Method:= Time_efficient) is new 
      Context(Kind => Byte_context, Implementation => Implementation);
   
end Sha1.Bytes;
