package body Sha1.Strings is

   use type Interfaces.Unsigned_64;

   function Hash
     (s: String; Implementation: Method:= Time_efficient) return Digest is
      C: Context(Kind => Byte_context, Implementation => Implementation);
   begin
      for i in s'range loop
         Feed(C, Byte_array'(1 => Character'Pos(s(i))));
      end loop;

      return Get_SHA1(C);
   end Hash;
   
end Sha1.Strings;
