package body Sha1.Streams is

   use type Interfaces.Unsigned_64;

   function Hash
     (s: access Ada.Streams.Root_stream_type'Class; 
      Length: Message_length;    -- In *bytes*
      Implementation: Method:= Time_efficient) return Digest is
      C: Context(Kind => Byte_context, Implementation => Implementation);
      char: Character;
      Remaining: Message_length:= Length;
      B: Byte_array(1..Block_size);
   begin
      -- Chunk reading
      while Remaining >= B'length loop
         Byte_array'Read(s, B);
         Feed(C, B);
         Remaining:= Remaining -  B'length;
      end loop;

      -- Remaining reading
      for i in 1 .. Remaining loop
         Character'Read(s, char);
         Feed(C, Byte_array'(1 => Character'Pos(char)));
      end loop;

      return Get_SHA1(C);
   end Hash;
   
end Sha1.Streams;

