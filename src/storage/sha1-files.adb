with Ada.Streams.Stream_IO;
with Sha1.Streams;

package body Sha1.Files is

   use type Interfaces.Unsigned_64;

   function Hash
     (Name: String; Implementation: Method:= Time_efficient) return Digest is
      use Ada.Streams.Stream_IO;
      C: Context(Kind => Byte_context, Implementation => Implementation);
      F: File_type;
      Result: Digest;
   begin
      if Character'size /= Byte'size then
         raise Unimplemented;
      end if;

      Open(F, Name => Name, Mode => In_file);
      Result:= 
         Sha1.Streams.Hash(Stream(F), Message_length(Size(F)));
      Close(F);

      return Result;
   exception
      when others =>
         if Is_open(F) then
            Close(F);
         end if;
         raise;
   end Hash;
   
end Sha1.Files;
