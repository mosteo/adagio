package body Adagio.Http is

   -- Read from socket until CRLF (stripped)
   function Get_line(Stream: access Ada.Streams.Root_stream_type'Class) 
      return String is
      c: Character;
      s: UString;
   begin
      loop
         Character'Read(Stream, c);
         exit when c = CR;
         ASU.Append(s, c);
      end loop;
      Character'Read(Stream, c);
      if c /= LF then
         raise Constraint_error;
      end if;
      return To_string(s);
   end Get_line;

end Adagio.Http;
