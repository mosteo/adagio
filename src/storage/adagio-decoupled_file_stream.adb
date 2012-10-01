with Adagio.Exceptions; use Adagio.Exceptions;

package body Adagio.Decoupled_file_stream is

   use type Stream_IO.Count;

   -- Overriden primitives:
   procedure Read(
      Stream : in out Decoupled_file_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
      F: Stream_IO.File_type;
   begin
      -- Open the associated file:
      Stream_IO.Open(F, Stream_IO.In_file, To_string(Stream.File), 
         "shared=yes");
      -- Read requested items from current position:
      Stream_IO.Read(F, Item, Last, Stream.Filepos);
      -- Close file:
      Stream_IO.Close(F);
      -- Mark new position:
      Set_position
        (Stream, 
         Stream.Filepos + Stream_IO.Positive_count(Last - Item'first + 1));
   exception
      when others =>
         if Stream_IO.Is_open(F) then
            Stream_IO.Close(F);
         end if;
         raise;
   end Read;

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Decoupled_file_stream;
      Item   : in Stream_Element_Array) is
   begin
      raise Unimplemented;
   end Write;

   -- Creation. We need a path to file:
   procedure Get_decoupled_file_stream
     (Stream: out Decoupled_file_stream; 
      File:   String) is
   begin
      Stream.File:= To_UString(File);
      Stream.Filepos:= Stream_IO.Positive_count'First; -- 1
   end Get_decoupled_file_stream;

   -- Seek
   procedure Set_position
     (Stream: in out Decoupled_file_stream;
      Pos: Stream_IO.Positive_count) is
   begin
      Stream.Filepos:= Pos;
   end Set_position;

   -- Size
   function Size(Stream: Decoupled_file_stream) return Stream_IO.Count is
      F: Stream_IO.File_type;
      size: Stream_IO.Count;
   begin
      -- Open the associated file:
      Stream_IO.Open(F, Stream_IO.In_file, To_string(Stream.File),
         "shared=yes");
      size:= Stream_IO.Size(F);
      Stream_IO.Close(F);
      
      return size;
   exception
      when others =>
         if Stream_IO.Is_open(F) then
            Stream_IO.Close(F);
         end if;
         raise;
   end Size;

   function End_of_stream(Stream: Decoupled_file_stream) 
      return boolean is
      F: Stream_IO.File_type;
      EOF: Boolean;
   begin
      -- Open the associated file:
      Stream_IO.Open(F, Stream_IO.In_file, To_string(Stream.File),
         "shared=yes");
      EOF:= Stream.Filepos > Stream_IO.Size(F);
      -- Close file:
      Stream_IO.Close(F);
      return EOF;
   exception
      when others =>
         if Stream_IO.Is_open(F) then
            Stream_IO.Close(F);
         end if;
         raise;
   end End_of_stream;

end Adagio.Decoupled_file_stream;
