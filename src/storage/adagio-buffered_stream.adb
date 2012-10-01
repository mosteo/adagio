with Adagio.Exceptions; use Adagio.Exceptions;

with Ada.Unchecked_deallocation;

package body Adagio.Buffered_stream is

   procedure Initialize (this: in out Controlled_buffer_type) is
   begin
      this.Data:= new Buffer_type (1 .. this.Size);
   end Initialize;

   procedure Finalize(this: in out Controlled_buffer_type) is
      Procedure Delete is new Unchecked_deallocation
        (Buffer_type, Buffer_type_access);
   begin
      Delete(this.Data);
   end Finalize;

   -- Refill
   procedure Refill(BStream: in out Buffered_stream) is
   begin
      Read(BStream.Source.all, BStream.Buffer.Data.all, BStream.Last);
      BStream.Pos:= 1;
   end Refill;

   -- Overriden primitives:
   procedure Read(
      Stream : in out Buffered_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
      Remaining: Stream_element_count:= Item'length;
      Available: Stream_element_count;
   begin
      -- Get as many data as desired
      while Remaining > 0 loop
         Available:= Stream.Last - Stream.Pos + 1;
         if Available = 0 then
            Refill(Stream);
            Available:= Stream.Last - Stream.Pos + 1;
         elsif Available < 0 then
            Raise Program_error;
         end if;
         if Available >= Remaining then
            -- Enough in buffer:
            Item(Item'first..Item'first + Remaining - 1):=
               Stream.Buffer.Data(Stream.Pos..Stream.Pos + Remaining - 1);
            Stream.Pos:= Stream.Pos + Remaining;
            Remaining:= 0;
         else
            -- Not enough:
            Item(Item'first..Item'first + Available - 1):=
               Stream.Buffer.Data(Stream.Pos..Stream.Pos + Available - 1);
            Stream.Pos:= Stream.Pos + Available;
            Remaining:= Remaining - Available;
         end if;
      end loop;
      Last:= Item'last; 
      -- Note: never last will be less than 'last.
   end Read;

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Buffered_stream;
      Item   : in Stream_Element_Array) is
   begin
      raise Unimplemented;
   end Write;

   -- Creation. It links a new buffered stream with any other stream:
   procedure Get_buffered_stream
     (BStream: in out Buffered_stream; 
      Stream:  Stream_access) is
   begin
      if Stream = null then
         raise Constraint_error;
      end if;
      BStream.Source:= Stream;
      BStream.Last:= BStream.Buffer.Data.all'first - 1;
      BStream.Pos:= Bstream.Buffer.Data.all'first;
   end Get_buffered_stream;

   -- Says if the internal buffer is depleted. Doesn't implies that the
   -- source stream is depleted. Thus, it can occur multiple times
   -- before the end of stream of the source.
   -- To check end of data, a reader MUST check both end of source and
   -- end of buffer.
   function End_of_buffer(BStream: in Buffered_stream) return boolean is
   begin
      return BStream.Pos > BStream.Last;
   end End_of_buffer;

end Adagio.Buffered_stream;
