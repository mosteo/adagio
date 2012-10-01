with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.Globals;

package body Adagio.Throttle_stream is

   -- Overriden primitives:
   procedure Read(
      Stream : in out Throttle_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Read(Stream.Source.all, Item, Last);
      if Stream.Counter + Item'length < Stream.Counter then
         Stream.Throttle.Cycle_work;
      end if;
      Stream.Counter:= Stream.Counter + Item'length;
      if Globals.Requested_exit then
         raise Interrupted;
      end if;
   end Read;

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Throttle_stream;
      Item   : in Stream_Element_Array) is
   begin
      raise Unimplemented;
   end Write;

   -- Creation. It links a new buffered stream with any other stream:
   procedure Get_throttle_stream
     (TStream: in out Throttle_stream; 
      Stream:  access Streams.Root_stream_type'Class) is
   begin
      TStream.Source:= Root_stream_access (Stream);
      TStream.Throttle.Start_work;
   end Get_throttle_stream;

end Adagio.Throttle_stream;
