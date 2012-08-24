-- Buffered stream

-- The generic parameter gives the buffer size in Stream_element units.

with Ada.Finalization;           
with Ada.Streams;                use Ada.Streams;

use Ada;

package Adagio.Buffered_stream is

   type Stream_access is access all Ada.Streams.Root_stream_type'Class;

   -- New stream type:
   type Buffered_stream (Size : Stream_element_count) is new 
      Ada.Streams.Root_Stream_Type with private;
   type Object_access is access all Buffered_stream;

   -- Overriden primitives:
   procedure Read(
      Stream : in out Buffered_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Buffered_stream;
      Item   : in Stream_Element_Array);

   -- Creation. It links a new buffered stream with any other stream:
   procedure Get_buffered_stream
     (BStream: in out Buffered_stream; 
      Stream:  Stream_access);

   -- Says if the internal buffer is depleted. Doesn't implies that the
   -- source stream is depleted. Thus, it can occur multiple times
   -- before the end of stream of the source.
   -- To check end of data, a reader MUST check both end of source and
   -- end of buffer.
   function End_of_buffer(BStream: in Buffered_stream) return boolean;

private

   subtype Buffer_type is 
      Stream_element_array;
   type Buffer_type_access is access all Buffer_type;

   type Controlled_buffer_type (Size : Stream_element_count) is new 
   Finalization.Controlled with
      record
         Data: Buffer_type_access;
      end record;

   procedure Initialize (this: in out Controlled_buffer_type);
   procedure Finalize   (this: in out Controlled_buffer_type);

   type Buffered_stream (Size : Stream_element_count) is new 
      Ada.Streams.Root_Stream_Type 
   with record
      Source : Stream_access;  -- or any descendent.
      Buffer : Controlled_buffer_type (Size);
      Last   : Stream_element_offset:= 0;
      Pos    : Stream_element_offset:= 1; -- Next element to serve.
   end record;

   -- Refills internal buffer: 
   procedure Refill (BStream: in out Buffered_stream);

end Adagio.Buffered_stream;
