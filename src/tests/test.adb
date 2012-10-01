with Agpl.Streams.Filter.Bandwidth_Throttle;
with Agpl.Streams.Filter.Buffered_Unbounded;
with Agpl.Streams.Filter.Deflate_Unbounded;
with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;
with Agpl.Streams.Controlled;
with Agpl.Streams.Observable;
with Agpl.Streams.Observable_Filter;
with Agpl.Types;
use  Agpl;

with Ada.Exceptions;
use  Ada;

with Text_IO;

procedure Test is
   C : Streams.Filter.Stream_Type;
begin
   null;
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
