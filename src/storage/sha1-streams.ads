With Ada.Streams;

package Sha1.Streams is

   function Hash
     (s: access Ada.Streams.Root_stream_type'Class; 
      Length: Message_length;    -- In *bytes*
      Implementation: Method:= Time_efficient) return Digest;
   
end Sha1.Streams;
