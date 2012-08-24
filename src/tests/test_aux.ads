with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Streams;
with GNAT.Debug_Pools;
package Test_Aux is

   type Stream_Element_Array_Access is access Ada.Streams.Stream_Element_Array;
   Pool : GNAT.Debug_Pools.Debug_Pool;
   -- for Stream_Element_Array_Access'Storage_Pool use Pool;

   type Udp_Message is new Ada.Finalization.Controlled with record
      Data : Stream_Element_Array_Access;
   end record;

   function Create (Data : in Ada.Streams.Stream_Element_Array) return Udp_Message;

   procedure Adjust   (This : in out Udp_Message);
   procedure Finalize (This : in out Udp_Message);

   package Udp_List is new Ada.Containers.Doubly_Linked_Lists (
      Udp_Message);

end Test_Aux;
