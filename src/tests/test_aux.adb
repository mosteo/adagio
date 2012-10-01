with Ada.Unchecked_Deallocation;
package body Test_Aux is

   function Create (Data : in Ada.Streams.Stream_Element_Array) return Udp_Message
   is
      Msg : Udp_Message :=
        (Ada.Finalization.Controlled with
           Data => new Ada.Streams.Stream_Element_Array'(Data));
   begin
      return Msg;
   end Create;

   procedure Adjust   (This : in out Udp_Message) is
   begin
      if This.Data /= null then
         This.Data := new Ada.Streams.Stream_Element_Array'(This.Data.all);
      end if;
   end Adjust;

   procedure Finalize (This : in out Udp_Message) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);
   begin
      Free (This.Data);
   end Finalize;

end Test_Aux;
