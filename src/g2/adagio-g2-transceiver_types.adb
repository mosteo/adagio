------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: adagio-g2-transceiver_types.adb,v 1.5 2004/03/29 19:13:31 Jano Exp $

with Adagio.Misc;
with Adagio.Statistics;
with Adagio.Statistics.Integers;

package body Adagio.G2.Transceiver_types is

   use Ada.Calendar;
   use type Agpl.Streams.Stream_Element_Array_Access;

   ------------
   -- Adjust --
   ------------

   procedure Adjust   (This : in out Fragment_Type) is
   begin
      if This.Data /= null then
         This.Data := new Stream_Element_Array'(This.Data.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Fragment_Type) is
   begin
      Agpl.Streams.Free (This.Data);
   end Finalize;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (This : in out Fragment_Type;
                       Data : in     Stream_Element_Array)
   is
   begin
      Agpl.Streams.Free (This.Data);
      This.Data := new Stream_Element_Array'(Data);
   end Set_Data;

   ------------------------------------------------------------------------
   -- Controls                                                           --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Packet_type) is
      pragma Unreferenced (This);
   begin
      Statistics.Object.Update (
         Stat_alive_packets,
         Statistics.Integers.Increment'Access,
         Statistics.Integers.Create (1));
   end Initialize;
   procedure Finalize   (This : in out Packet_type) is
      pragma Unreferenced (This);
   begin
      Statistics.Object.Update (
         Stat_alive_packets,
         Statistics.Integers.Increment'Access,
         Statistics.Integers.Create (-1));
   end Finalize;

   ------------------------------------------------------------------------
   -- Id_inbound                                                         --
   ------------------------------------------------------------------------
   -- Identify inbound packets:
   function Id_inbound (This : in Packet_type) return String is
   begin
      return Socket.Image (This.Source) & ":" &
         Misc.To_string (Natural (This.Header.nSequence));
   end Id_inbound;

   -----------------------------------------------------------------------
   -- Is_Complete                                                       --
   -----------------------------------------------------------------------
   function Is_complete (this : in Packet_type) return Boolean is
   begin
      for N in 1 .. this.Header.nCount loop
         if not this.Fragments (Integer (N)).Valid then
            return false;
         end if;
      end loop;

      return true;
   end Is_complete;

   -----------------------------------------------------------------------
   -- Older & Equal for inbound packets                                  --
   -----------------------------------------------------------------------
   function Older (Left, Right : in Packet_access) return Boolean is
   begin
      return Left.Arrived < Right.Arrived;
   end Older;

   function Equal (Left, Right : in Packet_list_by_arrival.Iterator_type)
      return Boolean is
      use PLbA;
   begin
      return Id_inbound (Element (Left).all) =
             Id_inbound (Element (Right).all);
   end Equal;

   ------------------------------------------------------------------------
   -- Soft_equal for outbound packets                                    --
   ------------------------------------------------------------------------
   function Soft_Equal (Left, Right : in Packet_access) return Boolean is
   begin
      return Left.Header.nSequence = Right.Header.nSequence;
   end Soft_Equal;

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   -- For headers: nSeq/nPart/nCount
   function To_string (H : Packet_header) return String is
   begin
      return Misc.To_string (Natural (H.nSequence)) & "/" &
             Misc.To_string (Natural (H.nPart)) & "/" &
             Misc.To_string (Natural (H.nCount));
   end To_string;

   ------------
   -- Create --
   ------------
   function Create (
      Data : in Stream_element_array; Dest : Socket.Sock_addr_type)
      return Udp_message
   is
   begin
      return
        (Last => Data'Length,
         Data => Data,
         Dest => Dest,
         Date => Clock);
   end Create;

begin
   Statistics.Object.Set (
      Stat_alive_packets, Statistics.Integers.Create (0));
end Adagio.g2.Transceiver_types;
