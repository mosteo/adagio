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
--  $Id: adagio-throttle_stream.ads,v 1.3 2004/01/21 21:05:42 Jano Exp $

-- Throttle stream

with Ada.Streams;                use Ada.Streams;
use Ada;

with Adagio.Throttler;

package Adagio.Throttle_stream is

   -- New stream type:
   type Throttle_stream (Throttle: access Throttler.Object) is new 
      Ada.Streams.Root_Stream_Type with private;

   type Stream_access is access all Throttle_stream;

   -- Overriden primitives:
   -- At each read, a Throttle.Cycle_work is performed.
   -- Thus, a Start_work should be ensured right before use.
   -- Lacking that, a HUGE delay could be triggered.
   -- Note that the creator will do that.
   procedure Read(
      Stream : in out Throttle_stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- Only reading is implemented. 
   -- Thus, any call to this raises Unimplemented_error.
	procedure Write(
      Stream : in out Throttle_stream;
      Item   : in Stream_Element_Array);

   -- Creation. It links a new buffered stream with any other stream:
   -- Will perform a Throttle.Start_work;
   procedure Get_throttle_stream
     (TStream: in out Throttle_stream; 
      Stream : access Streams.Root_stream_type'Class);

private

   type Counter_type is mod 2 ** 14;

   type Root_stream_access is access all Streams.Root_stream_type'Class;

   type Throttle_stream(Throttle: access Throttler.Object) is new 
      Ada.Streams.Root_Stream_Type with 
   record
      Source: Root_stream_access;  -- or any descendent.
      Counter: Counter_type:= 0;
   end record;

end Adagio.Throttle_stream;
