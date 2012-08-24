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
--  $Id: adagio-http-header-parser.ads,v 1.3 2004/01/21 21:05:37 Jano Exp $

with Adagio.Socket;

with Agpl.Bandwidth_Throttle;
with Agpl.Streams;

with Ada.Finalization;
with Ada.Streams; use Ada.Streams;

package Adagio.Http.Header.Parser is

   ------------------------------------------------------------------------
   -- Non-blocking parser                                                --
   ------------------------------------------------------------------------
   -- Will read until a full header set is obtained.
   type Object (Max_length : Stream_element_offset) is private;

   -- Clean
   procedure Reset (This : out Object);

   -- Check for new data to be read.
   -- Can raise exception if connection is bad.
   -- Can raise exception if not enough space.
   procedure Check (This : in out Object; Sock : in Socket.Object);

   -- Check but subject to a throttle:
   -- Which can be null to imply unlimited BW
   procedure Check (
      This     : in out Object; 
      Sock     : in     Socket.Object; 
      Throttle : in     Agpl.Bandwidth_Throttle.Object_Access);

   -- Check completed
   function Completed (This : in Object) return Boolean;

   -- Get a header set from a completed parsing:
   procedure Get_headers (This : in out Object; Result : out Header.Set);

private 

   type Object (Max_length : Stream_element_offset) is 
   new Ada.Finalization.Controlled with record
      Buffer : Agpl.Streams.Stream_Element_Array_Access :=
         new Stream_Element_Array (1 .. Max_Length);
      Next   : Stream_element_offset := 1;
   end record;

   procedure Adjust   (This : in out Object);
   procedure Finalize (This : in out Object);

   pragma Inline (Completed);

end Adagio.Http.Header.Parser;
