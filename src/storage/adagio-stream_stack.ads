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
--  $Id: adagio-stream_stack.ads,v 1.3 2004/01/21 21:05:42 Jano Exp $

--  Facility for chained filter streams.

with Adagio.Filter_stream;

with Ada.Finalization;           use Ada;
with Ada.Streams;                use Ada.Streams;

with Dynamic_vector;

package Adagio.Stream_stack is

   type Object (Stream : access Root_stream_type'Class) is private;

   procedure Reset (this : in out Object);

   procedure Chain (
      this   : in out Object; 
      Stream : in     Filter_stream.Stream_access);

   function Stream (this : in Object) return Filter_stream.Stream_access;

private

   package Stream_vector is new Dynamic_vector (Filter_stream.Stream_access);

   type Stream_access is access all Root_stream_type'Class;

   type Object (Stream : acess Root_stream_type'Class) is new 
      Finalization.Controlled with 
      record
         Streams : Stream_vector.Object (First => 1);
         Source  : Stream_access := Stream;
      end record;

   procedure Finalize (this : in out Object);

end Adagio.Stream_stack;
