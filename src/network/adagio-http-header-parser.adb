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
--  $Id: adagio-http-header-parser.adb,v 1.5 2004/02/29 20:36:45 Jano Exp $

with Adagio.Trace;

with Agpl.Streams.Memory_Arrays;


package body Adagio.Http.Header.Parser is

   Terminator : constant Stream_element_array := (13, 10, 13, 10);

   -- Clean
   procedure Reset (This : out Object) is
   begin
      This.Next := 1;
   end Reset;

   -- Check for new data to be read.
   -- Can raise exception if connection is bad.
   -- Can raise exception if not enough space.
   procedure Check (This : in out Object; Sock : in Socket.Object) is
   begin
      if not Socket.Is_alive (Sock) then
         Raise_exception (Socket.Socket_error'Identity,
            "Http.Header.Parser.Check: Connection reset by peer");
      end if;
      while Socket.Available (Sock) > 0 and not Completed (This) loop
         Stream_element'Read (
            Socket.Stream (Sock),
            This.Buffer (This.Next));
         This.Next := This.Next + 1;
      end loop;
   exception
      when Constraint_error =>
         Trace.Log ("Http.Header.Parser.Check: Too many header data: " &
            Agpl.Streams.To_string (This.Buffer.all), Trace.Warning);
         raise;
   end Check;

   -- Check but subject to a throttle:
   procedure Check (
      This     : in out Object;
      Sock     : in     Socket.Object;
      Throttle : in     Agpl.Bandwidth_Throttle.Object_Access)
   is
      Req : Stream_Element_Count;
      use type Agpl.Bandwidth_Throttle.Object_Access;
   begin
      if not Socket.Is_alive (Sock) then
         Raise_exception (Socket.Socket_error'Identity,
            "Http.Header.Parser.Check: Connection reset by peer");
      end if;
      while Socket.Available (Sock) > 0 and not Completed (This) loop

         if Throttle /= null then
            Throttle.Commit (1, Req);
            if Req = 0 then
               Throttle.Commit (1, Req, Extra => true);
            end if;

            exit when Req = 0;
         end if;

         Stream_element'Read (
            Socket.Stream (Sock),
            This.Buffer (This.Next));
         This.Next := This.Next + 1;

      end loop;
   exception
      when Constraint_error =>
         Trace.Log ("Http.Header.Parser.Check: Too many header data: " &
            Agpl.Streams.To_string (This.Buffer.all), Trace.Warning);
         raise;
   end Check;

   -- Check completed
   function Completed (This : in Object) return Boolean is
   begin
      return
         This.Next - This.Buffer'First > 4 and then
         This.Buffer (This.Next - 4 .. This.Next - 1) = Terminator;
   end;

   -- Get a header set from a completed parsing:
   procedure Get_headers (This : in out Object; Result : out Header.Set) is
      Mem    : aliased Agpl.Streams.Memory_Arrays.Stream_type (
         This.Buffer);
   begin
      Header.Parse (Result, Mem'Access, Read_response => true, Clean => true);
   end Get_headers;

   procedure Adjust   (This : in out Object) is
   begin
      This.Buffer := new Ada.Streams.Stream_Element_Array'(This.Buffer.all);
   end Adjust;

   procedure Finalize (This : in out Object) is
   begin
      Agpl.Streams.Free (This.Buffer);
   end Finalize;

end Adagio.Http.Header.Parser;
