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
--  $Id: adagio-g2.adb,v 1.3 2004/01/21 21:05:26 Jano Exp $

with Adagio.Misc;
with Strings.Fields;

with Text_io;

with Ada.Unchecked_conversion; use Ada;

with Gnat.Sockets; use Gnat;

package body Adagio.G2 is

   -- Convert a dotted string into an array:
   function To_address (S : String) return IPv4_address is
      Pos    : Natural := S'First;
      Last   : Natural;
      Result : IPv4_address;
      package Byte_io is new Text_io.Modular_io (Byte);
   begin
      for N in 1 .. 4 loop
         Byte_io.Get (S (Pos .. S'Last), Result (N), Last);
         Pos := Last + 2; -- Skip the dot.
      end loop;

      return Result;
   end To_address;

   -- Convert a 6-byte payload to a sock_address_type
   function To_address (
      this       : in String;
      Big_endian : in Boolean) return Socket.Sock_addr_type is
      Address : String :=
         Misc.To_string (Character'Pos (this (this'First))) & "." &
         Misc.To_string (Character'Pos (this (this'First + 1))) & "." &
         Misc.To_string (Character'Pos (this (this'First + 2))) & "." &
         Misc.To_string (Character'Pos (this (this'First + 3)));
      Port    : Network.Endian.Byte_array (1 .. 2) :=
         (1 => Character'Pos (this (this'First + 4)),
          2 => Character'Pos (this (this'First + 5)));
   begin
      return (
         Family => Sockets.Family_inet,
         Addr   => Sockets.Inet_addr (Address),
         Port   => Sockets.Port_type
            (Network.Endian.Convert (Port, Big_endian)));
   end To_address;

   function To_char_array (
      this       : in Socket.Sock_addr_type;
      Big_endian : in Boolean) return String is

      use Strings.Fields;
      Result     : Byte_array (1 .. 6);
      sAddr      : String := Socket.Image (this.Addr);
   begin
      for N in 1 .. 4 loop
         Result (N) := Byte (Integer'Value (Select_field (sAddr, N, '.')));
      end loop;
      Result (5 .. 6) := Network.Endian.Convert (
         Integer (this.Port), 2, Big_endian);

      return To_string (Result);
   end To_char_array;

   -- Convert a readable address (x.x.x.x:n) into a 6 byte payload:
   function To_char_array (
      this       : in String;
      Big_endian : in Boolean) return String is
   begin
      return To_char_array (Socket.To_address (this), Big_endian);
   end To_char_array;

   -- Convert a 6-byte payload to a readable address:
   function To_address (
      this       : in String;
      Big_endian : in Boolean) return String is
   begin
      return Socket.Image (To_address (this, Big_endian));
   end To_address;

   function To_time_t (
      this       : in String;
      Big_endian : in Boolean) return Time_t.Time_t is
      type Tuplet is new String (1 .. 4);
      function To_time_t is new Unchecked_conversion (Tuplet, Time_t.Time_t);
   begin
      if Big_endian = not Network.Endian.Little_endian then
         return To_time_t (Tuplet (This));
      else
         return To_time_t (Tuplet (To_string (Network.Endian.Invert (
            To_byte_array (this)))));
      end if;
   end To_time_t;

   function To_string (
      this       : in Time_t.Time_t;
      Big_endian : in Boolean) return String is
      type Tuplet is new String (1 .. 4);
      function To_tuplet is new Unchecked_conversion (Time_t.Time_t, Tuplet);
   begin
      if Big_endian = not Network.Endian.Little_endian then
         return String (To_Tuplet (This));
      else
         return To_string (Network.Endian.Invert (
            To_byte_array (String (To_tuplet (this)))));
      end if;
   end To_string;

   ------------------------------------------------------------------------
   -- Use_mesh                                                           --
   ------------------------------------------------------------------------
   function Use_mesh return Boolean is
   begin
      return Globals.Options.Library_mesh_active;
   end Use_mesh;

end Adagio.G2;
