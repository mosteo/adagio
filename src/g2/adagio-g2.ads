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
--  $Id: adagio-g2.ads,v 1.4 2004/01/28 15:33:18 Jano Exp $

with Adagio.Constants;  use Adagio.Constants;
with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.Globals.Options;
with Adagio.Network.Endian;
with Adagio.Socket;
with Adagio.Time_t;
with Adagio.Types; use Adagio.Types;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Exceptions; use Ada.Exceptions;

with Interfaces;

package Adagio.G2 is

   use type Agpl.Types.Ustrings.Ustring;

   -- Some constants for G2
   Network_id   :    Constant String:= "Gnutella2";
   Content_type :    Constant String:= "application/x-gnutella2";
   Vendor_code  :    Constant String:= "AGIO";

   -- Some types:
   subtype Byte is Network.Endian.Byte;
   subtype Byte_array is Network.Endian.Byte_array;

   subtype Guid_String is String (1 .. 16); -- A Guid seen as a string

   -- Some stats:
   Packets_sent     : Natural := 0;
   Packets_received : Natural := 0;
   pragma Atomic (Packets_sent);
   pragma Atomic (Packets_received);

   -- IPv4 address & port:
   subtype  IPv4_address is Byte_array (1 .. 4);
   subtype  Port_type is Interfaces.Unsigned_16;

   -- Convert a dotted string into an array:
   function To_address (S : String) return IPv4_address;

   -- Convert a byte array into a char array and viceversa
   function To_string (From : in Byte_array) return String
      renames Network.Endian.To_string;

   function To_byte_array (From : in String) return Byte_array
      renames Network.Endian.To_byte_array;

   -- Convert a 6-byte payload to a sock_address_type
   -- If more than 6-byte are supplied, excess is ignored
   function To_address (
      this       : in String;
      Big_endian : in Boolean) return Socket.Sock_addr_type;

   -- Convert a sock_addr_type into a 6 byte payload:
   function To_char_array (
      this       : in Socket.Sock_addr_type;
      Big_endian : in Boolean) return String;

   -- Convert a readable address (x.x.x.x:n) into a 6 byte payload:
   function To_char_array (
      this       : in String;
      Big_endian : in Boolean) return String;

   -- Convert a 6-byte payload to a readable address:
   -- If more than 6-byte are supplied, excess is ignored
   function To_address (
      this       : in String;
      Big_endian : in Boolean) return String;

   -- Conversions for timestamps:
   function To_time_t (
      this       : in String;
      Big_endian : in Boolean) return Time_t.Time_t;

   function To_string (
      this       : in Time_t.Time_t;
      Big_endian : in Boolean) return String;

   -- Debug:
   Logfile : Ustring renames Globals.Options.Debug_netlogfile;

   ------------------------------------------------------------------------
   -- Mesh related                                                       --
   ------------------------------------------------------------------------
   function Use_mesh return Boolean;

end Adagio.G2;
