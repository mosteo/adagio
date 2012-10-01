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
--  $Id: adagio-socket-ip.adb,v 1.3 2004/01/21 21:05:39 Jano Exp $

with Agpl.Strings;
use  Agpl;

with Gnat.Sockets;   use Gnat;

with Ada.Exceptions; use Ada;

package body Adagio.Socket.IP is

   -- Returns the public/private IP of this machine
   -- If more than one public, the first encountered
   -- If none private, 127.0.0.1
   function Get_IP(Public: Boolean:= true) return string is
      Host: Sockets.Host_entry_type:= 
         Sockets.Get_host_by_name(Sockets.Host_name);
   begin
      if Public then
         for n in 1 .. Sockets.Addresses_length(Host) loop
            if Kind(Sockets.Image(Sockets.Addresses(Host, n))) = 
               Adagio.Socket.IP.Public then
               return Sockets.Image(Sockets.Addresses(Host, n));
            end if;
         end loop;
         Exceptions.Raise_Exception(Constraint_error'Identity, "No public IP found");
         return "0.0.0.0"; -- To disable warning
      else
         for n in 1 .. Sockets.Addresses_length(Host) loop
            if Kind(Sockets.Image(Sockets.Addresses(Host, n))) = Internal then
               return Sockets.Image(Sockets.Addresses(Host, n));
            end if;
         end loop;
         return "127.0.0.1";
      end if;
   end;

   -- Specific form:
   function Get_IP(Kind : in Address_type) return string is
   begin
      case Kind is
         when Local    => return Local_address;
         when Public   => return Get_IP (Public => true);
         when Internal => 
            if Get_IP (Public => false) /= Local_address then
               return Get_IP (Public => false);
            else
               Exceptions.Raise_Exception(
                  Constraint_error'Identity, "No private IP found");
               return Local_address; -- Prevent warning
            end if;
      end case;
   end Get_IP;

   -- Says if a dotted address is public or not:
   function Kind(Address: String) return Address_type is 
   begin

      if Address(Address'First .. Address'First + 2) = "10." then
         return Internal;
      elsif Address(Address'First .. Address'First + 6) = "192.168" then
         return Internal;
      elsif Address(Address'First .. Address'First + 2) = "172" then
         declare
            Num : Natural := Natural'Value (
               Address (
                  Address'First + 4 .. 
                  Strings.Pos (Address, ".", Address'First + 4) - 1));
         begin
            if Num >= 16 and then Num <= 31 then
               return Internal;
            else
               return Public;
            end if;
         end;
      elsif Address(Address'First .. Address'First + 2) = "127" and then
         Address (Address'First .. Address'First + 8) = "127.0.0.1" then
         return Local;
      elsif Address(Address'First .. Address'First + 2) = "127" then
         return Internal;
      end if;

      return Public;
   end Kind;

   -- Says if is public:
   function Is_public (Address : in String) return Boolean is
   begin
      return Kind (Address) = Public;
   end Is_Public;

end Adagio.Socket.IP;
