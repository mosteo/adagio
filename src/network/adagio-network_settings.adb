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
--  $Id: adagio-network_settings.adb,v 1.3 2004/01/21 21:05:38 Jano Exp $

-- This package contains configuration settings about network topology.

with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Misc;
with Adagio.Socket.IP;
with Adagio.Trace;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Adagio.Network_settings is

   NATF_Address : Ustring := U ("127.0.0.1");

   ------------------------------------------------------------------------
   -- Get_NATF_Address                                                   --
   ------------------------------------------------------------------------
   -- Returns the public IP visible in the out world, which others can use to
   -- be port forwarded to us. It is initially 127.0.0.1 (hence invalid!!)
   -- until it is detected somehow and set with the Set_... function.
   -- In our case, our IP is obtained from the handshake with any server.
   function Get_NATF_Address return String is
   begin
      return S (NATF_Address);
   end Get_NATF_Address;
   
   ------------------------------------------------------------------------
   -- Set_NATF_Address                                                   --
   ------------------------------------------------------------------------
   -- Should be in dotted format
   procedure Set_NATF_Address (Address : in String) is
   begin
      NATF_Address := U (Address);
   end Set_NATF_Address;

   ------------------------------------------------------------------------
   -- Get_Reachable_IP                                                   --
   ------------------------------------------------------------------------
   -- Get reachable IP, public, NATF or internal in this order of preference:
   function Get_Reachable_IP return String is
   begin
      case Internet_Route is
         when Direct =>
            return Socket.IP.Get_IP (Public => true);
         when NatForward =>
            return Get_NATF_Address;
         when others =>
            return Socket.IP.Get_IP (Public => false);
      end case;
   end Get_Reachable_IP;

begin
   -- Obtain the routing:
   declare
      Route : constant String := S (Globals.Options.Network_InternetRoute);
   begin
      Internet_route := Routings'Value (Route);

      -- Ensure that things are right:
      if Internet_route = Direct then
         begin
            declare
               S : String := Socket.IP.Get_IP (Public => true);
            begin
               Trace.Log ("Network settings: Our public IP is " & S);
            end;
         exception
            when others =>
               -- No public IP found, downgrading to NAT:
               Internet_route := Nat;
               Trace.Log ("Network_settings: Direct connection was specified"
                  & " but no public IP found, switching to NAT.", 
                  Trace.Warning);
         end;
      elsif Internet_route = Nat or else Internet_route = NatForward then
         begin
            declare
               S : String := Socket.IP.Get_IP (Public => true);
            begin
               Internet_route := Direct;
               Trace.Log ("Network_settings: NAT connection was specified " &
               "but a public IP has been found, switching to direct.", 
               Trace.Warning);
            end;
         exception
            when others =>
               null; -- Right, there were no public IP
         end;
      end if;
      
      Trace.Log ("Network_settings: Internet route = " & 
         Routings'Image (Internet_route));
   end;

   -- Obtain valid addresses:
   declare
      Addresses : constant String := Misc.To_lower (S (
         Globals.Options.Network_ValidAddresses));
   begin
      Accept_public_addresses  := Addresses = "all" or Addresses = "public";
      Accept_private_addresses := Addresses = "all" or Addresses = "private";
      Trace.Log ("Network_settings: Valid addresses = " & Addresses);
   end;

end Adagio.Network_settings;

