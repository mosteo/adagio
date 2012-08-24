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
--  $Id: adagio-network_settings.ads,v 1.3 2004/01/21 21:05:38 Jano Exp $

-- This package contains configuration settings about network topology.

package Adagio.Network_settings is

   pragma Elaborate_body;

   Accept_public_addresses  : Boolean := true;
   Accept_private_addresses : Boolean := false;

   type Routings is (Direct, NatForward, Nat, None);

   Internet_route : Routings := Nat;
   
   ------------------------------------------------------------------------
   -- Get_NATF_Address                                                   --
   ------------------------------------------------------------------------
   -- Returns the public IP visible in the out world, which others can use to
   -- be port forwarded to us. It is initially 127.0.0.1 (hence invalid!!)
   -- until it is detected somehow and set with the Set_... function.
   -- In our case, our IP is obtained from the handshake with any server.
   function Get_NATF_Address return String;
   pragma Inline (Get_NATF_Address);
   
   ------------------------------------------------------------------------
   -- Set_NATF_Address                                                   --
   ------------------------------------------------------------------------
   -- Should be in dotted format
   procedure Set_NATF_Address (Address : in String);
   pragma Inline (Set_NATF_Address);

   ------------------------------------------------------------------------
   -- Get_Reachable_IP                                                   --
   ------------------------------------------------------------------------
   -- Get reachable IP, public, NATF or internal in this order of preference:
   function Get_Reachable_IP return String;


end Adagio.Network_settings;

