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
--  $Id: adagio-security.ads,v 1.4 2004/02/29 20:36:45 Jano Exp $

with Adagio.Ip_address;

with Agpl.Geoip;

package Adagio.Security is

   ------------------
   -- Add_ban_rule --
   ------------------
   -- Dotted format
   procedure Add_ban_rule (Address : in String; Mask : in String);

   ----------------
   -- Is_allowed --
   ----------------
   function Is_allowed (Address : in Ip_address.Inet_addr_type) 
      return Boolean;

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (Address : in Ip_address.Inet_addr_type) 
      return Boolean;

   type User_agent_ban_type is (Substring, Regexp);

   -------------------
   -- Add_ban_agent --
   -------------------
   -- Can raise:
   Syntax_error : exception;
   procedure Add_ban_agent (Agent : in String; Kind : in User_agent_ban_type);

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (Agent : in String) return Boolean;

   ---------------------
   -- Add_country_ban --
   ---------------------
   procedure Add_country_ban (
      Country : in Agpl.Geoip.Country_code; Allow : in Boolean := false);

   -----------------------
   -- Is_country_banned --
   -----------------------
   function Is_country_banned (
      Country : in Agpl.Geoip.Country_code) return Boolean;

end Adagio.Security;
