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
--  $Id: adalid-guid.ads,v 1.4 2004/01/21 21:05:24 Jano Exp $

package Adalid.GUID is

   type GUID is new String (1 .. 16);

   -- Used as identifier inside G2.
   My_GUID      : Adalid.GUID.GUID;

   -- Obtain a new random GUID
   function Create_GUID return Adalid.GUID.GUID;

   -- Return it as a series of 16 chars.
   function To_char_array (this : Adalid.GUID.GUID) return String;

   -- Returns the hex representation of the raw GUID.
   function To_hex (this : Adalid.GUID.GUID) return String;

   -- Returns the standard string representation of a GUID
   -- {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxx}
   function To_string (this : in Adalid.GUID.GUID) return String;

   -- Converts a standard GUID string representation to a guid
   function To_GUID (this : in String) return Adalid.GUID.GUID;

private

   pragma Inline (To_char_array);

   procedure Init;

end Adalid.GUID;
