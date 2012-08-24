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
--  $Id: adagio-os-socket.adb,v 1.3 2004/01/21 21:05:39 Jano Exp $

-- Package with OS dependent functions.

with Adagio.Trace;
with Strings.Fields;

package body Adagio.Os.Socket is

   ------------------------------------------------------------------------
   -- Get_error                                                          --
   ------------------------------------------------------------------------
   function Get_error (E : Exception_occurrence) return Errors is
      use Strings.Fields;
      Text : String  := Exception_message (E);
   begin
      declare
         Code : Integer := Integer'Value (
            Select_field (Select_field (Text, 2, '[') , 1, ']'));
      begin
         case Code is
            when 115  => 
	    	return Operation_would_block;
            when 9 => 
	    	return Socket_is_not_connected;
            when others => 
         	Trace.Log ("Os.Socket: " & Text, Trace.Warning);
	    	return Unknown_error;
         end case;
      end;
   exception
      when E : others =>
         Trace.Log ("Os.Socket.Get_error: " & Trace.Report (E), Trace.Error);
         return Unknown_error;
   end Get_error;

end Adagio.Os.Socket;