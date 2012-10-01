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

with Ada.Exceptions; use Ada;


with GNAT.Sockets.Constants;

with GNAT.Sockets.Linker_Options;
pragma Warnings (Off, GNAT.Sockets.Linker_Options);
--  Need to include pragma Linker_Options which is platform dependent.

package body GNAT.Sockets.Extra is

   Empty_Set : Socket_Set_Type;

   -----------------------
   -- Check_Read_socket --
   -----------------------
   -- True if socket is alive (readable even without data available)
   function Check_Read_Socket (Socket : in Socket_Type) return Boolean is
      RSet : Socket_Set_Type;
      Sel  : Selector_Type;
      Status : Selector_Status;
   begin
      Set (RSet, Socket);
      Create_Selector (Sel);

      begin
         Check_Selector (Sel, Rset, Empty_Set, Empty_Set, Status, Immediate);
         return Status in Completed | Expired;
      exception
         when others =>
            return False;
      end;
   end Check_Read_Socket;

   ------------------------
   -- Check_Write_socket --
   ------------------------
   -- True if the socket can be written without blocking on it
   function Check_Write_Socket (Socket : in Socket_Type) return Boolean is
      WSet : Socket_Set_Type;
      Sel  : Selector_Type;
      Status : Selector_Status;
   begin
      Set (WSet, Socket);
      Create_Selector (Sel);

      begin
         Check_Selector (Sel, Empty_Set, WSet, Empty_Set, Status, Immediate);
         return Status in Completed;
      exception
         when others =>
            return False;
      end;
   end Check_Write_Socket;

end Gnat.Sockets.Extra;
