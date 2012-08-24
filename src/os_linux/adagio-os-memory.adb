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
--  $Id: adagio-os-memory.adb,v 1.3 2004/01/21 21:05:39 Jano Exp $

with Agpl.Strings;
with Agpl.Strings.Fields;

with Helpers;

with Text_IO;

package body Adagio.Os.Memory is

   ------------------------------------------------------------------------
   -- Heap_usage                                                         --
   ------------------------------------------------------------------------
   -- Returns the heap memory in use in bytes
   -- This thread pid is OK, since all threads share the memory info (?)
   function Heap_usage return Natural is
      Pid : constant Integer := Helpers.Get_Pid;
      use Text_IO;
      File : File_Type;
      Line : String (1 .. 256);
      Last : Natural;
   begin
      Open (File, Name => "/proc/" & Agpl.Strings.To_String (Pid) & "/statm", Mode => In_file);
      Get_Line (File, Line, Last);
      Close (File);

      -- The second number in /proc/<pid>/statm gives the resident memory size
      return 1024 * Natural'Value (Agpl.Strings.Fields.Select_Field (Line (1 .. Last), 2));
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         return 0;
   end Heap_usage;

end Adagio.Os.Memory;
