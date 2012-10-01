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
--  $Id: adagio-os-memory.adb,v 1.3 2004/01/21 21:05:40 Jano Exp $

-- Package with OS dependent functions.


with Interfaces;
with Interfaces.C;
with System;

with Win32;
use  Win32;
with Win32.Winbase;
with Win32.Winerror;
with Win32.Winnt;

package body Adagio.Os.Memory is

   ------------------------------------------------------------------------
   -- LocalHeapSize                                                      --
   ------------------------------------------------------------------------
   function LocalHeapSize (Heap : in Win32.Winnt.HANDLE) return Natural is
      use Interfaces;
      use Interfaces.C;
      use Win32.Winbase;
      use Win32.Winerror;
      use Win32.Winnt;

      he     : aliased PROCESS_HEAP_ENTRY;
      Size   : Natural := 0;
      Blocks : Natural := 0;
   begin
      he.lpData := System.Null_address;
      loop
         if HeapWalk (Heap, he'unchecked_access) = 0 then
            if GetLastError = ERROR_NO_MORE_ITEMS then
               exit;
            else
               raise Walk_error;
            end if;
         end if;
         Blocks := Blocks + 1;
         if (Unsigned_32 (he.wFlags) and
            Unsigned_32 (PROCESS_HEAP_UNCOMMITTED_RANGE)) = 0
         then
            Size := Size + Natural (he.cbData);
--            Trace.Log ("UNCOMMITTED SIZE" & he.cbData'Img);
         end if;
      end loop;

      return Size;
   end LocalHeapSize;

   ------------------------------------------------------------------------
   -- MemorySize                                                         --
   ------------------------------------------------------------------------
   function MemorySize return Natural is
      use Win32.Winbase;
      use Win32.Winnt;

      heaps  : array (1 .. 100) of aliased HANDLE;
      nHeaps : DWORD;
      cSize  : Natural := 0;
   begin
      nHeaps :=
         GetProcessHeaps (heaps'length, heaps (heaps'first)'Unchecked_access);

      if Natural (nHeaps) = heaps'Last then
         raise Constraint_error;
      end if;

      for N in 1 .. Natural (nHeaps) loop
         cSize := cSize + LocalHeapSize (heaps (N));
      end loop;

      return cSize;
   end MemorySize;

   ------------------------------------------------------------------------
   -- Heap_usage                                                         --
   ------------------------------------------------------------------------
   -- Returns the heap memory in use in bytes
   function Heap_usage return Natural is
   begin
      return MemorySize;
   end Heap_usage;

end Adagio.Os.Memory;
