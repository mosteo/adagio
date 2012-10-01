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
--  $Id: protected_queue.adb,v 1.3 2004/01/21 21:05:44 Jano Exp $

with Charles.Lists.Double.Unbounded;

package body Protected_queue is

   protected body Object is

      procedure Put (Item : in Item_type) is
      begin
         Queue.Push_back (Data, Item);
      end Put;

      entry Get (Item : out Item_type) when not Queue.Is_empty (Data) is
      begin
         Item := Queue.Element (Queue.First (Data));
         Queue.Pop_front (Data);
      end;

      procedure Peek (Item : out Item_type; Success : out Boolean) is
      begin
         Success := Queue.Is_empty (Data);
         if Success then
            Item := Queue.Element (Queue.First (Data));
         end if;
      end Peek;

      function Is_empty return Boolean is
      begin
         return Queue.Is_empty (Data);
      end Is_empty;

      procedure Clear is
      begin
         Queue.Clear (Data);
      end Clear;

   end Object;

end Protected_queue;
