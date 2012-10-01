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
--  $Id: adagio-upload-active_clients.adb,v 1.3 2004/01/21 21:05:49 Jano Exp $

package body Adagio.Upload.Active_clients is

   function Less (L, R : Active_client) return Boolean is
   begin
      return S (L.Queue_id) < S (R.Queue_id);
   end Less;

   function Same (L, R : Active_client) return Boolean is
   begin
      return S (L.Queue_id) = S (R.Queue_id);
   end Same;

   use Active_client_list;

   protected body List is

      function Count (Client_id : in String) return Natural is
         Pos : Iterator_type := First (Uploads);
         Num : Natural       := 0;
      begin
         while Pos /= Back (Uploads) loop
            if S (Element (Pos).Id) = Client_id then 
               Num := Num + 1;
            end if;
            Pos := Succ (Pos);
         end loop;

         return Num;
      end Count;
   
      -- Add an upload if the clients is present less than times:
      procedure Add (
         This : in Active_client; Max : in Natural; Success : out Boolean) is
      begin

         if Contains (S (This.Queue_id)) then
            Success := false;
         elsif Count (S (This.Id)) < Max then
            Success := true;
            Insert (Uploads, This);
         else
            Success := false;
         end if;

      end Add;

      -- Remove if present
      procedure Remove (This : in Active_client) is
      begin
         Delete (Uploads, This);
      end Remove;

      function Contains (Queue_id : in String) return Boolean is
         Pos : Iterator_type := First (Uploads);
      begin
         while Pos /= Back (Uploads) loop
            if S (Element (Pos).Queue_Id) = Queue_id then 
               return true;
            end if;
            Pos := Succ (Pos);
         end loop;

         return false;
      end Contains;

   end List;

end Adagio.Upload.Active_clients;


















