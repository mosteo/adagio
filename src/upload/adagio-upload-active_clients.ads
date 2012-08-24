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
--  $Id: adagio-upload-active_clients.ads,v 1.3 2004/01/21 21:05:49 Jano Exp $

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Sets.Sorted.Unbounded;

package Adagio.Upload.Active_clients is

   pragma Elaborate_Body;

   type Active_client is record
      Id       : Ustring;
      Queue_id : Ustring;
   end record;

   function Less (L, R : Active_client) return Boolean;
   function Same (L, R : Active_client) return Boolean;

   package Active_client_list is new Charles.Sets.Sorted.Unbounded (
      Active_client, Less, Same);

   protected List is
      -- Add an upload if the clients is present less than times:
      -- Also, no repeated queue_ids uploading in different queues:
      procedure Add (
         This : in Active_client; Max : in Natural; Success : out Boolean);

      -- Remove if present
      procedure Remove (This : in Active_client);

      -- Expects a QUEUE_ID
      function Contains (Queue_id : in String) return Boolean;

   private

      Uploads : Active_client_list.Container_type;

   end List;

end Adagio.Upload.Active_clients;


















