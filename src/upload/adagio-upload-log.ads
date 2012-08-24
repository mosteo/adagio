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
--  $Id: adagio-upload-log.ads,v 1.1 2004/02/24 15:26:14 Jano Exp $

with Agpl.Http.Server.Sort_handler;
with Agpl.Http.Server.Sort_handler.Simple_list;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar;
use  Ada;

package Adagio.Upload.Log is

   type Object is record
      Filename  : Ustring;
      Client    : Ustring;
      Last_seen : Calendar.Time;
      Address   : Ustring;
      Code      : Ustring;
      Country   : Ustring;
      Queue     : Ustring;
   end record;

   procedure Generate (
      This : in  Object;
      Anew : in  Boolean;
      Row  : out Agpl.Http.Server.Sort_handler.Data_row);

   package Records is new Agpl.Http.Server.Sort_handler.Simple_list (
      Object,
      Generate);

end Adagio.Upload.Log;