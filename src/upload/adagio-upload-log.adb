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
--  $Id: adagio-upload-log.adb,v 1.1 2004/02/24 15:26:14 Jano Exp $

with Agpl.Calendar.Format;

with Templates_parser;

package body Adagio.Upload.Log is

   Yes_no : constant array (Boolean) of Ustring := 
      (true => U ("YES"), false => U ("NO"));

   procedure Generate (
      This : in  Object;
      Anew : in  Boolean;
      Row  : out Agpl.Http.Server.Sort_handler.Data_row)
   is
      use Agpl.Http.Server.Sort_handler;
      use Templates_parser;
      use type Calendar.Time;
      Now : Calendar.Time := Calendar.Clock;
   begin
      -- Filename
      Append (Row, (This.Filename, This.Filename));
      -- Client
      Append (Row, (This.Client, This.Client));
      -- Hour
      Append (Row, (
         U (Agpl.Calendar.Format.Hour (This.Last_seen)),
         Rpad (Float (Now - This.Last_seen), 10)));
      -- Address
      Append (Row, (This.Address, This.Address));
      -- Code
      Append (Row, (This.Code, This.Code));
      -- Country
      Append (Row, (This.Country, This.Country));
      -- New
      Append (Row, (Yes_no (Anew), Yes_no (Anew)));
      -- Queue
      Append (Row, (This.Queue, This.Queue));
   end Generate;

end Adagio.Upload.Log;
