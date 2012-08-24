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
--  $Id: aenea-countries.ads,v 1.2 2004/02/04 21:31:02 Jano Exp $

--  Keep track of hubs in each country:

with Agpl.Geoip;
use  Agpl.Geoip;
with Agpl.Http.Server.Sort_handler;

with Templates_parser;

package Aenea.Countries is

   ------------------------------------------------------------------------
   -- Sum_hub                                                            --
   ------------------------------------------------------------------------
   -- Add or remove a hub from a country:
   -- Use Inc => -1 to remove a hub.
   procedure Sum_hub (Country : in Country_code; Inc : in Integer := 1);

   ------------------------------------------------------------------------
   -- Hubs_in_country                                                    --
   ------------------------------------------------------------------------
   function Hubs_in_country (Country : in Country_code) return Natural;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Generates the data for the html report.
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------------------------------------------------------------------
   -- Total_hubs                                                         --
   ------------------------------------------------------------------------
   -- SINGLE1 <-- num of hubs
   function Total_hubs return Templates_parser.Translate_table;

   -------------
   -- Iterate --
   -------------
   procedure Iterate
     (Process :   not null access procedure (Key   : in String;
                                             Count : in Integer));
   -- Process will be called for each key/count pair

end Aenea.Countries;
