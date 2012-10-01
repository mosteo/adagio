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
--  networks Adagio connects with. All Row collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: aenea-hub-report.adb,v 1.2 2004/01/26 20:47:15 Jano Exp $

with Aenea.Globals.Options;

with Agpl.Calendar.Format;
use  Agpl.Calendar.Format;
with Agpl.Strings;

with Ada.Calendar;
use  Ada;

procedure Aenea.Hub.Report (
   This : in Object; 
   Row  : out Agpl.Http.Server.Sort_handler.Data_row)
is
   use Agpl.Http.Server.Sort_handler;
   use type Ada.Calendar.Time;
   use ASU;
begin
   -- Address + Nick
   if Globals.Options.Gui_HideAddresses then
      Append (Row,
         (Value       => "<hidden> - " & This.Nick,
          Order_value => "<hidden> - " & This.Nick));
   else
      Append (Row,
         (Value       => This.Address & " - " & This.Nick,
          Order_value => This.Address & " - " & This.Nick));
   end if;
   -- Leaves
   Append (Row,
      (Value       => U (This.Leaves'Img),
       Order_value => U (Integer'Image (Integer'Last - This.Leaves))));
   -- Msgs
   Append (Row,
      (Value       => U (Integer'Image (This.Stat_queries + This.Stat_pings)),
       Order_value => U (Integer'Image (
          Integer'Last - This.Stat_queries + This.Stat_pings))));
   -- Answers
   Append (Row,
      (Value       => U (This.Stat_answers'Img),
       Order_value => U (Integer'Image (Integer'Last - This.Stat_answers))));
   -- Answers in a row
   Append (Row,
      (Value       => U (This.Stat_answers_in_a_row'Img),
       Order_value => U (Integer'Image (
          Integer'Last -This.Stat_answers_in_a_row))));
   -- First seen
   Append (Row,
      (Value       => U (Image (Ada.Calendar.Clock - This.Stat_First_seen)),
       Order_value => U (Image (Ada.Calendar.Clock - This.Stat_First_seen))));
   -- Last seen
   Append (Row,
      (Value       => U (Image (Ada.Calendar.Clock - This.Stat_Last_seen)),
       Order_value => U (Image (Ada.Calendar.Clock - This.Stat_Last_seen))));
   -- Address
   if Globals.Options.Gui_HideAddresses then
      Append (Row,
         (Value       => U ("<hidden>"),
          Order_value => U ("<hidden>")));
   else
      Append (Row,
         (Value       => This.Address,
          Order_value => This.Address));
   end if;
   -- Nick
   Append (Row,
      (Value       => This.Nick,
       Order_value => This.Nick));
   -- Failures
   Append (Row,
      (Value       => U (This.Failures'Img),
       Order_value => U (Integer'Image (Integer'Last - This.Failures))));
   -- Country name
   if This.Country_code /= "??" then
      Append (Row,
         (Value       => This.Country_name,
          Order_value => This.Country_name));
   else
      Append (Row,
         (Value       => This.Country_name,
          Order_value => U ("ZZZZZZZZ")));
   end if;
   -- Country code; change ?? to unknown
   if This.Country_code /= "??" then
      Append (Row,
         (Value       => U (This.Country_code),
          Order_value => U (This.Country_code)));
   else
      Append (Row,
         (Value       => U ("Unknown"),
          Order_value => U ("ZZZZZZZ")));
   end if;
   -- Vendor
   Append (Row,
      (Value       => This.Vendor,
       Order_value => This.Vendor));
   -- Version
   Append (Row,
      (Value       => This.Version,
       Order_value => This.Version));
   -- Status
   Append (Row,
      (Value       => U (This.Status'Img),
       Order_value => U (This.Status'Img)));
end Aenea.Hub.Report;
