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
--  $Id: adagio-upload-queue-create_from_xml.adb,v 1.3 2004/01/21 21:05:50 Jano Exp $

-- Creation of a queue from Xml node

With
Adagio.Misc,
Adagio.Upload.Client,
Adagio.Upload.Ratings,
Adagio.XML.Utils;

Use
Adagio,
Adagio.Upload.Client,
Adagio.Upload.Ratings;

procedure Adagio.Upload.Queue.Create_from_xml (
   This  : out Queue.Object;
   Node  : in  Xml.Node;
   Ratio : in  Float) is

   Preempt : Preemptions;
   Order   : Orderings;
begin

   if Xml.Get_attribute ("preemption", "active", Node, "no") = "no" then
      Preempt.Kind := None;
   else
      Preempt.Time :=
         Xml.Utils.Get_duration ("preemption", "time", Node, 0.0);
      Preempt.Size :=
         Xml.Utils.Get_size ("preemption", "size", Node, 0);
      if Preempt.Time /= 0.0 and Preempt.Size /= 0 then
         Preempt.Kind := Both;
      elsif Preempt.Time /= 0.0 then
         Preempt.Kind := Time;
      elsif Preempt.Size /= 0 then
         Preempt.Kind := Size;
      else
         Preempt.Kind := None;
      end if;
   end if;

   Order.Kind := Order_types'Value (Xml.Get_attribute (Node, "type", "fifo"));
   Order.Expression := U (Misc.To_lower (
      Xml.Get_attribute ("type", "expression", Node, "Position")));

   -- Ensure valid expression
   if Order.Kind = rated and then
      not Check_syntax (S (Order.Expression))
   then
      return;
   end if;

   This.Create (
      This'Unrestricted_access,
      Name      => Xml.Get_attribute (Node, "name", ""),
      Length    => Xml.Utils.Get_num (Node, "length", 64),
      Uploads   => Xml.Utils.Get_num (Node, "ActiveClients", 2),
      Bandwidth => File_size (Float (Xml.Utils.Get_speed ("uploads",
                     "bandwidth", Globals.Config, File_size'Last)) * Ratio),
      Minimum_speed => Xml.Utils.Get_speed (Node, "MinimumClientSpeed", 0),
      Average_period => Xml.Utils.Get_duration (Node, "AveragePeriod", 60.0),
      Criteria   => Xml.Get_attribute ("criteria", "is", Node, "true"),
      Preemptive => Preempt,
      Ordering   => Order,
      Base_folder => S (Globals.Data_folder)
      );


end Adagio.Upload.Queue.Create_from_xml;










