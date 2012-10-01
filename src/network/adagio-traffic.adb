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
--  $Id: adagio-traffic.adb,v 1.4 2004/02/24 15:26:13 Jano Exp $

--with Adagio.Misc;

with Agpl.Calendar.Format;

with Charles.Lists.Double.Unbounded;

package body Adagio.Traffic is

   package Traffic_lists is new Charles.Lists.Double.Unbounded (
      Object, "=");
   use Traffic_lists;

   Packets : Container_type;

   Ways_desc : constant array (Ways) of Ustring :=
      (Incoming => U ("In"), Outgoing => U ("Out"));

   protected Safe is
   procedure Add (This : in Object);
   procedure Clear;
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
   end Safe;

   protected body Safe is
   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
   begin
      while Length (Packets) >= Max_traffic loop
         Delete_last (Packets);
      end loop;
      Prepend (Packets, This);
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Clear (Packets);
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      I    : Iterator_type := First (Packets);
   begin
      while I /= Back (Packets) loop
         declare
            Row : Data_row;
            Now : Calendar.Time := Calendar.Clock;
            Q   : Object renames Element (I);
         begin
            -- Arrival
            Append (Row, (
               U (Agpl.Calendar.Format.Hour (Q.Arrival)),
               Rpad (Float (Now - Q.Arrival), 10)));
            -- Protocol
            Append (Row, (Q.Protocol, Q.Protocol));
            -- Way
            Append (Row, (Ways_desc (Q.Way), Ways_desc (Q.Way)));
            -- Address
            Append (Row, (Q.From, Q.From));
            -- Name
            Append (Row, (Q.Name, Q.Name));
            -- Data
            Append (Row, (Q.Data, Q.Data));

            Append (Data, Row);
            I := Succ (I);
         end;
      end loop;
   end Http_report;
   end Safe;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (This : in Object) is
   begin
      Safe.Add (This);
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Safe.Clear;
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
   begin
      Safe.Http_report (Data);
   end Http_report;

end Adagio.Traffic;
