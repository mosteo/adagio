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
--  $Id: adagio-event_log.adb,v 1.3 2004/02/24 15:26:12 Jano Exp $

with Agpl.Calendar.Format;

with Charles.Lists.Double.Unbounded;

package body Adagio.Event_log is

   package Traffic_lists is new Charles.Lists.Double.Unbounded (
      Object, "=");
   use Traffic_lists;

   Msgs    : Container_type;

   Pending : Natural := 0;

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
      while Length (Msgs) >= Max_traffic loop
         Delete_last (Msgs);
      end loop;
      Prepend (Msgs, This);
      Pending := Pending + 1;
   end Add;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      Clear (Msgs);
   end Clear;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      I    : Iterator_type := First (Msgs);
      Pos  : Positive      := 1;
   begin
      while I /= Back (Msgs) loop
         declare
            Row : Data_row;
            Now : constant Calendar.Time := Calendar.Clock;
            Q   : Object renames Element (I);
         begin
            -- Arrival
            Append (Row, (
               U (Agpl.Calendar.Format.Hour (Q.Arrival)),
               Rpad (Float (Now - Q.Arrival), 10)));
            -- Level
            Append (Row, (
               U (Levels'Image (Q.Level)), U (Levels'Image (Q.Level))));
            -- Text
            Append (Row, (Q.Text, Q.Text));
            -- Read
            Append (Row, (
               U (Boolean'Image (Pos <= Pending)), Null_ustring));

            Append (Data, Row);
            I := Succ (I);
         end;
         Pos := Pos + 1;
      end loop;
      Pending := 0;
   end Http_report;
   end Safe;

   ------------------------------------------------------------------------
   -- New_events                                                         --
   ------------------------------------------------------------------------
   -- Says how many new events are there since last check.
   function New_events return Natural is
   begin
      return Pending;
   end New_events;

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

end Adagio.Event_log;
