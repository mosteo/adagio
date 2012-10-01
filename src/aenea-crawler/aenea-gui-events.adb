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
--  $Id: aenea-gui-events.adb,v 1.1 2004/03/03 00:06:05 Jano Exp $

with Agpl.Calendar.Format;

package body Aenea.Gui.Events is

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   procedure Add (Text : in String; Level : in Positive) is
   begin
      Records.Add ((
         Timestamp => Ada.Calendar.Clock,
         Level     => Levels'Val (Levels'Pos (Levels'First) + Level - 1),
         Text      => U (Text)));
   end Add;

   ------------------------------------------------------------------------
   -- Trace                                                              --
   ------------------------------------------------------------------------
   procedure Trace (Text : in String; Level : in Positive) is
   begin
      Traces.Add ((
         Timestamp => Ada.Calendar.Clock,
         Level     => Levels'Val (Levels'Pos (Levels'First) + Level - 1),
         Text      => U (Text)));
   end Trace;

   procedure Generate_row (
      This   : in  Event_type;
      Is_new : in  Boolean;
      Row    : out Agpl.Http.Server.Sort_handler.Data_row)
   is
      use Agpl.Http.Server.Sort_handler;
      use Ada.Calendar;
      Now : constant Time := Clock;
   begin
      -- Timestamp
      Append (Row, (
         U (Agpl.Calendar.Format.Hour (This.Timestamp)),
         RPad (Float (Now - This.Timestamp), 15)));
      -- Level
      Append (Row, (
         U (Levels'Image (This.Level)),
         U (Levels'Image (This.Level))));
      -- Text
      Append (Row, (This.Text, This.Text));
      -- New
      Append (Row, (U (Is_new'Img), U (Is_new'Img)));
   end Generate_row;

end Aenea.Gui.Events;
