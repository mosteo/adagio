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
--  $Id: aenea-hub.ads,v 1.11 2004/03/22 07:14:55 Jano Exp $

with Aenea.Countries;
with Aenea.Gui.Events;
with Aenea.Net;
with Aenea.Hub.Create_Event;
--  with Aenea.Persistent;
with Aenea.Signals;
with Aenea.Trace;

with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.G2.Packet.Queue;
with Adagio.Socket;

with Agpl.Event_Queues.Calendar;

with Ada.Calendar;
use  Ada;

package body Aenea.Hub.Actions is

   ------------------------------------------------------------------------
   -- Address                                                            --
   ------------------------------------------------------------------------
   -- Return a socket address
   function Address (This : in Object) return Adagio.Socket.Sock_addr_type
   is
   begin
      return Adagio.Socket.To_address (Id (This));
   end Address;

   ------------------------------------------------------------------------
   -- Ask                                                                --
   ------------------------------------------------------------------------
   -- Chooses either to make a PI or CRAWLA and calls Ping/Query appropriately.
   procedure Ask_Now (This : in out Object) is
      Kind : Signals.Ask_Kinds;
   begin
      if This.Pings >= Globals.Options.Walk_Pings then
         This.Pings := 0;
         Kind       := Signals.CRAWLR;
      else
         Kind       := Signals.PI;
      end if;

      case Kind is
         when Signals.CRAWLR =>
            Query (This);
         when Signals.PI     =>
            Ping (This);
      end case;
   end Ask_Now;

   procedure Ask_Later (This : in out Object; After : in Duration := 0.0) is
   begin
      Create_Event (This, (Kind => Signals.Ask), After);
   end Ask_Later;

   ------------------------------------------------------------------------
   -- Cancel                                                             --
   ------------------------------------------------------------------------
   -- Cancels last programmed event
   procedure Cancel (This : in out Object) is
   begin
      Agpl.Event_Queues.Calendar.Cancel (Net.Events, This.Event);
   end Cancel;

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   -- Counts a new hub freshly found
   procedure Count (This : in out Object; Signal : in Signals.Object) is
   begin
      -- Assignments
      This.Leaves     := Signal.Leaves;
      This.Max_Leaves := Signal.Max_Leaves;
      This.Nick       := Signal.Nick;
      This.Vendor     := Signal.Vendor;
      This.Version    := Signal.Version;
      This.Failures   := 0;
      Hub.Relocate (This);           -- Compute country data.

      -- Countings
      Net.Counter.Add_Hubs (1);
      Countries.Sum_hub (Hub.Get_country_code (This));
      Hub.By_Vendor.Sum_Key  (S (This.Vendor), 1);
      Hub.By_Version.Sum_Key (S (This.Version), 1);
      Hub.Add_To_Uptime (This, 1);
      Hub.Add_To_By_Leaves (This, This.Leaves);
      Net.Counter.Add_Leaves (
         Leaves     => This.Leaves,
         Max_leaves => This.Max_Leaves,
         Multiplier => Hub.Compute_Multiplier (S (This.Vendor)));

      -- Stats
      This.Stat_Answers          := This.Stat_Answers + 1;
      This.Stat_Answers_In_A_Row := 1;
      This.Stat_First_Seen       := Calendar.Clock;
      This.Stat_Last_Seen        := Calendar.Clock;
   end Count;

   ------------------------------------------------------------------------
   -- Discount                                                           --
   ------------------------------------------------------------------------
   -- Discounts a hub which has been gone
   procedure Discount (This : in out Object) is
   begin
      -- Countings
      Net.Counter.Add_Hubs (-1);
      Countries.Sum_hub (Hub.Get_country_code (This), -1);
      Hub.By_Vendor.Sum_Key  (S (This.Vendor), -1);
      Hub.By_Version.Sum_Key (S (This.Version), -1);
      Hub.Add_To_Uptime (This, -1);
      Hub.Del_To_By_Leaves (This);
      Net.Counter.Add_Leaves (
         Leaves     => -This.Leaves,
         Max_leaves => -This.Max_Leaves,
         Multiplier => Hub.Compute_Multiplier (S (This.Vendor)));
   end Discount;

   ------------------------------------------------------------------------
   -- Expire                                                             --
   ------------------------------------------------------------------------
   -- Will trigger an expiration signal after the given time
   procedure Expire (
      This  : in out Object;
      After : in Duration := Globals.Options.Walk_RefreshUnit)
   is
   begin
      Create_Event (
         This,
         (Kind => Signals.Expire),
         After);
   end Expire;

   ------------------------------------------------------------------------
   -- Fail                                                               --
   ------------------------------------------------------------------------
   -- A hub being queried has failed his deadline
   procedure Fail (This : in out Object) is
   begin
      This.Failures := This.Failures + 1;
   end Fail;

   ------------------------------------------------------------------------
   -- Maintenance                                                        --
   ------------------------------------------------------------------------
   -- Will trigger a maintenance signal after the given time
   procedure Maintenance (
      This  : in out Object;
      After : in Duration := Globals.Options.Walk_RefreshUnit)
   is
   begin
      Create_Event (
         This,
         (Kind => Signals.Maintenance),
         After);
   end Maintenance;

   ------------------------------------------------------------------------
   -- Ping                                                               --
   ------------------------------------------------------------------------
   -- Launchs a ping
   procedure Ping (This : in out Object) is
      P : G2.Packet.Object := G2.Packet.Create ("PI");
   begin
      Gui.Events.Add ("[>] PI     - " & Id (This), 1);
      Trace.Log ("PINGING " & Id (This) & Stats (This), Trace.Never);

      This.Stat_pings     := This.Stat_pings + 1;
      This.Pings          := This.Pings      + 1;

      G2.Packet.Queue.Send (
         Net.Outgoing,
         P,
         Address (This),
         Safe => Globals.Options.Walk_SafePackets);
   end Ping;

   ------------------------------------------------------------------------
   -- Query                                                              --
   ------------------------------------------------------------------------
   -- Launchs a query. If another was pending, returns without sending.
   Anonymous : constant Ustring := U ("Anonymous");
   procedure Query (This : in out Object) is
      P : G2.Packet.Object := G2.Packet.Create ("CRAWLR");
      C : G2.Packet.Object;
   begin
      Gui.Events.Add ("[>] CRAWLR - " & Id (This), 1);
      Trace.Log ("QUERYING " & Id (This) & Stats (This), Trace.Never);

      This.Stat_queries := This.Stat_queries + 1;

      -- Request extra info if not known already:
      if This.Version = U (Version_UNKN) or else
         This.Vendor  = U (Vendor_UNKN)
      then
         C := G2.Packet.Create ("REXT");
         G2.Packet.Add_Child (P, C);
      end if;

      if This.Nick = Anonymous then
         C := G2.Packet.Create ("RNAME");
         G2.Packet.Add_Child (P, C);
      end if;

      if Globals.Options.Walk_RequestLeaves then
         C := G2.Packet.Create ("RLEAF");
         G2.Packet.Add_Child (P, C);
      end if;

      if false then
         Adagio.G2.Packet.Parsing.Trace_Tree (P, Trace.Warning);
      end if;

      G2.Packet.Queue.Send (
         Net.Outgoing,
         P,
         Address (This),
         Safe => Globals.Options.Walk_SafePackets);
   end Query;

   ------------------------------------------------------------------------
   -- Timeout                                                            --
   ------------------------------------------------------------------------
   procedure Timeout (
      This  : in out Object;
      After : in Duration := Globals.Options.Walk_Timeout)
   is
   begin
      Create_Event (
         This,
         (Kind => Signals.Timeout),
         After);
   end Timeout;

   ------------------------------------------------------------------------
   -- Update                                                             --
   ------------------------------------------------------------------------
   -- Updates the data about a known hub which has just answered again
   procedure Update (This : in out Object; Answer : in Signals.Object) is
      use type Calendar.Time;
      Now : Calendar.Time := Calendar.Clock;
   begin
      case Answer.Answer_Kind is
         when Signals.CRAWLA =>
            -- Countings
            if This.Vendor /= Answer.Vendor then
               Hub.By_Vendor.Sum_Key  (S (This.Vendor),  -1);
               Hub.By_Vendor.Sum_Key  (S (Answer.Vendor), 1);
            end if;
            if This.Version /= Answer.Version then
               Hub.By_Version.Sum_Key (S (This.Version),  -1);
               Hub.By_Version.Sum_Key (S (Answer.Version), 1);
            end if;

            Hub.Update_Uptime    (This);
            Hub.Update_By_Leaves (This, Answer.Leaves);

            Net.Counter.Add_Leaves (
               Leaves     => -This.Leaves,
               Max_leaves => -This.Max_Leaves,
               Multiplier => Hub.Compute_Multiplier (S (This.Vendor)));
            Net.Counter.Add_Leaves (
               Leaves     => Answer.Leaves,
               Max_leaves => Answer.Max_Leaves,
               Multiplier => Hub.Compute_Multiplier (S (Answer.Vendor)));

            -- Assignments
            This.Leaves     := Answer.Leaves;
            This.Max_Leaves := Answer.Max_Leaves;
            This.Nick       := Answer.Nick;
            This.Vendor     := Answer.Vendor;
            This.Version    := Answer.Version;
            This.Failures   := 0;

            -- Stats
            This.Stat_Answers          := This.Stat_Answers          + 1;
            This.Stat_Answers_In_A_Row := This.Stat_Answers_In_A_Row + 1;
            This.Stat_Last_Seen        := Calendar.Clock;

            -- Data to be kept:
            -- if Now - This.Stat_First_Seen > Persistent.Longest_Uptime_Time then
            --   Persistent.Longest_Uptime_Time := Now - This.Stat_First_Seen;
            --   Persistent.Longest_Uptime_Nick := This.Nick;
            --   Persistent.Save;
            -- end if;

         when Signals.PO =>

            -- Assignments
            Hub.Update_Uptime    (This);
            This.Failures   := 0;

            -- Stats
            This.Stat_Answers          := This.Stat_Answers          + 1;
            This.Stat_Answers_In_A_Row := This.Stat_Answers_In_A_Row + 1;
            This.Stat_Last_Seen        := Calendar.Clock;
      end case;
   end Update;

end Aenea.Hub.Actions;
