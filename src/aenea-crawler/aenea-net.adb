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
--  $Id: aenea-net.adb,v 1.5 2004/01/26 20:47:15 Jano Exp $

with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver_Types;
with Adagio.Globals.Options;
with Adagio.Socket;
with Adagio.Types;                  use Adagio.Types;

with Agpl.Chronos;
use Agpl;

package body Aenea.Net is

   Overtime : constant Duration := 3.0;

   function "/" (L : Natural; R : Integer) return Float is
   begin
      return Float (L) / Float (R);
   end "/";

   -- UDP socket
   UDPS : aliased Adagio.Socket.Object;

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Do all necessary net initialization
   procedure Init is
   begin
      -- Create listener socket
      Adagio.Socket.Create_datagram (UDPS);
      Adagio.Socket.Bind (UDPS, "0.0.0.0", Adagio.Globals.Options.G2_port);
      -- Transceiver
      UDPT := new Adagio.G2.Transceiver.Object;
      Adagio.G2.Transceiver.Start (UDPT.all, UDPS'Access, Incoming'Access);
      --  Queue limits
      --  Set these in the config file!
--      G2.Packet.Queue.Max_pending_packets := Natural'Last;
--      G2.Transceiver_Types.Max_packets    := Natural'Last;

      -- Unlimited outbound bandwidth (throttling is done elsewhere)
      G2.Transceiver.Set_BW_Limits (UDPT.all, Speed'Last, Speed'Last);
   end Init;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Event_queue.Shutdown (Events);
      Adagio.G2.Transceiver.Shutdown (UDPT.all);
      Adagio.Socket.Close (UDPS);
   end Shutdown;

   ------------------------------------------------------------------------
   -- Counter                                                            --
   ------------------------------------------------------------------------
   -- Encapsulates host counts
   protected body Counter is

      --------------
      -- Add_Hubs --
      --------------
      procedure Add_Hubs (Hubs : Integer) is
      begin
         Counter.Hubs       := Counter.Hubs      + Hubs;
         Counter.Est_Total  := Counter.Est_Total + Float (Hubs);
      end Add_Hubs;

      ----------------
      -- Add_Leaves --
      ----------------
      procedure Add_Leaves (Leaves : Integer; Max_Leaves : Integer; Multiplier : Float) is
      begin
         Counter.Leaves     := Counter.Leaves     + Leaves;
         Counter.Est_Total  := Counter.Est_Total  + Float (Leaves) * Multiplier;
         Counter.Max_leaves := Counter.Max_leaves + Max_leaves;
      end Add_Leaves;

      ----------------
      -- Add_Unique --
      ----------------
      procedure Add_Unique (Address : in String) is
         use String_Node_Maps;
      begin
         Include (Uniques, Address, (Last_Seen => Clock));
      end Add_Unique;

      ----------
      -- Hubs --
      ----------
      function Hubs_count return Natural is
      begin
         return Hubs;
      end Hubs_count;

      ------------
      -- Leaves --
      ------------
      function Leaves_count return Natural is
      begin
         return Leaves;
      end Leaves_count;

      ------------------
      -- Unique_Count --
      ------------------
      function Unique_Count return Natural is
      begin
         return Natural (String_Node_Maps.Length (Uniques));
      end Unique_Count;

      -----------
      -- Total --
      -----------
      function Total_Count return Natural is
      begin
         return Natural (Est_Total);
      end Total_Count;

      ----------------
      -- Max leaves --
      ----------------
      function Max_leaves_count return Natural is
      begin
         return Max_leaves;
      end Max_leaves_count;

      ---------------
      -- Push_hubs --
      ---------------
      procedure Push_hubs (Hubs : Natural) is
      begin
         Avg.Push (Avg_hubsc, Hubs);
      end Push_hubs;
      function Avg_hubs return Natural is
      begin
         return Natural (Avg.Average (Avg_hubsc));
      end Avg_hubs;

      -----------------
      -- Push_leaves --
      -----------------
      procedure Push_leaves (Leaves : Natural) is
      begin
         Avg.Push (Avg_leavesc, Leaves);
      end Push_leaves;
      function Avg_leaves return Natural is
      begin
         return Natural (Avg.Average (Avg_leavesc));
      end Avg_leaves;

      ----------------
      -- Push_total --
      ----------------
      procedure Push_Total (Total : Natural) is
      begin
         Avg.Push (Avg_totalc, Total);
      end Push_Total;
      function  Avg_Total return Natural is
      begin
         return Natural (Avg.Average (Avg_totalc));
      end Avg_Total;

      -----------------
      -- Push_Unique --
      -----------------
      procedure Push_Unique (Unique : Natural) is
      begin
         Avg.Push (Avg_uniques, Unique);
      end Push_Unique;
      function Avg_Unique return Natural is
      begin
         return Natural (Avg.Average (Avg_uniques));
      end Avg_Unique;

      --------------------
      --  Purge_Uniques --
      --------------------
      procedure Purge_Uniques (Purged : out Natural;
                               Age    : in Duration := 10.0 * 60.0) is
         use String_Node_Maps;
         Cron: Chronos.Object;
         I   : Cursor := First (Uniques);
         J   : Cursor;
         Now : constant Time := Clock;
      begin
         Purged := 0;
         while I /= No_Element loop
            J := Next (I);
            if Now - Element (I).Last_Seen >= Age then
               Delete (Uniques, I);
               Purged := Purged + 1;
            end if;
            I := J;
         end loop;
          if Cron.Elapsed > Overtime then
            Trace.Log ("Net.Counter.Purge_Uniques abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         end if;
      end Purge_Uniques;

   end Counter;

   ------------------------------------------------------------------------
   -- Report_hubs                                                        --
   ------------------------------------------------------------------------
   -- Single translation
   -- SINGLE1 <-- Current hubs
   -- SINGLE2 <-- Averaged hubs
   function Report_hubs return Templates_parser.Translate_table is
      use Templates_parser;
      Avg : Natural;
   begin
      begin
         Avg := Counter.Avg_hubs;
      exception
         when others =>
            Avg := Counter.Hubs_count;
      end;
      return (
         Assoc ("SINGLE1", Counter.Hubs_count),
         Assoc ("SINGLE2", Avg));
   end Report_hubs;

end Aenea.Net;
