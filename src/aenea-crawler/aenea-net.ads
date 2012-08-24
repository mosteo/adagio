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
--  $Id: aenea-net.ads,v 1.5 2004/01/26 20:47:15 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Signals;
with Aenea.Trace;
with Aenea.Types;

with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver;

with Average_queue;

with Agpl.Event_Queues.Calendar;
with Agpl.Delayer;

with Templates_parser;

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;

package Aenea.Net is

   package Event_Queue renames Agpl.Event_Queues.Calendar;

   function "/" (L : Natural; R : Integer) return Float;
   package Avg is new Average_queue (Natural, "+", "/");

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   -- Do all necessary net initialization
   procedure Init;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown;

   ------------------------------------------------------------------------
   -- UDP Transceiver                                                    --
   ------------------------------------------------------------------------
   UDPT : Adagio.G2.Transceiver.Object_access;

   ------------------------------------------------------------------------
   -- Packet Queues                                                      --
   ------------------------------------------------------------------------
   Incoming : aliased G2.Packet.Queue.Object;
   Outgoing :         G2.Packet.Queue.Object;

   ------------------------------------------------------------------------
   -- Event queue                                                        --
   ------------------------------------------------------------------------
   Events : Event_queue.Object (
      Stack_Size => 1024 * 1024,
      Tracer     => Trace.General'Access);

   ------------------------------------------------------------------------
   -- Signal_Context                                                     --
   ------------------------------------------------------------------------
   -- Context used in events carrying an automaton signal.
   -- The sequence must match that of the receiving hub or it will be discarded
   -- This implies that any hub can only have a valid pending event at a time.
   type Signal_Context (Kind : Signals.Kinds) is new
   Agpl.Event_Queues.Context_Type with record
      Address  : Ustring;
      Sequence : Types.Sequences;
      Signal   : Signals.Object (Kind => Kind);
   end record;

   ------------------------------------------------------------------------
   -- Delayer                                                            --
   ------------------------------------------------------------------------
   Delayer : Agpl.Delayer.Object (
      Natural (Globals.Options.Walk_Delay * 1000.0));

   --  Types for the counter.
   type Unique_Node is record
      Last_Seen : Time;
   end record;

   package String_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Unique_Node, "<", "=");

   ------------------------------------------------------------------------
   -- Counter                                                            --
   ------------------------------------------------------------------------
   -- Encapsulates host counts
   protected Counter is

      --------------
      -- Add_Hubs --
      --------------
      procedure Add_Hubs (Hubs : Integer);

      ----------------
      -- Add_Leaves --
      ----------------
      procedure Add_Leaves (Leaves : Integer; Max_Leaves : Integer; Multiplier : Float);

      procedure Add_Unique (Address : in String);
      --  Just the payload address seen as string.

      ---------------
      -- Push_hubs --
      ---------------
      procedure Push_hubs (Hubs : Natural);
      function  Avg_hubs return Natural;

      -----------------
      -- Push_leaves --
      -----------------
      procedure Push_leaves (Leaves : Natural);
      function  Avg_leaves return Natural;

      ----------------
      -- Push_total --
      ----------------
      procedure Push_Total (Total : Natural);
      function  Avg_Total return Natural;

      -----------------
      -- Push_Unique --
      -----------------
      procedure Push_Unique (Unique : Natural);
      function Avg_Unique return Natural;

      ----------
      -- Hubs_count --
      ----------
      function Hubs_count return Natural;

      ------------
      -- Leaves_count --
      ------------
      function Leaves_count return Natural;

      -----------
      -- Total_count --
      -----------
      function Total_count return Natural;

      function Unique_Count return Natural;

      ----------------
      -- Max leaves --
      ----------------
      function Max_leaves_count return Natural;

      -------------------
      -- Purge_Uniques --
      -------------------
      procedure Purge_Uniques (Purged : out Natural ;
                               Age    : in  Duration := 10.0 * 60.0);
      --  Remove unique nodes not seen in this time.

   private
      Hubs       : Natural := 0;
      Leaves     : Natural := 0;
      Est_Total  : Float   := 0.0; -- Estimated total nodes
      Max_leaves : Natural := 0;

      --  Unique nodes:
      Uniques    : String_Node_Maps.Map;

      -- Averages computers:
      Avg_hubsc   : Avg.Object (Globals.Options.Walk_AveragingSamples);
      Avg_leavesc : Avg.Object (Globals.Options.Walk_AveragingSamples);
      Avg_totalc  : Avg.Object (Globals.Options.Walk_AveragingSamples);
      Avg_uniques : avg.Object (Globals.Options.Walk_AveragingSamples);
   end Counter;

   ------------------------------------------------------------------------
   -- Report_hubs                                                        --
   ------------------------------------------------------------------------
   -- Single translation
   -- SINGLE1 <-- Current hubs
   -- SINGLE2 <-- Averaged hubs
   function Report_hubs return Templates_parser.Translate_table;

end Aenea.Net;
