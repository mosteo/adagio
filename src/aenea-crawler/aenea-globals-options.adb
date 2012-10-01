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
--  $Id: aenea-globals-options.adb,v 1.13 2004/03/22 07:14:55 Jano Exp $

--  Centralized facilities for all configuration options.
--  These variables are all initialized during elaboration from the xml file.

with Adagio.Globals.Options;
with Adagio.Os;
with Adagio.Trace;
with Adagio.Xml.Utils;

with Aenea.Misc;
with Aenea.Trace;
with Aenea.Xml;

with Ada.Command_Line;

package body Aenea.Globals.Options is

   package AGO renames Adagio.Globals.Options;

   -- Shortcuts
   function L (S : in String) return String renames Misc.To_lower;
   function Attr (
      Path, Name : in String;
      Node       : in Xml.Node;
      Def        : in String;
      Pos        : in Natural := 1;
      Unique     : in Boolean := true) return String
      renames Xml.Get_attribute;
   function Attr (Path, Name : in String; Node : in Xml.Node; Def : in String)
      return UString is
      R : constant String := Attr (Path, Name, Node, Def);
   begin
      return U (R);
   end Attr;

   ------------------------------------------------------------------------
   -- Check_Command_Line                                                 --
   ------------------------------------------------------------------------
   -- Inspects for specific parameters to be overruled
   procedure Check_Command_Line is
      package Cl renames Ada.Command_Line;
   begin
      -- Database connect string
      for I in 1 .. Cl.Argument_Count loop
         if Cl.Argument (I) = "-pgsql" then
            db_pgsql_ConnectString := U (Cl.Argument (I + 1));
         end if;
      end loop;
   end Check_Command_Line;

   ------------------------------------------------------------------------
   -- Set_Options                                                        --
   ------------------------------------------------------------------------
   procedure Set_options is
      package XUtils renames Adagio.Xml.Utils;
      Yes : constant String := "yes";
      No  : constant String := "no";
      function S (B : in Boolean) return String is
      begin
         if B then
            return "yes";
         else
            return "no";
         end if;
      end S;
   begin

      -- GLOBAL
      Globals_GeoIP                 := U (Attr (
         "globals", "GeoIP", Config, S (Globals_GeoIP)));
      Globals_Shutdown_Active       := L (Attr ("globals/shutdown",
         "active", Config, S (Globals_Shutdown_Active))) = Yes;
      Globals_Shutdown_Deadline     := XUtils.Get_duration (
         "globals/shutdown", "deadline", Config, Globals_Shutdown_Deadline);
      Globals_Shutdown_OnDisconnect       := L (Attr ("globals/shutdown",
         "OnDisconnect", Config, S (Globals_Shutdown_OnDisconnect))) = Yes;

      -- Database
      db_pgsql_ConnectString        := U (Attr (
         "db/pgsql", "ConnectString", Config, S (db_pgsql_ConnectString)));
      Db_Histogram_Grain            := XUtils.Get_num (
         "db", "HistogramGrain", Config, Db_Histogram_Grain);

      -- WALK
      walk_RefreshUnit              := XUtils.Get_duration (
         "walk", "RefreshUnit", Config, walk_RefreshUnit);
      walk_pings                    := XUtils.Get_num (
         "walk", "pings", Config, walk_pings);
      walk_delay                    := XUtils.Get_duration (
         "walk", "delay", Config, walk_delay);
      walk_timeout                  := XUtils.Get_duration (
         "walk", "timeout", Config, walk_timeout);
      walk_InsertDelay              := XUtils.Get_duration (
         "walk", "InsertDelay", Config, walk_InsertDelay);
      walk_InsertPeriod             := XUtils.Get_duration (
         "walk", "InsertPeriod", Config, walk_InsertPeriod);
      walk_InsertAbortPeriod        := XUtils.Get_duration (
         "walk", "InsertAbortPeriod", Config, walk_InsertAbortPeriod);
      walk_SavePeriod               := XUtils.Get_duration (
         "walk", "SavePeriod", Config, walk_SavePeriod);
      walk_SaveHubs                 := L (Attr ("walk",
         "SaveHubs", Config, S (walk_SaveHubs))) = Yes;
      walk_BestUptimesPeriod        := XUtils.Get_duration (
         "walk", "BestUptimesPeriod", Config, walk_BestUptimesPeriod);
      walk_AveragingSamples         := XUtils.Get_num (
         "walk", "AveragingSamples", Config, walk_AveragingSamples);
      walk_Failures                 := XUtils.Get_num (
         "walk", "failures", Config, walk_failures);
      walk_purge_active             := L (Attr ("walk/purge",
         "active", Config, S (walk_purge_active))) = Yes;
      walk_purge_period             := XUtils.Get_duration (
         "walk/purge", "period", Config, walk_purge_period);
      walk_purge_age                := XUtils.Get_duration (
         "walk/purge", "age", Config, walk_purge_age);
      walk_MaxOutboundPendingPackets := XUtils.Get_num
        ("walk", "MaxOutboundPendingPackets", Config, walk_MaxOutboundPendingPackets);
      walk_RequestLeaves             := L (Attr ("walk",
         "RequestLeaves", Config, S (walk_RequestLeaves))) = Yes;
      walk_SafePackets             := L (Attr ("walk",
         "SafePackets", Config, S (walk_SafePackets))) = Yes;

      -- TOPOGRAM
      topo_MinimumCount             := XUtils.Get_num (
         "topogram", "MinimumCount", Config, topo_MinimumCount);
      topo_type                     := U (Attr (
         "topogram", "type", Config, S (topo_type)));
      topo_width                    := XUtils.Get_num (
         "topogram", "width", Config, topo_MinimumCount);
      topo_height                   := XUtils.Get_num (
         "topogram", "height", Config, topo_MinimumCount);

      topo_radial_LevelDelta        := XUtils.Get_float (
         "topogram/radial", "LevelDelta", Config, topo_radial_LevelDelta);

      topo_topdown_LevelDelta       := XUtils.Get_float (
         "topogram/topdown", "LevelDelta", Config, topo_topdown_LevelDelta);
      topo_topdown_Increment        := XUtils.Get_float (
         "topogram/topdown", "increment", Config, topo_topdown_Increment);
      topo_topdown_BranchLength     := XUtils.Get_float (
         "topogram/topdown", "BranchLength",Config,topo_topdown_BranchLength);

      topo_tree_HSpace              := XUtils.Get_float (
         "topogram/tree", "HSpace", Config, topo_tree_HSpace);
      topo_tree_VSpace              := XUtils.Get_float (
         "topogram/tree", "VSpace", Config, topo_tree_VSpace);
      topo_tree_pile                := XUtils.Get_boolean (
         "topogram/tree", "pile", Config, topo_tree_pile);

      topo_dynamic_MaxIterations    := XUtils.Get_num ("topogram/dynamic",
         "MaxIterations", Config, topo_dynamic_MaxIterations);
      topo_dynamic_convergence      := XUtils.Get_float ("topogram/dynamic",
         "convergence", Config, topo_dynamic_convergence);
      topo_dynamic_MassFactor       := XUtils.Get_float ("topogram/dynamic",
         "MassFactor", Config, topo_dynamic_MassFactor);
      topo_dynamic_LinkForce        := XUtils.Get_float ("topogram/dynamic",
         "LinkForce", Config, topo_dynamic_LinkForce);
      topo_dynamic_HubForce         := XUtils.Get_float ("topogram/dynamic",
         "HubForce", Config, topo_dynamic_HubForce);
      topo_dynamic_CoreForce        := XUtils.Get_float ("topogram/dynamic",
         "CoreForce", Config, topo_dynamic_CoreForce);
      topo_dynamic_LinkPow          := XUtils.Get_float ("topogram/dynamic",
         "LinkPow", Config, topo_dynamic_LinkPow);
      topo_dynamic_HubPow         := XUtils.Get_float ("topogram/dynamic",
         "HubPow", Config, topo_dynamic_HubPow);
      topo_dynamic_CorePow        := XUtils.Get_float ("topogram/dynamic",
         "CorePow", Config, topo_dynamic_CorePow);
      topo_dynamic_MaxForce       := XUtils.Get_float ("topogram/dynamic",
         "MaxForce", Config, topo_dynamic_MaxForce);
      topo_UmbralNew              := XUtils.Get_duration ("topogram/dynamic",
         "UmbralNew", Config, topo_UmbralNew);

      -- G2
      AGO.G2_port                   := XUtils.Get_num (
         "network/Gnutella2", "port", Config, AGO.G2_port);

      -- GWC2
      gwc2_BootstrapHubs            := XUtils.Get_num (
         "network/GWebCache2", "BootstrapHubs", Config, gwc2_BootstrapHubs);

      -- GUI
      gui_HideAddresses             := L (Attr ("gui",
         "HideAddresses", Config, S (gui_HideAddresses))) = Yes;
      gui_HtmlRoot                  := U (Attr ("gui",
         "HtmlRoot", Config, S (gui_HtmlRoot)));
      gui_HtmlStyle                 := U (Attr ("gui",
         "HtmlStyle", Config, S (gui_HtmlStyle)));

      -- DEBUG
      AGO.debug_active              := L (Attr (
         "debug", "active", Config, S (AGO.debug_active))) = Yes;
      AGO.debug_logfile             := Attr (
         "debug", "logfile", Config, S (AGO.debug_logfile));
      AGO.debug_netlogfile          := Attr (
         "debug", "netlogfile", Config, S (AGO.debug_netlogfile));
      AGO.debug_loglevel            := U (L (Attr (
         "debug", "loglevel", Config, S (AGO.debug_loglevel))));
      AGO.debug_ConsoleEcho         := L (Attr (
         "debug", "ConsoleEcho", Config, S (AGO.debug_ConsoleEcho))) = Yes;
      AGO.debug_PurgeOnStartup      := L (Attr ("debug",
         "PurgeOnStartup", Config, S (AGO.debug_PurgeOnStartup))) = Yes;

      Adagio.Trace.Check_changed_level;

   end Set_options;

begin

   Set_options;

   Check_Command_Line;

exception
   when E : others =>
      Adagio.Os.Message_Box ("", "Globals.Options: " & Trace.Report (E));
end Aenea.Globals.Options;
