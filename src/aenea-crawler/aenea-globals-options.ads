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
--  $Id: aenea-globals-options.ads,v 1.14 2004/03/22 07:14:55 Jano Exp $

--  Centralized facilities for all configuration options.
--  These variables are all initialized during elaboration from the xml file.

package Aenea.Globals.Options is

   pragma Elaborate_body;

   ------------------------------------------------------------------------
   -- Set_options                                                        --
   ------------------------------------------------------------------------
   -- Apply the options in the configuration file
   procedure Set_options;

   -- All variables here will respect case in the XML file.
   -- String values will be lower cased.

   -- GLOBAL
   Globals_GeoIP                             : Ustring      := U ("geoip.csv");
   Globals_Shutdown_Active                   : Boolean      := true;
   Globals_Shutdown_Deadline                 : Duration     := 22.0 * 60.0 * 60.0;
   Globals_Shutdown_OnDisconnect             : Boolean      := True;

   -- WALK
   walk_RefreshUnit                          : Duration     := 300.0;
   walk_pings                                : Natural      := 0;
   walk_delay                                : Duration     := 0.10;
   walk_timeout                              : Duration     := 150.0;
   walk_InsertDelay                          : Duration     := 180.0;
   walk_InsertPeriod                         : Duration     := 60.0;
   walk_InsertAbortPeriod                    : Duration     := 60.0;
   walk_SavePeriod                           : Duration     := 60.0 * 60.0;
   walk_SaveHubs                             : Boolean      := true;
   walk_BestUptimesPeriod                    : Duration     := 59.0;
   walk_AveragingSamples                     : Natural      := 60;
   walk_failures                             : Natural      := 1;
   walk_purge_active                         : Boolean      := true;
   walk_purge_period                         : Duration     := 15.0 * 60.0;
   walk_purge_age                            : Duration     := 6.0 * 60.0;
   walk_MaxOutboundPendingPackets            : Natural      := 128;
   walk_RequestLeaves                        : Boolean      := false;
   walk_SafePackets                          : Boolean      := false;

   -- DB
   db_pgsql_ConnectString                    : Ustring      := U ("address=127.0.0.1 port=5432 dbname=g2");
   Db_Histogram_Grain                        : Natural      := 20;

   -- TOPOGRAM
   topo_MinimumCount                         : Natural      := 10;
   topo_type                                 : Ustring      := U ("radial");
   topo_width                                : Natural      := 800;
   topo_height                               : Natural      := 600;
   topo_UmbralNew                            : Duration     := 5.0 * 60.0;

   topo_radial_LevelDelta                    : Float        := 0.99;

   topo_topdown_BranchLength                 : Float        := 100.0;
   topo_topdown_Increment                    : Float        := 0.2618;
   topo_topdown_LevelDelta                   : Float        := 1.01;

   topo_tree_HSpace                          : Float        := 20.0;
   topo_tree_VSpace                          : Float        := 30.0;
   topo_tree_pile                            : Boolean      := false;

   topo_dynamic_MaxIterations                : Positive     := 1000;
   topo_dynamic_convergence                  : Float        := 1.0;
   topo_dynamic_MassFactor                   : Float        := 0.0025;
   topo_dynamic_LinkForce                    : Float        := 1.0;
   topo_dynamic_HubForce                     : Float        := 2.0;
   topo_dynamic_CoreForce                    : Float        := 10.0;
   topo_dynamic_LinkPow                      : Float        := 2.0;
   topo_dynamic_HubPow                       : Float        := 2.0;
   topo_dynamic_CorePow                      : Float        := 2.0;
   topo_dynamic_MaxForce                     : Float        := 100.0;

   -- GWC2
   gwc2_BootstrapHubs                        : Natural      := 10;

   -- GUI
   gui_HideAddresses                         : Boolean      := true;
   gui_HtmlRoot                              : Ustring      := U ("");
   gui_HtmlStyle                             : Ustring    := U ("estilo.css");

end Aenea.Globals.Options;
