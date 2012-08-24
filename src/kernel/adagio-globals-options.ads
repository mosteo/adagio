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
--  $Id: adagio-globals-options.ads,v 1.19 2004/03/29 19:13:32 Jano Exp $

--  Centralized facilities for all configuration options.
--  These variables are all initialized during elaboration from the xml file.

with Adagio.Os.Memory;
with Adagio.Types; use Adagio.Types;

package Adagio.Globals.Options is

   pragma Elaborate_body;

   use type Types.File_Size;

   ------------------------------------------------------------------------
   -- Set_options                                                        --
   ------------------------------------------------------------------------
   -- Apply currently loaded config file.
   procedure Set_options;

   -- All variables here will respect case in the XML file.
   -- String values will be lower cased.

   -- GLOBALS
   globals_maxCPU                   : Natural      := 80;
   globals_watchdog_deadline        : Duration     := 45.0;
   globals_TimeUnit                 : Duration     := 0.5;
   globals_DataFolder               : Ustring      := U ("data");
   globals_GeoIP                    : Ustring      := U ("geoip.csv");

   -- GUI
   gui_active                       : Boolean      := true;
   gui_address                      : Ustring      := U ("127.0.0.1");
   gui_port                         : Natural      := 24444;
   gui_LookAndFeel                  : Ustring      := U ("java");
   gui_HtmlRoot                     : Ustring      := U ("html/");
   gui_HtmlStyle                    : Ustring      := U ("estilo.css");
   gui_LinesPerPage                 : Positive     := 1000;
   gui_user                         : Ustring      := U ("lhalkjhelnr");
   gui_pass                         : Ustring      := U ("djfsu42k343");
   gui_GraphPeriod                  : Duration     := 60.0 * 60.0;
   gui_listeners                    : Positive     := 1;
   gui_StackSize                    : Natural      :=
      Os.Memory.Max_stack_size;

   -- LIBRARY
   library_AllowBrowse              : Boolean      := false;
   library_DelayedStartup           : Boolean      := true;
   library_FolderAddingInterval     : Duration     := 0.1;
   library_FileAddingInterval       : Duration     := 0.1;
   library_AutosaveInterval         : Duration     := 300.0;
   library_MaxPendingSearches       : Natural      := 100;
   library_MaxSearches              : Natural      := 10;
   library_SearchTimeout            : Duration     := 3.0;
   library_QRTSize                  : Positive     := 20;
   library_TTHSize                  : Positive     := 10;
   library_HashingCPUusage          : Positive     := 85;
   library_HashSpeed                : Ustring      := U ("slow");
   library_mesh_active              : Boolean      := true;
   library_mesh_sources             : Natural      := 100;
   library_mesh_TTL                 : Duration     := 24.0 * 60.0 * 60.0;
   library_mesh_CleanupPeriod       : Duration     := 60.0;

   -- SECURITY
   security_policy_allow            : Boolean      := true;

   -- UPLOADS
   uploads_SafeQueues               : Boolean      := false;
   uploads_AutosavePeriod           : Duration     := 300.0;
   uploads_MaxPerClient             : Natural      := 1;
   uploads_MaxUnknown               : Natural      := 100;
   uploads_bandwidth                : Speed        := Speed'Last;
   uploads_ShareBandwidth           : Boolean      := true;
   uploads_throttle                 : Float        := 0.75;
   uploads_MinimumSendDelay         : Duration     := 0.2;
   uploads_SendBufferSize           : Positive     := 64 * 1024;
   uploads_RememberClientPeriod     : Duration     := 3600.0;
   uploads_QueuePollWindow          : Duration     := 75.0;
   uploads_RequestDeadline          : Duration     := 60.0;

   -- NETWORK
   network_ConnectionsPerSecond     : Positive     := 10;
   network_BufferSize               : Positive     := 16 * 1024;
   network_proxy                    : Ustring      := U ("");
   network_InternetRoute            : Ustring      := U ("direct");
   network_ValidAddresses           : Ustring      := U ("all");
   network_MaxHandshakeSize         : Positive     := 1024;
   network_ThrottleHandshakes       : Boolean      := false;

   -- GNUTELLA2
   G2_HandshakeAsk                  : Ustring      :=
      U ("GNUTELLA CONNECT/0.6");
   G2_HandshakeAnswer               : Ustring      := U ("GNUTELLA/0.6");
   G2_connect                       : Boolean      := true;
   G2_port                          : Natural      := 24610;
   G2_PollPeriod                    : Duration     := 1.0;
   G2_PollConnection                : Duration     := 0.5;
   G2_SendPeriod                    : Duration     := 0.2;
   G2_QRTUpdatePeriod               : Duration     := 15.0 * 60.0;
   G2_QRTDelay                      : Duration     := 30.0;
   G2_ConnectTimeout                : Duration     := 15.0;
   G2_HandshakeTimeout              : Duration     := 45.0;
   G2_retries                       : Natural      := 3;
   G2_RestPeriod                    : Duration     := 300.0;
   G2_CachedServers                 : Natural      := 1000;
   G2_ConfidencePeriod              : Duration     := 2.0 * 3600.0;
   G2_MaximumAge                    : Duration     := 3600.0;
   G2_PingDelay                     : Duration     := 60.0;
   G2_PingTimeout                   : Duration     := 300.0;
   G2_ServerInfoDelay               : Duration     := 180.0;
   G2_ActiveServers                 : Natural      := 2;
   G2_TryServers                    : Natural      := 30;
   G2_LinkBandwidth                 : Speed        := Speed'Last;
   G2_UdpBandwidthIn                : Speed        := Speed'Last;
   G2_UdpBandwidthOut               : Speed        := 16 * 1024;
   G2_UdpBuffers                    : Natural      := 256;
   G2_UdpOutboundTimeout            : Duration     := 60.0;
   G2_MaxPacketQueueLength          : Natural      := 1024;
   G2_CompressedLink                : Boolean      := true;
   G2_AltLocations                  : Natural      := 10;
   G2_MaxHeaders                    : Positive     := 4096;

   -- GWEBCACHE2
   GWC2_connect                     : Boolean      := false;
   GWC2_CachedServers               : Natural      := 1000;
   GWC2_ConnectTimeout              : Duration     := 10.0;
   GWC2_AnswerTimeout               : Duration     := 10.0;
   GWC2_WaitPeriod                  : Duration     := 15.0 * 60.0;
   GWC2_LocalTest                   : Boolean      := false;
   GWC2_LocalTest_port              : Natural      := 36765;

   -- HOSTCACHE
   hostcache_purge                  : Boolean      := true;
   hostcache_purge_period           : Duration     := 30.0;
   hostcache_save                   : Boolean      := true;
   hostcache_save_period            : Duration     := 60.0;

   -- CHAT
   chat_log                         : Boolean      := true;
   chat_logfile                     : Ustring      := U ("log/chat.log");
   chat_enabled                     : Boolean      := true;
   chat_answer                      : Ustring      := U ("Away");
   chat_AwayMessage                 : Ustring      := U (
      "Hello. This is an away bot, nobody here. My master is using " &
      "Adagio server, you can check it at http://agio.sourceforge.net");

   -- SEARCH
   G2_search_priorities_high           : Natural      := 10;
   G2_search_priorities_medium         : Natural      := 20;
   G2_search_priorities_low            : Natural      := 40;
   G2_search_priorities_medium_threshold : Natural    := 5;
   G2_search_priorities_low_threshold    : Natural    := 20;
   G2_search_priorities_stop_threshold   : Natural    := 100;
   G2_search_WatchdogPeriod            : Duration     := 15.0;
   G2_search_MinimumRequeryWait        : Duration     := 30.0 * 60.0;
   G2_search_HubTimeout                : Duration     :=  5.0 * 60.0;
   G2_search_HubRestPeriod             : Duration     := 10.0 * 60.0;
   G2_search_KeyDuration               : Duration     := 24.0 * 60.0 * 60.0;
   G2_search_SendingThrottle           : Duration     := 0.2;
   G2_search_PurgePeriod               : Duration     :=  1.0 * 60.0;
   G2_search_PurgeAge                  : Duration     :=
      Duration'Max (30.0 * 60.0, G2_search_HubTimeout);
   G2_search_MaxHitsPerPacket          : Positive     := 64;
   G2_search_SaveHits                  : Boolean      := true;
   G2_search_SaveHitsDelay             : Duration     := 30.0;
   G2_search_MinimumLeaves             : Natural      := 10;
   G2_search_HubGrowingPeriod          : Duration     := 60.0 * 60.0;

   -- DOWNLOAD
   download_active                  : Boolean      := true;
   download_incomplete              : Ustring      := U ("incomplete/");
   download_finished                : Ustring      := U ("finished/");
   download_bandwidth               : Speed        := Speed'Last;

   -- G2 specific
   G2_download_ChunkSize               : Types.File_Size := 64 * 1024;
   G2_download_deflate                 : Boolean      := true;
   G2_download_MaxActive               : Positive     := 32;
   G2_download_MaxAlive                : Positive     := 1024;
   G2_download_MaxBackoffPeriod        : Duration     := 60.0 * 24.0;
   G2_download_MaxFailures             : Natural      := 10;
   G2_download_ForgetFailedSources     : Boolean      := true;

   -- DEBUG
   debug_active                     : Boolean      := true;
   debug_logfile                    : Ustring      := U ("log/adagio.log");
   debug_netlogfile                 : Ustring      := U ("log/network.log");
   debug_loglevel                   : Ustring      := U ("informative");
   debug_ConsoleEcho                : Boolean      := false;
   debug_DebugStatistics            : Boolean      := false;
   debug_PurgeOnStartup             : Boolean      := true;
   debug_heartbeat                  : Duration     := 0.9;
   debug_MemoryPeriod               : Duration     := 60.0;
   debug_CrawlerAllowed             : Boolean      := true;

private

   ------------------------------------------------------------------------
   -- Set_Web_Stats                                                      --
   ------------------------------------------------------------------------
   -- procedure Set_Web_Stats;

end Adagio.Globals.Options;
