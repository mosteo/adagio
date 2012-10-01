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
--  $Id: adagio-globals-options.adb,v 1.22 2004/03/29 19:13:32 Jano Exp $

--  Centralized facilities for all configuration options.
--  These variables are all initialized during elaboration from the xml file.

with
Adagio.Misc,
-- Adagio.Statistics.Booleans,
-- Adagio.Statistics.Tpl,
Adagio.Xml,
Adagio.Xml.Utils,
-- Agpl.Folders,
Agpl.Filesystem;


package body Adagio.Globals.Options is

   package Folders renames Agpl.Filesystem;

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
      R : String := Attr (Path, Name, Node, Def);
   begin
      return U (R);
   end Attr;

   ------------------------------------------------------------------------
   -- Set_Options                                                        --
   ------------------------------------------------------------------------
   procedure Set_options is
      package XUtils renames Xml.Utils;
	Yes : constant String := "yes";
	No  : constant String := "no";

	Pragma Unreferenced( No );

      function S (B : in Boolean) return String is
      begin
         if B then
            return "yes";
         else
            return "no";
         end if;
      end S;
   begin
      -- GLOBALS
      globals_maxCPU            := XUtils.Get_num (
         "globals", "maxCPU", Config, globals_maxCPU);
      globals_watchdog_deadline := XUtils.Get_duration (
         "globals/watchdog", "deadline", Config, globals_watchdog_deadline);
      globals_TimeUnit          := XUtils.Get_duration (
         "globals", "TimeUnit", Config, globals_TimeUnit);
      globals_DataFolder        := Attr (
         "globals", "DataFolder", Config, S (globals_DataFolder));
      globals_GeoIP             := Attr (
         "globals", "GeoIP", Config, S (globals_GeoIP));

      -- GUI
      gui_active                := L (Attr (
         "gui", "active", Config, S (gui_active))) = Yes;
      gui_address               := Attr (
         "gui", "address", Config, S (gui_address));
      gui_port                  := XUtils.Get_num (
         "gui", "port", Config, gui_port);
      gui_LookAndFeel           := Attr (
         "gui", "LookAndFeel", Config, S (gui_LookAndFeel));
      gui_HtmlRoot              := U (Attr (
         "gui", "HtmlRoot", Config, S (gui_HtmlRoot)));
      gui_HtmlStyle             := U (Attr (
         "gui", "HtmlStyle", Config, S (gui_HtmlStyle)));
      gui_LinesPerPage          := XUtils.Get_num (
         "gui", "LinesPerPage", Config, gui_LinesPerPage);
      gui_user                  := U (Attr (
         "gui", "user", Config, S (gui_user)));
      gui_pass                  := U (Attr (
         "gui", "pass", Config, S (gui_pass)));
      gui_GraphPeriod           := XUtils.Get_duration (
         "gui", "GraphPeriod", Config, gui_GraphPeriod);
      gui_listeners             := XUtils.Get_num (
         "gui", "listeners", Config, gui_Listeners);
      gui_StackSize             := XUtils.Get_size (
         "gui", "StackSize", Config, gui_StackSize);

      -- LIBRARY
      library_AllowBrowse       := L (Attr (
         "library", "AllowBrowse", Config, S (library_AllowBrowse))) = Yes;
      library_DelayedStartup    := L (Attr ("library",
         "DelayedStartup", Config, S (library_DelayedStartup))) = Yes;
      library_FolderAddingInterval := XUtils.Get_duration ("library",
         "FolderAddingInterval", Config, library_FolderAddingInterval);
      library_FileAddingInterval   := XUtils.Get_duration ("library",
         "FileAddingInterval", Config, library_FileAddingInterval);
      library_AutosaveInterval     := XUtils.Get_duration ("library",
         "AutosaveInterval", Config, library_AutosaveInterval);
      library_MaxPendingSearches   := XUtils.Get_num ("library",
         "MaxPendingSearches", Config, library_MaxPendingSearches);
      library_MaxSearches       := XUtils.Get_num ("library",
         "MaxSearches", Config, library_MaxSearches);
      library_SearchTimeout     := XUtils.Get_duration (
         "library", "SearchTimeout", Config, library_SearchTimeout);
      library_QRTSize           := XUtils.Get_num (
         "library", "QRTSize", Config, library_QRTSize);
      library_TTHSize           := XUtils.Get_num (
         "library", "TTHSize", Config, library_TTHSize);
      library_HashingCPUusage   := XUtils.Get_num (
         "library", "HashingCPUusage", Config, library_HashingCPUusage);
      library_HashSpeed         := Attr (
         "library", "HashSpeed", Config, S (library_HashSpeed));
      library_mesh_active       := L (Attr (
         "library/mesh", "active", Config, S (library_mesh_active))) = Yes;
      library_mesh_sources      := XUtils.Get_num (
         "library/mesh", "sources", Config, library_mesh_sources);
      library_mesh_TTL          := XUtils.Get_duration (
         "library/mesh", "TTL", Config, library_mesh_TTL);
      library_mesh_CleanupPeriod    := XUtils.Get_duration ("library/mesh",
         "CleanupPeriod", Config, library_mesh_CleanupPeriod);

      -- SECURITY
      security_policy_allow     := L (Attr (
         "security/policy", "allow", Config, S (library_mesh_active))) = Yes;

      -- UPLOADS
      uploads_SafeQueues        := L (Attr (
         "uploads", "SafeQueues", Config, S (uploads_SafeQueues))) = Yes;
      uploads_AutosavePeriod    := XUtils.Get_duration (
         "uploads", "AutosavePeriod", Config, uploads_AutosavePeriod);
      uploads_MaxPerClient      := XUtils.Get_Num (
         "uploads", "MaxPerClient", Config, uploads_MaxPerClient);
      uploads_MaxUnknown        := XUtils.Get_num (
         "uploads", "MaxUnknown", Config, uploads_MaxUnknown);
      uploads_bandwidth         := XUtils.Get_speed (
         "uploads", "bandwidth", Config, uploads_bandwidth);
      uploads_ShareBandwidth    := L (Attr ("uploads",
         "ShareBandwidth", Config, S (uploads_ShareBandwidth))) = Yes;
      uploads_throttle          := Float'Value (Attr (
         "uploads", "throttle", Config, Float'Image (uploads_throttle)));
      uploads_MinimumSendDelay  := XUtils.Get_duration (
         "uploads", "MinimumSendDelay", Config, uploads_MinimumSendDelay);
      uploads_SendBufferSize    := XUtils.Get_num (
         "uploads", "SendBufferSize", Config, uploads_SendBufferSize);
      uploads_RememberClientPeriod := XUtils.Get_duration ("uploads",
         "RememberClientPeriod", Config, uploads_RememberClientPeriod);
      uploads_QueuePollWindow   := XUtils.Get_duration (
         "uploads", "QueuePollWindow", Config, uploads_QueuePollWindow);
      uploads_RequestDeadline   := XUtils.Get_duration (
         "uploads", "RequestDeadline", Config, uploads_RequestDeadline);

      -- NETWORK
      network_ConnectionsPerSecond := XUtils.Get_num ("network",
         "ConnectionsPerSecond", Config, network_ConnectionsPerSecond);
      network_BufferSize        := XUtils.Get_num (
         "network", "BufferSize", Config, network_BufferSize);
      network_proxy             := Attr (
         "network", "proxy", Config, S (network_proxy));
      network_InternetRoute     := Attr (
         "network", "InternetRoute", Config, S (network_InternetRoute));
      network_ValidAddresses    := Attr (
         "network", "ValidAddresses", Config, S (network_ValidAddresses));
      network_MaxHandshakeSize  := XUtils.Get_Size ("network",
         "MaxHandshakeSize", Config, network_MaxHandshakeSize);
      network_ThrottleHandshakes:= XUtils.Get_Boolean (
         "network", "ThrottleHandshakes", Config, network_ThrottleHandshakes);

      -- GNUTELLA2
      G2_HandshakeAsk           := Attr (
         "network/Gnutella2", "HandshakeAsk", Config, S (G2_HandshakeAsk));
      G2_HandshakeAnswer        := Attr ("network/Gnutella2",
         "HandshakeAnswer", Config, S (G2_HandshakeAnswer));
      G2_connect                := L (Attr ("network/Gnutella2",
         "connect", Config, S (G2_connect))) = Yes;
      G2_port                   := XUtils.Get_num (
         "network/Gnutella2", "port", Config, G2_port);
      G2_PollPeriod             := XUtils.Get_duration (
         "network/Gnutella2", "PollPeriod", Config, G2_PollPeriod);
      G2_PollConnection         := XUtils.Get_duration (
         "network/Gnutella2", "PollConnection", Config, G2_PollConnection);
      G2_SendPeriod             := XUtils.Get_duration (
         "network/Gnutella2", "SendPeriod", Config, G2_SendPeriod);
      G2_QRTUpdatePeriod        := XUtils.Get_duration ("network/Gnutella2",
         "QRTUpdatePeriod", Config, G2_QRTUpdatePeriod);
      G2_QRTDelay               := XUtils.Get_duration (
         "network/Gnutella2", "QRTDelay", Config, G2_QRTDelay);
      G2_ConnectTimeout         := XUtils.Get_duration ("network/Gnutella2",
         "ConnectTimeout", Config, G2_ConnectTimeout);
      G2_HandshakeTimeout       := XUtils.Get_duration ("network/Gnutella2",
         "HandshakeTimeout", Config, G2_HandshakeTimeout);
      G2_retries                := XUtils.Get_num (
         "network/Gnutella2", "retries", Config, G2_retries);
      G2_RestPeriod             := XUtils.Get_duration (
         "network/Gnutella2", "RestPeriod", Config, G2_RestPeriod);
      G2_CachedServers          := XUtils.Get_num (
         "network/Gnutella2", "CachedServers", Config, G2_CachedServers);
      G2_ConfidencePeriod       := XUtils.Get_duration ("network/Gnutella2",
         "ConfidencePeriod", Config, G2_ConfidencePeriod);
      G2_MaximumAge             := XUtils.Get_duration (
         "network/Gnutella2", "MaximumAge", Config, G2_MaximumAge);
      G2_PingDelay              := XUtils.Get_duration (
         "network/Gnutella2", "PingDelay", Config, G2_PingDelay);
      G2_PingTimeout            := XUtils.Get_duration (
         "network/Gnutella2", "PingTimeout", Config, G2_PingTimeout);
      G2_ServerInfoDelay        := XUtils.Get_duration (
         "network/Gnutella2", "ServerInfoDelay", Config, G2_ServerInfoDelay);
      G2_ActiveServers          := XUtils.Get_num (
         "network/Gnutella2", "ActiveServers", Config, G2_ActiveServers);
      G2_TryServers             := XUtils.Get_Num (
         "network/Gnutella2", "TryServers", Config, G2_TryServers);
      G2_LinkBandwidth          := XUtils.Get_speed ("network/Gnutella2",
         "LinkBandwidth", Config, G2_LinkBandwidth);
      G2_UdpBandwidthIn         := XUtils.Get_speed ("network/Gnutella2",
         "UdpBandwidthIn", Config, G2_UdpBandwidthIn);
      G2_UdpBandwidthOut        := XUtils.Get_speed ("network/Gnutella2",
         "UdpBandwidthOut", Config, G2_UdpBandwidthOut);
      G2_UdpOutboundTimeout     := XUtils.Get_duration ("network/Gnutella2",
         "UdpOutboundTimeout", Config, G2_UdpOutboundTimeout);
      G2_UdpBuffers             := XUtils.Get_num (
         "network/Gnutella2", "UdpBuffers", Config, G2_UdpBuffers);
      G2_MaxPacketQueueLength   := XUtils.Get_num (
         "network/Gnutella2", "MaxPacketQueueLength", Config, G2_MaxPacketQueueLength);
      G2_CompressedLink         := Xml.Get_attribute ("network/Gnutella2",
         "CompressedLink", Config, S (G2_CompressedLink)) = Yes;
      G2_AltLocations           := XUtils.Get_num (
         "network/Gnutella2", "AltLocations", Config, G2_AltLocations);
      G2_MaxHeaders             := XUtils.Get_num (
         "network/Gnutella2", "MaxHeaders", Config, G2_MaxHeaders);

      -- GWEBCACHE2
      GWC2_connect              := L (Attr ("network/GWebCache2",
         "connect", Config, S (GWC2_connect))) = Yes;
      GWC2_CachedServers        := XUtils.Get_num (
         "network/GWebCache2", "CachedServers", Config, GWC2_CachedServers);
      GWC2_ConnectTimeout       := XUtils.Get_duration ("network/GWebCache2",
         "ConnectTimeout", Config, GWC2_ConnectTimeout);
      GWC2_AnswerTimeout        := XUtils.Get_duration ("network/GWebCache2",
         "AnswerTimeout", Config, GWC2_AnswerTimeout);
      GWC2_WaitPeriod           := XUtils.Get_duration (
         "network/GWebCache2", "WaitPeriod", Config, GWC2_WaitPeriod);
      GWC2_LocalTest            := L (Attr ("network/GWebCache2",
         "LocalTest", Config, S (GWC2_LocalTest))) = Yes;
      GWC2_LocalTest_port       := XUtils.Get_num (
         "network/GWebCache2/LocalTest", "port", Config, GWC2_LocalTest_port);

      -- HOSTCACHE
      hostcache_purge           := L (Attr (
         "hostcache", "purge", Config, S (hostcache_purge))) = Yes;
      hostcache_purge_period    := XUtils.Get_duration (
         "hostcache/purge", "period", Config, hostcache_purge_period);
      hostcache_save            := L (Attr (
         "hostcache", "save", Config, S (hostcache_save))) = Yes;
      hostcache_save_period     := XUtils.Get_duration (
         "hostcache/save", "period", Config, hostcache_save_period);

      -- CHAT
      chat_log                  := L (Attr (
         "chat", "log", Config, S (chat_log))) = Yes;
      chat_logfile              := Attr (
         "chat", "logfile", Config, S (chat_logfile));
      chat_enabled              := L (Attr (
         "chat", "enabled", Config, S (chat_enabled))) = Yes;
      chat_answer               := U (L (Attr (
         "chat", "answer", Config, S (chat_answer))));
      chat_AwayMessage          := Attr (
         "chat", "AwayMessage", Config, S (chat_AwayMessage));

      -- SEARCH
      G2_search_priorities_high    := XUtils.Get_num (
         "network/Gnutella2/search/priorities", "high", Config, G2_Search_priorities_high);
      G2_search_priorities_medium  := XUtils.Get_num (
         "network/Gnutella2/search/priorities", "medium", Config, G2_Search_priorities_medium);
      G2_search_priorities_low     := XUtils.Get_num (
         "network/Gnutella2/search/priorities", "low", Config, G2_Search_priorities_low);
      G2_search_priorities_medium_threshold  := XUtils.Get_num (
         "network/Gnutella2/search/priorities/medium", "threshold", Config,
         G2_Search_priorities_medium_threshold);
      G2_search_priorities_low_threshold     := XUtils.Get_num (
         "network/Gnutella2/search/priorities/low", "threshold", Config,
         G2_Search_priorities_low_threshold);
      G2_search_priorities_stop_threshold     := XUtils.Get_num (
         "network/Gnutella2/search/priorities/stop", "threshold", Config,
         G2_Search_priorities_stop_threshold);
      G2_search_WatchdogPeriod     := XUtils.Get_Duration (
         "network/Gnutella2/search", "WatchdogPeriod", Config, G2_search_WatchdogPeriod);
      G2_search_MinimumRequeryWait := XUtils.Get_Duration (
         "network/Gnutella2/search", "MinimumRequeryWait", Config, G2_search_MinimumRequeryWait);
      G2_search_HubTimeout         := XUtils.Get_Duration (
         "network/Gnutella2/search", "HubTimeout", Config, G2_search_HubTimeout);
      G2_search_HubRestPeriod      := XUtils.Get_Duration (
         "network/Gnutella2/search", "HubRestPeriod", Config, G2_search_HubRestPeriod);
      G2_search_KeyDuration        := XUtils.Get_Duration (
         "network/Gnutella2/search", "KeyDuration", Config, G2_search_KeyDuration);
      G2_search_SendingThrottle    := XUtils.Get_Duration (
         "network/Gnutella2/search", "SendingThrottle", Config, G2_search_SendingThrottle);
      G2_search_PurgePeriod        := XUtils.Get_Duration (
         "network/Gnutella2/search", "PurgePeriod", Config, G2_search_PurgePeriod);
      G2_search_PurgeAge           := XUtils.Get_Duration (
         "network/Gnutella2/search", "PurgeAge", Config, G2_search_PurgeAge);
      G2_search_MaxHitsPerPacket   := XUtils.Get_num (
         "network/Gnutella2/search", "MaxHitsPerPacket", Config, G2_search_MaxHitsPerPacket);
      G2_search_SaveHits           := XUtils.Get_Boolean (
         "network/Gnutella2/search", "SaveHits", Config, G2_search_SaveHits);
      G2_search_SaveHitsDelay      := XUtils.Get_Duration (
         "network/Gnutella2/search", "SaveHitsDelay", Config, G2_search_SaveHitsDelay);
      G2_search_MinimumLeaves      := XUtils.Get_num (
         "network/Gnutella2/search", "MinimumLeaves", Config, G2_search_MinimumLeaves);
      G2_search_HubGrowingPeriod   := XUtils.Get_Duration (
         "network/Gnutella2/search", "HubGrowingPeriod", Config, G2_search_HubGrowingPeriod);

      -- DOWNLOAD
      download_active           := XUtils.Get_Boolean (
         "download", "active", Config, Download_Active);
      download_incomplete       := U (Folders.Ensure_Slash (Attr ("download",
         "incomplete", Config, S (download_incomplete))));
      download_finished         := U (Folders.Ensure_Slash (Attr ("download",
         "finished", Config, S (download_finished))));
      download_bandwidth        := XUtils.Get_speed ("download",
         "bandwidth", Config, download_bandwidth);

      -- G2 specific
      G2_download_ChunkSize        := XUtils.Get_Size (
         "network/Gnutella2/download", "ChunkSize", Config, G2_download_ChunkSize);
      G2_download_deflate          := XUtils.Get_Boolean (
         "network/Gnutella2/download", "deflate", Config, G2_Download_Deflate);
      G2_download_MaxActive        := XUtils.Get_Num (
         "network/Gnutella2/download", "MaxActive", Config, G2_download_MaxActive);
      G2_download_MaxAlive         := XUtils.Get_Num (
         "network/Gnutella2/download", "MaxAlive", Config, G2_download_MaxAlive);
      G2_download_MaxBackoffPeriod := XUtils.Get_Duration (
         "network/Gnutella2/download", "MaxBackoffPeriod", Config, G2_download_MaxBackoffPeriod);
      G2_download_MaxFailures      := XUtils.Get_Num (
         "network/Gnutella2/download", "MaxFailures", Config, G2_download_MaxFailures);
      G2_download_ForgetFailedSources := XUtils.Get_Boolean (
         "network/Gnutella2/download", "ForgetFailedSources", Config, G2_Download_ForgetFailedSources);

      -- DEBUG
      debug_active              := L (Attr (
         "debug", "active", Config, S (debug_active))) = Yes;
      debug_logfile             := Attr (
         "debug", "logfile", Config, S (debug_logfile));
      debug_netlogfile          := Attr (
         "debug", "netlogfile", Config, S (debug_netlogfile));
      debug_loglevel            := U (L (Attr (
         "debug", "loglevel", Config, S (debug_loglevel))));
      debug_ConsoleEcho         := L (Attr (
         "debug", "ConsoleEcho", Config, S (debug_ConsoleEcho))) = Yes;
      debug_DebugStatistics     := L (Attr ("debug",
         "DebugStatistics", Config, S (debug_DebugStatistics))) = Yes;
      debug_PurgeOnStartup      := L (Attr (
         "debug", "PurgeOnStartup", Config, S (debug_PurgeOnStartup))) = Yes;
      debug_heartbeat           := XUtils.Get_duration (
         "debug", "heartbeat", Config, debug_heartbeat);
      debug_MemoryPeriod        := XUtils.Get_duration (
         "debug", "MemoryPeriod", Config, debug_MemoryPeriod);
      debug_CrawlerAllowed      := L (Attr ("debug",
         "CrawlerAllowed", Config, S (debug_CrawlerAllowed))) = Yes;

      -- ENFORCED VALUES
      -- Maximum is 21, more will cause stack overflows
      library_QRTSize  := Natural'Min (21, library_QRTSize);
      -- Not more than 5 G2 servers
      G2_ActiveServers := Natural'Min (5, G2_ActiveServers);

      -- Minimum graph period: 10 mins.
      gui_GraphPeriod := Duration'Max (600.0, gui_GraphPeriod);

      -- Pass some relevant stats
      -- Set_Web_Stats;

   end Set_options;

   ------------------------------------------------------------------------
   -- Set_Web_Stats                                                      --
   ------------------------------------------------------------------------
   -- procedure Set_Web_Stats is
   -- begin
   --   Statistics.Tpl.Object.Set ("DOWNLOADACTIVE", Statistics.Booleans.Create (Download_Active));
   -- end Set_Web_Stats;

begin

   Set_options;

end Adagio.Globals.Options;
