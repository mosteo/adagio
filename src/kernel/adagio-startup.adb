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
--  $Id: adagio-startup.adb,v 1.11 2004/03/22 07:14:57 Jano Exp $

with Adagio.Connect.Peer_manager;
with Adagio.Debug;
with Adagio.Download.Startup;
with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.G2.Local_query;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Gui;
pragma Warnings (Off);
with Adagio.Heartbeat;
pragma Warnings (On);
with Adagio.Library;
with Adagio.Library.Tasks;
with Adagio.Os.Memory_stats;
with Adagio.Os.Shutdown;
with Adagio.Network.Tasks;
with Adagio.Searches.Manager;
with Adagio.Server;
with Adagio.Server.Tasks;
with Adagio.Security;
with Adagio.Security.Files;
with Adagio.Statistics.Booleans;
with Adagio.Statistics.Tpl;
with Adagio.Trace;
with Adagio.Upload;
with Adagio.Upload.Queue.Manager;
with Adagio.Xml;
with Adagio.Xml.Utils;
--  with Adagio_resources;

with Agpl.Geoip.Embedded;
with Agpl.Http.Server.Sort_handler;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Adagio.Startup is

   ----------
   -- Init --
   ----------

   procedure Init is
      package XUtil renames Adagio.Xml.Utils;
      Success : Boolean;
   begin
      --  Parsing of adagio.xml is done during elaboration.

      --  Load GeoIP database
      Agpl.Geoip.Embedded.Init (S (Globals.Options.Globals_GeoIP), Success);
      if not Success then
         Trace.Log ("Startup: Geoip initialization failed from path: " &
            S (Globals.Options.Globals_GeoIP), Trace.Error);
         raise Initialization_error;
      end if;

      --  Sorting gui settings
      Agpl.Http.Server.Sort_handler.Set_settings_file (
         S (Globals.Data_folder) & "sortprefs.dat");

      --  Prepare GUID:
      Globals.Prepare_GUID;

      --  Check memory usage:
      Os.Memory_stats.Memory_task.Start (Globals.Options.debug_MemoryPeriod);

      --  Set various throttles:
      Globals.Main_throttle.Set_target_usage (Globals.Options.globals_maxCPU);
      Globals.Hash_throttle.Set_target_usage (
         Globals.Options.library_HashingCPUusage);

      --  Start the GUI
      if Globals.Options.Gui_active then
         Adagio.Gui.Init (
            S (Globals.Options.Gui_address), Globals.Options.Gui_port);
      end if;

      --  Fill user profile:
      Globals.My_profile := Xml.Get ("gProfile", Globals.Config);

      --  Security rules
      declare
         Files : Xml.Node_array :=
            Xml.Get_all ("security/file", Globals.Config);
      begin
         for N in Files'Range loop
            if Xml.Get_attribute (Files (N), "active", "yes") = "yes" then
               Security.Files.Add_security_file (
                  Xml.Get_attribute (Files (N), "path", ""));
            end if;
         end loop;
      end;
      declare
         Agents : Xml.Node_array :=
            Xml.Get_all ("security/ban", Globals.Config);
      begin
         for N in Agents'Range loop
            begin
            if Xml.Get_attribute (Agents (N), "active", "yes") = "yes" then
               if Xml.Get_attribute (Agents (N), "type", "") = "regexp" then
                  Security.Add_ban_agent (
                     Xml.Get_attribute (Agents (N), "client", "xlxlxlxlxlx"),
                     Security.Regexp);
               elsif Xml.Get_attribute (Agents (N), "type", "") = "substring"
               then
                  Security.Add_ban_agent (
                     Xml.Get_attribute (Agents (N), "client", "xlxlxlxlxlx"),
                     Security.Substring);
               else
                  Trace.Log ("Skipping incorrect agent ban for " &
                     Xml.Get_attribute (Agents (N), "client", "(missing)"),
                     Trace.Warning);
               end if;
            end if;
            exception
               when Security.Syntax_error =>
                  Trace.Log ("Skipping rule (syntax error) for " &
                     Xml.Get_attribute (Agents (N), "client", "(missing)"),
                     Trace.Warning);
            end;
         end loop;
      end;
      declare
         Codes : Xml.Node_array :=
            Xml.Get_all ("security/country", Globals.Config);
      begin
         for N in Codes'Range loop
            Security.Add_country_ban (
               Xml.Get_attribute     (Codes (N), "code", "zz"),
               Xml.Utils.Get_boolean (Codes (N), "allow", False));
         end loop;
      end;

      --  Add untrusted folders:
      declare
         Dirs : Xml.Node_array :=
            Xml.Get_all ("library/unshared", Globals.Config);
      begin
         for N in Dirs'Range loop
            Library.Object.Untrust (
               Xutil.Get_path (Dirs (N), "path", "----"));
         end loop;
      end;
      --  Add trusted folders:
      declare
         Shared  : Xml.Node_array :=
            Xml.Get_all ("library/shared", Globals.Config);
      begin
         for N in Shared'Range loop
            Library.Object.Trust (
               Xutil.Get_path (Shared (N), "path", "----"));
         end loop;
      end;

      --  Library load:
      --  It must be done after untrusted/trusted folders addition.
      Library.Object.Initialize (
         Delayed => Globals.Options.Library_DelayedStartup);

      --  Add shared folders:
      --  After loading to prevent rehashings:
      declare
         Shared  : Xml.Node_array :=
            Xml.Get_all ("library/shared", Globals.Config);
         Success : Boolean;
      begin
         for N in Shared'Range loop
            Library.Object.Add (
               Xutil.Get_path (Shared (N), "path", "----"), Success,
               Xutil.Get_duration (Shared (N), "RescanPeriod", Duration'Last)
               );
         end loop;
      end;

      --  Start library maintenance tasks:
      Library.Tasks.Folder_maintenance.Start (
         Globals.Options.Library_FolderAddingInterval);
      Library.Tasks.File_maintenance.Start (
         Globals.Options.Library_FileAddingInterval);

      --  Hostcache load:
      Server.List.Initialize;

      --  Hostcache maintenance:
      Server.Tasks.Maintenance.Start;

      --  Connect to networks:
      Network.Tasks.Start (1.0);

      --  Start upload manager
      Upload.Queue.Manager.Object.Init (Globals.Data_folder & "uploads.dat");

      --  Start searches if necessary
      if Globals.Options.Download_Active then
         Searches.Manager.Start;
      end if;

      --  Start downloads
      Download.Startup.Init;

      --  Web gui stats
      Set_Web_Stats;

      --  Initialization completed.
   end Init;

   --------------
   -- Shutdown --
   --------------
   procedure Shutdown is
   begin
      ------------
      -- Memory --
      ------------
      Os.Memory_stats.Memory_task.Shutdown;

      -----------
      -- Peers --
      -----------
      Connect.Peer_manager.Shutdown;

      -------------
      -- Library --
      -------------
      Library.Object.Save;

      -------------
      -- Uploads --
      -------------
      Upload.Queue.Manager.Object.Shutdown;

      --------------
      -- Searches --
      --------------
      Searches.Manager.Stop;

      ---------------
      -- Downloads --
      ---------------
      Download.Startup.Shutdown;

      --------------
      -- Networks --
      --------------
      Network.List.Disconnect_all;
      --  Give some time for cleanup:
      delay 2.0;

      -------------------
      -- Query workers --
      -------------------
      G2.Local_query.Shutdown;

      ----------------
      -- Host cache --
      ----------------
      Server.List.Purge;
      Server.List.Save;
      if Server.Tasks.Maintenance'Callable then
         Server.Tasks.Maintenance.Shutdown;
      end if;

      ------------------------------
      -- Optional memory cleanups --
      ------------------------------
      Xml.Delete (Globals.Config);

      -------------------
      -- Free lockfile --
      -------------------
      Globals.Remove_lock;

      ---------
      -- GUI --
      ---------
      GUI.Shutdown;

      while not Debug.Tracing_finished loop
         delay 0.1;
      end loop;

   end Shutdown;

   ------------------------------------------------------------------------
   -- Set_Web_Stats                                                      --
   ------------------------------------------------------------------------
   procedure Set_Web_Stats is
   begin
      --  Some stats for web gui
      Statistics.Tpl.Object.Set (
         "DOWNLOADACTIVE", Statistics.Booleans.Create (Globals.Options.Download_Active));
   end Set_Web_Stats;

end Adagio.Startup;
