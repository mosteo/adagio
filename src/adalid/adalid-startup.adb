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
--  $Id: adalid-startup.adb,v 1.6 2004/03/29 19:13:30 Jano Exp $

with Adalid.Events;
with Adalid.Globals;
with Adalid.Gui;
with Adalid.Trace;

with Adagio.Connect;
with Adagio.Connect.Peer_manager;
with Adagio.Debug;
with Adagio.Globals.Options;
with Adagio.Network;
with Adagio.Os.Memory_stats;
with Adagio.Security.Files;
with Adagio.Server;
with Adagio.Server.Tasks;
with Adagio.Xml;
with Adagio.Xml.Utils;

with Agpl.Geoip.Embedded;
with Agpl.Http.Server;
with Agpl.Http.Server.Sort_handler;
with Agpl.Streams.Deflate;

with Gnat.Traceback;

with System.Address_image;

with Adagio.Socket.Throttled;

package body Adalid.Startup is

   package Security renames Adagio.Security;
   package Xml renames Adagio.Xml;

   ----------
   -- Init --
   ----------

   procedure Init is
      Success : Boolean;
   begin
      -- Parsing of Adalid.xml is done during elaboration.
      -- Load GeoIP database
      Agpl.Geoip.Embedded.Init (
         S (Adagio.Globals.Options.Globals_GeoIP), Success);
      if not Success then
         Trace.Log ("Startup: Geoip initialization failed from path: " &
            S (Adagio.Globals.Options.Globals_GeoIP), Trace.Error);
         raise Initialization_error;
      end if;

      -- Sorting gui settings
      Agpl.Http.Server.Sort_handler.Set_settings_file (
         S (Adagio.Globals.Data_folder) & "sortprefs.dat");

      -- Prepare GUID:
      Adagio.Globals.Prepare_GUID;

      -- Check memory usage:
      Adagio.Os.Memory_stats.Memory_task.Start (
         Adagio.Globals.Options.debug_MemoryPeriod);

      -- Set various throttles:
      Adagio.Globals.Main_throttle.Set_target_usage (
         Adagio.Globals.Options.globals_maxCPU);

      -- Start the GUI 
      if Adagio.Globals.Options.Gui_active then
         Gui.Init (
            S (Adagio.Globals.Options.Gui_address), 
            Adagio.Globals.Options.Gui_port);
      end if;

      -- Fill user profile:
      Adagio.Globals.My_profile := 
         Xml.Get ("gProfile", Adagio.Globals.Config);

      -- Security rules
      declare
         Files : Xml.Node_array :=
            Xml.Get_all ("security/file", Globals.Config);
      begin
         for N in Files'Range loop
            if Xml.Get_attribute (Files (N), "active", "yes") = "yes" then
               Adagio.Security.Files.Add_security_file (
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
               Xml.Utils.Get_boolean (Codes (N), "allow", false));
         end loop;
      end;

      -- Hostcache load:
      Adagio.Server.List.Initialize;

      -- Hostcache maintenance:
      Adagio.Server.Tasks.Maintenance.Start;

      -- Initialization completed.
   end Init;

   --------------
   -- Shutdown --
   --------------
   procedure Shutdown is
   begin
      ------------
      -- Memory --
      ------------
      Adagio.Os.Memory_stats.Memory_task.Shutdown;

      -----------
      -- Peers --
      -----------
      Adagio.Connect.Peer_manager.Shutdown;

      --------------
      -- Networks --
      --------------
      Adagio.Network.List.Disconnect_all;
      -- Give some time for cleanup:
      delay 2.0;

      ----------------
      -- Host cache --
      ----------------
      Adagio.Server.List.Purge;
      Adagio.Server.List.Save;
      if Adagio.Server.Tasks.Maintenance'Callable then
         Adagio.Server.Tasks.Maintenance.Shutdown;
      end if;

      ---------
      -- GUI --
      ---------
      while not Adagio.Debug.Tracing_finished loop
         delay 0.1;
      end loop;
      GUI.Shutdown;

      -------------------
      -- Free lockfile --
      -------------------
      Adagio.Globals.Remove_lock;

   end Shutdown;

   -------------------
   -- Interruptions --
   -------------------
   protected body Interruptions_handler is
      -- Orderly shutdowns
      procedure Int_quit is
      begin
         Trace.Log ("Termination signal received, exiting...", 
            Trace.Informative);
         Adagio.Globals.Requested_exit := true;
      end Int_quit;

      -- High distress protection faults! 
      procedure Int_seg_violation is
         use Gnat;
         Callstack : Traceback.Tracebacks_array (1 .. 20);
         Last      : Natural;
         Locs      : Ustring;
      begin
         Trace.Log ("************************", Trace.Always);
         Trace.Log ("*** ACCESS VIOLATION ***", Trace.Always);
         Traceback.Call_chain (Callstack, Last);
         for N in Callstack'First .. Last loop
            ASU.Append (Locs, System.Address_image (Callstack (N)) & " ");
         end loop;
         Trace.Log ("*** CALLSTACK: " & S (Locs), Trace.Always);
         Trace.Log ("************************", Trace.Always);
      end Int_seg_violation;
   end Interruptions_handler;

end Adalid.Startup;
