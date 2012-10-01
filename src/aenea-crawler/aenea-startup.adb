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
--  $Id: aenea-startup.adb,v 1.7 2004/01/26 20:47:15 Jano Exp $

with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.GWCache2;
with Adagio.Server;

with Aenea.Globals;
with Aenea.Globals.Options;
with Aenea.Gui;
with Aenea.Gui.Handlers;
with Aenea.Gwc2;
with Aenea.Hub;
with Aenea.Net;
with Aenea.Persistent;
with Aenea.Trace;
with Aenea.Walker;
with Aenea.Xml;

with Agpl.Geoip.Embedded;

package body Aenea.Startup is

   ----------
   -- Init --
   ----------

   procedure Init is
      Success : Boolean;
   begin
      -- Parsing of xml file is done during elaboration.

      -- Restore persistent data
      Persistent.Object.Restore;

      -- Some debug traces.
      Trace.Log ("Hub Object size is:" & Integer'Image (Hub.Object'Size / 8),
         Trace.Always);

      -- Geoip stuff:
      Agpl.Geoip.Embedded.Init (S (Globals.Options.Globals_GeoIP), Success);
      if not Success then
         raise Initialization_error;
      end if;

      -- Hardcoded GWC2 servers:
      Hardcoded_servers:
      begin
         declare
            use type Xml.Node_array;
            Serv  : Adagio.GWCache2.Server_access;
            Nodes1: constant Xml.Node_array := Xml.Get_all ("network/GWebCache2/root",
               Globals.Config);
            Nodes2: constant Xml.Node_array := Xml.Get_all ("network/GWebCache2/root",
               Adagio.Globals.Config);
            Nodes : constant Xml.Node_array := Nodes1 & Nodes2;
         begin
            for N in Nodes'Range loop
               Serv := Adagio.GWCache2.Create (
                  Xml.Get_attribute (Nodes (N), "url", ""),
                  Is_root => True);
               Adagio.Server.List.Add (Adagio.Server.Object_access (Serv));
            end loop;
         end;
      exception
         when Adagio.Server.Server_already_cached =>
            null;
         when E : others =>
            Trace.Log ("Startup.Init: " & Trace.Report (E), Trace.Warning);
      end Hardcoded_servers;

      -- Network startup
      Net.Init;

      -- Start walker
      Aenea.Walker.Start;

      -- Hardcoded starting hubs:
      Hardcoded_hubs:
      declare
         Nodes : constant Xml.Node_array := Xml.Get_all ("walk/hub",
            Globals.Config);
      begin
         for N in Nodes'Range loop
            Walker.Adder.Add_Found (
               Xml.Get_Attribute (Nodes (N), "address", ""));
         end loop;
      end Hardcoded_hubs;

      -- GWC2 hubs:
      GWC2.Query (Globals.Options.GWC2_BootstrapHubs);

      -- GUI
      Gui.Init (
         S (Adagio.Globals.Options.Gui_address),
         Adagio.Globals.Options.Gui_port);

   end Init;

   --------------
   -- Shutdown --
   --------------
   procedure Shutdown is
   begin
      -- Signal shutdown to Adagio components:
      Adagio.Globals.Requested_exit := True;

      -- Gui:
      Gui.Shutdown;

      -- Network shutdown:
      Net.Shutdown;

      -- Lock removal:
      Adagio.Globals.Remove_lock;

   end Shutdown;

end Aenea.Startup;
