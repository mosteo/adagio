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
--  $Id: adagio-gui-commands.adb,v 1.8 2004/03/10 23:50:00 Jano Exp $

with Adagio.Debug;
with Adagio.Download;
with Adagio.Download.Add;
with Adagio.Event_log;
with Adagio.G2.Core;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Gui.Graphs;
with Adagio.Gui.Handlers;
with Adagio.Query.Incoming;
with Adagio.Searches.Manager;
with Adagio.Searches;
with Adagio.Startup;
with Adagio.Trace;
with Adagio.Traffic;
with Adagio.Upload.Log;

with Agpl.Base64;
with Agpl.Dynamic_vector;
with Agpl.Http.Server;
with Agpl.Http.Server.Simple_handler;
with Agpl.Http.Server.Sort_handler;
with Agpl.Magnet;

with Aws;
with Aws.Mime;
with Aws.Parameters;
with Aws.Response;
with Aws.Status;
with Aws.Status.Set;

package body Adagio.Gui.Commands is

   type Command_function is access 
      function (Request : in Aws.Status.Data) return Aws.Response.Data;

   type Command_entry is record
      Action : Ustring;
      Doer   : Command_function;
   end record;

   package Command_lists is new Agpl.Dynamic_vector (Command_entry);
   Commands : Command_lists.Object (First => 1);
   use Command_lists;

   function Request_Redirect (Req : in Aws.Status.Data; To : in String) return Aws.Response.Data
      renames Agpl.Http.Server.Request_Redirect;

   ------------------------------------------------------------------------
   -- Disconnect_hub                                                     --
   ------------------------------------------------------------------------
   function Disconnect_hub (Request : in Aws.Status.Data) 
      return Aws.Response.Data 
   is
      Id : constant String := 
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "address");
   begin
      G2.Core.Disconnect_hub (Id);
      delay 0.25;
      return Agpl.Http.Server.Sort_handler.Get_page (
         Gui.Handlers.Hubs_object, Request);
   end Disconnect_hub;

   ------------------------------------------------------------------------
   -- Add_hub                                                            --
   ------------------------------------------------------------------------
   function Add_hub (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Address : constant String := 
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "address");
      Port    : constant String := 
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "port");
   begin
      G2.Core.Add_hub (Address, Natural'Value (Port));
      delay 0.25;
      return Agpl.Http.Server.Sort_handler.Get_page (
         Gui.Handlers.Hubs_object, Request);
   end Add_hub;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   function Shutdown (Request : in Aws.Status.Data) 
      return Aws.Response.Data 
   is
      pragma Unreferenced (Request);
   begin
      Globals.Requested_exit := true;
      return Aws.Response.Build (
         Aws.Mime.Text_plain,
         "Adagio is going down now!!");
   end Shutdown;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   function Clear (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      What : constant String := 
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "what");
   begin
      if What = "events" then
         Event_log.Clear;
      elsif What = "queries" then
         Query.Incoming.Clear;
      elsif What = "traffic" then
         Traffic.Clear;
      elsif What = "ufinished" then
         Upload.Log.Records.Clear;
      elsif What = "graphs" then
         Graphs.Clear;
      end if;

      declare
         New_req : Aws.Status.Data := Request; -- To copy auth data
      begin
         if What /= "graphs" then
            Aws.Status.Set.Request (
               New_req, 
               Aws.Status.GET, "/" & What & ".html", Aws.Http_version);
         else
            Aws.Status.Set.Request (
               New_req, 
               Aws.Status.GET, "/home.html", Aws.Http_version);
         end if;
         return Agpl.Http.Server.Callback_function (New_req);
      end;
   end Clear;

   ------------------------------------------------------------------------
   -- Set_log                                                            --
   ------------------------------------------------------------------------
   function Set_log (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      New_level : constant String := 
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "level");
   begin
      Globals.Options.Debug_loglevel := U (New_level);
      Trace.Check_changed_level;
      return Agpl.Http.Server.Sort_handler.Get_page (
         Gui.Handlers.Events_object, Request);
   end Set_log;

   ------------------------------------------------------------------------
   -- Reset_stats                                                        --
   ------------------------------------------------------------------------
   function Reset_stats (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      if Debug.Debug_statistics_enabled then
         Debug.Reset_alive_tasks;
      end if;
      return Aws.Response.Build(
         Aws.Mime.Text_plain,
         "Stats have been reset");
   end Reset_stats;

   ------------------------------------------------------------------------
   -- Graph_period                                                       --
   ------------------------------------------------------------------------
   function Graph_period (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Period : Duration := Duration'Value (
         Aws.Parameters.Get (
            Aws.Status.Parameters (Request), "period"));
   begin
      Gui.Graphs.Set_time_range (Period);
      delay 0.25;
      return Request_Redirect (Request, "/home.html");
   end Graph_period;

   ------------------------------------------------------------------------
   -- Reload_config                                                      --
   ------------------------------------------------------------------------
   function Reload_config (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
   begin
      Globals.Load_config;
      Globals.Options.Set_options;
      Startup.Set_Web_Stats;
      return Request_Redirect (Request, "/options.html");
   end Reload_config;

   ------------------------------------------------------------------------
   -- Search                                                             --
   ------------------------------------------------------------------------
   function Search (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
      Prio   : constant String := 
         Aws.Parameters.Get (Params, "priority");
   begin
      Trace.Log ("Creating search for '" & Target & "' with priority " &
         Prio, Trace.Debug);

      Searches.Manager.Create_search (
         Target   => Target, 
         Priority => Searches.Priorities'Value (Prio));

      -- Return search page:
      return Request_Redirect (Request, "/searches.html");
   end Search;

   ------------------------------------------------------------------------
   -- Delete_Search                                                      --
   ------------------------------------------------------------------------
   function Delete_Search (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
   begin
      Trace.Log ("Deleting search for '" & Target & "'", Trace.Debug);

      Searches.Manager.Delete_search (Searches.From_String (Target));

      -- Return search page:
      return Request_Redirect (Request, "/searches.html");
   end Delete_Search;

   ------------------------------------------------------------------------
   -- Pause_Search                                                       --
   ------------------------------------------------------------------------
   function Pause_Search (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
   begin
      Trace.Log ("Pausing search for '" & Target & "'", Trace.Debug);

      Searches.Manager.Pause_Search (Searches.From_String (Target));

      -- Return search page:
      return Request_Redirect (Request, "/searches.html");
   end Pause_Search;

   ------------------------------------------------------------------------
   -- Resume_Search                                                      --
   ------------------------------------------------------------------------
   function Resume_Search (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
   begin
      Trace.Log ("Resuming search for '" & Target & "'", Trace.Debug);

      Searches.Manager.Resume_Search (Searches.From_String (Target));

      -- Return search page:
      return Request_Redirect (Request, "/searches.html");
   end Resume_Search;

   ------------------------------------------------------------------------
   -- Search_Priority                                                    --
   ------------------------------------------------------------------------
   function Search_Priority (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
      Prio   : constant String := 
         Aws.Parameters.Get (Params, "priority");
   begin
      Trace.Log ("Updating search for '" & Target & "' with priority " &
         Prio, Trace.Debug);

      Searches.Manager.Set_Priority (
         Id        => Searches.From_String (Target), 
         Priority  => Searches.Priorities'Value (Prio));

      -- Return search page:
      return Request_Redirect (Request, "/searches.html");
   end Search_Priority;

   ------------------------------------------------------------------------
   -- Results_actions                                                    --
   ------------------------------------------------------------------------
   function Results_Actions (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Target : constant String := 
         Aws.Parameters.Get (Params, "target");
      Option : constant String := 
         Aws.Parameters.Get (Params, "option");
      Family : constant String := 
         Aws.Parameters.Get (Params, "family");
      Srch   : Searches.Search_Id := Searches.From_String (Target);
   begin
      if Option = "expand" then
         Searches.Manager.Set_Expanded (Srch, Family);
      elsif Option = "contract" then
         Searches.Manager.Set_Expanded (Srch, Family, false);
      else
         raise Constraint_Error;
      end if;
      return Request_Redirect (Request, "/results.html");
   end Results_Actions;

   ------------------------------------------------------------------------
   -- Download                                                           --
   ------------------------------------------------------------------------
   function Download (Request : in Aws.Status.Data)
      return Aws.Response.Data 
   is
      Params : constant Aws.Parameters.List := 
         Aws.Status.Parameters (Request);
      Secure : constant Boolean := Boolean'Value (
         Aws.Parameters.Get (Params, "secure"));
      Url    : constant String  := Agpl.Base64.To_String (
         Aws.Parameters.Get (Params, "magnet"));
   begin
      Adagio.Download.Add (Agpl.Magnet.Create (Url), Secure);
      return Request_Redirect (Request, "/downloads.html");
   end Download;

   ------------------------------------------------------------------------
   -- Command_handler                                                    --
   ------------------------------------------------------------------------
   function Command_handler (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
      Params : Aws.Parameters.List := Aws.Status.parameters (Request);
      Action : constant String     := Aws.Parameters.Get (Params, "action");
   begin
      for N in Commands.First .. Last (Commands) loop
         if Action = S (Commands.Vector (N).Action) then
            return Commands.Vector (N).Doer (Request);
         end if;
      end loop;
      -- Default, error:
      return Aws.Response.Build (
         Aws.Mime.Text_plain, "Unknown command: command" &
            Aws.Parameters.Uri_format (Params));
   end Command_handler;

   Command_object : Agpl.Http.Server.Simple_handler.Object (
      Command_handler'Access);

   ------------------------------------------------------------------------
   -- Register                                                           --
   ------------------------------------------------------------------------
   procedure Register is
   begin
      Agpl.Http.Server.Register_handler ("/command", Command_object);

      -- Commands
      Append (Commands, (U ("shutdown"), Shutdown'Access));
      Append (Commands, (U ("disconnect_hub"), Disconnect_hub'Access));
      Append (Commands, (U ("add_hub"), Add_hub'Access));
      Append (Commands, (U ("clear"), Clear'Access));
      Append (Commands, (U ("set_log"), Set_log'Access));
      Append (Commands, (U ("reset_stats"), Reset_stats'Access));
      Append (Commands, (U ("graph_period"), Graph_period'Access));
      Append (Commands, (U ("reload_config"), Reload_config'Access));
      Append (Commands, (U ("search"), Search'Access));
      Append (Commands, (U ("delete_search"), Delete_Search'Access));
      Append (Commands, (U ("pause_search"), Pause_Search'Access));
      Append (Commands, (U ("resume_search"), Resume_Search'Access));
      Append (Commands, (U("change_search_priority"),Search_Priority'Access));
      Append (Commands, (U ("results"), Results_Actions'Access));
      Append (Commands, (U ("download"), Download'Access));
   end Register;

end Adagio.Gui.Commands;
