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
--  $Id: adalid-gui-commands.adb,v 1.1 2004/03/22 07:14:54 Jano Exp $

with Adalid.Globals;
with Adalid.Globals.Options;
with adalid.Gui.Handlers;
with Adalid.Trace;

with Adagio.Debug;
with Adagio.Event_log;
with Adagio.Globals;
with Adagio.Globals.Options;
--with Adagio.Gui.Graphs;
with Adagio.Traffic;

with Agpl.Dynamic_vector;
with Agpl.Http.Server;
with Agpl.Http.Server.Simple_handler;
with Agpl.Http.Server.Sort_handler;

with Aws;
with Aws.Mime;
with Aws.Parameters;
with Aws.Response;
with Aws.Status;
with Aws.Status.Set;

package body Adalid.Gui.Commands is

   type Command_function is access 
      function (Request : in Aws.Status.Data) return Aws.Response.Data;

   type Command_entry is record
      Action : Ustring;
      Doer   : Command_function;
   end record;

   package Command_lists is new Agpl.Dynamic_vector (Command_entry);
   Commands : Command_lists.Object (First => 1);
   use Command_lists;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   function Shutdown (Request : in Aws.Status.Data) 
      return Aws.Response.Data 
   is
      pragma Unreferenced (Request);
   begin
      Adagio.Globals.Requested_exit := true;
      return Aws.Response.Build(
         Aws.Mime.Text_plain,
         "Adalid is going down now!!");
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
         Adagio.Event_log.Clear;
      elsif What = "traffic" then
         Adagio.Traffic.Clear;
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
      Adagio.Globals.Options.Debug_loglevel := U (New_level);
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
      if Adagio.Debug.Debug_statistics_enabled then
         Adagio.Debug.Reset_alive_tasks;
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
--      Gui.Graphs.Set_time_range (Period);
      delay 0.75;
      declare
         New_req : Aws.Status.Data := Request; -- To copy auth data
      begin
         Aws.Status.Set.Request (
            New_req, Aws.Status.GET, "/home.html", Aws.Http_version);
         return Agpl.Http.Server.Callback_function (New_req);
      end;
   end Graph_period;

   ------------------------------------------------------------------------
   -- Reload_config                                                      --
   ------------------------------------------------------------------------
   function Reload_config (Request : in Aws.Status.Data) 
      return Aws.Response.Data
   is
   begin
      Adagio.Globals.Load_config;
      Adalid.Globals.Load_config;
      Adagio.Globals.Options.Set_options;
      Adalid.Globals.Options.Set_options;
      declare
         New_req : Aws.Status.Data := Request; -- To copy auth data
      begin
         Aws.Status.Set.Request (
            New_req, Aws.Status.GET, "/options.html", Aws.Http_version);
         return Agpl.Http.Server.Callback_function (New_req);
      end;
   end Reload_config;

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
      Append (Commands, (U ("clear"), Clear'Access));
      Append (Commands, (U ("set_log"), Set_log'Access));
      Append (Commands, (U ("reset_stats"), Reset_stats'Access));
      Append (Commands, (U ("graph_period"), Graph_period'Access));
      Append (Commands, (U ("reload_config"), Reload_config'Access));
   end Register;

end Adalid.Gui.Commands;
