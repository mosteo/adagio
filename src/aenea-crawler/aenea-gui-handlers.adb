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
--  $Id: aenea-gui-handlers.adb,v 1.9 2004/03/10 23:49:58 Jano Exp $

with Aenea.Countries;
with Aenea.Debug;
with Aenea.Globals;
with Aenea.Globals.Options;
with Aenea.Gui.Bitmaps;
with Aenea.Gui.Events;
with Aenea.Gui.Graphs;
with Aenea.Hub;
-- with Aenea.Hub.Topo;
with Aenea.Misc;
with Aenea.Net;
with Aenea.Persistent;
with Aenea.Startup;
with Aenea.Tables;
with Aenea.Types;
with Aenea.Walker;

with Adagio.Convert;
with Adagio.Event_Log;
with Adagio.Globals.Options;
with Adagio.Os.Memory;
with Adagio.Statistics;

with Strings.Utils;

with Agpl.Event_queues.Calendar;
with Agpl.Http.Server;
with Agpl.Http.Server.Simple_handler;
with Agpl.Http.Server.Single_handler;
with Agpl.Http.Server.Sort_handler;
with Agpl.Strings;

with Aws.Messages;
with Aws.Mime;
with Aws.Parameters;
with Aws.Response;
with Aws.Status;
with Templates_parser;
use  Templates_parser;

with Ada.Calendar; use Ada.Calendar;
use  Ada;

package body Aenea.Gui.Handlers is

   use type Calendar.Time;

   -- Startup:
   Startup : constant Calendar.Time := Calendar.Clock;

   ------------------------------------------------------------------------
   -- Uptimes_Single_Handler                                             --
   ------------------------------------------------------------------------
   function Uptimes_Single_Handler
      return Templates_Parser.Translate_Table
   is
      Single4,
      Single5,
      Single6 : Vector_Tag;
      Uptimes : constant Types.Uptimes_Array := Persistent.Object.Get_Top_Ten_Uptimes;
   begin
      for I in Uptimes'Range loop
         Single4 := Single4 &
            Strings.Utils.Trim (Misc.Image (Uptimes (I).Uptime));
         Single5 := Single5 & S (Uptimes (I).Nick);
         Single6 := Single6 & S (Uptimes (I).Version);
      end loop;
      return (
         Assoc ("SINGLE1", Hub.By_Uptime.Total_Count),
         Assoc ("SINGLE2",
            Strings.Utils.Trim (Misc.Image (Persistent.Object.Get_Longest_Uptime_Time))),
         Assoc ("SINGLE3", Persistent.Object.Get_Longest_Uptime_Nick),
         Assoc ("SINGLE4", Single4),
         Assoc ("SINGLE5", Single5),
         Assoc ("SINGLE6", Single6)
         );
   end Uptimes_Single_Handler;

   ------------------------------------------------------------------------
   -- Idx_handler                                                        --
   ------------------------------------------------------------------------
   function Idx_handler (Request : in Aws.Status.Data)
      return Aws.Response.Data is
      URI : constant String := Agpl.Http.Server.Get_root & "index.html";

      function Get_Count return Natural is
      begin
         return Net.Counter.Avg_Unique;
      exception
         when others =>
            return Net.Counter.Unique_Count;
      end Get_Count;

   begin
      declare
         Translat : Translate_table :=
                      (Assoc ("VERSION", Version),
                       Assoc ("netsize", Get_Count));
      begin
         return Aws.Response.Build
           (Aws.Mime.Content_type (URI),
            String'(Parse (URI, Translat & Agpl.Http.Server.Standard_xlats (Request))),
            Cache_control => Aws.Messages.No_cache);
      end;

   end Idx_Handler;
   ------------------------------------------------------------------------
   -- Raw_handler                                                        --
   ------------------------------------------------------------------------
   Memory_Peak : Natural := Adagio.Os.Memory.Heap_Usage;

   function Raw_handler (Request : in Aws.Status.Data)
      return Aws.Response.Data is
      URI : constant String := Agpl.Http.Server.Get_root & "raw.html";
      Now : constant Calendar.Time   := Calendar.Clock;

      function Reboot_Time return String is
      begin
         if Globals.Options.Globals_Shutdown_Active  then
            return
               Strings.Utils.Trim (Misc.Image (
                  Globals.Options.Globals_Shutdown_Deadline - (Now - Startup)));
         else
            return "Disabled";
         end if;
      end Reboot_Time;

      Mem_Usage : constant Natural := Adagio.Os.Memory.Heap_Usage;
   begin
      Memory_Peak := Natural'Max (Memory_Peak, Mem_Usage);
      declare
         use Agpl.Event_Queues.Calendar;
         Translat : Translate_table
           := (
               Assoc ("VERSION", Version),
               Assoc ("netsize", Net.Counter.Total_count),
               Assoc ("hubs", Net.Counter.Hubs_count),
               Assoc ("leaves", Net.Counter.Leaves_count),
               Assoc ("uniquenodes", Net.Counter.Unique_count),
               Assoc ("leavesperhub",
                      Misc.To_string (
                                      Float (Net.Counter.Leaves_count) /
                                        Float (Integer'Max (Net.Counter.Hubs_count, 1)))),
               Assoc ("hubsperleaf",
                      Misc.To_String
                        (Float (Net.Counter.Leaves_Count) /
                         Float (Net.Counter.Unique_Count - Net.Counter.Hubs_Count))),
               Assoc ("tracked", Walker.Adder.Count),
               Assoc ("purged", Walker.Last_Number_Purged),
               Assoc ("purgedunique", Walker.Last_Number_Purged_Unique),
               Assoc ("events", Length (Net.Events)),
               Assoc ("nextevent", Agpl.Strings.Trim
                        (Duration'Image (Get_Next_Deadline (Net.Events) - Clock))),
               Assoc ("inpackets", Net.Incoming.Length),
               Assoc ("packets", Net.Outgoing.Length),
--                 Assoc ("checkerlast", Agpl.Strings.Trim
--                          (Duration'Image (Now - Walker.Task_Checker_Last.Get))),
--                 Assoc ("eventmax", Agpl.Strings.Trim
--                          (Duration'Image (Walker.Event_Max.Get))),
--                 Assoc ("longesteventkind", Misc.To_Lower (Walker.Longest_Event_Kind'Img)),
               --                 Assoc ("eventkind", Misc.To_Lower (Walker.Event_Kind'Img)),
               Assoc ("overtimedtask", S (Debug.Overtimed_Task.Get)),
               Assoc ("overtime", Agpl.Strings.Trim (Duration'Image (Debug.Overtime.Get))),
               Assoc ("memory",     Adagio.Convert.To_Size (Mem_Usage)),
               Assoc ("memorypeak", Adagio.Convert.To_Size (Memory_Peak)),
               Assoc ("uptime",
                      Strings.Utils.Trim (Misc.Image (Now - Startup))),
               Assoc ("reboot", Reboot_Time)
              );
      begin
         return Aws.Response.Build (
            Aws.Mime.Content_type (URI),
            String'(Parse (URI, Translat & Agpl.Http.Server.Standard_xlats (Request))),
            Cache_control => Aws.Messages.No_cache);
      end;
   end Raw_handler;

   ---------------------
   -- Command_handler --
   ---------------------

   function Command_handler (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      Params : Aws.Parameters.List := Aws.Status.Parameters (Request);
      Action : constant String := Aws.Parameters.Get (Params, "action");

      ------------------------------------------------------------------------
      -- Graph_period                                                       --
      ------------------------------------------------------------------------
      procedure Graph_period (Request : in Aws.Status.Data)
      is
         Period : constant Duration := Duration'Value
           (Aws.Parameters.Get
              (Aws.Status.Parameters (Request), "period"));
      begin
         Gui.Graphs.Set_time_range (Period);
      end Graph_period;

   begin
      if Action = "reload_config" then
         Globals.Init;
         Globals.Options.Set_options;
      elsif Action = "shutdown" then
         Aenea.Startup.Shutdown;
      elsif Action = "obliterate" then
         Walker.Adder.Obliterate;
      elsif Action = "debug_stop" then
         Walker.Debug_Stop := True;
      elsif Action = "graph_period" then
         Graph_Period (Request);
      else
         return Aws.Response.Build
           (Aws.Mime.Text_plain,
            "Command unknown. Use: " &
            "reload_config, shutdown, obliterate, debug_stop");
      end if;
      return Aws.Response.Build (
         Aws.Mime.Text_plain, "Command applied: " & Action);
   end Command_handler;

   Raw_object : Agpl.Http.Server.Simple_handler.Object (Raw_handler'Access);
   Idx_object : Agpl.Http.Server.Simple_handler.Object (Idx_handler'Access);
   Hub_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Walker.Report'Access,
      Single => Agpl.Http.Server.Sort_handler.Void_singleton'Access,
      Page   => new String'("hubs.html"));
   Countries_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Countries.Report'Access,
      Single => Net.Report_hubs'Access,
      Page   => new String'("countries.html"));
   Events_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Gui.Events.Records.Http_Report'Access,
      Single => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page   => new String'("events.html"));
   Trace_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Gui.Events.Traces.Http_Report'Access,
      Single => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page   => new String'("trace.html"));
   -- De-linked handlers:
   Hub2_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Walker.Report'Access,
      Single => Agpl.Http.Server.Sort_handler.Void_singleton'Access,
      Page   => new String'("hubs2.html"));
   Countries2_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Countries.Report'Access,
      Single => Net.Report_hubs'Access,
      Page   => new String'("countries2.html"));
   Vendors_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Hub.By_Vendor.Report'Access,
      Single => Hub.By_Vendor.Total_Report'Access,
      Page   => new String'("vendors.html"));
   Versions_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Hub.By_Version.Report'Access,
      Single => Hub.By_Version.Total_Report'Access,
      Page   => new String'("versions.html"));
   Uptimes_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Hub.By_Uptime.Report'Access,
      Single => Uptimes_Single_Handler'Access,
      Page   => new String'("uptimes.html"));
   Status_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Hub.By_Status.Report'Access,
      Single => Hub.By_Status.Total_Report'Access,
      Page   => new String'("status.html"));
   Leaves_object : Agpl.Http.Server.Sort_handler.Object (
      Source => Hub.By_Leaves.Report'Access,
      Single => Hub.By_Leaves.Total_Report'Access,
      Page   => new String'("leaves.html"));
   Stats_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Adagio.Statistics.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("stats.html"));
   Graphs_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Agpl.Http.Server.Single_handler.Null_singleton'Access,
      Page      => new String'("graphs.html"));
   Command_object : Agpl.Http.Server.Simple_handler.Object (Command_handler'Access);
   DB_time_object : Agpl.Http.Server.Sort_handler.Object
     (Source => Tables.By_Insertion_Total_Time.Report'Access,
      Single => Tables.By_Insertion_Total_Time.Total_Report'Access,
      Page   => new String'("dbtotal.html"));

   ------------------------------------------------------------------------
   -- Events_object                                                      --
   ------------------------------------------------------------------------
   function Adagio_Events_singleton return Templates_parser.Translate_table;
   Adagio_Events_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => Adagio.Event_log.Http_report'Access,
      Single    => Adagio_Events_singleton'Access,
      Page      => new String'("aevents.html"));

   ------------------------------------------------------------------------
   -- Events_singleton                                                   --
   ------------------------------------------------------------------------
   function Adagio_Events_singleton return Templates_parser.Translate_table
   is
   begin
      return (1 => Assoc ("SINGLE1", Adagio.Globals.Options.Debug_Loglevel));
   end Adagio_Events_singleton;

begin
   -- Skin:
   Agpl.Http.Server.Set_root (S (Globals.Options.Gui_HtmlRoot));
   Agpl.Http.Server.Set_style_sheet (S (Globals.Options.Gui_HtmlStyle));

   -- Registrations:
   Agpl.Http.Server.Register_handler ("/raw.html",    Raw_object);
   Agpl.Http.Server.Register_handler ("/index.html",  Idx_object);
   Agpl.Http.Server.Register_handler ("/hubs.html",   Hub_object);
   Agpl.Http.Server.Register_handler ("/countries.html", Countries_object);
   Agpl.Http.Server.Register_handler ("/events.html", Events_object);
   Agpl.Http.Server.Register_handler ("/aevents.html", Adagio_Events_object);
   Agpl.Http.Server.Register_handler ("/trace.html",  Trace_object);
   Agpl.Http.Server.Register_handler ("/hubs2.html",  Hub2_object);
   Agpl.Http.Server.Register_handler ("/countries2.html", Countries2_object);
   Agpl.Http.Server.Register_handler ("/vendors.html", Vendors_object);
   Agpl.Http.Server.Register_handler ("/versions.html", Versions_object);
   Agpl.Http.Server.Register_handler ("/uptimes.html", Uptimes_object);
   Agpl.Http.Server.Register_handler ("/status.html", Status_object);
   Agpl.Http.Server.Register_handler ("/leaves.html", Leaves_object);
   Agpl.Http.Server.Register_handler ("/stats.html",  Stats_object);
   Agpl.Http.Server.Register_handler ("/graphs.html",  Graphs_object);
   Agpl.Http.Server.Register_handler ("/command",     Command_object);
   Agpl.Http.Server.Register_handler ("/dbtotal.html",     DB_time_object);

   --  Charts:
   Gui.Bitmaps.Register;

   Agpl.Http.Server.Set_server_name (Version);
end Aenea.Gui.Handlers;
