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
--  $Id: adagio-gui-handlers.adb,v 1.10 2004/03/29 19:13:31 Jano Exp $

with Adagio.BW_usage;
with Adagio.Convert;
with Adagio.Download.Manager;
with Adagio.File.Http_report;
with Adagio.G2;
with Adagio.G2.Core.Search_Report;
with Adagio.G2.Core.Stats;
with Adagio.G2.Local_query;
with Adagio.G2.Transceiver_types;
with Adagio.Globals.Options;
with Adagio.Globals;
with Adagio.Library;
with Adagio.Misc;
with Adagio.Query.Incoming;
with Adagio.Os.Memory;
with Adagio.Searches;
with Adagio.Searches.Hit_Family;
with Adagio.Searches.Manager;
with Adagio.Server;
with Adagio.Server.Http_report;
with Adagio.Statistics;
with Adagio.Statistics.Tpl;
with Adagio.Traffic;
with Adagio.Xml;
with Adagio.Upload;
with Adagio.Upload.Log;
with Adagio.Upload.Queue.Manager;
with Strings.Utils;

with Agpl.Base64;

with Aenea.Countries;

with Agpl.Http.Server.Simple_handler;
with Agpl.Http.Server.Single_handler;
with Agpl.Http.Server.Single2_handler;
with Agpl.Strings;

with Aws.Parameters;
with Aws.Response;
with Aws.Status;

with Dom.Core;
with Dom.Core.Nodes;
with Dom.Core.Attrs;

with Ada.Calendar;
use  Ada;

package body Adagio.Gui.Handlers is

   use  Templates_parser;

   ------------------------------------------------------------------------
   -- Events_singleton                                                   --
   ------------------------------------------------------------------------
   function Events_singleton return Templates_parser.Translate_table
   is
   begin
      return (1 => Assoc ("SINGLE1", Globals.Options.Debug_Loglevel));
   end Events_singleton;

   ------------------------------------------------------------------------
   -- Get_Handler                                                        --
   ------------------------------------------------------------------------
   function Get_Handler (Request : in Aws.Status.Data) return Translate_table is
      Params   : constant Aws.Parameters.List := Aws.Status.Parameters (Request);
      Srch_s   : constant String := Aws.Parameters.Get (Params, "search");
      Family_s : constant String := Aws.Parameters.Get (Params, "family");
      Srch     : constant Searches.Search_Id := Searches.From_String (Srch_s);
      Family   : constant Searches.Hit_Family.Family_Id := Searches.Hit_Family.To_Family_Id (Family_s);
   begin
      return (
         Assoc ("SINGLE1", Agpl.Base64.To_Base64 (Searches.Manager.Get_Magnet (Srch, Family, Secure => false))),
         Assoc ("SINGLE2", Agpl.Base64.To_Base64 (Searches.Manager.Get_Magnet (Srch, Family, Secure => true))),
         Assoc ("SINGLE3", Searches.Manager.Get_Name (Srch, Family)),
         Assoc ("SINGLE4", Convert.To_Size (Natural (Searches.Manager.Get_Size (Srch, Family)))));
   end Get_Handler;

   ------------------------------------------------------------------------
   -- Home_handler                                                       --
   ------------------------------------------------------------------------
   function Home_handler return Translate_table is
      use type Calendar.Time;
      Session_speed     : Ustring;
      Session_completed : Ustring;
      Uptime            : Duration := Calendar.Clock - Globals.Adagio_start;
      Total_packets     : Natural;
   begin
      begin
         Session_speed := U (Statistics.Image (Statistics.Object.Get (
            Upload.Queue.Manager.Stat_session_speed)));
      exception
         when Statistics.Value_not_defined =>
            Session_speed := U ("0.0kB/s");
      end;
      begin
         Session_completed := U (Statistics.Image (Statistics.Object.Get (
            Upload.Queue.Manager.Stat_session_completed)));
      exception
         when Statistics.Value_not_defined =>
            Session_completed := U ("0");
      end;
      begin
         Total_packets := G2.Packets_sent + G2.Packets_received;
      exception
         when Constraint_error =>
            Total_packets := Natural'Last;
      end;
      return (
         Assoc (
            "SINGLE1", 
            G2.Core.Connected_hubs),
         Assoc (
            "SINGLE2", 
            Library.Object.Num_files),
         Assoc (
            "SINGLE3", 
            Library.Object.Num_pending_files),
         Assoc (
            "SINGLE4", 
            Upload.Queue.Manager.Object.Num_active_uploads),
         Assoc (
            "SINGLE5", 
            Upload.Queue.Manager.Object.Num_waiting),
         Assoc (
            "SINGLE6", 
            Convert.To_size (Float (
               Upload.Queue.Manager.Object.Get_session_sent))),
         Assoc (
            "SINGLE7", 
            S (Session_speed)),
         Assoc (
            "SINGLE8", 
            Agpl.Strings.To_string (
               Float (Upload.Queue.Manager.Object.Get_mean_speed) * 100.0 /
               Float (Globals.Options.Uploads_bandwidth), 1)),
         Assoc (
            "SINGLE9", 
            G2.Local_query.Query_hits),
         Assoc (
            "SINGLE10", 
            Agpl.Strings.To_string (
               Float (G2.Local_query.Query_hits) * 3600.0 /
               Float (Uptime), 1)),
         Assoc (
            "SINGLE11", 
            G2.Packets_sent),
         Assoc (
            "SINGLE12", 
            G2.Packets_received),
         Assoc (
            "SINGLE13", 
            Event_log.New_events),
         Assoc (
            "SINGLE14", 
            Server.List.Available ("GWebCache2")),
         Assoc (
            "SINGLE15", 
            Server.List.Available ("Gnutella2")),
         Assoc (
            "SINGLE16", 
            S (Session_completed)),
         Assoc (
            "SINGLE17",
            Agpl.Strings.Trim (
               Misc.Image (Uptime))),
         Assoc (
            "SINGLE18",
            Agpl.Strings.To_string (
               Float (Total_packets) / Float (Uptime), 1)),
         Assoc (
            "SINGLE19",
            Searches.Manager.Get_Running_Searches),
         Assoc (
            "SINGLE20",
            Searches.Manager.Get_Paused_Searches),
         Assoc (
            "SINGLE21",
            Searches.Manager.Get_New_Hits)
         );
   end Home_handler;

   ------------------------------------------------------------------------
   -- Load_handler                                                       --
   ------------------------------------------------------------------------
   function Load_handler return Translate_table is
      In1, In5, InS : Float;
      Ou1, Ou5, OuS : Float;
      In1u, In5u, InSu : Float;
      Ou1u, Ou5u, OuSu : Float;
   begin
      BW_usage.Get_in  (In1, In5, InS, BW_usage.TCP);
      BW_usage.Get_out (Ou1, Ou5, OuS, BW_usage.TCP);
      BW_usage.Get_in  (In1u, In5u, InSu, BW_usage.UDP);
      BW_usage.Get_out (Ou1u, Ou5u, OuSu, BW_usage.UDP);

      return (
         Assoc (
            "SINGLE1", 
            Convert.To_size (In1)),
         Assoc (
            "SINGLE2", 
            Misc.To_string (
               In1 * 100.0 / 
               (Float (Globals.Options.G2_LinkBandwidth) + 
                Float (Globals.Options.Uploads_bandwidth)))),
         Assoc (
            "SINGLE14", 
            Convert.To_size (Ou1)),
         Assoc (
            "SINGLE15", 
            Misc.To_string (
               Ou1 * 100.0 / 
               (Float (Globals.Options.G2_LinkBandwidth) + 
                Float (Globals.Options.Uploads_bandwidth)))),
         Assoc (
            "SINGLE3", 
            Convert.To_size (In1u)),
         Assoc (
            "SINGLE4", 
            Misc.To_string (
               In1u / Float (Globals.Options.G2_UdpBandwidthIn) * 100.0)),
         Assoc (
            "SINGLE5", 
            Convert.To_size (Ou1u)),
         Assoc (
            "SINGLE6", 
            Misc.To_string (
               Ou1u / Float (Globals.Options.G2_UdpBandwidthOut) * 100.0)),
         Assoc (
            "SINGLE7", 
             G2.Core.Stats.Pending_udp_in),
         Assoc (
            "SINGLE8", 
            Misc.To_string (
               Float (G2.Core.Stats.Pending_udp_in) * 100.0 / 
               Float (G2.Transceiver_types.Max_packets))),
         Assoc (
            "SINGLE9", 
             G2.Core.Stats.Pending_udp_out),
         Assoc (
            "SINGLE10", 
            Misc.To_string (
               Float (G2.Core.Stats.Pending_udp_out) * 100.0 / 
               Float (G2.Transceiver_types.Max_packets))),
         Assoc (
            "SINGLE11", 
             G2.Core.Stats.Pending_udp_out_throttle),
         Assoc (
            "SINGLE12", 
            Upload.Queue.Manager.Object.Num_active_uploads + 
            Upload.Queue.Manager.Object.Num_waiting),
         Assoc (
            "SINGLE13", 
             Convert.To_size (OS.Memory.Heap_usage)),
         Assoc (
            "SINGLE16", 
             OS.Memory.Heap_usage / (1024 * 1024)),
         Assoc (
            "SINGLE17", 
             G2.Local_query.Get_concurrent_searches),
         Assoc (
            "SINGLE18", 
            Misc.To_string (
             Float (G2.Local_query.Get_concurrent_searches * 100) /
             Float (Globals.Options.Library_MaxSearches))),
         Assoc (
            "SINGLE19",
            G2.Core.Stats.Pending_udp_out_latency),
         Assoc (
            "SINGLE20",
            Natural (G2.Core.Stats.Remote_Searching_Latency)),
         Assoc (
            "SINGLE21",
            Misc.Image (G2.Core.Stats.Remote_Searching_Latency))
            );
   end Load_handler;

   ------------------------------------------------------------------------
   -- Network_handler                                                    --
   ------------------------------------------------------------------------
   function Network_Handler return Translate_table is
      Hubs : constant Natural := G2.Core.Stats.Get_Searching_Hubs;
      Thbs : constant Natural := G2.Core.Stats.Get_Searching_Candidate_Hubs;
      Leav : constant Natural := G2.Core.Stats.Get_Searching_Leaves;
      Hbdv : constant Natural := Natural'Max (Hubs, 1);
      Thdv : constant Natural := Natural'Max (Thbs, 1);
   begin
      return (
         Assoc ("SINGLE1", Thbs),
         Assoc ("SINGLE2", Hubs),
         Assoc ("SINGLE3", Leav),
         Assoc ("SINGLE4", Leav / Hbdv),
         Assoc ("SINGLE5", 100 * Hubs / Thdv),
         Assoc ("SINGLE6", Leav / 2 + Hubs)
            );
   end Network_Handler;

   ------------------------------------------------------------------------
   -- Options_handler                                                    --
   ------------------------------------------------------------------------
   procedure Options_handler (
      Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      use Agpl.Http.Server.Sort_handler;
      use Xml;
      Children : Node_array := Get_all (Globals.Config);
      -------------
      -- Add_row --
      -------------
      procedure Add_row (Option : in String; Value : in String) is
         Row : Data_row;
         UO  : Ustring := U (Option);
         UV  : Ustring := U (Value);
      begin
         -- Ad hoc exceptions:
         if Misc.Starts (Option, "/uploads/queue") then
            return;
         end if;

         Append (Row, (UO, UO));
         Append (Row, (UV, UV));
         Append (Data, Row);
      end Add_row;
      --------------------
      -- Enumerate_node --
      --------------------
      -- Goes for childs recursively.
      procedure Enumerate_node (Current : in Node; Prefix : in String) is
         Children : Node_Array      := Get_all (Current);
         Name     : constant String := Get_name (Current);
         Value    : constant String := Get_value (Current, "");
         Trimmed  : constant String := 
            Strings.Utils.Trim (Strings.Utils.Simplify (Value));
         package DCN renames Dom.Core.Nodes;
         package DCA renames Dom.Core.Attrs;
         package DC  renames Dom.Core;
      begin
         -- Primary value:
         if Trimmed /= "" then
            Add_row (Prefix & "/" & Name, Value);
         end if;
         -- Enumerate attributes
         declare
            Attrs : DC.Named_node_map := DCN.Attributes (Current);
         begin
            for N in 0 .. DCN.Length (Attrs) - 1 loop
               Add_row (
                  Prefix & "/" & Name & "/" & DCA.Name (DCN.Item (Attrs, N)),
                  DCA.Value (DCN.Item (Attrs, N)));
            end loop;
         end;
         -- Enumerate children
         for N in Children'range loop
            Enumerate_node (Children (N), Prefix & "/" & Name);
         end loop;
      end Enumerate_node;
   begin
      for N in Children'range loop
         Enumerate_node (Children (N), "");
      end loop;
   end Options_handler;

   ------------------------------------------------------------------------
   -- Uploads_handler                                                    --
   ------------------------------------------------------------------------
   Show_lost    : Boolean := true;
   Queue_filter : Ustring := Null_ustring;
   -- Uploads_sort
   procedure Uploads_sort (Data : out Agpl.Http.Server.Sort_handler.Data_set) 
   is
   begin
      Upload.Queue.Manager.Object.Http_report (
         S (Queue_filter), Show_lost, Data);
   end Uploads_sort;
   -- Uploads_single
   function Uploads_single return Templates_parser.Translate_table is
   begin
      return (
         Assoc ("QUEUE", Queue_filter),
         Assoc ("SHOW_LOST", Show_lost));
   end Uploads_single;

   Uploads_bis  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Uploads_sort'Access,
      Single    => Uploads_single'Access,
      Page      => new String'("uploads.html"));

   ------------------------------------------------------------------------
   -- Results_Handler                                                    --
   ------------------------------------------------------------------------
   Results_Redirector : Agpl.Http.Server.Sort_handler.Object (
      Source    => Searches.Manager.Http_Report_Search'Access,
      Single    => Searches.Manager.Http_Report_Search'Access,
      Page      => new String'("results.html"));
   function Results_Handler (Request : in Aws.Status.Data) return Aws.Response.Data is
      Search : Searches.Search_Id := Searches.From_String (
         Aws.Parameters.Get (Aws.Status.Parameters (Request), "target"));
   begin
      if Aws.Parameters.Get (Aws.Status.Parameters (Request), "target") /= "" then
         Searches.Manager.Http_Report_Set_Search (Search);
      end if;
      return Agpl.Http.Server.Sort_handler.Get_page (Results_Redirector, Request);
   end Results_Handler;

   ------------------------------------------------------------------------
   -- Uploads_Handler                                                    --
   ------------------------------------------------------------------------
   function Uploads_handler (Request : in Aws.Status.Data)
      return Aws.Response.Data
   is
      Params : Aws.Parameters.List := Aws.Status.Parameters (Request);
      Lost   : Boolean;
      Queue  : Ustring := U (Aws.Parameters.Get (Params, "queue"));
   begin
      begin 
         Lost := Boolean'Value (Aws.Parameters.Get (Params, "show_lost"));
      exception
         when others =>
            Lost := Show_lost;
      end;
      Show_lost    := Lost;
      if Queue = U ("show_all") then
         Queue_filter := Null_ustring;
      elsif ASU.Length (Queue) > 0 then
         Queue_filter := Queue;
      end if;

      return Agpl.Http.Server.Sort_handler.Get_page (Uploads_bis, Request);
   end Uploads_handler;
  
   Root_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Agpl.Http.Server.Single_handler.Null_singleton'Access,
      Page      => new String'("index.html"));
   Contents_Object : Agpl.Http.Server.Single_Handler.Object (
      Single    => Statistics.Tpl.Translator_Default'Access, 
      Page      => new String'("contents.html"));
   Home_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Home_handler'Access,
      Page      => new String'("home.html"));
   Library_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => File.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("library.html"));
   Hashes_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => File.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("hashes.html"));
   Uploads_object : Agpl.Http.Server.Simple_handler.Object (
      Handler   => Uploads_handler'Access);
   Ufinished_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => Upload.Log.Records.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("ufinished.html"));
   Queries_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Query.Incoming.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("queries.html"));
   Traffic_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Traffic.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("traffic.html"));
   Load_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Load_handler'Access,
      Page      => new String'("load.html"));
   Graphs_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Agpl.Http.Server.Single_handler.Null_singleton'Access,
      Page      => new String'("graphs.html"));
   Stats_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Statistics.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("stats.html"));
   Countries_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => Aenea.Countries.Report'Access, 
      Single    => Aenea.Countries.Total_hubs'Access,
      Page      => new String'("countries.html"));
   Cache_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Server.Http_report'Access, 
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("cache.html"));
   Options_object: Agpl.Http.Server.Sort_handler.Object (
      Source    => Options_handler'Access, 
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("options.html"));
   Searches_object : Agpl.Http.Server.Sort_Handler.Object (
      Source    => Searches.Manager.Http_Report'Access,
      Single    => Agpl.Http.Server.Sort_Handler.Null_Singleton'Access,
      Page      => new String'("searches.html"));
   Network_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => G2.Core.Search_Report'Access,
      Single    => Network_handler'Access,
      Page      => new String'("network.html"));
   Downloads_object : Agpl.Http.Server.Sort_handler.Object (
      Source    => Download.Manager.Http_Report_Downloads'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("downloads.html"));
   Results_Object : Agpl.Http.Server.Simple_handler.Object (
      Handler   => Results_handler'Access);
   Get_Object : Agpl.Http.Server.Single2_handler.Object (
      Single    => Get_Handler'Access,
      Page      => new String'("get.html"));

   ------------------------------------------------------------------------
   -- Register                                                           --
   ------------------------------------------------------------------------
   procedure Register is
   begin
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Hubs_Object,       Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Library_Object,    Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Hashes_Object,     Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Ufinished_Object,  Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Queries_Object,    Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Traffic_Object,    Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Stats_Object,      Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Countries_Object,  Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Events_Object,     Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Cache_Object,      Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Options_Object,    Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Searches_Object,   Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Network_Object,    Globals.Options.Gui_LinesPerPage);
      Agpl.Http.Server.Sort_Handler.Set_Lines_Per_Page (Downloads_Object,  Globals.Options.Gui_LinesPerPage);
      
      Agpl.Http.Server.Register_handler ("/",             Root_object);
      Agpl.Http.Server.Register_handler ("/contents.html",Contents_Object);
      Agpl.Http.Server.Register_handler ("/home.html",    Home_object);
      Agpl.Http.Server.Register_handler ("/hubs.html",    Hubs_object);
      Agpl.Http.Server.Register_handler ("/library.html", Library_object);
      Agpl.Http.Server.Register_handler ("/hashes.html",  Hashes_object);
      Agpl.Http.Server.Register_handler ("/uploads.html", Uploads_object);
      Agpl.Http.Server.Register_handler ("/ufinished.html", Ufinished_object);
      Agpl.Http.Server.Register_handler ("/queries.html", Queries_object);
      Agpl.Http.Server.Register_handler ("/traffic.html", Traffic_object);
      Agpl.Http.Server.Register_handler ("/load.html",    Load_object);
      Agpl.Http.Server.Register_handler ("/graphs.html",  Graphs_object);
      Agpl.Http.Server.Register_handler ("/stats.html",   Stats_object);
      Agpl.Http.Server.Register_handler ("/countries.html", Countries_object);
      Agpl.Http.Server.Register_handler ("/events.html",  Events_object);
      Agpl.Http.Server.Register_handler ("/cache.html",   Cache_object);
      Agpl.Http.Server.Register_handler ("/options.html", Options_object);
      Agpl.Http.Server.Register_handler ("/searches.html", Searches_Object);
      Agpl.Http.Server.Register_handler ("/network.html", Network_Object);
      Agpl.Http.Server.Register_handler ("/results.html", Results_Object);
      Agpl.Http.Server.Register_handler ("/get.html",     Get_Object);
      Agpl.Http.Server.Register_handler ("/downloads.html",Downloads_Object);
   end Register;

end Adagio.Gui.Handlers;
