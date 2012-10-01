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
--  $Id: adalid-gui-handlers.adb,v 1.1 2004/03/22 07:14:54 Jano Exp $

with Adalid.Globals;

with Adagio.Convert;
with Adagio.Globals.Options;
with Adagio.Globals;
with Adagio.Server;
with Adagio.Server.Http_report;
with Adagio.Statistics;
with Adagio.Traffic;
with Adagio.Xml;
with Strings.Utils;

with Agpl.Http.Server.Simple_handler;
with Agpl.Http.Server.Single_handler;
with Agpl.Strings;

with Aws.Parameters;
with Aws.Response;
with Aws.Status;

with Dom.Core;
with Dom.Core.Nodes;
with Dom.Core.Attrs;

with Ada.Calendar;
use  Ada;

package body Adalid.Gui.Handlers is

   use  Templates_parser;

   package Xml renames Adagio.Xml;

   ------------------------------------------------------------------------
   -- Events_singleton                                                   --
   ------------------------------------------------------------------------
   function Events_singleton return Templates_parser.Translate_table
   is
   begin
      return (1 => Assoc ("SINGLE1", Adagio.Globals.Options.Debug_Loglevel));
   end Events_singleton;

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

   Root_object : Agpl.Http.Server.Single_handler.Object (
      Single    => Agpl.Http.Server.Single_handler.Null_singleton'Access,
      Page      => new String'("index.html"));
   Traffic_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Adagio.Traffic.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("traffic.html"));
   Stats_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Adagio.Statistics.Http_report'Access,
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("stats.html"));
   Cache_object  : Agpl.Http.Server.Sort_handler.Object (
      Source    => Adagio.Server.Http_report'Access, 
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("cache.html"));
   Options_object: Agpl.Http.Server.Sort_handler.Object (
      Source    => Options_handler'Access, 
      Single    => Agpl.Http.Server.Sort_handler.Null_singleton'Access,
      Page      => new String'("options.html"));

   ------------------------------------------------------------------------
   -- Register                                                           --
   ------------------------------------------------------------------------
   procedure Register is
   begin
      Agpl.Http.Server.Register_handler ("/",             Root_object);
      Agpl.Http.Server.Register_handler ("/traffic.html", Traffic_object);
      Agpl.Http.Server.Register_handler ("/stats.html",   Stats_object);
      Agpl.Http.Server.Register_handler ("/events.html",  Events_object);
      Agpl.Http.Server.Register_handler ("/cache.html",   Cache_object);
      Agpl.Http.Server.Register_handler ("/options.html", Options_object);
   end Register;

end Adalid.Gui.Handlers;
