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
--  $Id: adagio-globals.adb,v 1.5 2004/03/10 23:50:01 Jano Exp $

with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.GUID;
with Adagio.Os;
with Adagio.Xml;

with Agpl.Command_line;

with Ada.Command_line;  use  Ada.Command_line;
with Ada.Streams.Stream_io;

with Gnat.Directory_operations;
with Gnat.Os_lib; use Gnat;

pragma Elaborate_All (Agpl.Command_Line);
pragma Elaborate_All (Adagio.Os);
pragma Elaborate_All (Adagio.Xml);

package body Adagio.Globals is

   ------------------------------------------------------------------------
   -- Default_xml_config                                                 --
   ------------------------------------------------------------------------
   -- String with the default xml document to use if none is provided.
   Default_xml_config : constant String := 
      "<root>" &
      "   <uploads>" &
      "      <queue name=""small files"">" &
      "         <criteria is=""Smaller_than 1mB""/>" &
      "         <type expression=""-Bytes_sent"">rated</type>" &
      "      </queue>" &
      "      <queue name=""fifo"">" &
      "         <preemption active=""yes"" time=""10m"" size=""10mB""/>" &
      "      </queue>" &
      "      <queue name=""smaller first"">" &
      "         <preemption active=""yes"" time=""10m"" size=""10mB""/>" &
      "         <type expression=""-File_size"">rated</type>" &
      "      </queue>" &
      "      <queue name=""rare first"">" &
      "         <preemption active=""yes"" time=""10m"" size=""10mB""/>" &
      "         <type expression=""-Uploads"">rated</type>" &
      "      </queue>" &
      "   </uploads>" &
      "   <network>" &
      "      <Gnutella2 connect=""yes"">" &
      "         <root address=""127.0.0.1:36765"" rating=""10000""/> " &
      "      </Gnutella2>" &
      "      <GWebCache2 connect=""yes"">" &
      "   <root url=""http://www.gwc2.ip3.com/cgi-bin/gwc2/gcache.cgi""/> " &
      "   <root url=""http://g2cache.theg2.net/gwcache/lynnx.asp""/> " &
      "   <root url=""http://g2.instantnetworks.net/g2/bazooka.php""/> " &
      "   <root url=""http://bazooka1.servehttp.com/g2/bazooka.php""/> " &
      "      </GWebCache2>" &
      "   </network>" &
      "   <security>" &
      "      <file path=""./security.xml"" active=""yes"" /> " &
      "   </security>" &
      "   <debug> " &
      "      <ConsoleEcho>no</ConsoleEcho> " &
      "   </debug> " &
      "</root>"
      ;

   Config_file  : Ustring;
   UData_folder : Ustring;

   -- Load initial config:
   procedure Load_config is
      Doc: XML.Document;
      use type Xml.Document;
   begin
      if Argument_count = 2 and then Argument (1) = "-f" then
         if Os_lib.Is_regular_file (Argument (2)) then
            Config_file := U (Argument (2));
            Doc:= XML.Parse(Argument (2));
         else
            Os.Message_box (
               "Adagio", "Config file not found: " & Argument (2));
            raise Initialization_error;
         end if;
      else
         Config_file := U (Agpl.Command_line.Program_name & ".xml");
         if Os_lib.Is_regular_file (S (Config_file)) then
            Doc:= XML.Parse(S (Config_file));
         else
            -- Silently skip. All things will run on defaults.
            Doc := Xml.From_string (Default_XML_config);
         end if;
      end if;
      
      -- Pass config to global config depots:
      if Config /= null then
         Xml.Delete (Config);
      end if;
      Config := Doc;

      -- Get data folder:
      declare
         S : String :=
            Xml.Get_attribute ("globals/DataFolder", "path", Doc, "data");
      begin
         if S (S'Last) = OS.Folder_separator then
            UData_folder := U (S);
         else
            UData_folder := U (S & OS.Folder_separator);
         end if;
      end;

      begin
         if not Os_lib.Is_directory (Data_folder) then
            Directory_operations.Make_dir (Data_folder);
         end if;
      exception
         when others =>
            null;
      end;
      
   end Load_config;

   Lockfile : Ada.Streams.Stream_io.File_type;
   procedure Check_lock is
      Name : String := S (Config_file) & ".lock";
      use Ada.Streams.Stream_io;
   begin
      if Os_lib.Is_regular_file (Name) then
         Open (Lockfile, Out_File, Name, "shared=no");
      else
         Create (Lockfile, Out_File, Name, "shared=no");
      end if;
   exception
      when others =>
         Os.Message_Box ("Conflict",
            "There is another instance using this configuration." &
            " You shouldn't run" &
            " two Adagio instances using the same configuration files or " &
            " sharing the same data files. This instance will exit now.");
         raise Initialization_error;
   end Check_lock;

   ------------------------------------------------------------------------
   -- Data_folder                                                        --
   ------------------------------------------------------------------------
   function Data_folder return String is
   begin
      return S (UData_folder);
   end Data_folder;
   function Data_folder return UString is
   begin
      return UData_folder;
   end Data_folder;

   ------------------------------------------------------------------------
   -- Prepare_GUID                                                       --
   ------------------------------------------------------------------------
   procedure Prepare_GUID is
      shns : constant String := 
         "http://www.shareaza.com/schemas/GProfile.xsd";
      use type Xml.Node;
      Inserted : Xml.Node;
   begin
      -- Ensure we have the guid ready:
      GUID.Init;

      -- Prepare the profile branch if non-existant:
      Inserted := Xml.Get ("gProfile", Config);
      if Inserted = Xml.Null_node then
         Inserted := Xml.Add (Config, "gProfile");
      end if;
      Xml.Set_attribute (Inserted, "xmlns", shns);
      if Xml.Get ("gProfile/gnutella", Config) = Xml.Null_node then
         Inserted := Xml.Add (Inserted, "gnutella");
      end if;

      -- Insert the GUID in the profile branch:
      declare
         Pre_Id : String := GUID.To_string (GUID.My_GUID);
         -- Trim {}
         Id     : String := Pre_Id (Pre_Id'First + 1 .. Pre_Id'Last - 1);
      begin
         Xml.Set_attribute (
            Xml.Get ("gProfile/gnutella", Config), "guid", Id);
      end;
   end Prepare_GUID;

   ------------------------------------------------------------------------
   -- Remove_lock                                                        --
   ------------------------------------------------------------------------
   -- Removes the lock file for aesthetic purposes
   procedure Remove_lock is
      use Ada.Streams.Stream_IO;
   begin
      Delete (Lockfile);
   end Remove_lock;

begin
   Load_config;
   Check_lock;
end Adagio.Globals;
