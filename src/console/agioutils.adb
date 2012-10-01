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
--  $Id: agioutils.adb,v 1.4 2004/03/01 18:51:52 Jano Exp $

with Adagio;
with Adagio.Misc;
with Adagio.Xml; 
use  Adagio;

with Aws.Client;
with Aws.Response;

with Gnat.Os_lib; 
use  Gnat;

with Ada.Calendar;   use Ada.Calendar;
with Ada.Command_line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_deallocation; use Ada;
with Text_io; use Text_io;

procedure Agioutils is

   use type Xml.Node;

   Config       : Xml.Document := null;

   Usage_error : Exception;

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

   ------------
   -- Server --
   ------------
   function Server return String is
   begin
      return "127.0.0.1:" & Xml.Get_attribute ("gui", "port", Config, "24444");
   end Server;

   -------------------
   -- Reload_Config --
   -------------------
   procedure Reload_Config is
      Response : Aws.Response.Data;
   begin
      Response := Aws.Client.Get (
         Server & "/command?action=reload_config",
         Xml.Get_attribute ("gui", "user", Config, ""),
         Xml.Get_attribute ("gui", "pass", Config, "")
         );
   end Reload_Config;

   ---------------------
   -- Shutdown ---------
   ---------------------
   procedure Shutdown is
      Response : Aws.Response.Data;
   begin
      Response := Aws.Client.Get (
         Server & "/command?action=shutdown",
         Xml.Get_attribute ("gui", "user", Config, ""),
         Xml.Get_attribute ("gui", "pass", Config, "")
         );
   end Shutdown;

   -----------------
   -- Load_config --
   -----------------
   procedure Load_config is
      Doc   : XML.Document;
      use Ada.Command_line;
      Config_found : Boolean := false;
   begin
      if Argument_count < 1 then
         raise Usage_error;
      end if;

      for N in 1 .. Argument_count loop
         if Argument (N) = "-f" then
            Config_found := true;
            if N + 1 > Argument_count then
               raise Usage_error;
            end if;

            if Os_lib.Is_regular_file (Argument (N + 1)) then
               Doc:= XML.Parse(Argument (N + 1));
            else
               Put_line ("Configuration file not found: " & Argument (N + 1));
            end if;
         end if;
      end loop;

      if not Config_found then
         if Os_lib.Is_regular_file ("adagio.xml") then
            Doc:= XML.Parse("adagio.xml");
         else
            -- Run on defaults
            Doc := Xml.From_string (Default_XML_config);
         end if;
      end if;

      -- Pass config to global config depots:
      Config := Doc;

   exception
      when Usage_error =>
         Put_line ("agioutils [-f config.xml] [-s] [--shutdown] [-r] [--reload-config]");
         Put_line ("   -f: Specify a configuration file.");
         Put_line ("   -s, --shutdown: Shutdown the adagio instance for the" &
                   " given config file.");
         Put_line ("   -r, --reload-config: Reloads and partially applies the configuration file.");
   end Load_config;

   procedure Do_things is
      use Ada.Command_line;
   begin
      for N in 1 .. Argument_count loop
         if Misc.To_lower (Argument (N)) = "-s" or else Misc.To_lower (Argument (N)) = "--shutdown" then
            Shutdown;
         elsif Misc.To_lower (Argument (N)) = "-r" or else Misc.To_lower (Argument (N)) = "--reload-config" then
            Reload_Config;
         end if;
      end loop;
   end Do_things;

begin
   
   Load_config;

   if Config /= null then
      Do_things;
   end if;

end Agioutils;
