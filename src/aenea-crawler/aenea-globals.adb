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
--  $Id: aenea-globals.adb,v 1.4 2004/03/10 23:49:58 Jano Exp $

with Aenea.Os;

with Agpl.Command_line;

with Ada.Command_line;  use  Ada.Command_line;
with Ada.Streams.Stream_io;

with Gnat.Directory_operations;
with Gnat.Os_lib; use Gnat;

package body Aenea.Globals is

   use type Xml.Document;

   Config_file  : Ustring := Null_Ustring;
   UData_folder : Ustring;

   ------------------------------------------------------------------------
   -- Default_xml_config                                                 --
   ------------------------------------------------------------------------
   -- String with the default xml document to use if none is provided.
   Default_xml_config : constant String := 
      "<root/>";

   -- Load initial config:
   procedure Load_config is
      Doc     : XML.Document := null;
      Default : constant String := Agpl.Command_line.Program_name & ".xml";
   begin
      if Argument_count = 2 and then Argument (1) = "-f" then
         if Os_lib.Is_regular_file (Argument (2)) then
            Config_file := U (Argument (2));
            Doc:= XML.Parse(Argument (2));
         end if;
      else
         if Os_lib.Is_regular_file (Default) then
            Config_file := U (Default);
            Doc:= XML.Parse(Default);
         end if;
      end if;

      if Doc = null then
         Doc := Xml.From_string (Default_xml_config);
      end if;

      -- Pass config to global config depots:
      -- Free previously if needed:
      if Config /= null then
         Xml.Delete (Config);
      end if;
      Config := Doc;

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

   ----------
   -- Init --
   ----------
   -- Call it to load all configuration.
   procedure Init is
   begin
      Load_config;
   end Init;

begin

   Init;

end Aenea.Globals;
