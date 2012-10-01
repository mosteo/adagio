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
--  $Id: adagio-gui.ads,v 1.3 2004/01/21 21:05:27 Jano Exp $

-- Contains procedures pertaining to the configuration wizard

with Adagio.Folder;
with Adagio.Globals.Options;
with Adagio.Library;
with Adagio.Misc;
with Adagio.Network_Settings;
with Adagio.Types; use Adagio.Types;
with Adagio.Trace;
with Adagio.Xml;

with Agpl.Http.Server;
with Agpl.Http.Server.Single_Handler;
with Agpl.Http.Server.Single2_Handler;

with Aws.Parameters;
with Aws.Status;
with Templates_Parser;

with Gnat.Os_Lib;

with Ada.Streams.Stream_IO;

package body Adagio.Gui.Config is

   use type Types.File_Size;

   function Presentation return Templates_Parser.Translate_Table;
   function Submit (Request : in Aws.Status.Data) return Templates_Parser.Translate_Table;

   ------------------------------------------------------------------------
   -- Config_Object                                                      --
   ------------------------------------------------------------------------
   -- Returns the standard config page to fill-in.
   Config_Object : Agpl.Http.Server.Single_Handler.Object (
      Single => Presentation'Access,
      Page   => new String'("config.html"));

   ------------------------------------------------------------------------
   -- Submit_Object                                                     --
   ------------------------------------------------------------------------
   -- Determines if we are receiving changes or not and dispatchs appropriately.
   Submit_Object : Agpl.Http.Server.Single2_Handler.Object (
      Single => Submit'Access,
      Page   => new String'("config.html"));

   ------------------------------------------------------------------------
   -- Presentation                                                       --
   ------------------------------------------------------------------------
   -- Returns the values for the forms in the config page.
   function Presentation return Templates_Parser.Translate_Table is
      use Templates_Parser;
      F : constant Folder.Object := Library.Object.Get_First_Folder;
      P : Ustring;
      use type Folder.Object;
   begin
      if F = Folder.Null_Folder then
         P := U ("/p2p");
      else
         P := U (Folder.Path (F));
      end if;
      return (
         Assoc ("ALLOWBROWSE", Globals.Options.Library_AllowBrowse),
         Assoc ("SHAREDPATH", S (P)),
         Assoc ("NICK", Xml.Get_Attribute ("gProfile/identity/handle", "primary", Globals.Config, "Anonymous")),
         Assoc ("UPLOADBANDWIDTH", Misc.To_String (Natural (Globals.Options.Uploads_Bandwidth) / 1024)),
         Assoc ("INTERNETROUTE", Network_Settings.Internet_Route'Img),
         Assoc ("ACTIVESERVERS", Globals.Options.G2_ActiveServers),
         Assoc ("WARNING", Gnat.Os_Lib.Is_Regular_File ("adagio.xml")),
         Assoc ("G2PORT", Globals.options.G2_Port)
         );
   end Presentation;

   ------------------------------------------------------------------------
   -- Submit                                                             --
   ------------------------------------------------------------------------
   function Submit (Request : in Aws.Status.Data) return Templates_Parser.Translate_Table is
      use Aws;
      use Templates_Parser;
      Trans : Translate_Table (1 .. 20);
      Pos   : Positive := Trans'First + 1;

      Params: constant Parameters.List := Status.Parameters (Request);

      procedure Create_Config is
         Repl : Translate_Table (1 .. Parameters.Count (Params));
      begin
         for I in Repl'Range loop
            Repl (I) := Assoc (Parameters.Get_Name (Params, I), Parameters.Get_Value (Params, I));
         end loop;
         declare 
            Result : constant String := Parse ("config_template.xml", Repl);
            use Ada.Streams.Stream_IO;
            F : File_Type;
         begin
            Trace.Log ("Saving configuration file to " & Parameters.Get (Params, "SAVETO"), Trace.Debug);
            Create (F, Name => Parameters.Get (Params, "SAVETO"), Mode => Out_File);
            String'Write (Stream (F), Result);
            Close (F);
         exception
            when E : others =>
               Trans (Pos)     := Assoc ("ERRORSAVING", true);
               Trans (Pos + 1) := Assoc ("ERRORSAVINGTXT", "Unable to save to given file: "
                  & Trace.Report (E));
               Pos := Pos + 2;
               if Is_Open (F) then
                  Close (F);
               end if;
         end;
      end Create_Config;
   begin
      -- LIBRARY
      if not Gnat.Os_Lib.Is_Directory (Parameters.Get (Params, "SHAREDPATH")) then
         Trans (Pos)     := Assoc ("ERRORLIBRARY", true);
         Trans (Pos + 1) := Assoc ("ERRORLIBRARYTXT", "The shared path is not a valid folder.");
         Pos := Pos + 2;
      elsif Parameters.Get (Params, "SHAREDPATH") = "/" then
         Trans (Pos)     := Assoc ("ERRORLIBRARY", true);
         Trans (Pos + 1) := Assoc ("ERRORLIBRARYTXT", "DON'T SHARE YOUR ENTIRE DRIVES!!");
         Pos := Pos + 2;
      end if;

      -- PROFILE
      if Parameters.Get (Params, "NICK") = "" then
         Trans (Pos)     := Assoc ("ERRORPROFILE", true);
         Trans (Pos + 1) := Assoc ("ERRORPROFILETXT", "Please supply a nickname.");
         Pos := Pos + 2;
      end if;

      -- UPLOADS
      declare
         Vel : Speed;
      begin
         Vel := Speed'Value (Parameters.Get (Params, "UPLOADBANDWIDTH"));
      exception
         when others =>
            Trans (Pos)     := Assoc ("ERRORUPLOADS", true);
            Trans (Pos + 1) := 
               Assoc ("ERRORUPLOADSTXT", "Incorrect bandwidth value (use a integer expression).");
            Pos := Pos + 2;
      end;
         
      -- NETWORK
      declare
         Serv : Positive;
      begin
         Serv := Positive'Value (Parameters.Get (Params, "ACTIVESERVERS"));
         if Serv > 2 then
            Trans (Pos)     := Assoc ("ERRORNETWORK", true);
            Trans (Pos + 1) := 
               Assoc ("ERRORNETWORKTXT", "You can connect to 1 or 2 servers.");
         end if;
      exception
         when others =>
            Trans (Pos)     := Assoc ("ERRORNETWORK", true);
            Trans (Pos + 1) := 
               Assoc ("ERRORNETWORKTXT", "Incorrect servers value (use 1 or 2).");
            Pos := Pos + 2;
      end;

      -- SAVE
      if Parameters.Get (Params, "SAVETO") = "" then
         Trans (Pos)     := Assoc ("ERRORSAVING", true);
         Trans (Pos + 1) := Assoc ("ERRORSAVINGTXT", "Please supply a file name.");
         Pos := Pos + 2;
      end if;

      if Pos > Trans'First + 1 then
         Trans (Trans'First) := Assoc ("ERROR", true);
      else
         Trans (Trans'First) := Assoc ("OK", true);
         Create_Config;
      end if;
      Trans (Pos) := Assoc ("SAVETO", Parameters.Get (Params, "SAVETO"));
      return Trans (Trans'First .. Pos) & Presentation;
   exception
      when E : others =>
         Trace.Log ("Gui.Config.Submit: " & Trace.Report (E), Trace.Error);
         raise;
   end Submit;

   ------------------------------------------------------------------------
   -- Register                                                           --
   ------------------------------------------------------------------------
   -- Registers the configuration page handlers.
   procedure Register is
   begin
      Agpl.Http.Server.Register_Handler ("/config.html", Config_Object);
      Agpl.Http.Server.Register_Handler ("/submit_config", Submit_Object);
   end Register;
   
end Adagio.Gui.Config;
