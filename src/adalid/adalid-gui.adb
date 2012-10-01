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
--  $Id: adalid-gui.adb,v 1.1 2004/03/22 07:14:54 Jano Exp $

with Adalid.Gui.Commands;
with Adalid.Gui.Handlers;
with Adalid.Trace;

with Adagio.Globals.Options;

with Agpl.Http.Server;

with AWS.Config;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.Log;
with AWS.Response;
with AWS.Server;
use  AWS;

with Text_io;

package body Adalid.Gui is

   -- HTTP Server instance:
   Server: AWS.Server.HTTP;

   -------------------------
   -- Exceptions_callback --
   -------------------------
   procedure Excep (
      E           : in     Ada.Exceptions.Exception_Occurrence;
      Log         : in out AWS.Log.Object;
      Error       : in     AWS.Exceptions.Data;
      Answer      : in out Response.Data) 
   is
      pragma Unreferenced (Log);
      pragma Unreferenced (Error);
      pragma Unreferenced (Answer);
      use Text_io;
   begin
      Put_line ("AWS server error: " & Trace.Report (E));
      Trace.Log ("AWS server error: " & Trace.Report (E), Trace.Error);
   end Excep;

   ----------
   -- Init --
   ----------
   procedure Init(Address : String := "127.0.0.1"; Port: Positive:= 4444) is
      Options: AWS.Config.Object;
   begin
      Agpl.Http.Server.Set_root (S (Adagio.Globals.Options.Gui_HtmlRoot));
      Agpl.Http.Server.Set_style_sheet (S (Adagio.Globals.Options.Gui_HtmlStyle));
      Agpl.Http.Server.Set_server_name (User_agent);
      Agpl.Http.Server.Register_user_pass (
         S (Adagio.Globals.Options.Gui_user),
         S (Adagio.Globals.Options.Gui_pass));

--      Gui.Bitmaps.Register;
      Gui.Commands.Register;
      Gui.Handlers.Register;

--      Gui.Graphs.Start;

      AWS.Config.Set.Server_name(Options, "Adalid");
      AWS.Config.Set.Server_host(Options, Address);
      AWS.Config.Set.Server_port(Options, Port);
      AWS.Config.Set.Session    (Options, true);
      AWS.Config.Set.Max_connection(
         Options, Adagio.Globals.Options.Gui_Listeners);
      AWS.Config.Set.Line_stack_size (
         Options, Adagio.Globals.Options.Gui_StackSize);
      AWS.Server.Set_unexpected_exception_handler (Server, Excep'Access);
      AWS.Server.Start(
         Server, Agpl.Http.Server.Callback_function'access, Options);
   end Init;

   --------------
   -- Shutdown --
   --------------
   procedure Shutdown is
   begin
      AWS.Server.Shutdown(Server);
   end Shutdown;

end Adalid.Gui;
