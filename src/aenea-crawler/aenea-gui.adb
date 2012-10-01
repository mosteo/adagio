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
--  $Id: aenea-gui.adb,v 1.4 2004/03/03 00:06:04 Jano Exp $

--  with Aenea_Resources;
with Aenea.Gui.Graphs;
with Aenea.Trace;

with Adagio.Globals.Options;
with Adagio.Os.Memory;

with Agpl.Http.Server;

with AWS.Config;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.Log;
with AWS.Response;
with AWS.Server;
use  AWS;

package body Aenea.Gui is

   -- HTTP Server instance:
   Server: AWS.Server.HTTP;

   -- Max number of connected GUIs:
   Max_GUIs: constant:= 1;

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
   begin
      Trace.Log ("AWS server error: " & Trace.Report (E), Trace.Error);
   end Excep;

   ----------
   -- Init --
   ----------
   procedure Init(Address : String := "0.0.0.0"; Port: Positive:= 4444) is
      Options: AWS.Config.Object;
   begin
      Gui.Graphs.Start;
      Gui.Graphs.Set_Time_Range (24.0 * 60.0 * 60.0);

      AWS.Config.Set.Server_name(Options, "Aenea");
      AWS.Config.Set.Server_host(Options, Address);
      AWS.Config.Set.Server_port(Options, Port);
      AWS.Config.Set.Max_connection(Options, Max_GUIs);
      AWS.Server.Set_unexpected_exception_handler (Server, Excep'Access);
      Aws.Config.Set.Line_stack_size (Options, Adagio.Os.Memory.Max_Stack_Size);
      AWS.Server.Start(
         Server, Agpl.Http.Server.Callback_function'Access, Options);
      Agpl.Http.Server.Register_user_pass (
         S (Adagio.Globals.Options.Gui_user),
         S (Adagio.Globals.Options.Gui_pass));
   end Init;

   --------------
   -- Shutdown --
   --------------
   procedure Shutdown is
   begin
      AWS.Server.Shutdown(Server);
   end Shutdown;

end Aenea.Gui;
