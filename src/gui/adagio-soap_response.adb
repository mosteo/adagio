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
--  $Id: adagio-soap_response.adb,v 1.3 2004/01/21 21:05:27 Jano Exp $

with Adagio.Debug;
with Adagio.G2.Core;
with Adagio.Globals;
with Adagio.Library;
with Adagio.Misc;
with Adagio.Network;
with Adagio.OS;
with Adagio.Statistics;
with Adagio.Trace;
with Adagio.Upload.Queue;
with Adagio.Upload.Queue.Manager;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with AWS.Status;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.Response.Error;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Types;

use SOAP.Parameters;
use SOAP.Types;

with Ada.Calendar; use Ada.Calendar;

package body Adagio.SOAP_response is

   use type Agpl.Types.Ustrings.Ustring;

   AwsNs : constant String := "awsns:";
   pragma Unreferenced (AwsNs);

   -----------------
   -- Get_servers --
   -----------------
   function Get_servers return Soap_array is
      Net         : G2.Core.Network_access := G2.Core.Network_access (
         Network.List.Get ("Gnutella2"));
      Null_set    : Object_set (1 .. 0);
      Null_result : SOAP_array := A (Null_set, "servers");
      use type G2.Core.Network_access;
   begin
      if Net = null then
         return Null_result;
      else
         declare
            Servers      : G2.Core.Report_array := G2.Core.Report (Net.all);
            Soap_servers : Object_set (1 .. Servers'Length);
            Pos          : Integer := Servers'First;
         begin
            for N in Soap_servers'Range loop
               Soap_servers (N) := +R (
                  (+I (Servers (Pos).Uptime, "uptime"),
                   +S (S (Servers (Pos).Status), "status"),
                   +S (S (Servers (Pos).Id), "id")),
                  "server");
               Pos := Pos + 1;
            end loop;
            return A (Soap_servers, "servers");
         end;
      end if;
   exception
      when E : others =>
         Trace.Log ("Soap_response.Get_servers: " & Trace.Report (E),
            Trace.Error);
         return Null_result;
   end Get_servers;
   -------------
   -- Library --
   -------------
   function Library(Action: String; Path: String) return String is
      Success: boolean;
   begin
      if Action = "add" then
         Adagio.Library.Object.Add(Path, Success);
      elsif Action = "remove" then
         Adagio.Library.Object.Remove(Path, Success);
      elsif Action = "share" then
         Adagio.Library.Object.Share(Path, Success);
      elsif Action = "unshare" then
         Adagio.Library.Object.Unshare(Path, Success);
      else
         return "Unknown action for library: " & Action;
      end if;
      if Success then
         return "";
      else
         return "Library: Unable to complete operation: " & Action;
      end if;
   end Library;

   -------------
   -- Process --
   -------------
   function Process(Data: AWS.Status.Data) return AWS.Response.Data is
      Payload: constant SOAP.Message.Payload.Object:=
                  SOAP.Message.XML.Load_Payload(AWS.Status.Payload(Data));
      Params:  constant SOAP.Parameters.List:=
         SOAP.Message.Parameters(Payload);
      Resp:    SOAP.Message.Response.Object:=
         SOAP.Message.Response.From(Payload);
      Resp_params: SOAP.Parameters.List;
      Name_space : constant String := SOAP.Message.Name_space (Payload);
      function Create_profile(Parms: Soap.Parameters.List) return String is
         us: UString;
      begin
         for k in 1 .. Soap.Parameters.Argument_count(parms) loop
            us:= us & Soap.Types.Name(Soap.Parameters.Argument(parms, k));
            us:= us & ": ";
            us:= us & Soap.Types.Image(Soap.Parameters.Argument(parms, k));
            us:= us & "; ";
         end loop;
         return To_string(us);
      end Create_profile;
      pragma Unreferenced (Create_profile);
   begin
      if Name_space /= Adagio.Soap_response.Namespace then
         -- Return a error response 
         declare
            package SMRE renames Soap.Message.Response.Error;
            Resp : SMRE.Object := SMRE.Build (
               SMRE.Client, 
               "Wrong namespace, must be: " & Namespace);
         begin
            Trace.Log( "Soap call: Wrong namespace: " & Name_space);
            SOAP.Message.Set_name_space (Resp, Soap_response.Namespace);
            return SOAP.Message.Response.Build(Resp);
         end;
      end if;
      declare
         Procedure_name: String:=
            SOAP.Message.Payload.Procedure_name(Payload);
      begin
         -- Trace.Log
         --  ("SOAP call: " & Procedure_name & ": " & Create_profile(Params));
         -- Selection on procedure name:
         -----------
         -- SHUTDOWN
         if Procedure_name = "shutdown" then
            Globals.Requested_Exit := true;
         -----------
         -- KILL
         elsif Procedure_name = "kill" then
            OS.Kill_me;
         -----------
         -- LIBRARY
         elsif Procedure_name = "library" then
            Resp_params:= +S(Library
                              (SOAP.Parameters.Get(Params, "action"),
                               SOAP.Parameters.Get(Params, "path")),
                               "library");
            SOAP.Message.Set_parameters(Resp, Resp_params);
         ----------
         -- UPTIME
         elsif Procedure_name = "uptime" then
            Resp_params :=
               +I (Natural (Clock - Globals.Adagio_start), "uptime");
            SOAP.Message.Set_parameters(Resp, Resp_params);
         ----------
         -- SERVERS
         elsif Procedure_name = "servers" then
            Resp_params := +Get_servers;
            SOAP.Message.Set_parameters(Resp, Resp_params);
         ---------
         -- QUEUES
         elsif Procedure_name = "queues" then
            declare
               Queues : Ustring_array :=
                  Upload.Queue.Manager.Object.Report;
               OA     : Object_set (Queues'Range);
            begin
               for N in OA'Range loop
                  OA (N) := +S (S (Queues (N)), "queue");
               end loop;
               Resp_params := +A (OA, "queues");
               SOAP.Message.Set_parameters(Resp, Resp_params);
            end;
         ----------
         -- LOGS
         elsif Procedure_name = "logs" then
            declare
               Logs : Ustring_array := Trace.Get_logs;
               OA   : Object_set (Logs'Range);
            begin
               for N in OA'Range loop
                  OA (N) := +S (S (Logs (N)), "log");
               end loop;
               Resp_params := +A (OA, "logs");
               SOAP.Message.Set_parameters(Resp, Resp_params);
            end;
         ----------
         -- STATISTICS
         elsif Procedure_name = "statistics" then
            declare
               Stats : Statistics.Stat_array := Statistics.Object.Report;
               OA   : Object_set (Stats'Range);
            begin
               for N in OA'Range loop
                  OA (N) := +R ((
                     +S (S (Stats (N).Key), "key"),
                     +S (S (Stats (N).Value), "value")), 
                     "statistic");
               end loop;
               Resp_params := +A (OA, "statistics");
               SOAP.Message.Set_parameters(Resp, Resp_params);
            end;
            if Debug.Debug_statistics_enabled then
               Debug.Reset_alive_tasks;
            end if;
         ----------
         -- UPLOADS
         elsif Procedure_name = "uploads" then
            declare
               Queue : String  := Soap.Parameters.Get (Params, "queue");
               From  : Natural := Soap.Parameters.Get (Params, "from");
               Qty   : Natural := Soap.Parameters.Get (Params, "quantity");
               Kind  : String  := Soap.Parameters.Get (Params, "kind");
               Lost  : Boolean := Misc.To_lower (Kind) = "all";
               Data  : Upload.Queue.Report_array :=
                  Upload.Queue.Manager.Object.Report_queue (
                     Queue, From, Qty, Lost);
               OA    : Object_set (Data'Range);
            begin
               for N in OA'Range loop
                  declare
                     Item : Upload.Queue.Queue_report_type renames Data (N);
                  begin
                     OA (N) := +R ((
                        +S (S (Item.Client_id), "client_id"),
                        +I (Item.Position, "position"),
                        +F (Long_float (Item.Speed), "speed"),
                        +S (S (Item.Status), "status"),
                        +S (S (Item.Client), "client"),
                        +S (S (Item.File), "path")),
                        "upload");
                  end;
               end loop;
               Resp_params := +A (OA, "uploads");
               SOAP.Message.Set_parameters(Resp, Resp_params);
            end;
         end if;
      exception
         -- Unhandled errors:
         when E: others =>
            Trace.Log("Soap call: " & Trace.Report(E), Trace.Error);
            Resp_params:= +S("Exception: " & Trace.Report(E), "error");
            SOAP.Message.Set_parameters(Resp, Resp_params);
      end;
      -- Response:
      return SOAP.Message.Response.Build(Resp);
   end Process;

end Adagio.SOAP_response;
