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
--  $Id: adagio-connect-peer.ads,v 1.3 2004/01/21 21:05:36 Jano Exp $

--  Will handshake with a unknown TCP source until its purpose is know.

with Adagio.Connect.Peer_Manager;
with Adagio.G2.Browse_Peer;
with Adagio.G2.Chat_Factory;
with Adagio.G2.Upload_Client;
with Adagio.Globals.Options;
with Adagio.Guid;
with Adagio.Socket;
with Adagio.Trace;
with Adagio.Upload;
with Adagio.Upload.Client;
with Adagio.Upload.Queue.Manager;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Strings.Fields;

with Ada.Exceptions; use Ada.Exceptions;

package body Adagio.Resolver_Tcp is

   ------------------------------------------------------------------------
   -- Create_Pushed                                                      --
   ------------------------------------------------------------------------
   function Create_pushed (Addr : in Socket.Sock_addr_type) 
      return Object_access 
   is
      This : Object_access := new Object;
   begin
      Socket.Create_stream   (This.Link);
      Socket.Set_blocking_io (This.Link, false);
      This.Pushing := true;

      Connection : 
      begin
         Socket.Connect (
            This.Link, Socket.Image (Addr.Addr), Natural (Addr.Port));
      exception
         when E : Socket.Socket_error =>
            case Socket.Get_error (E) is
               when Socket.Operation_would_block =>
                  null; -- OK
               when others =>
                  Socket.Close (This.Link);
                  raise;
            end case;
      end Connection;

      return This;
   exception
      when others =>
         if This /= null then
            Connect.Peer.Free (Connect.Peer.Object_Access (This));
         end if;
         raise;
   end Create_pushed;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creates a resolver for the given connected socket.
   function Create (Sock    : in Socket.Object) return Object_Access
   is
      This : Object_Access := new Object;
   begin
      This.Link    := Sock;
      Socket.Set_Blocking_Io (This.Link, false);
      return This;
   end Create;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id
   -- Will be "RESOLVER:IP:Port"
   function Id (This : in Object) return String is
   begin
      return "RESOLVER:" & Socket.Image (Socket.Get_Peer_Name (This.Link));
   end Id; 

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Process until headers received, then create according managers.
   procedure Process (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type)
   is
   begin
      if This.Pushing then
         Push (This);
         if This.Pushing then
            return; -- <-- Pushing still not ended
         end if;
      end if;

      begin
         Http.Header.Parser.Check (This.Request, This.Link);
      exception
         when Socket.Socket_error =>
            Trace.Log (
               "Connection lost resolving request: " & Id (This), 
               Trace.Informative);
            Context.Is_Done := true;
            return;
      end;

      -- Early exit if not completed
      if not Http.Header.Parser.Completed (This.Request) then
         return;
      end if;

      -- Request completed by remote party, resolve it:
      declare
         Headers : Http.Header.Set;
      begin
         Context.Is_done := true;
         Http.Header.Parser.Get_headers (This.Request, Headers);

         Trace.Log (
            "Resolver: Headers_received:", File => S (Globals.Options.Debug_NetLogfile));
         Trace.Log (
            Http.Header.Write (Headers), File => S (Globals.Options.Debug_NetLogfile));

         -- Dispatch
         declare
            use Strings.Fields;
            Response : constant String := Http.Header.Get_response (Headers);
            Action   : constant String := Select_field (Response, 1);
         begin
            if Action /= "GET" then
               -- Look for something else
               -- CHAT
               if Action = "CHAT" then
                  declare
                     P : Connect.Peer.Object_access;
                  begin
                     P := Connect.Peer.Object_access (
                        G2.Chat_Factory.Create (
                           S (Globals.Options.Chat_answer),
                           This.Link, 
                           Headers));
                     begin
                        Connect.Peer_manager.Object.Add (P);
                     exception
                        when others =>
                           Connect.Peer.Free (P);
                           raise;
                     end;
                  end;
                  return;
               else
                  Trace.Log ("Resolver: " &
                     "Unknown request: " & Response, Trace.Warning);
                  Raise_exception (
                     Unknown_Request'Identity, "Headers: " & Response);
               end if;
            elsif Response = "GET / HTTP/1.1" then
               -- Browse host
               declare
                  P : Connect.Peer.Object_access;
               begin
                  P := Connect.Peer.Object_access (
                     G2.Browse_peer.Create (This.Link, Headers));
                  begin
                     Connect.Peer_manager.Object.Add (P);
                  exception
                     when others =>
                        Connect.Peer.Free (P);
                        raise;
                  end;
               end;
               return;
            else
               -- Regular upload, create:
               declare
                  P : Upload.Client.Object_Access;
               begin
                  P := G2.Upload_Client.Create_Handshaked (This.Link, Headers);
                  Upload.Queue.Manager.Object.Enqueue (P);
               end;
            end if;
         end;
      end;
   end Process;

   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   -- Do the push things.
   -- Must set This.Pushing to false when done
   procedure Push (This : in out Object) is
      Response : Http.Header.Set;
      use type Socket.Error_type;
   begin
      Http.Header.Set_response (
         Response, "PUSH guid:" & Guid.To_hex (Guid.My_guid));

      -- Connection alive?
      if (not Socket.Is_alive (This.Link)) or else 
         Socket.Connection_failed (This.Link)
      then
         Raise_exception (Connection_lost'Identity, "Can't connect (pushing)");
         return;
      elsif not Socket.Is_writable (This.Link) then
         return;
      else
         begin
            -- Atomic write, either succeeds or fails:
            Http.Header.Write (Response, Socket.Stream (This.Link).all);
            -- Push done, step to handshake:
            This.Pushing := false;
         exception
            when E : Socket.Socket_error =>
               if Socket.Get_error (E) = Socket.Operation_would_block then
                  return;
               else
                  raise;
               end if;
         end;
      end if;
   end Push;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object) is
      pragma Unreferenced (This);
   begin 
      null;
      -- Nothing to do, since the socket must remain open for the resolved task.
   end Finalize;

end Adagio.Resolver_TCP;
