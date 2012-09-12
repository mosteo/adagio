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
--  $Id: adagio-g2-listener.adb,v 1.12 2004/01/21 21:05:26 Jano Exp $

-- G2 listeners: TCP and UDP

With
Adagio.Connect.Peer_Manager,
Adagio.Globals,
Adagio.Misc,
Adagio.Resolver_Tcp,
Adagio.Routing,
Adagio.Trace;

package body Adagio.G2.Listener is

   procedure Start(this: in out Object; Port: Natural) is
   begin
      -- Start accepter:
      this.Accepter := new Accepter_task;

      -- Start listener:
      this.Listener := new Listener_task;
      this.Listener.Start(this'Unrestricted_access, Port);
   end Start;

   -- Retrieve the UDP connected endpoint:
   function Get_udp (this : in Object) return Socket.Object_access is
   begin
      return this.Udp'Unrestricted_Access;
   end Get_udp;

   -- Shutdown
   procedure Shutdown(this: in out Object) is
   begin
      this.Listener.Shutdown;
   end Shutdown;

   -------------------
   -- Listener Task --
   -------------------

   task body Listener_task is
      Listening_port:   Natural;
      Exit_requested:   Boolean:= false;
      Parent:           Object_access;

      -- Connect
      procedure Connect is
      begin
         -- Tcp listener:
         Trace.Log ("G2.Listener ready at 0.0.0.0:" &
            Misc.To_String (Listening_Port), Trace.Informative);
         Socket.Create_stream (Parent.Tcp);
         Socket.Bind          (Parent.Tcp, "0.0.0.0", Listening_port);
         Socket.Listen        (Parent.Tcp, 15);
         Parent.Accepter.Accept_socket (Parent, Parent.Tcp);
         -- Udp listener:
         Socket.Create_datagram (Parent.Udp);
         Socket.Bind            (Parent.Udp, "0.0.0.0", Listening_port);
      end Connect;

      -- Disconnect
      procedure Disconnect is
      begin
         begin
            Socket.Shutdown (Parent.Tcp);
            Socket.Close    (Parent.Tcp);
         exception
            when E : others =>
               Trace.Log ("G2.Listener: " & Trace.Report (E), Trace.Debug);
         end;
         begin
            Socket.Shutdown (Parent.Udp);
            Socket.Close    (Parent.Udp);
         exception
            when E : others =>
               Trace.Log ("G2.Listener: " & Trace.Report (E), Trace.Debug);
         end;
      end Disconnect;

   begin
      select
         accept Start(this: in Object_access; Port: in Natural) do
            Trace.Log ("Listening at" & Natural'Image (Port));
            Listening_port:= Port;
            Parent:= this;
            -- Establish connections
            Connect;
         end Start;
      or
         terminate;
      end select;
      loop
         begin
            select
               -- Exit signaled by a superior being:
               accept Shutdown do
                  Exit_requested:= true;
               end;
            or
               -- Add a new client to our pipes:
               -- However, we pass it directly to the resolver.
               accept Get_client(S: in Socket.Object) do
                  Adagio.Connect.Peer_Manager.Object.Add (Resolver_Tcp.Create (S));
               end;
            or
               -- Well earned sleep:
               delay 60.0;
            end select;

            exit when Exit_requested;
         exception
            when E: others =>
               Trace.Log("G2.Listener_task [loop]: " & Trace.Report(E),
                  Trace.Error);
         end;
      end loop;
      Disconnect;
      Trace.Log("G2.Listener_task exited", Trace.Informative);
   exception
      when E: Others =>
         Trace.Log("G2.Listener_task [body]: " & Trace.Report(E),
            Trace.Error);
   end Listener_task;

   -------------------
   -- Accepter Task --
   -------------------

   task body Accepter_task is
      Conn:       Socket.Object;
      Parent:     Object_access;
   begin
      select
         accept Accept_Socket
           (this: in Object_access; S: in Socket.Object) do
            Conn:= S;
            Parent:= this;
         end Accept_socket;
      or
         terminate;
      end select;
      Main: loop
         declare
            New_client: Socket.Object;
            Proceed   : Boolean;
         begin
            -- Wait for incomings:
            begin
               Proceed := false;
               Socket.Accept_socket(Conn, New_client);
               Proceed := true;
            exception
               when E : Socket.Socket_error =>
                  exit Main when Globals.Requested_exit;
                  Proceed := false;
                  Trace.Log ("G2.Listener [Accept]: " & Trace.Report (E),
                     Trace.Error);
               when Socket.Security_ban =>
                  Proceed := false;
                  Trace.Log ("G2.Listener: Rejected incoming connection " &
                     "[security ban]", Trace.Informative);
            end;

            if Proceed then
               if Routing.TCP_Reachable (
                     Socket.Image (Socket.Get_peer_name (New_client))) then
                  -- Pass it to listener:
                  Parent.Listener.Get_client(New_client);
                  -- Remove it:
               else
                  Socket.Close (New_client);
                  Trace.Log ("G2.Listener: Closing unreachable connection.");
               end if;
            end if;

            exit when Globals.Requested_exit;
            delay 1.0;
         exception
            when E: others =>
               Trace.Log("G2.Accepter_task [loop]: " & Trace.Report(E),
                  Trace.Error);
         end;
      end loop Main;
      Socket.Close (Conn);
      Trace.Log("G2.Accepter_task exited", Trace.Informative);
   end Accepter_task;

end Adagio.G2.Listener;
