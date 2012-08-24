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
--  $Id: adagio-g2-listener.ads,v 1.3 2004/01/21 21:05:26 Jano Exp $

with Adagio.Socket;

package Adagio.G2.Listener is

   -- Will listen on TCP/UDP port
   type Object is limited private;
   type Object_access is access all Object;

   -- Begin to listen
   procedure Start(this: in out Object; Port: Natural);

   -- Retrieve the UDP connected endpoint:
   function Get_udp (this : in Object) return Socket.Object_access;

   -- Shutdown
   procedure Shutdown(this: in out Object);

private

   task type Listener_task is
      -- Start
      entry Start(this: in Object_access; Port: in Natural);
      -- Receive a connected client
      entry Get_client(S : in Socket.Object);
      -- Shutdown
      entry Shutdown;
   end Listener_task;
   type Listener_access is access all Listener_task;

   task type Accepter_task is
      -- Blocks waiting for clients
      entry Accept_socket
        (this : in Object_access; S : in Socket.Object);
   end Accepter_task;
   type Accepter_access is access all Accepter_task;

   type Object is record
      Accepter    : Accepter_access;       -- Blocking task.
      Listener    : Listener_access;       -- Listening task.
      Udp         : aliased Socket.Object; -- Udp endpoint.
      Tcp         : Socket.Object;         -- Tcp endpoint
   end record;

end Adagio.G2.Listener;
