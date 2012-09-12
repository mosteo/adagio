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

With
Adagio.Connect.Peer,
Adagio.Globals.Options,
Adagio.Http,
Adagio.Http.Header.Parser,
Adagio.Socket,
Ada.Streams;

Use
Ada;

package Adagio.Resolver_Tcp is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Unknown_Request : exception;
   Connection_Lost : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Adagio.Connect.Peer.Object with private;
   type Object_access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Create_Pushed                                                      --
   ------------------------------------------------------------------------
   -- Creates a resolver which will push to the given address
   function Create_pushed (Addr : in Socket.Sock_addr_type)
      return Object_Access;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creates a resolver for the given connected socket.
   function Create (Sock    : in Socket.Object) return Object_Access;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id
   -- Will be "RESOLVER:IP:Port"
   function Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   -- Do the push things.
   -- Must set This.Pushing to false when done
   procedure Push (This    : in out Object);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object);

private

   type Object is new Adagio.Connect.Peer.Object with
      record
         Link    : Socket.Object;
         Request : Http.Header.Parser.Object (
            Max_Length => Streams.Stream_Element_Offset (
               Globals.Options.Network_MaxHandshakeSize));
         Pushing : Boolean := false; -- If a outbound push is in proccess
      end record;

end Adagio.Resolver_TCP;
