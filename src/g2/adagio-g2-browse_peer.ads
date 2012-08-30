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
--  $Id: adagio-g2-browse_peer.ads,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Adagio.Chronos;
with Adagio.Connect.Peer;
with Adagio.Http.Header;
with Adagio.Http.Header.Parser;
with Adagio.Library;
with Adagio.Socket;
with Adagio.Streams;
with Dynamic_vector;

with Ada.Streams;
use  Ada;

package Adagio.G2.Browse_peer is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Unknown_protocol : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Connect.Peer.Object with private;
   type Object_access is access all Object;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   -- May raise exception if some problems arise.
   function Create (From : in Socket.Object; Request : in Http.Header.Set)
      return Object_access;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id
   -- It's BROWSE/address:port, since only a browse is allowed per host
   function Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object);

   ------------------------------------------------------------------------
   -- Handshake                                                          --
   ------------------------------------------------------------------------
   -- Do the handshaking
   procedure Handshake (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Reject                                                             --
   ------------------------------------------------------------------------
   -- Do the rejections
   procedure Reject (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type);

private

   package Element_vector is new Dynamic_vector (
      Ada.Streams.Stream_element,
      Initial_size => 256 * 1024,
      Grow_factor  => 2.0);

   type Status_type is (Handshaking, Rejecting);
   type Handshaking_stages is (One, Two, Three, Four);

   use type Ada.Streams.Stream_element_offset;

   type Object is new Connect.Peer.Object with record
      Socket     : Adagio.Socket.Object;
      Link       : Adagio.Socket.Stream_access;
      Status     : Status_type := Handshaking;

      Handshake_stage : Handshaking_stages := Two;
      Headers    : Http.Header.Parser.Object (1024);
      Deflate    : Boolean := false;

      Timeout    : Chronos.Object;

      Data       : Element_vector.Object (First => 1);
      Pos        : Natural := 1; -- Next element to send
      Buffer     : Ada.Streams.Stream_element_array (1 .. 1024 * 256);
         -- For packets creation

      ZBuffer    : Adagio.Streams.Element_array_access;
      Last       : Adagio.Streams.Stream_element_offset;

      Uprod_sent : Boolean := false;

      Files      : Library.File_list.Container_type;
      Curr_file  : Library.File_list.Iterator_type;
   end record;

end Adagio.G2.Browse_peer;

