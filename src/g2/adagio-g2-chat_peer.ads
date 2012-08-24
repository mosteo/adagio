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
--  $Id: adagio-g2-chat_peer.ads,v 1.4 2004/03/10 23:49:59 Jano Exp $

with Adagio.Chronos;
with Adagio.Connect.Peer;
with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.Http.Header;
with Adagio.Http.Header.Parser;
with Adagio.Socket;

package Adagio.G2.Chat_peer is

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Unknown_protocol : exception; -- If text/plain isn't accepted.

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
   -- It's CHAT/address:port, since only a chat is allowed per host
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

   ------------------------------------------------------------------------
   -- Do_timeout                                                         --
   ------------------------------------------------------------------------
   -- Send a timeout message.
   procedure Do_timeout (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Do_chat                                                            --
   ------------------------------------------------------------------------
   -- Normal chat workings
   procedure Do_chat (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Send_phrase                                                        --
   ------------------------------------------------------------------------
   -- Send a text
   -- If success is false then the link is saturated; retry later.
   procedure Send_phrase (
      This    : in out Object; 
      Text    : in     String;
      Success : out    Boolean);

   ------------------------------------------------------------------------
   -- Receive_phrase                                                     --
   ------------------------------------------------------------------------
   -- Receives a phrase
   -- Will return Last = 0 if nothing read.
   -- It returns complete lines, will not break them.
   procedure Read_phrase (
      This : in out Object;
      Text :    out String;
      Last :    out Natural);

   ------------------------------------------------------------------------
   -- Send_packet                                                        --
   ------------------------------------------------------------------------
   -- Tries to send a packet
   procedure Send_packet (
      This    : in out Object; 
      Packet  : in     G2.Packet.Object;
      Success : out    Boolean);

private

   Chat_content_type : constant String := "application/x-gnutella2";

   type Status_type is (Handshaking, Rejecting, Connected, Timeouting);
   type Handshaking_stages is (One, Two, Three, Four);

   type Object is new Connect.Peer.Object with record
      Socket     : Adagio.Socket.Object;
      Link       : Adagio.Socket.Stream_access;
      Last_reply : Chronos.Object; -- For timeouts;
      Status     : Status_type := Handshaking;

      Nick       : Ustring;
      Guid       : String (1 .. 16);

      Incoming   : G2.Packet.Parsing.Object;
      Reply_sent : Boolean := false; -- Have we sent the auto reply?
      Handshake_stage : Handshaking_stages := Two;
      Headers    : Http.Header.Parser.Object (1024);
      Uproc_sent : Boolean := false;
      Uproc_rcv  : Boolean := false;
      Uprod_sent : Boolean := false;
      Uprod_rcv  : Boolean := false;
      Chatreq_sent : Boolean := false;
      Chatreq_rcv  : Boolean := false;
      Chatans_sent : Boolean := false;
   end record;

end Adagio.G2.Chat_peer;

