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
--  $Id: adagio-http.ads,v 1.3 2004/01/21 21:05:37 Jano Exp $

-- Abstract root for an object that does a handshake (optionally throttled)
-- to a given connected socket. You must overwrite the abstract functions to
-- process/generate the necessary headers.

with Adagio.Http.Header.Parser;
with Adagio.Socket;

with Agpl.Bandwidth_Throttle;
with Agpl.Streams.Filter.Bandwidth_Throttle;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Finalization;
with Ada.Streams;

package Adagio.Http.Handshaker is

   -- Some convenience headers to use:
   Connection_Close,
   Connection_Keep_Alive : constant Header.Object;

   type Object (
      Max_Data   :        Ada.Streams.Stream_Element_Count -- Max headers size
      )
   is abstract tagged limited private;

   type Object_Access is access all Object'Class;

   -- Raised when a inappropriate function is called:
   Wrong_Status : exception;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Say which Socket and Throttle to use.
   -- If throttle = null, no throttling will be applied.
   procedure Create (
      This     : in out Object;
      Link     : in     Socket.Object_Access;
      Throttle : in     Agpl.Bandwidth_Throttle.Object_Access);

   ------------------------------------------------------------------------
   -- Got_Answer                                                         --
   ------------------------------------------------------------------------
   -- Called when we receive some answer.
   -- Overload it to do any checking on the headers received and to supply
   -- a new handshake set. If the set is empty, the handshaking is considered
   -- done and the next call to Process will return Finished (and can be safely omitted).
   -- Note that if a "Connection: Close" is in Answer, you can suply Reply but
   -- probably the socket will be closed in the remote side anyway.
   procedure Got_Answer (
      This   : in     Object; 
      Answer : in     Header.Set;
      Reply  :    out Header.Set) is abstract;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- This proc must be called periodically until Finished is true which
   -- means the handshaking has ended.
   -- Any exception is propagated (socket closed, etc).
   -- Not to be overriden
   procedure Process (This : in out Object; Finished : out Boolean);

   ------------------------------------------------------------------------
   -- Request_Sent                                                       --
   ------------------------------------------------------------------------
   -- Called when our request has been completely sent.
   -- Set Finished to instruct no wait for a new reply.
   -- Process will report Finished in its next call.
   procedure Request_Sent (This : in out Object; Finished : out Boolean) is abstract;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Used to initiate conversation. If the supplied set is empty, then
   -- a request will be awaited for. If not, the supplied set will be sent
   -- to the remote party.
   procedure Start (
      This    : in out Object; 
      Request : in     Header.Set := Header.Empty_Set);

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access);

private

   Connection_Close      : constant Header.Object := (U ("Connection"), U ("Close"));
   Connection_Keep_Alive : constant Header.Object := (U ("Connection"), U ("Keep-Alive"));

   type Status_Type is (Waiting, Sending, Receiving, Done);
   
   type Object (
      Max_Data   :        Ada.Streams.Stream_Element_Count
      )
   is abstract new Ada.Finalization.Limited_Controlled with record
      Link    : Socket.Object_Access;
      Throttle: Agpl.Bandwidth_Throttle.Object_Access;
      Thrott  : Agpl.Streams.Filter.Bandwidth_Throttle.Stream_Type;
      Parser  : Header.Parser.Object (Max_Data);
      Status  : Status_Type := Waiting;
   end record;

   procedure Finalize   (This : in out Object);

end Adagio.Http.Handshaker;
