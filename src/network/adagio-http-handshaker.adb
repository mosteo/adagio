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

with Adagio.Trace;

with Agpl.Streams;

with Ada.Unchecked_Deallocation;

package body Adagio.Http.Handshaker is

   package ASFBandwidth_Throttle renames
      Agpl.Streams.Filter.Bandwidth_Throttle;

   use type Ada.Streams.Stream_Element_Count;
   use type Agpl.Bandwidth_Throttle.Object_Access;
   use type Header.Set;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Say which Socket and Throttle to use.
   procedure Create (
      This     : in out Object;
      Link     : in     Socket.Object_Access;
      Throttle : in     Agpl.Bandwidth_Throttle.Object_Access) is
   begin
      This.Link     := Link;
      This.Throttle := Throttle;
      Agpl.Streams.Filter.Bandwidth_Throttle.Create (
         This.Thrott,
         Socket.Stream (Link.all),
         This.Throttle,
         This.Throttle);
   end Create;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- This proc must be called periodically until Finished is true which
   -- means the handshaking has ended.
   -- Any exception is propagated (socket closed, etc).
   -- Not to be overriden
   procedure Process (This : in out Object; Finished : out Boolean) is
   begin
      Trace.Log ("Handshake status: " & This.Status'Img, Trace.Always);
      case This.Status is
         when Waiting =>
            Finished := false;
         when Done =>
            Finished := true;
         when Sending =>
            if Socket.Is_Writable (This.Link.all) then
               if ASFBandwidth_Throttle.Get_Buffered_Write_Count (This.Thrott) > 0 
               then
                  ASFBandwidth_Throttle.Flush (This.Thrott);
               end if;
            end if;
            if ASFBandwidth_Throttle.Get_Buffered_Write_Count (This.Thrott) = 0 then
               Request_Sent (Object'Class (This), Finished);
               if Finished then
                  This.Status := Done;
               else
                  This.Status := Receiving;
               end if;
            else
               Finished := false;
            end if;
         when Receiving =>
            Header.Parser.Check (This.Parser, This.Link.all, This.Throttle);
            if Header.Parser.Completed (This.Parser) then
               declare
                  Answer : Header.Set;
                  Reply  : Header.Set;
               begin
                  Header.Parser.Get_Headers (This.Parser, Answer);
                  Got_Answer (Object'Class (This), Answer, Reply);
                  if Header.Is_Empty (Reply) then
                     Finished    := true;
                     This.Status := Done;
                  else
                     Finished    := false;
                     This.Status := Waiting;
                     Start (This, Reply);
                  end if;
               end;
            else
               Finished := false;
            end if;
      end case;
   end Process;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Used to initiate conversation. If the supplied set is empty, then
   -- a request will be awaited for. If not, the supplied set will be sent
   -- to the remote party.
   procedure Start (
      This    : in out Object; 
      Request : in     Header.Set := Header.Empty_Set) is
   begin
      if This.Status /= Waiting then
         raise Wrong_Status;
      end if;

      if not Header.Is_Empty (Request) then
         Header.Write (Request, This.Thrott, Send_CRLF => true);
         This.Status  := Sending;
      else
         This.Status  := Receiving;
      end if;
   end Start;

   procedure Finalize   (This : in out Object) is
   begin
      null;
   end Finalize;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access) is
      procedure Delete is new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      Delete (This);
   end Free;

end Adagio.Http.Handshaker;
