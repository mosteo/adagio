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
--  $Id: adagio-g2-chat_peer.adb,v 1.8 2004/04/01 22:11:24 Jano Exp $

--  with Adagio.G2.Core;

With
SHA1,
Adagio.Constants,
Adagio.Globals.Options,
Adagio.GUID,
Adagio.Misc,
Adagio.Streams,
Adagio.Trace,
Adagio.Unicode;

Use
SHA1,
Adagio.Constants;


package body Adagio.G2.Chat_peer is

   use type Packet.Object;

   Chat_timeout : Duration := 300.0;

   Away_msg : Ustring renames Globals.Options.Chat_AwayMessage;
   Log_chat : Boolean renames Globals.Options.Chat_log;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   function Create (From : in Socket.Object; Request : in Http.Header.Set)
      return Object_access is
      Peer : Object_access;
   begin
      if not Misc.Contains (Misc.To_lower (
         Http.Header.Get (Request, "Accept")), Chat_content_type)
      then
         raise Unknown_protocol;
      end if;

      -- Creation
      Peer := new Object;
      Peer.Socket := From;
      Peer.Link   := Socket.Stream (From);
      if not Globals.Options.Chat_enabled then
         Peer.Status := Rejecting;
      end if;
      return Peer;
   end Create;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id
   function Id (This : in Object) return String is
   begin
      return "CHAT/" & Socket.Image (Socket.Get_peer_name (This.Socket).Addr);
   end Id;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
   begin
      Context.Sleep := 0.5;
      -- Check for timeout
      if Chronos.Elapsed (This.Last_reply) > Chat_timeout then
         if This.Status = Connected then
            This.Status := Timeouting;
         else
            Finalize (This);
            Context.Is_done := true;
            return;
         end if;
      end if;

      -- Check for closing:
      if not Socket.Is_alive (This.Socket) then
         Trace.Log ("G2 chat connection ended by remote party.",
            Trace.Informative);
         Finalize (This);
         Context.Is_done := true;
         return;
      end if;

      -- Dispatch according to state:
      case This.Status is
         when Handshaking =>
            Handshake  (Object'Class (This), Context);
         when Connected =>
            Do_chat    (Object'Class (This), Context);
         when Rejecting =>
            Reject     (Object'Class (This), Context);
         when Timeouting =>
            Do_timeout (Object'Class (This), Context);
      end case;
   end Process;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object) is
   begin
      Socket.Close (This.Socket);
   end Finalize;

   ------------------------------------------------------------------------
   -- Handshake                                                          --
   ------------------------------------------------------------------------
   -- Do the handshaking
   procedure Handshake (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
      pragma Unreferenced (Context);
      procedure Stage_two is
         Response : Http.Header.Set;
      begin
         Http.Header.Set_response (Response, "CHAT/0.2 200 OK");
         Http.Header.Add (Response, "Accept",       Chat_content_type);
         Http.Header.Add (Response, "Content-Type", Chat_content_type);
         Http.Header.Add (Response, "User-Agent",   User_agent);
         begin
            Http.Header.Write (
               Response,
               This.Link.all,
               Send_response => true,
               Send_crlf     => true);
            Trace.Log ("G2.Chat_peer.Stage_two [Sent]: " &
               Http.Header.Write (Response));
            This.Handshake_stage := Three;
         exception
            when E : Socket.Socket_error =>
               case Socket.Get_error (E) is
                  when Socket.Operation_would_block =>
                     Trace.Log ("G2.Chat_peer.Handshaking (Two): " &
                        "Headers delayed, link full", Trace.Warning);
                  when others =>
                     raise;
               end case;
            when others =>
               raise;
         end;
      end Stage_two;

      procedure Stage_three is
         Response : Http.Header.Set;
      begin
         Http.Header.Parser.Check (This.Headers, This.Socket);
         if Http.Header.Parser.Completed (This.Headers) then
            Http.Header.Parser.Get_headers (This.Headers, Response);
            Trace.Log ("G2.Chat_peer.Stage_three [Read]: " &
               Http.Header.Write (Response));
            -- Verify content-type
            if not Misc.Contains (Misc.To_lower (
               Http.Header.Get (Response, "Content-Type")), Chat_content_type)
            then
               Trace.Log ("G2.Chat_peer.Stage_three: Negotiation failed.");
               raise Unknown_protocol;
            else
               G2.Packet.Parsing.Create (
                  This.Incoming,
                  Adagio.Streams.Stream_access (Socket.Stream (This.Socket)),
                  Socket.Available_socket'Access);
               This.Handshake_stage := Four;
               Trace.Log ("G2.Chat_peer.Stage_three: Handshaking completed.");

            end if;
         end if;
      end Stage_three;

      procedure Stage_four is
         P       : Packet.Object;
         Pout    : Packet.Object;
         Paux    : Packet.Object;
         Success : Boolean;
      begin
         -- Read packets and report them
         Packet.Parsing.Check (This.Incoming, Result => P);
         if P /= Packet.Null_packet then
            Trace.Log ("CHAT<-- " & Packet.To_hex (P));
            Packet.Parsing.Trace_tree (P);
            -- Rcv UPROC
            if Packet.Is_a (P, "/UPROC") then
               This.Uproc_rcv  := true;
               This.Uprod_sent := false;
            -- Rcv UPROD
            elsif Packet.Is_a (P, "/UPROD") then
               This.Uprod_rcv  := true;
            -- Rcv CHATREQ
            elsif Packet.Is_a (P, "/CHATREQ") then
               This.Chatreq_rcv := true;
               This.Chatans_sent := false;
               This.Guid := Packet.Payload (Packet.Get_child (P, "USERGUID"));
            end if;
         end if;
         -- Send UPROC
         if false and not This.Uproc_sent then
            Pout := Packet.Create ("UPROC");
            Send_packet (This, Pout, Success);
            This.Uproc_sent := Success;
         end if;
         -- Send CHATREQ
         if not This.Chatreq_sent and false then
            Pout := Packet.Create ("CHATREQ");
            Paux := Packet.Create ("USERGUID",
               Guid.To_char_array (Guid.My_guid));
            Packet.Add_child (Pout, Paux);
            Send_packet (This, Pout, Success);
            This.Chatreq_sent := Success;
         end if;
         -- Send UPROD
         if This.Uproc_rcv and not This.Uprod_sent then
            Pout := Packet.Create_uprod;
            Send_packet (This, Pout, Success);
            This.Uprod_sent := Success;
         end if;
         -- Send CHATANS
         if This.Chatreq_rcv and not This.Chatans_sent
         then
            Pout := Packet.Create ("CHATANS");
            Paux := Packet.Create ("USERGUID", This.Guid);
            Packet.Add_child (Pout, Paux);
            Paux := Packet.Create ("ACCEPT");
            Packet.Add_child (Pout, Paux);
            Send_packet (This, Pout, Success);
            This.Chatans_sent := Success;
         end if;
         -- Step to chat
         if This.Chatans_sent then
            This.Status := Connected;
         end if;
      end Stage_four;
   begin
      case This.Handshake_stage is
         when One =>
            raise Unimplemented;
         when Two =>
            Stage_two;
         when Three =>
            Stage_three;
         when Four =>
            Stage_four;
      end case;
   end Handshake;

   ------------------------------------------------------------------------
   -- Reject                                                             --
   ------------------------------------------------------------------------
   -- Do the rejections
   procedure Reject (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
      Response : Http.Header.Set;
   begin
      Http.Header.Set_response (Response, "HTTP/1.1 404 Chat disabled");
      begin
         Http.Header.Write (
            Response,
            This.Link.all,
            Send_response => true,
            Send_crlf => true);
         Context.Is_done := true;
         Finalize (This);
         return;
      exception
         when E : Socket.Socket_error =>
            case Socket.Get_error (E) is
               when Socket.Operation_would_block =>
                  Trace.Log ("G2.Chat_peer.Rejecting: " &
                     "Headers delayed, link full", Trace.Warning);
               when others =>
                  raise;
            end case;
         when others =>
            raise;
      end;
   end Reject;

   ------------------------------------------------------------------------
   -- Do_timeout                                                         --
   ------------------------------------------------------------------------
   -- Send a timeout message.
   procedure Do_timeout (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
      Success : Boolean;
   begin
      Send_phrase (
         This, "(Automatic) Closing chat because of timeout.", Success);
      if Success then
         Context.Is_done := true;
         Finalize (This);
      end if;
   end Do_timeout;

   ------------------------------------------------------------------------
   -- Do_chat                                                            --
   ------------------------------------------------------------------------
   -- Normal chat workings
   procedure Do_chat (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
      pragma Unreferenced (Context);
      Success : Boolean;
      Text    : String (1 .. 1024);
      Last    : Natural;
   begin
      if not This.Reply_sent then
         Send_phrase (This, S (Away_msg), Success);
         This.Reply_sent := Success;
      end if;
      Read_phrase (This, Text, Last);
      if Last > 0 then
         This.Reply_sent := false;
      end if;
   end Do_chat;

   ------------------------------------------------------------------------
   -- Send_phrase                                                        --
   ------------------------------------------------------------------------
   -- Send a text
   -- If success is false then the link is saturated; retry later.
   procedure Send_phrase (
      This    : in out Object;
      Text    : in     String;
      Success : out    Boolean)
   is
      P : Packet.Object := Packet.Create ("CMSG");
      B : Packet.Object := Packet.Create ("BODY", Unicode.To_utf8 (Text));
   begin
      Packet.Add_child (P, B);
      Packet.Atomic_write (This.Link, P, Success);
      if Log_chat and then Success then
         Trace.Log (
            "CHAT/Me: " & Text, Trace.Informative,
            File => S (Globals.Options.Chat_logfile));
      end if;
   end Send_phrase;

   ------------------------------------------------------------------------
   -- Receive_phrase                                                     --
   ------------------------------------------------------------------------
   -- Receives a phrase
   -- Will return Last = 0 if nothing read.
   -- It returns complete lines, will not break them.
   procedure Read_phrase (
      This : in out Object;
      Text :    out String;
      Last :    out Natural)
   is
      P    : Packet.Object;
      Read : Ustring;
   begin
      Packet.Parsing.Check (This.Incoming, Result => P);
      if P /= Packet.Null_packet then
         Trace.Log ("CHAT<-- " & Packet.To_hex (P));
         Packet.Parsing.Trace_tree (P);

         if Packet.Is_a (P, "/CMSG/BODY") then
            Read := U (Unicode.G2_to_string (
               Packet.Payload (Packet.Get_child (P, "BODY")),
               Packet.Big_endian (P)));
            Last := Integer'Min (ASU.Length (Read), Text'Last);
            pragma Assert (Text'First = 1);
            Text (Text'First .. Last) := ASU.Slice (Read, 1, Last);
            if Log_chat then
               Trace.Log (
                  "CHAT/He: " & S (Read),
                  Trace.Informative,
                  File => S (Globals.Options.Chat_logfile));
            end if;
         end if;
      else
         Last := 0;
      end if;
   end Read_phrase;

   ------------------------------------------------------------------------
   -- Send_packet                                                        --
   ------------------------------------------------------------------------
   -- Tries to send a packet
   procedure Send_packet (
      This    : in out Object;
      Packet  : in     G2.Packet.Object;
      Success : out    Boolean) is
   begin
      delay 0.1;
      G2.Packet.Write (This.Link, Packet);
      Trace.Log ("CHAT-->" & G2.Packet.To_hex (Packet));
      G2.Packet.Parsing.Trace_tree (Packet);
      Success := true;
   exception
      when E : Socket.Socket_error =>
         case Socket.Get_error (E) is
            when Socket.Operation_would_block =>
               Success := false;
            when others =>
               raise;
         end case;
   end Send_packet;

end Adagio.G2.Chat_peer;

