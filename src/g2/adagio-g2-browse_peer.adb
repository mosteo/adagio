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
--  $Id: adagio-g2-browse_peer.adb,v 1.3 2004/01/21 21:05:25 Jano Exp $

With
SHA1,
Adagio.Constants,
Adagio.Convert,
--Adagio.G2.Core,
Adagio.G2.Local_query,
Adagio.G2.Packet,
Adagio.Globals.Options,
Adagio.Memory_stream_constrained,
Adagio.Misc,
Adagio.Trace,
Adagio.Zutil,
Adagio.File;

Use
SHA1,
Adagio.Constants;

package body Adagio.G2.Browse_peer is

   use type Packet.Object;
   use Element_vector;

   Browse_timeout      : Duration         := 300.0;
   Browse_content_type : constant String  := "application/x-gnutella2";
   Children_per_hit    : constant Natural := 32;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   function Create (From : in Socket.Object; Request : in Http.Header.Set)
      return Object_access is
      Peer : Object_access;
   begin
      -- Check content-type
      if not Misc.Contains (Misc.To_lower (
         Http.Header.Get (Request, "Accept")), Browse_content_type)
      then
         raise Unknown_protocol;
      end if;

      -- Creation
      Peer := new Object;

      -- Check deflate
      if Misc.Contains (Misc.To_lower (
         Http.Header.Get (Request, "Accept-Encoding")), "deflate")
      then
         Peer.Deflate := true; -- Change to true when we do deflating.
      end if;

      Peer.Socket := From;
      Peer.Link   := Socket.Stream (From);
      if not Globals.Options.Library_AllowBrowse then
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
      return "BROWSE/" &
         Socket.Image (Socket.Get_peer_name (This.Socket).Addr);
   end Id;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type) is


   begin
      Context.Sleep := 0.1;
      -- Check for timeout
      if Chronos.Elapsed (This.Timeout) > Browse_timeout then
         Trace.Log ("Browse timeout for " & Id (This));
         Finalize (This);
         Context.Is_done := true;
         return;
      end if;

      -- Check for closing:
      if not Socket.Is_alive (This.Socket) then
         Trace.Log ("G2 browse connection ended by remote party.");
         Finalize (This);
         Context.Is_done := true;
         return;
      end if;

      -- Dispatch according to state:
      case This.Status is
         when Handshaking =>
            Handshake  (This, Context);
         when Rejecting =>
            Reject     (This, Context);
      end case;
   end Process;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object) is
   begin
      Socket.Close        (This.Socket);
      Adagio.Streams.Free (This.ZBuffer);
   end Finalize;

   ------------------------------------------------------------------------
   -- Handshake                                                          --
   ------------------------------------------------------------------------
   -- Do the handshaking
   procedure Handshake (
      This    : in out Object;
      Context : in out Connect.Peer.Context_type)
   is
      -- Build the response
      procedure Stage_two is
         use Memory_stream_constrained;
         use type File.Object;
         use Library.File_list;
         M    : aliased Memory_stream_constrained.Stream_type;
         P, H : Packet.Object;
         procedure Add_packet (P : in Packet.Object) is
         begin
            if P = Packet.Null_packet then
               return;
            end if;
            Create (M, This.Buffer'Address, This.Buffer'Length);
            Packet.Write (M'access, P);
            for N in 1 .. Index (M) loop
               Append (This.Data, This.Buffer (N));
            end loop;
         end Add_packet;
         C : Chronos.Object;
         Num_children : Natural := 0;
      begin
         -- UPROD
         if not This.Uprod_sent then
            P := Packet.Create_uprod;
            Add_packet (P);
            This.Uprod_sent := true;
            Library.Object.Get_all_files (This.Files);
            This.Curr_file := First (This.Files);
         end if;
         -- HITS
         if This.Curr_file /= Back (This.Files) then
            P := Local_query.Create_Simple_Hit_Skeleton;
         else
            P := Packet.Null_packet;
         end if;
         while This.Curr_file /= Back (This.Files) loop
            -- New QH
            if Num_children = Children_per_hit then
               Add_packet (P);
               P := Local_query.Create_simple_hit_skeleton;
               Num_children := 0;
            end if;
            -- New H
--            Trace.Log ("Browse: Adding file " & File.Path (
--               Element (This.Curr_file)));
            Local_query.Create_hit_child (
               Element (This.Curr_file), Packet.Big_endian (P), H);
            Packet.Add_child (P, H);
            Num_children   := Num_children + 1;
            -- Next file
            This.Curr_file := Succ (This.Curr_file);
            -- Time preempt
            if Chronos.Elapsed (C) >= 1.0 and then
               This.Curr_file /= Back (This.Files)
            then
               Add_packet (P);
               return; -- Early exit if too much time used creating payload.
            end if;
         end loop;
         Add_packet (P);

         -- Deflation?
         if This.Deflate then
            This.ZBuffer := new Ada.Streams.Stream_element_array (
               1 ..
               Ada.Streams.Stream_element_offset (Last (This.Data)) + 1024);
            Zutil.Deflate (
               Ada.Streams.Stream_element_array (
                  This.Data.Vector (1 .. Last (This.Data))),
               This.ZBuffer.all,
               This.Last);
            Trace.Log ("G2.Browse_peer: Payload ready; Size (deflated): " &
               Convert.To_size (Natural (This.Last)));
         else
            Trace.Log ("G2.Browse_peer: Payload ready; Size: " &
               Convert.To_size (Last (This.Data)));
         end if;

         This.Handshake_stage := Three;
      end Stage_two;

      -- Send the reply headers
      procedure Stage_three is
         Response : Http.Header.Set;
      begin
         Http.Header.Set_response (Response, "HTTP/1.1 200 OK");
         Http.Header.Add (Response, "Content-Type", Browse_content_type);
         Http.Header.Add (Response, "Server",       User_agent);
         Http.Header.Add (Response, "Connection",   "close");
         Http.Header.Add (Response, "Content-Length",
            Misc.To_string (Last (This.Data)));
         if This.Deflate then
            Http.Header.Add (Response, "Content-Encoding", "deflate");
         end if;
         begin
            Http.Header.Write (
               Response,
               This.Link.all,
               Send_response => true,
               Send_crlf     => true);
            Trace.Log ("G2.Browse_peer.Stage_three [Sent]: " &
               Http.Header.Write (Response));
            This.Handshake_stage := Four;
         exception
            when E : Socket.Socket_error =>
               case Socket.Get_error (E) is
                  when Socket.Operation_would_block =>
                     Trace.Log ("G2.Browse_peer.Handshaking (Three): " &
                        "Headers delayed, link full", Trace.Warning);
                  when others =>
                     raise;
               end case;
            when others =>
               raise;
         end;
      end Stage_three;

      procedure Stage_four is
      begin
         if This.Deflate then
            while This.Pos <= Natural (This.Last) loop
               begin
                  Ada.Streams.Stream_element'Write (
                     This.Link, This.ZBuffer (
                        Ada.Streams.Stream_element_offset (This.Pos)));
                  This.Pos := This.Pos + 1;
               exception
                  when E : Socket.Socket_error =>
                     case Socket.Get_error (E) is
                        when Socket.Operation_would_block =>
                           return;
                        when others =>
                           raise;
                     end case;
               end;
            end loop;
         else
            while This.Pos <= Last (This.Data) loop
               begin
                  Ada.Streams.Stream_element'Write (
                     This.Link, This.Data.Vector (This.Pos));
                  This.Pos := This.Pos + 1;
               exception
                  when E : Socket.Socket_error =>
                     case Socket.Get_error (E) is
                        when Socket.Operation_would_block =>
                           return;
                        when others =>
                           raise;
                     end case;
               end;
            end loop;
         end if;
         Socket.Close (This.Socket);
         Context.Is_done := true;
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
      Http.Header.Set_response (Response, "HTTP/1.1 404 Browse disabled");
      begin
         Http.Header.Write (
            Response,
            This.Link.all,
            Send_response => true,
            Send_crlf => true);
         Socket.Close (This.Socket);
         Context.Is_done := true;
         Finalize (This);
         return;
      exception
         when E : Socket.Socket_error =>
            case Socket.Get_error (E) is
               when Socket.Operation_would_block =>
                  Trace.Log ("G2.Browse_peer.Rejecting: " &
                     "Headers delayed, link full", Trace.Warning);
               when others =>
                  raise;
            end case;
         when others =>
            raise;
      end;
   end Reject;

end Adagio.G2.Browse_peer;

