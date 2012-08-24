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
--  $Id: adagio-g2-upload_client.adb,v 1.12 2004/02/24 15:26:10 Jano Exp $

with Adagio.Connect.Peer;
with Adagio.Connect.Peer_manager;
with Adagio.File;
with Adagio.G2.Browse_peer;
with Adagio.G2.Chat_factory;
with Adagio.G2.Mesh;
with Adagio.G2.Meshes;
with Adagio.G2.Mesh_element;
with Adagio.Globals.Options;
with Adagio.Guid;
with Adagio.Http.Header.Response;
with Adagio.Library;
with Adagio.Misc;
with Adagio.Security;
with Adagio.Socket;
with Adagio.Socket.IP;
with Adagio.Trace;
with Adagio.Upload.Resource.Factory;
with Adagio.Upload.Resource.File;
with Strings.Fields;
with Strings.Utils;
with TigerTree;

with Aws.Url;

with ada.Calendar;
with Ada.Real_time; use Ada;
with Ada.Tags;      use Ada.Tags;
with Ada.Unchecked_deallocation;

package body Adagio.G2.Upload_client is

   procedure Free is new Unchecked_deallocation (
      Stream_element_array, Stream_array_access);

   function V (This : in Upload.Resource.Handle) return 
      Upload.Resource.Object_access renames Upload.Resource.V;

   Min_poll    : constant := 15;
   Poll_window : Duration renames Globals.Options.Uploads_QueuePollWindow;
   Wait_queued : Duration := 4.0;
   Num_alt_locations : Natural renames Globals.Options.G2_AltLocations;

   ------------------------------------------------------------------------
   -- Parse for alternates                                               --
   ------------------------------------------------------------------------
   -- Add any alt-locations to the mesh as unverified.
   procedure Parse_for_alternates (This : in Object; Src : in String) is
      use Strings.Fields;
   begin
      if Src = "" then 
         return;
      end if;
      for N in 1 .. Count_fields (Src, ',') loop
         G2.Mesh.Object.Add (
            G2.Mesh_element.Create (
               Upload.Resource.Id (Upload.Resource.V (This.Resource).all),
               Select_field (Select_field (Src, N, ','), 3, '/'),
               Verified => false));
--         Trace.Log ("Adding Alt-Locations:");
--         Trace.Log ("Key  : " &
--               Upload.Resource.Id (Upload.Resource.V (This.Resource)));
--         Trace.Log ("Value: " &
--               Select_field (Select_field (Src, N, ','), 3, '/'));
      end loop;
   exception
      when E : others =>
         Trace.Log ("G2.Upload_client.Parse_alternates: " &
            Trace.Report (E) & " for " & Src);
   end Parse_for_alternates;

   ------------------------------------------------------------------------
   -- Create_pushed                                                      --
   ------------------------------------------------------------------------
   -- Creation with push pending.
   -- Receives simply a connected socket with pending push to be sent
   -- The new object is allocated in the heap.
   function Create_pushed (Addr : in Socket.Sock_addr_type) 
      return Upload.Client.Object_access is
      This : Object_access := new Object;
   begin
      Socket.Create_stream   (This.Socket);
      Socket.Set_blocking_io (This.Socket, false);
      This.Status := Push_pending;
      This.Id     := U (Socket.Image (Addr.Addr));

      Connection : 
      begin
         Socket.Connect (
            This.Socket, Socket.Image (Addr.Addr), Natural (Addr.Port));
      exception
         when E : Socket.Socket_error =>
            case Socket.Get_error (E) is
               when Socket.Operation_would_block =>
                  null; -- OK
               when others =>
                  Socket.Close (This.Socket);
                  raise;
            end case;
      end Connection;

      return Upload.Client.Object_access (This);
   exception
      when others =>
         if This /= null then
            Upload.Client.Free (Upload.Client.Object_access (This));
         end if;
         raise;
   end Create_pushed;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation regular. Headers are waiting to be read.
   -- The new object is allocated in the heap.
   function Create (Sock : in Socket.Object) 
      return Upload.Client.Object_access is
      This : Object_access := new Object;
   begin
      This.Socket := Sock;
      Socket.Set_blocking_io (This.Socket, false);
      This.Status := Handshaking;
      This.Id     := 
         U (Socket.Image (Socket.Get_peer_name (This.Socket).Addr));
      This.Link   := Stream_access (Socket.Stream (This.Socket));

      return Upload.Client.Object_access (This);
   exception
      when others =>
         if This /= null then
            Socket.Close (This.Socket);
            Upload.Client.Free (Upload.Client.Object_access (This));
         end if;
         raise;
   end Create;

   ------------------------------------------------------------------------
   -- Create_Handshaked                                                  --
   ------------------------------------------------------------------------
   -- The handshake has been read already:
   function Create_Handshaked (
      Sock : in Socket.Object; Request : in Http.Header.Set)
      return Upload.Client.Object_access
   is
      This : Object_access := new Object;
   begin
      This.Request := Request;
      This.Socket  := Sock;
      This.Id      := 
         U (Socket.Image (Socket.Get_peer_name (This.Socket).Addr));
      This.Link    := Stream_access (Socket.Stream (This.Socket));
      This.Status  := Resolving;

      return Upload.Client.Object_access (This);
   exception
      when others =>
         if This /= null then
            Socket.Close (This.Socket);
            Upload.Client.Free (Upload.Client.Object_access (This));
         end if;
         raise;
   end Create_Handshaked;

   ------------------------------------------------------------------------
   -- Push_speed                                                         --
   ------------------------------------------------------------------------
   procedure Push_speed (
      This : in out Object;
      Sent : in File_size) is
      use Ada.Calendar;
      Now : Calendar.Time := Clock;
   begin
      Average_speeds.Push (This.Avg_speed, 
        (Sent => Sent, Time => Now - This.Last_sent));
      This.Last_sent := Now;
   end Push_speed;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Do whatever processing the queued client needs.
   -- This function is invoked periodically.
   procedure Process (
      This    : in out Object;
      Context : in     Upload.Client.Queue_context;  -- Info for the client
      Result  : out    Upload.Client.Client_results  -- Info for the queue
      ) is
      
      use Real_time;
      use type File.Object;

      ----------
      -- Push --
      ----------
      procedure Push is
         Response : Http.Header.Set;
         use type Socket.Error_type;
      begin
         Http.Header.Set_response (Response, "PUSH guid:" &
            Guid.To_hex (Guid.My_guid));

         -- Connection failed?
         if (not Socket.Is_alive (This.Socket)) or else 
            Socket.Connection_failed (This.Socket)
         then
            Cancel (This);
            Raise_exception (Adagio.Upload.Client.Connection_lost'Identity,
               "Can't connect (pushing)");
            return;
         elsif not Socket.Is_writable (This.Socket) then
            return;
         else
            This.Link   := Stream_access (Socket.Stream (This.Socket));
            begin
               Http.Header.Write (Response, This.Link.all);
               -- Push done, step to handshake
               This.Status := Handshaking;
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

      -- Handshake --
      procedure Handshake is
      begin
         begin
            Http.Header.Parser.Check (This.Headers, This.Socket);
         exception
            when Socket.Socket_error =>
               if This.Is_done then
                  Result.Is_done := true;
                  return;
               else
                  raise;
               end if;
         end;
         if Http.Header.Parser.Completed (This.Headers) then
            Result.Is_done := false;
            Http.Header.Parser.Get_headers (This.Headers, This.Request);
            Trace.Log ("G2.Upload_client: Request_received:");
            Trace.Log (Http.Header.Write (This.Request));

            This.Name   := U (Http.Header.Get (This.Request, "User-Agent"));
            if Http.Header.Get (This.Request, "X-Nick") /= "" then
               This.Nick   := 
                  U (Strings.Utils.Trim (Aws.Url.Decode (
                     Http.Header.Get (This.Request, "X-Nick"))));
            end if;
            This.Listen := U (Http.Header.Get (This.Request, "Listen-IP"));

            -- Verify user-agent bans:
            if Security.Is_banned (S (This.Name)) then
               Trace.Log ("Rejecting User-Agent (banning enforced): " &
                  S (This.Name), Trace.Debug);
               Raise_exception (Upload.Client.User_agent_is_banned'Identity,
                  "User-Agent: " & S (This.Name));
            end if;

            -- Verify
            declare
               use Strings.Fields;
               use Strings.Utils;
               Response : String := Http.Header.Get_response (This.Request);
               Action   : String := Select_field (Response, 1);
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
                              This.Socket, 
                              This.Request));
                        begin
                           Connect.Peer_manager.Object.Add (P);
                        exception
                           when others =>
                              Connect.Peer.Free (P);
                              raise;
                        end;
                     end;
                     This.Reroute   := true;
                     Result.Is_done := true;
                     return;
                  else
                     Trace.Log ("G2.Upload_client.Handshake: " &
                        "Unknown request: " & Response);
                     Raise_exception (Upload.Client.Unknown_request'Identity, 
                        "Request: " & Response);
                  end if;
               elsif Response = "GET / HTTP/1.1" then
                  -- Browse host
                  declare
                     P : Connect.Peer.Object_access;
                  begin
                     P := Connect.Peer.Object_access (
                        G2.Browse_peer.Create (This.Socket, This.Request));
                     begin
                        Connect.Peer_manager.Object.Add (P);
                     exception
                        when others =>
                           Connect.Peer.Free (P);
                           raise;
                     end;
                  end;
                  This.Reroute   := true;
                  Result.Is_done := true;
                  return;
               end if;

               -- Search requested resource:
               begin
                  This.Resource :=
                     Upload.Resource.Create (
                        Upload.Resource.Factory.Create (Response));
               exception
                  when Upload.Resource.Unavailable =>
                     Trace.Log ("G2.Upload_client.Process.Handshake:" &
                        " Requested resource not found: " & Response);
                     This.Status := Rejecting;
                     return;
                  when Upload.Resource.Unknown =>
                     Trace.Log ("G2.Upload_client.Process.Handshake:" &
                        " Unknown request: " & Response, Trace.Warning);
                     This.Status := Rejecting;
                     return;
                  when Upload.Resource.Malformed_request =>
                     Trace.Log ("G2.Upload_client.Process.Handshake:" &
                        " Malformed request: " & Response, Trace.Warning);
                     This.Status := Rejecting;
                     return;
                  when E : others =>
                     Trace.Log ("G2.Upload_client.Process.Handshake: " & 
                        Trace.Report (E), Trace.Warning);
                     This.Status := Rejecting;
                     return;
               end;
            end;

            -- Alternate sources
            if Use_mesh then
               Parse_for_alternates (This,
                  Http.Header.Get (This.Request, "Alt-Location"));
               Parse_for_alternates (This,
                  Http.Header.Get (This.Request, "X-Alt-Location"));
               Parse_for_alternates (This,
                  Http.Header.Get (
                     This.Request, "X-Gnutella-Alternate-Location"));
            end if;

            -- Step to deliberations of queue manager:
            This.Status := Waiting_queuing;
            Result.Awakening := Clock;
            Trace.Log ("Received request for " & 
               Upload.Resource.Name (V (This.Resource).all), Trace.Debug);
         end if;
      end Handshake;
      ----------
      -- Resolve
      procedure Resolve is
      begin
         Result.Is_done := false;
         Trace.Log ("G2.Upload_client: Request_received:");
         Trace.Log (Http.Header.Write (This.Request));

         This.Name   := U (Http.Header.Get (This.Request, "User-Agent"));
         if Http.Header.Get (This.Request, "X-Nick") /= "" then
            This.Nick   := 
               U (Strings.Utils.Trim (Aws.Url.Decode (
                  Http.Header.Get (This.Request, "X-Nick"))));
         end if;
         This.Listen := U (Http.Header.Get (This.Request, "Listen-IP"));

         -- Verify user-agent bans:
         if Security.Is_banned (S (This.Name)) then
            Trace.Log ("Rejecting User-Agent (banning enforced): " &
               S (This.Name), Trace.Debug);
            Raise_exception (Upload.Client.User_agent_is_banned'Identity,
               "User-Agent: " & S (This.Name));
         end if;

         -- Search requested resource:
         declare
            use Strings.Fields;
            use Strings.Utils;
            Response : String := Http.Header.Get_response (This.Request);
            Action   : String := Select_field (Response, 1);
         begin
            This.Resource :=
               Upload.Resource.Create (
                  Upload.Resource.Factory.Create (Response));
         exception
            when Upload.Resource.Unavailable =>
               Trace.Log ("G2.Upload_client.Process.Resolving:" &
                  " Requested resource not found: " & Response);
               This.Status := Rejecting;
               return;
            when Upload.Resource.Unknown =>
               Trace.Log ("G2.Upload_client.Process.Resolving:" &
                  " Unknown request: " & Response, Trace.Warning);
               This.Status := Rejecting;
               return;
            when Upload.Resource.Malformed_request =>
               Trace.Log ("G2.Upload_client.Process.Resolving:" &
                  " Malformed request: " & Response, Trace.Warning);
               This.Status := Rejecting;
               return;
            when E : others =>
               Trace.Log ("G2.Upload_client.Process.Resolving: " & 
                  Trace.Report (E), Trace.Warning);
               This.Status := Rejecting;
               return;
         end;

         -- Alternate sources
         if Use_mesh then
            Parse_for_alternates (This,
               Http.Header.Get (This.Request, "Alt-Location"));
            Parse_for_alternates (This,
               Http.Header.Get (This.Request, "X-Alt-Location"));
            Parse_for_alternates (This,
               Http.Header.Get (
                  This.Request, "X-Gnutella-Alternate-Location"));
         end if;

         -- Step to deliberations of queue manager:
         This.Status := Waiting_queuing;
         Result.Awakening := Clock;
         Trace.Log ("Received request for " & 
            Upload.Resource.Name (V (This.Resource).all), Trace.Debug);
      end Resolve;
      ----------------------
      -- Queue resolution --
      procedure Queue_resolution is 
         Response : Http.Header.Set;
         Sent     : File_size := 0;

         Partial  : Boolean := Http.Header.Get (This.Request, "Range") /= "";

         Wait     : Natural := Natural'Max (Min_poll, Min_poll + 
            Natural'Min (120 - Min_poll, Context.Position * 2));
         PollMax  : Natural := Wait + Natural (Poll_window);
         Queuable : Boolean := 
            Http.Header.Get (This.Request, "X-Queue") /= "";

         function Get_start return File_size is
            use Strings.Fields;
            Start : File_size := File_size'Value (
               Select_field (Select_field (
                  Http.Header.Get (This.Request, "Range"), 2, '='), 1, '-'));
         begin
            return Start + 1;
         exception
            when Constraint_error =>
               return 1;
         end Get_start;

         use type Calendar.Time;
      begin
         Http.Header.Clear (Response);

         if Context.Must_start then

            if Partial then
               Http.Header.Set_response (Response, "HTTP/1.1 206 OK");
            else
               Http.Header.Set_response (Response, "HTTP/1.1 200 OK");
            end if;
            Http.Header.Add (Response, "Connection", "Keep-Alive");
            Result.Awakening := Clock + To_time_span (Minimum_send_delay);

            -- Create headers related with ranges:
            Http.Header.Response.Create_response (
               Response, 
               This.Request, 
               Upload.Resource.Size (V (This.Resource).all));

            This.Remaining_size := File_size'Value (
               Http.Header.Get (Response, "Content-Length"));

            if Partial then
               This.Next_to_Send := Get_start;
            else
               This.Next_To_Send := 1;
            end if;

            if Partial then
               declare
                  R : Upload.Resource.Object_access := V (This.Resource);
               begin
                  Upload.Resource.Set_position (R.all, Natural (Get_start));
               end;
            end if;

            Upload.Resource.Stream (
               V (This.Resource).all, 
               Upload.Resource.Stream_access (This.Source));
         else
            if Queuable then
               Http.Header.Set_response (Response, "HTTP/1.1 503 Queued");
               Http.Header.Add (Response, "Connection", "Keep-Alive");
               Http.Header.Add (Response, "X-Queue", 
                  "position=" & Misc.To_string (Context.Position) & "," &
                  "length=" & Misc.To_string (Context.Current_slots) & "," &
                  "pollMin=" & Misc.To_string (Wait) & "," &
                  "pollMax=" & Misc.To_string (PollMax));
               Result.Awakening := Clock + To_time_span (Wait_queued);
               
               This.nextPollMin := Calendar.Clock + Duration (Wait);
               This.NextPollMax := Calendar.Clock + Duration (PollMax);
            else
               Http.Header.Set_response (Response, "HTTP/1.1 503 Busy");
            end if;
            Http.Header.Add (Response, "Content-Length", "0");
         end if;

         -- HTTP/1.1 headers:
         Http.Header.Add (Response, "Server", 
            User_agent & " (Shareaza compatible)");
         Http.Header.Add (Response, "X-Network", "G1, G2");
         Http.Header.Add (Response, "Accept-Range", "bytes");
         Http.Header.Add (Response, "Content-Type", 
            Upload.Resource.Content_type (
               Upload.Resource.V (This.Resource).all));

         if Upload.Resource.V (This.Resource)'Tag = 
            Upload.Resource.File.Object'Tag 
         then
            Http.Header.Add (Response, "X-TigerTree-Path", 
               "/gnutella/tigertree/v3?urn:tree:tiger/:" &
               TigerTree.To_base32 (File.TTH (
                  Upload.Resource.File.File (
                     Upload.Resource.File.Object_access (
                        Upload.Resource.V (This.Resource)).all))));
         end if;

         -- ALT-LOCATIONS
         -- We report public locations or private-to-private
         if Use_mesh and then Num_alt_locations > 0 then
            declare
               E : Meshes.Element_array :=
                  Mesh.Object.Get (
                     Upload.Resource.Id (V (This.Resource).all),
                     Num_alt_locations);
               A : Ustring;
               Public : Boolean := Socket.IP.Is_public (
                  Socket.Image (Socket.Get_peer_name (This.Socket)));
               Prev   : Boolean := false;
            begin
               for N in E'Range loop
                  if Socket.IP.Is_public (
                     Mesh_element.Location (E (N))) or else
                     not Public 
                  then
                     if Prev then
                        ASU.Append (A, ", ");
                     end if;
                     ASU.Append (A, "http://");
                     ASU.Append (A, Mesh_element.Location (E (N)));
                     ASU.Append (A, "/uri-res/N2R?");
                     ASU.Append (A, Mesh_element.Key (E (N)));
                     Prev := true;
                  end if;
               end loop;
               declare
                  Alt : String := S (A);
               begin
                  if Alt /= "" then
                     Http.Header.Add (Response, "Alt-Location", Alt);
                  end if;
               end;
            end;
         end if;

--         DISABLED: I don't like this Gnutella method.
--            I'll implement it only if someday I add G1 support.
--         if not Misc.Contains (Misc.To_lower (Name (This)), "shareaza") then
--            Http.Header.Add (Response, "X-Thex-URI", 
--               "/gnutella/thex/v1?urn:tree:tiger/:" &
--               TigerTree.To_base32 (File.TTH (This.File)));
--         end if;

         begin
            Http.Header.Write (
               Response, 
               This.Link.all,
               Send_response => true,
               Send_crlf     => true);
            Trace.Log ("G2.Upload_client.Queue_resolution: Sent");
            Trace.Log (Http.Header.Write (Response));
            Sent := Http.Header.Write (Response)'Length + 2;

            if Context.Must_start then
               This.Status    := Uploading;
               This.Last_sent := Calendar.Clock;
               -- Add as alternate source:
               if Use_mesh and then S (This.Listen) /= "" then
                  G2.Mesh.Object.Add (
                     G2.Mesh_element.Create (
                        Upload.Resource.Id (
                           Upload.Resource.V (This.Resource).all),
                        S (This.Listen),
                        Verified => true));
               end if;
            elsif Queuable then
               Http.Header.Parser.Reset (This.Headers);
               This.Status := Queued;
            else
               This.Status := Done;
               Result.Is_done := true;
               return;
            end if;
         exception
            when E : Socket.Socket_error =>
               case Socket.Get_error (E) is
                  when Socket.Operation_would_block =>
                     Trace.Log ("G2.Upload_client.Queue_resolution: " &
                        "Headers delayed, link full", Trace.Warning);
                  when others =>
                     raise;
               end case;
            when others => 
               raise;
         end;

         Result.Is_done   := false;
         Result.Sent      := Sent;
         Result.Received  := 0;
      end Queue_resolution;

      ------------
      -- Upload --
      procedure Upload is
         -- Remaining to be sent in this processing:
         Remaining : File_size := File_size'Min (
            This.Remaining_size + This.Buffer_ava, 
            File_size (Context.Allowed_up));
         Chunk     : File_size;
         Start     : Time := Clock;
         Elapsed   : Time_span;
      begin
         Result.Is_uploading := true;
         loop
            -- Exit when no more to write.
            exit when 
               Remaining = 0 or 
               This.Buffer_ava + This.Remaining_size = 0;
            -- Check connection:
            if not Socket.Is_alive (This.Socket) then
               Cancel (This);
               Raise_exception (Adagio.Upload.Client.Connection_lost'Identity,
                  "Client dropped connection");
               return;
            end if;
            if not Socket.Is_writable (This.Socket) then
               Push_speed (This, Result.Sent);
               Result.Awakening := Clock + To_time_span (0.3);
--             Result.Awakening := Clock + To_time_span (Minimum_send_delay);
--             Trace.Log ("G2.Upload_client: Sending deferred (link full): "
--                         & Adagio.Upload.Client.Queue_id (This));
               return;
            end if;
            -- Refill the buffer if necessary:
            if This.Buffer_ava = 0 then
               if This.Buffer = null then
                  This.Buffer := new Stream_element_array (
                     1 .. Stream_element_offset (16 * 1024));
               end if;
               Chunk := File_size'Min (
                  This.Remaining_size, This.Buffer'Length);
               Stream_element_array'Read (This.Source, This.Buffer (1 .. 
                  Stream_element_offset (Chunk)));
               This.Buffer_pos := 1;
               This.Buffer_Ava := Chunk;
               This.Remaining_size := This.Remaining_size - Chunk;
            end if;
            -- Calculate data to be sent
            Chunk := File_size'Min (Remaining, This.Buffer_Ava);
            begin
               Write (This.Link.all, This.Buffer (
                  Stream_element_offset (This.Buffer_pos) .. 
                  Stream_element_offset (This.Buffer_pos + Chunk) - 1));

               This.Buffer_pos     := This.Buffer_pos + Chunk;
               This.Buffer_ava     := This.Buffer_ava - Chunk;
               Remaining           := Remaining - Chunk;
               Result.Sent         := Result.Sent + Chunk;
            exception
               when E : Socket.Socket_error =>
                  case Socket.Get_error (E) is
                     when Socket.Operation_would_block =>
                        --Trace.Log (
                        -- "G2.Upload_client: Sending deferred (link error): "
                        -- & Adagio.Upload.Client.Queue_id (This));
                        exit;
                     when others =>
                        raise;
                  end case;
               when others =>
                  raise;
            end;
         end loop;

         if This.Remaining_size + This.Buffer_ava = 0 then
            Http.Header.Parser.Reset (This.Headers);
            Http.Header.Clear (This.Request);
            This.Status := Handshaking;
            This.Is_done := true;
         end if;
         Elapsed := Clock - Start;
         if Elapsed < To_time_span (Minimum_send_delay) then
            Elapsed := To_time_span (Minimum_send_delay);
         end if;
         Result.Awakening := Clock + Elapsed + Elapsed;

         -- Speeds
         Push_speed (This, Result.Sent);
        
      end Upload;

      ------------------
      -- Check_queued --
      procedure Check_queued is
         use type Ada.Calendar.Time;
      begin
         if not Socket.Is_alive (This.Socket) then
            raise Adagio.Upload.Client.Connection_lost;
         elsif Calendar.Clock < This.nextPollMin and then 
            Socket.Available (This.Socket) > 0
         then
            -- Drop, polling too fast:
            raise Adagio.Upload.Client.Client_polled_too_soon;
         elsif Calendar.Clock > this.nextPollMax then
            -- Drop, no request within time frame:
            raise Adagio.Upload.Client.Client_missed_poll_deadline;
         else
            Http.Header.Parser.Check (This.Headers, This.Socket);
            if Http.Header.Parser.Completed (This.Headers) then
               This.Status := Handshaking;
               Result.Awakening := Clock;
            else
               Result.Awakening := Clock + To_time_span (Wait_queued);
            end if;
         end if;
      end Check_queued;

      ---------------
      -- Do_reject --
      procedure Do_reject is
         Success : Boolean;
      begin
         Reject (This, Adagio.Upload.Client.Unavailable, Success);
         if Success then
            Result.Is_done := true;
         else
            Result.Awakening := Clock + To_time_span (1.0);
         end if;
      end Do_reject;

   begin
      -- Defaults
      Result.Is_done   := false;
      Result.Sent      := 0;
      Result.Received  := 0;
      Result.Awakening := Clock + To_time_span (Minimum_send_delay);

      case This.Status is
         when Push_pending =>
            Push;
         when Handshaking =>
            Handshake;
         when Resolving =>
            Resolve;
         when Waiting_queuing =>
            Queue_resolution;
         when Uploading =>
            Upload;
         when Queued =>
            Check_queued;
         when Rejecting =>
            Do_reject;
         when Done =>
            Trace.Log (
               "G2.Upload_client: Event for done client", Trace.Warning);
         when others =>
            raise Unimplemented;
      end case;
   exception
      when Adagio.Upload.Client.User_agent_is_banned |
           Adagio.Upload.Client.Connection_lost |
           Adagio.Upload.Client.Client_polled_too_soon |
           Adagio.Upload.Client.Unknown_request |
           Adagio.Upload.client.Client_missed_poll_deadline | 
           Socket.Socket_error =>
           raise;
      when E : others =>
         Trace.Log ("G2.Upload_client: " & Trace.Report (E), Trace.Error);
         raise;
   end Process;

   ------------------------------------------------------------------------
   -- Resource                                                       --
   ------------------------------------------------------------------------
   function Requested_resource (This : in Object) return 
      Upload.Resource.Handle is
   begin
      return This.Resource;
   end Requested_resource;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Get an unique id for the client. Ideally should be IP independent and
   --    portable across networks.
   -- For G2, its the source IP:port
   function Id (This : in Object) return String is
   begin
      return S (This.Id);  
   end Id;

   ------------------------------------------------------------------------
   -- Speed                                                              --
   ------------------------------------------------------------------------
   function Speed (This : in Object) return Float is
   begin
      return Average_speeds.Average (This.Avg_speed) / 1024.0;
   exception
      when others =>
         return 0.0;
   end Speed;

   ------------------------------------------------------------------------
   -- Name                                                                  --
   ------------------------------------------------------------------------
   function Name (This : in Object) return String is
      Name : constant String := S (This.Name);
      Nick : constant String := S (This.Nick);
   begin
      if Nick = "" then
         return Name;
      else
         return Name & " (" & Nick & ")";
      end if;
   end Name;

   ------------------------------------------------------------------------
   -- Address                                                            --
   ------------------------------------------------------------------------
   function Address (This : in Object) return String is
   begin
      return Socket.Image (Socket.Get_peer_name (This.Socket).Addr);
   end Address;

   ------------------------------------------------------------------------
   -- Cancel                                                             --
   ------------------------------------------------------------------------
   -- Should close connection and free all resources.
   procedure Cancel (This : in out Object) is 
   begin
      if This.Buffer /= null then
         Free (This.Buffer);
      end if;
      if not This.Reroute then
         Socket.Close (This.Socket);
      end if;
   end Cancel;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      Cancel (This);
      Upload.Client.Finalize (Upload.Client.Object (This));
   end Finalize;

   function Add (L, R : in Chunk_speed) return Chunk_speed is
   begin
      return (
         Sent => L.Sent + R.Sent,
         Time => L.Time + R.Time);
   end Add;

   function Div (L : in Chunk_speed; R : in Integer) return Float is
      pragma Unreferenced (R);
   begin
      return Float (L.Sent) / Float (L.Time);
   end Div;

   ------------------------------------------------------------------------
   -- Reject                                                             --
   ------------------------------------------------------------------------
   procedure Reject (
      This   : in Object; 
      Reason : in Upload.Client.Reject_reason; 
      Done   : out Boolean) is
      use type Upload.Client.Reject_reason;
      use type Calendar.Time;
      use type Socket.Error_type;
      Resp : Http.Header.Set;
   begin
      if Reason = Upload.Client.Busy or else 
         Library.Object.Count_pending_folders > 0
      then
         Http.Header.Set_response (Resp, "HTTP/1.1 503 Busy");
         Http.Header.Add (Resp, "Retry-After", "600");
      elsif Reason = Upload.Client.Unavailable then
         Http.Header.Set_response (Resp, "HTTP/1.1 404 Unavailable");
      else
         raise Unimplemented;
      end if;

      if Socket.Is_alive (This.Socket) then
         if Socket.Is_writable (This.Socket) then
            begin
               Http.Header.Write (Resp, This.Link.all, true, true);
               Done := true;
            exception
               when E : Socket.Socket_error =>
                  if Socket.Get_error (E) = Socket.Operation_would_block then
                     Done := false;
                  else
                     raise;
                  end if;
               when others =>
                  raise;
            end;
         else
            Done := false;
         end if;
      else
         Done := true;
      end if;
   end Reject;

end Adagio.G2.Upload_client;
