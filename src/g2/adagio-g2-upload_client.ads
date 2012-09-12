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
--  $Id: adagio-g2-upload_client.ads,v 1.6 2004/02/29 20:36:42 Jano Exp $

With
Adagio.Globals.Options,
Adagio.Http,
Adagio.Http.Header,
Adagio.Http.Header.Parser,
Adagio.Socket,
Adagio.Types,
Adagio.Upload.Client,
Adagio.Upload.Resource,
Average_queue,
Ada.Calendar,
Ada.Streams;


Use
Ada,
Ada.Streams,
Adagio.Types;

package Adagio.G2.Upload_client is

   Minimum_send_delay : Duration
      renames Globals.Options.Uploads_MinimumSendDelay;

   type Object is new Upload.Client.Object with private;
   type Object_access is access all Object;
-- for Object_access'Storage_pool use Debug_pool;

   ------------------------------------------------------------------------
   -- Create_pushed                                                      --
   ------------------------------------------------------------------------
   -- Creation with push pending (connection pending).
   -- The new object is allocated in the heap.
   function Create_pushed (Addr : in Socket.Sock_addr_type)
      return Upload.Client.Object_access;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation regular. Headers are waiting to be read.
   -- The new object is allocated in the heap.
   function Create (Sock : in Socket.Object)
      return Upload.Client.Object_access;

   ------------------------------------------------------------------------
   -- Create_Handshaked                                                  --
   ------------------------------------------------------------------------
   -- The handshake has been read already:
   function Create_Handshaked (
      Sock : in Socket.Object; Request : in Http.Header.Set)
      return Upload.Client.Object_access;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Do whatever processing the queued client needs.
   -- This function is invoked periodically.
   procedure Process (
      This    : in out Object;
      Context : in     Upload.Client.Queue_context;  -- Info for the client
      Result  : out    Upload.Client.Client_results  -- Info for the queue
      );

   ------------------------------------------------------------------------
   -- Resource                                                       --
   ------------------------------------------------------------------------
   -- Get the requested resource
   function Requested_resource (This : in Object) return
      Upload.Resource.Handle;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Get an unique id for the client. Ideally should be IP independent and
   --    portable across networks.
   function Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Name                                                                  --
   ------------------------------------------------------------------------
   -- Client name (i.e. Shareaza 1.8.2.0)
   function Name (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Address                                                            --
   ------------------------------------------------------------------------
   function Address (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Reject                                                             --
   ------------------------------------------------------------------------
   procedure Reject (
      This   : in Object;
      Reason : in Upload.Client.Reject_reason;
      Done   : out Boolean);

   ------------------------------------------------------------------------
   -- Speed                                                              --
   ------------------------------------------------------------------------
   function Speed (This : in Object) return Float;

   ------------------------------------------------------------------------
   -- Cancel                                                             --
   ------------------------------------------------------------------------
   -- Should close connection and free all resources.
   procedure Cancel (This : in out Object);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object);

private

   type Chunk_speed is record
      Sent : File_size;
      Time : Duration;
   end record;

   function Add (L, R : in Chunk_speed) return Chunk_speed;
   function Div (L : in Chunk_speed; R : in Integer) return Float;

   package Average_speeds is new Average_queue (Chunk_speed, Add, Div);

   type Status_type is (
      Push_pending,        -- Connection started but not finished
      Handshaking,         -- Receiving request
      Resolving,           -- Inspecting the handshake
      Waiting_queuing,     -- Waiting for the queuing resolution
      Queued,              -- Waiting in queue
      Uploading,           -- Actively sending data
      Rejecting,           -- We are sending a 404 and dropping
      Done);

   type Stream_access is access all Root_stream_type'Class;
   type Stream_array_access is access all Stream_element_array;

   type Object is new Adagio.Upload.Client.Object with record
      Status   : Status_type;
      Socket   : Adagio.Socket.Object;
      Id       : Ustring;

      Link     : Stream_access; -- Connection with the client

      Resource : Upload.Resource.Handle;
      Source   : Stream_access; -- File we are sending

      Name    : Ustring;        -- Servant name
      Listen  : Ustring;        -- Where the other is listening
      Nick    : Ustring;        -- Uploader Nick

      Headers : Http.Header.Parser.Object (
         Ada.Streams.Stream_element_offset (Globals.Options.G2_MaxHeaders));
      Request : Http.Header.Set;
      Remaining_size : File_size;
      Next_to_send   : File_size;

      -- Data to be sent
      Buffer     : Stream_array_access; -- Fixed size intermediate.
                                        -- The true buffer is at Resource.File
      Buffer_ava : File_size := 0; -- Valid data in buffer.
      Buffer_pos : File_size := 1; -- First data to be sent.

      -- Queue things:
      nextPollMin : Calendar.Time;
      nextPollMax : Calendar.Time;

      -- Possible completion flag:
      Is_done     : Boolean := false;

      -- Speed averaging:
      Last_sent   : Calendar.Time;
      Avg_speed   : Average_speeds.Object (50);

      -- Re-routed requests
      Reroute     : Boolean := false;
   end record;

end Adagio.G2.Upload_client;
