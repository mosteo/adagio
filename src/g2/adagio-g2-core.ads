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
--  $Id: adagio-g2-core.ads,v 1.19 2004/03/29 19:13:30 Jano Exp $

with Adagio.Chronos;
with Adagio.G2.Listener;
with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.G2.Packet.Queue;
with Adagio.G2.Search;
with Adagio.G2.Transceiver;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Http.Header;
with Adagio.Http.Header.Parser;
with Adagio.Network;
with Adagio.Searches.Handler;
with Adagio.Server;
with Adagio.Tcp_slot;
with Adagio.User_profile;
with Adagio.Xml;

with Average_queue;
with Circular_stream;

with Agpl.Http.Server.Sort_handler;

with Zlib.Streams;

with Ada.Calendar;   use Ada;
with Ada.Streams;
with System;

with Charles.Maps.Sorted.Strings.Unbounded;

pragma Elaborate_all (Average_queue);

package Adagio.G2.Core is

   pragma Elaborate_body;

   -- Some constants for G2
   Network_id:       Constant String:= "Gnutella2";
   Content_type:     Constant String:= "application/x-gnutella2";

   -------------------
   -- Network stuff --
   -------------------

   type Network_type is new Network.Object with private;
   type Network_access is access all Network_type;

   -- Gives the network identifier
   function Id(this: in Network_type) return String;

   -- Connect to that network. Will get servers and connect them as needed.
   procedure Connect(this: in out Network_type);

   -- Disconnect:
   procedure Disconnect(this: in out Network_type);

   -- Says status of the network.
   function Status(this: in Network_type) return Network.Network_status;

   -- Obtain search handler. Can return null if the network is not to be
   -- searched:
   function Get_Search_Handler (This : in Network_type)
      return Searches.Handler.Object_Access;

   ------------------
   -- Server stuff --
   ------------------

   type Server_type is new Server.Object with private;
   type Server_access is access all Server_type;
   type Server_array is array (Positive range <>) of Server_access;
--   for Server_access'Storage_pool use Debug_pool;

   -- Get a unique id to identify it:
   function Id (this: in Server_type) return String;
   pragma Inline (Id);

   function Describe (this: in Server_type) return String;
   pragma Inline (Describe);

   -- Get network it belongs:
   function Net (this: in Server_type) return String;

   -- Evaluate its goodness to be connected:
   function Rate (this: in Server_type) return Server.Rating;

   -- Prepare everything for a new server connection attempt:
   procedure Prepare_connect (This : in out Server_type);

   -- Establish a connection:
   procedure Connect(this: in out Server_type);

   -- Do the handshaking:
   procedure Handshake(this: in out Server_type);

   -- Disconnect:
   -- If spare is true, this uptime will not be accounted for ratings.
   procedure Disconnect(this: in out Server_type);
   procedure Disconnect2(
      this: in out Server_type; Spare : in Boolean := false);
   -- Disconnects and lowers rating to 1!
   procedure Disconnect_hub(this: in String);

   -- Clear: dispose all resources
   procedure Clear (this : in out Server_type);

   -- Dump:
   procedure Serialize
     (Stream: access Ada.Streams.Root_stream_type'Class;
      this: in Server_type);
   for Server_type'Output use Serialize;

   -- True when the server is to be purged:
   function Dropable (this : in Server_type) return Boolean;

   -- True when ready to connect
   function Is_Ready (This : in Server_Type) return Boolean;

   -- Check against connection settings:
   function Reachable (this : in Server_type) return Boolean;

   -- Recover:
   function Restore
     (Stream: access Ada.Streams.Root_stream_type'Class) return Server_type;
   for Server_type'Input use Restore;

   -- Check for data to read:
   procedure Check_pipes  (this : in out Server_type);

   -- Send pending compressed data:
   procedure Send_pending (this : in out Server_type);

   -- Create a new server from a dotted adress:port
   procedure Create (
      this     : out Server_type;
      Net      : in  Network_access;
      Address  : in  String;
      Port     : in  Natural;
      Seen     : in Calendar.Time := Calendar.Clock);

   -- Compare servers by its id:
   function Equal (L, R : Server_Access) return Boolean;
   pragma Inline (Equal);

   ------------------------------------------------------------------------
   -- Add_hub                                                            --
   ------------------------------------------------------------------------
   -- Creates a hub and adds it to the cache, if possible
   procedure Add_hub (
      Address : in String; Port : in Natural; Score : in Float := 1000000.0);

   ------------------------------------------------------------------------
   -- Send                                                               --
   ------------------------------------------------------------------------
   -- Send a packet to a server
   procedure Send (
      this           : in     Server_access;
      P              : in     Packet.Object;
      In_response_to : in     Packet.Object := Packet.Null_packet);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Initialize (this : in out Server_type);
   procedure Adjust     (this : in out Server_type);
   procedure Finalize   (this : in out Server_type);

   ------------------------------------------------------------------------
   -- Add_score                                                          --
   ------------------------------------------------------------------------
   -- Adds score to a server ensuring no overflow
   procedure Add_score (This : in out Server_type; Score : in Server.Rating);

   ------------------
   -- Surveillance --
   ------------------

   -- Monitors that we are really connected.
   task type Connector_type is
      entry Start(this: in Network_access);
   end Connector_type;
   type Connector_access is access all Connector_type;

   -- Polls data from servers:
   task type Polling_type is
      pragma Storage_size (1024 * 1024);
      entry Start(this: in Network_access);
   end Polling_type;
   type Polling_access is access all Polling_type;

   -- Maintenance on servers: keep-alive, searches:
   procedure Maintenance (This : Server_access);

   ----------------
   -- Dispatcher --
   ----------------
   procedure Dispatcher (Net : access Network_type);

   ------------------------------------------------------------------------
   -- Sender                                                             --
   ------------------------------------------------------------------------
   -- Manages pending packets for the server
   procedure Sender (This : access Server_type);
   -- Manages pending outbound udp packets
   task type Sender_udp is
      Pragma Storage_size (64000);
      entry Start (Network : in Network_access);
   end Sender_udp;
   type Sender_udp_access is access all Sender_udp;


   ------------------------
   -- Processing packets --
   ------------------------
   -- Source will be null for UDP packets
   procedure Process_packet (
      Net    : in Network_access;
      Source : in Server_access;
      Item   : in Packet.Queue.Item_type);

   ----------------------
   -- Creating packets --
   ----------------------
   -- Create a LNI packet with info about us, in respect to the given server.
   function Create_LNI (
      Net         : in Network_access;
      Destination : in Server_access) return Packet.Object;

   -- Create a KHL packet
   function Create_KHL (Net : in Network_access) return Packet.Object;

   type Server_report_record is record
      Uptime : Natural;
      Status : Ustring;
      Id     : Ustring;
   end record;

   type Report_array is array (Positive range <>) of Server_report_record;

   ------------
   -- Report --
   ------------
   function Report (Net : Network_type) return Report_array;

   --------------------
   -- Connected hubs --
   --------------------
   function Connected_hubs return Natural;

   ------------------------------------------------------------------------
   -- Hubs_http_handler                                                  --
   ------------------------------------------------------------------------
   procedure Hubs_http_handler (
      Data : out Agpl.Http.Server.Sort_Handler.Data_set);

private

   use type Ada.Streams.Stream_element_count;

   Protocol_descr : constant Ustring := U ("G2 TCP");

   -- Helper
   function Get_num is new Xml.Get_numeric_attribute_from_path(Natural);

   -- Helper for availables:
   function Available_cstream (
      This : access Ada.Streams.Root_stream_type'class) return Natural;
   pragma Inline (Available_cstream);

   ---------------------
   -- Connection slot --
   ---------------------
   type Server_status is (
      Disconnected, Disconnecting, Connecting, Handshaking, Connected);

   type G2_tcp_slot is new Tcp_slot.Object with record
      Index         : Natural := 0; -- Should not be modified.
      Outbound      : aliased G2.Packet.Queue.Object;
      -- Queue for outbound messages.
      -- Aliased in queries (process_packet and local_query packages)
      Head          : Http.Header.Set; -- Headers for connection
      -- Response from server
      Http_parser   : Http.Header.Parser.Object (
         Ada.Streams.Stream_element_offset (Globals.Options.G2_MaxHeaders));
      Packet_parser : G2.Packet.Parsing.Object; -- For pipes checking.

      -- Deflate related:
      Deflate       : Boolean := false;
      -- Compressed inbound/outbound stream: go to the circular streams:
      ZStream_in    : aliased Zlib.Streams.Stream_type;
      ZStream_out   : aliased Zlib.Streams.Stream_type;
      -- Circular stream: data pendig to be sent/read:
      CStream_in    : aliased Circular_stream.Stream_type (
         Size => Ada.Streams.Stream_element_count (
            Packet.Max_packet_size * 4));
      CStream_out   : aliased Circular_stream.Stream_type (
         Size => Ada.Streams.Stream_element_count (
            Packet.Max_packet_size * 2));
      -- Buffer for outbound pending data to be written:
      OBuffer       : Ada.Streams.Stream_element_array (1 .. 1024);
      OBuffer_used  : Boolean := false;
      OLast         : Ada.Streams.Stream_element_offset;
      -- Automatic flushing of pending zdata
      ZCron         : Adagio.Chronos.Object; -- To flush streams every 5 sec.
      ZFlushed      : Boolean := true;
      ZCreated      : Boolean := false; -- Says if the ZStreams have been set.

      -- Temporary data from the server:
      User_agent    : Ustring;
   end record;
   type G2_tcp_slot_access is access all G2_tcp_slot;
   type Pool_slot is limited record
      Slot        : aliased G2_tcp_slot;
      Server      : Server_access;    -- The server!
      Status      : Server_status;    -- Server status
      In_use      : Boolean := false; -- Internal mark of valid
   end record;
   type Slot_access is access all Pool_slot;
   type Slot_array is array (Positive range <>) of Pool_slot;

   Try_servers : Natural renames Globals.Options.G2_TryServers;
   subtype Try_range is Positive range 1 .. Try_servers;

   ------------------
   -- Servers pool --
   ------------------

   -- Some related exceptions
   Storage_exhausted : Exception;

   protected type Server_pool is
      pragma Priority (System.Priority'Last);
      -- Add a server:
      -- Initially status is set to connecting
      procedure Add (this : in Server_access);

      -- Get a server by Id.
      procedure Get (
         Id     : in  String;
         Status : in  Server_status;
         Server : out Server_access);
      procedure Get (
         Id     : in  UString;
         Status : in  Server_status;
         Server : out Server_access);

      -- Get next server:
      -- Will return null if no more:
      procedure Get_next (Server : in out Server_access);

      -- Get first server:
      -- Will return null if no more:
      procedure Get_first (Server : out Server_access);

      -- Get newest connected server:
      procedure Get_newest (Server : out Server_access);

      -- Get a connected server by Id or null if not found:
      function Get (Id : in UString; Status : in Server_status)
         return Server_access;

      -- Get all servers in a given status as array
      function Get_array (Status : in Server_status) return Server_array;

      -- Get all queues of connected servers but one (optional)
      function Get_queues (But : in Server_access := null) return
         Packet.Queue.Object_array;

      -- As previous but with addresses too (includes port!):
      function Get_Queues_And_Addresses (But : in Server_Access := null)
         return Packet.Queue.Address_Queue_Array;

      -- Count valid servers in any active (not disconnected) status:
      function Active_count return Natural;

      -- Count used slots:
      function Count return Natural;

      -- Count valid servers in a given status:
      function Status_count (Status : in Server_status) return Natural;

      -- Remove a server. Only owner allowed. Must be checked out
      procedure Remove (Server : in out Server_access);
      pragma Inline (Remove);

      -- Disconnect all servers, without pity, regardeless of checking!
      procedure Disconnect_all;

      -- Read status:
      function Status (Server : in Server_access) return Server_status;
      pragma Inline (Status);

      -- Set status:
      procedure Set_status (
         Server : in Server_access;
         Status : in Server_status);
      pragma Inline (Set_status);

      -- Get an array of readable addresses of connected neighbours:
      function Address_list return Ustring_array;

      -- Get a report of all servers:
      function Report return Report_array;

   private

      -- Data:
      Slots : Slot_array (Try_range);
   end Server_pool;
   type Server_pool_access is access all Server_pool;

   -------------------
   -- Network stuff --
   -------------------

   package Cluster_list is new
      Charles.Maps.Sorted.Strings.Unbounded (Server_access, "<", Equal);

   type Network_type is new Adagio.Network.Object with
      record
         Status      :  Network.Network_status := Network.Disconnected;
         Connector   :  Connector_access;          -- Task which connects us.
         Polling     :  Polling_access;            -- Get data from servers.

         Servers     :  Server_pool;               -- Servers for the network.

         Port        :  Natural:= 4610;            -- Port for this instance

         Inbound     :  aliased Packet.Queue.Object;
         Outbound    :  aliased Packet.Queue.Object;
                                                   -- Queues for G2 packets

         Listener    :  G2.Listener.Object;        -- Listener for incomings.
         Send_udp    :  Sender_udp_access;         -- Outbound packets.

         Transceiver :  G2.Transceiver.Object_access;
                                                   -- Semi-reliable yaddayadda

         Searcher    : G2.Search.Object_access;
            -- Create only if downloading enabled
      end record;

   type Stream_access is access all Ada.Streams.Root_stream_type'Class;

   -- Auxiliaries for average uptime queue:
   function Average (Left : in Duration; Right : in Integer) return float;
   pragma Inline (Average);

   package Average_uptime is new Average_queue (Duration, "+", Average);

   ------------------
   -- Server stuff --
   ------------------
   type QRT_status_type is (Not_sent, Sending, Sent);
   type Connection_stages is (
      Starting,
      Connecting,
      Handshake_preparing,
      Handshake_sending_first,
      Handshake_receiving,
      Handshake_sending_last);

   type Server_type is new Adagio.Server.Object with record
      Slot             : G2_tcp_slot_access;       -- Connection data.
      pragma Atomic (Slot);

      Network          : Network_access;           -- Back reference.

      Address          : Ustring;                  -- Name/ip
      Port             : Natural;                  -- Port
      Local_port       : Natural;                  -- Local listening port

      Connection_stage : Connection_stages := Starting;
      Connection_start : Calendar.Time;            -- Connection start
      Handshake_start  : Calendar.Time;            -- Handshake  start
      Failures         : Natural:= 0;              -- Connection failures
      Successes        : Natural:= 0;              -- Connection successes
      Last_try_connect : Calendar.Time := Past_aeons; -- Try to connect
      Last_try         : Calendar.Time := Past_aeons; -- Updated on disconnect

      Is_root          : Boolean := false;         -- Non-discardable.

      Last_packet_time : Calendar.Time;            -- For keep-alive.
      Last_ping_time   : Calendar.Time;            -- To not flood.
      Last_update      : Calendar.Time :=
         Calendar.Time_of (1976, 9, 6);            -- LNI sends
      Last_seen        : Calendar.Time := Calendar.Clock;
                                                   -- Last succesful com.
      Delayed_send     : Calendar.Time := Past_aeons;
      Last_delay       : Duration := 0.75;
                                                   -- All tcp packets will
                                                   -- be delayed until this
                                                   -- time.

      Max_leaves       : Natural := 0;             -- Leaves info
      Num_leaves       : Natural := 0;

      Uptimes          : Average_uptime.Object (5);  -- Average uptimes
      Score            : Server.Rating := 300.0;     -- Goodness
      pragma Atomic (Score);

      User_profile     : Adagio.User_profile.Object; -- User profile
      Profile_requested: Boolean := false;

      QRT_timestamp    : Calendar.Time := Past_aeons;
      Checked_QRP      : Calendar.Time := Past_aeons;
      QRT_reset        : Boolean := false;
      QRT_status       : QRT_status_type := Not_sent;
      QRT_packets      : Natural;
      QRT_packets_sent : Natural;

      Apt_For_Search   : Boolean := false; -- 15 seconds before sending searches to it

      Packets_in       : Natural := 0; -- Sent and received packets
      Packets_out      : Natural := 0;
   end record;

   ----------------------
   -- Network instance --
   ----------------------
   The_network: G2.Core.Network_access;

end Adagio.G2.Core;
