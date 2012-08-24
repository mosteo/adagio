-- Webcache version 2
with Adagio.Constants; use Adagio.Constants;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Network;
with Adagio.Searches.Handler;
with Adagio.Server;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar;
with Ada.Streams;       use Ada;
with System;

with Pragmarc.Assignment;
with Pragmarc.Queue_bounded;

package Adagio.Gwcache2 is

   pragma Elaborate_body(Adagio.Gwcache2);

   Network_id: constant String:= "GWebCache2";

   -- Minimum time between two queries to the same cache:
   Sleep_time : Duration renames Globals.Options.GWC2_WaitPeriod;

   -- Type for results:
   type Network_node is record
      Address:       Ustring;    -- Dotted/named address.
      Port:          Natural;
   end record;

   type Network_node_array is array(Positive range <>) of Network_node;

   -- Setting the client - id
   subtype Acronyms is String (1 .. 4);
   procedure Set_client_id (
      Acronym : in Acronyms := "AGIO";
      Name    : in String   :=
         User_agent (User_agent'First + 7 .. User_agent'Last));

   -- Function to obtain a few nodes for a given network:
   -- Will query any GWCache following its internal criterion
   -- Desired can be negative and no exception will raise
   function Query_any(Network_id: in String; Desired: Integer := 20)
      return Network_node_array;

   -- Webcache network type:
   type Network_type (Target_network: access String)
      is new Network.Object with private;
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
   function Get_Search_Handler (This : in Network_Type)
      return Searches.Handler.Object_Access;

   -- Webcache server type:
   type Server_type is new Server.Object with private;
   type Server_access is access all Server_type;

   -- Creation from URL, returns allocated and initialized server:
   function Create(Url : String; Is_root : Boolean := false)
      return Server_access;
   -- Get a unique id to identify it:
   function Id(this: in Server_type) return String;
   -- Description
   function Describe(this: in Server_type) return String
      renames Id;
   -- Get network it belongs:
   function Net(this: in Server_type) return String;
   -- Evaluate its goodness to be connected:
   function Rate(this: in Server_type) return Server.Rating;
   -- True when the server is to be purged:
   function Dropable (this : in Server_type) return Boolean;
   -- Ready to connect:
   function Is_Ready (This : in Server_Type) return Boolean;
   -- Connect:
   procedure Connect(this: in out Server_type);
   -- Disconnect:
   procedure Disconnect(this: in out Server_type);
   -- Dump:
   procedure Serialize
     (Stream: access Streams.Root_stream_type'Class;
      this: in Server_type);
   for Server_type'Output use Serialize;
   -- Recover:
   function Restore
     (Stream: access Streams.Root_stream_type'Class) return Server_type;
   for Server_type'Input use Restore;

private

   Pre_cached_nodes: constant:= 1000;

   -- Auxiliary:
   procedure Assign is new Pragmarc.Assignment(Network_node);
   -- Queue type:
   package Queue is new Pragmarc.Queue_bounded(Network_node, Assign);

   ----------------
   -- Query task --
   ----------------
   task type Inquirer is
      pragma Storage_Size (1024 * 1024);
      entry Query_any(this: Network_access; Net: String);
   end Inquirer;

   -- Webcache network type:
   type Network_type(Target_network: access String) is new
      Network.Object with record
      Nodes: Queue.Handle(Pre_cached_nodes, System.Priority'Last);
      The_task: Inquirer;                             -- Task to prefetch
   end record;

   type Server_type is new Server.Object with record
      Url:                    Ustring;                -- Path to script
      Successes, Failures:    Natural:= 0;            -- Self-explanatory
      Last_access:            Calendar.Time;
                                                      -- Last try. Never we'll
                                                      -- requery faster than
                                                      -- once an hour.
      Is_root :               Boolean := false;       -- Undeletable.
   end record;


end Adagio.Gwcache2;
