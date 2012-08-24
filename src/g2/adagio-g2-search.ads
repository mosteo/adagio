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

-- Now leaks the queues array, no finalization method.

with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver;
with Adagio.Os.Memory;
with Adagio.Searches.Handler;
with Adagio.Searches;
with Adagio.Trace;

with Agpl.Event_Queues.Calendar;
with Agpl.Http.Server.Sort_Handler;
use  Agpl.Http.Server.Sort_Handler;

with Charles.Multimaps.Sorted.Unbounded;
with Charles.Maps.Sorted.Strings.Unbounded;

with Ada.Calendar;
use  Ada;

package Adagio.G2.Search is

   pragma Elaborate_Body;

   -- Searches are event-driven. Unscheduled hubs are programmed an event and marqued as
   -- scheduled. Once the event triggers, the hubs are marked as unscheduled, and await
   -- a QKA. The QKA triggers the launching of a Q2, if there are searches for the hub.
   -- If not, it is rescheduled with the MinimumRest delay.
   -- Once a QA arrives, the hub is rescheduled with Max (MinimumRest, RemoteBan)
   -- When a hub is selected because of schedule, its leaves value is checked against the
   -- minimum for a query. If not enough, it is rescheduled using the GrowingRest period.

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Searches.Handler.Object with private;
   type Object_access is access all Object;

   ------------------------------------------------------------------------
   -- Create_Search                                                      --
   ------------------------------------------------------------------------
   -- Notify the creation of a new search                                
   procedure Create_Search (
      This : access Object; Target : in Searches.Search_Id);

   ------------------------------------------------------------------------
   -- Delete_Search                                                      --
   ------------------------------------------------------------------------
   procedure Delete_Search (
      This : access Object; Target : in Searches.Search_Id);

   ------------------------------------------------------------------------
   -- Get_Custom_Info                                                    --
   ------------------------------------------------------------------------
   -- Intended to allow each network to provide some progress info.
   function Get_Custom_Info (
      This : access Object; Target : in Searches.Search_Id) return String;

   ------------------------------------------------------------------------
   -- Get_Hubs                                                           --
   ------------------------------------------------------------------------
   -- Alive ones
   function Get_Hubs (This : access Object) return Natural;
   -- Tracked ones
   function Get_Tracked_Hubs (This : access Object) return Natural;

   ------------------------------------------------------------------------
   -- Get_Latency                                                        --
   ------------------------------------------------------------------------
   -- Latency of the remote search (minimum wait locally imposed on new events).
   function Get_Latency (This : access Object) return Duration;

   ------------------------------------------------------------------------
   -- Get_Leaves                                                         --
   ------------------------------------------------------------------------
   function Get_Leaves (This : access Object) return Natural;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (This : access Object; Data : out Data_Set);

   ------------------------------------------------------------------------
   -- Process_Search_Packet                                              --
   ------------------------------------------------------------------------
   procedure Process_Search_Packet (
      This : access Object; Item : G2.Packet.Queue.Item_Type);

   ------------------------------------------------------------------------
   -- Set_Paused                                                         --
   ------------------------------------------------------------------------
   procedure Set_Paused (
      This   : access Object; 
      Target : in     Searches.Search_Id; 
      Paused : in     Boolean := true);

   ------------------------------------------------------------------------
   -- Set_Priority                                                       --
   ------------------------------------------------------------------------
   procedure Set_Priority (
      This     : access Object; 
      Target   : in     Searches.Search_Id; 
      Priority : in     Searches.Priorities);

   ------------------------------------------------------------------------
   -- Set_Queues                                                         --
   ------------------------------------------------------------------------
   -- Informs of TCP queues available for firewalled searching
   procedure Set_Queues (
      This     : access Object;
      Queues   : in Packet.Queue.Address_Queue_Array);

   ------------------------------------------------------------------------
   -- Set_Start_Nodes                                                    --
   ------------------------------------------------------------------------
   -- Informs of possible start servers (known alive hubs)
   -- Receives an array of addresses
   procedure Set_Start_Nodes (This : access Object; Nodes : Ustring_Array);

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Sets up the searcher
   procedure Start (
      This        : access Object; 
      Sender      : in G2.Packet.Queue.Object_Access;
      Transceiver : in G2.Transceiver.Object_Access);

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown (This : in out Object);

   -- Debug
   procedure Debug_Test (This : in out Object);

private

   use type Calendar.Time;

   subtype Query_Key is String (1 .. 4); -- 32 bits of key

   Null_Key : constant Query_Key := "    ";

   Guid_Not_Found : exception;

   -- Data about known hubs
   type Node_Type is record
      Address     : Ustring;
      Scheduled   : Boolean       := false;      -- Says if a hub has a scheduled query
      Next_QEvent : Calendar.Time := Past_aeons; -- Time for the next query event
      Last_access : Calendar.Time := Past_aeons; -- Last time it has been sent a msg
      Last_QA     : Calendar.Time := Past_aeons; -- Last QA received
      Key         : Query_Key     := Null_Key; 
      Key_Time    : Calendar.Time := Past_aeons; -- Time at which we got the query key for this host.
      Unknown_leaves : Boolean    := true;       -- If we don't know how many leaves it has.
      Leaves      : Natural       := 0;
      Growing     : Boolean       := false;      -- True if waiting for it to grow leaves.
      Alive       : Boolean       := false;      -- True when we got some QA from it.
   end record;
   type Node_Access is access all Node_Type;

   ------------------------------------------------------------------------
   -- Create_Node                                                        --
   ------------------------------------------------------------------------
   function Create_Node (
      Address     : in String;
      Last_Access : Calendar.Time := Past_Aeons;
      Last_QA     : Calendar.Time := Past_Aeons;
      Key         : Query_key     := Null_Key) return Node_Type;

   ------------------------------------------------------------------------
   -- Is_Dropable                                                        --
   ------------------------------------------------------------------------
   function Is_Dropable (Node : access Node_Type) return Boolean;

   -- Data kept by every search about the queried hubs
   type Searched_Node is record
      Last_Reply : Calendar.Time := Past_Aeons; -- Last time the hub has answered the srch
   end record;

   -- List to be used by a single search; not to be confused with the global Search_List
   -- which refers to searches and not hubs.
   -- Indexed by address
   package Searched_List is new Charles.Maps.Sorted.Strings.Unbounded (
      Searched_Node, "<", "=");

   -- Empty;
   Null_Searched_List : Searched_List.Container_Type;

   -- G2 queries
   type G2_Search is record
      Hub_accesses   : Natural := 0;
      Leaf_accesses  : Natural := 0;
      Priority       : Searches.Priorities;
      Priority_delta : Natural;                       -- Computed from priority [and hits]
      Search         : Searches.Search_Id;            -- Id of the search
      Payload        : Searches.Payload_Access;
      Guid           : Guid_String;                   -- Stored as String
      Paused         : Boolean := false;
      Searched_nodes : Searched_List.Container_Type;  -- Hubs already searched.
   end record;
   type G2_Search_Access is access all G2_Search;

   -- Collections to be managed
   -- Running searches, indexed by next priority
   package Search_List is new Charles.Multimaps.Sorted.Unbounded (
      Natural, G2_Search_Access, "<", "=");
   -- Running searches, indexed by guid
   package Guid_List is new Charles.Maps.Sorted.Strings.Unbounded (
      G2_Search_Access, "<", "=");
   -- Known hubs, indexed by address:port
   package Hub_List is new Charles.Maps.Sorted.Strings.Unbounded (
      Node_Type, "<", "=");

   -- Direct access to data:
   function Get_Access is new Hub_List.Generic_Element (Node_Access);

   -- NO ACCESS TO THE MANAGER SHOULD BE MADE FROM WITHIN THIS PROTECTED OBJECT
   -- BECAUSE IT IS ACCESED INDIRECTLY BY THE MANAGER
   -- THUS PRODUCING MUTUAL DEADLOCKS
   protected type Safe_Object (Parent : access Object) is
      procedure Add_New_Hub (Address : in String; Just_Searched : in Boolean := false);
      procedure Add_New_Hub (
         Address : in String; Just_Searched : in Boolean := false; Hub : out Node_Access);
      procedure Create_Search (
         Target   : in Searches.Search_Id;
         Payload  : in Adagio.Searches.Payload;
         Priority : in Searches.Priorities;
         PDelta   : in Natural);
      procedure Delete_Hub (Address : in String);
      procedure Delete_Search (Target : in Searches.Search_Id);
      procedure Discount_Hub_Data (Hub : access Node_Type);
      procedure Do_QA  (Item : in G2.Packet.Queue.Item_Type);
      procedure Do_QKA (Item : in G2.Packet.Queue.Item_Type);
      -- False if no search could be queried for this hub right now
      function  Exists_Search_For (Hub : in Node_Access) return Boolean;
      function  Get_Custom_Info (Target : in Searches.Search_Id) return String;
      function  Get_Hubs return Natural;
      -- Rotates through neighbors:
      procedure Get_Next_Queue (Queue : out Packet.Queue.Address_Queue);
      function  Get_Tracked_Hubs return Natural;
      function  Get_Id_From_Guid (Gu : in Guid_String) return Searches.Search_Id;
      -- Says the index that should apply if it were new:
      function  Get_Index_For (Srch   : in G2_Search_Access) return Natural;
      function  Get_Leaves return Natural;
      procedure Http_Report (Data : out Data_Set);
      function  Is_Neighbor (Address : in String) return Boolean;
      function  Must_Search return Boolean; -- True if searches enqueued
      procedure Pause_Search  (Target : in Searches.Search_Id; Paused : in Boolean);
      procedure Perform_Searches_Rollback; -- Moves all indexes back 
      -- The hubs are only dropped if there are running searches.
      procedure Process_Drop_Event (Address : in String);
      procedure Process_Query_Event (Address : in String);
      -- Creates events for querying and checking if alive:
      procedure Program_Node_Query (
         Hub : access Node_Type; For_Time: in Calendar.Time);
      procedure Program_Start_Nodes;
      procedure Purge;
      procedure Query_Hub (Hub : in Node_Access);
      -- Gives the search to be sent to a hub, or null if no valid candidate!
      -- Updates priority counters
      procedure Select_Search_For (Hub : in Node_Access; Srch : out G2_Search_Access);
      procedure Send_To_Neighbor (Address : in String; P : in Packet.Object);
      procedure Send_To_Queues (P : in Packet.Object);
      procedure Set_Priority (
         Target : in Searches.Search_Id; Priority : Searches.Priorities; PDelta : Natural);
      procedure Set_Queues (New_Queues : in Packet.Queue.Address_Queue_Array);
      procedure Set_Start_Nodes (Nodes : in Ustring_Array);
      procedure Start; -- Creates first events

   private
      Hubs        : Hub_List.Container_Type;
      Searches    : Search_List.Container_Type;
      Guids       : Guid_List.Container_Type;
      Queues      : Packet.Queue.Address_Queue_Array_Access;
      Queue_Index : Positive := 1;

      Starters    : Hub_List.Container_Type;
      Next_Packet : Calendar.Time               := Calendar.Clock;
      Leaves      : Natural                     := 0;
      Alive_Hubs  : Natural                     := 0;
   end Safe_Object;

   type Object is new Searches.Handler.Object with record
      Events      : Agpl.Event_Queues.Calendar.Object (
                        Stack_Size => Os.Memory.Max_Stack_Size,
                        Tracer     => Trace.General'Access);
      Safe        : Safe_Object (Object'Access);
      Sender      : G2.Packet.Queue.Object_Access;
      Transceiver : G2.Transceiver.Object_Access;

      Latency     : Duration := 0.0;
      pragma Atomic (Latency);
   end record;

end Adagio.G2.Search;
