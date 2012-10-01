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
--with Adagio.Debug;
with Adagio.G2.Hit;
with Adagio.G2.Packet;
with Adagio.Globals.Options;
with Adagio.Guid;
with Adagio.Misc;
with Adagio.Network.Endian;
with Adagio.Network_Settings;
with Adagio.Searches.Manager;
with Adagio.Server;
with Adagio.Socket.IP;
with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Statistics.Strings;
with Sha1;

with Agpl.Event_Queues;

with Text_IO;
with Ada.Unchecked_Deallocation;
with Adagio.Trace;

package body Adagio.G2.Search is

   Start_Servers_Num : constant Natural := Globals.Options.G2_UdpBuffers / 4;

   Stat_Events       : constant String  := "Remote search - Pending events";
   Stat_Hubs         : constant String  := "Remote search - Tracked hubs";
   Stat_Latency      : constant String  := "Remote search - Latency";

   Nul : Character renames Constants.Nul;

   package Endian renames Adagio.Network.Endian;
   package Event_Queue renames Agpl.Event_Queues.Calendar;
   package Manager renames Adagio.Searches.Manager;

   use type Searches.Search_Id;

   ------------------------------------------------------------------------
   -- Event contexts                                                     --
   ------------------------------------------------------------------------
   type Node_Context_Type is new Agpl.Event_Queues.Context_Type with record
      Address : Ustring;
      Parent  : Object_Access;
   end record;
   type Search_Context_type is new Agpl.Event_Queues.Context_Type with record
      Search  : Searches.Search_Id;
      Parent  : Object_Access;
   end record;
   type Default_Context_Type is new Agpl.Event_Queues.Context_Type with record
      Parent  : Object_Access;
   end record;

   ------------------------------------------------------------------------
   -- Purge_Event                                                        --
   ------------------------------------------------------------------------
   procedure Purge_Event (Context : Agpl.Event_Queues.Context_Type'Class) is
      Event        : Event_Queue.Event_Type;
      Def_Context  : Default_Context_Type := Default_Context_Type (Context);
   begin
      Trace.Log ("G2.Search.Purge_Event", Trace.Debug);

      -- Reprogram after desired time:
      Event_Queue.Create (
         Def_Context.Parent.Events,
         Event,
         Calendar.Clock + Globals.Options.G2_Search_PurgePeriod,
         Purge_Event'Access,
         Def_Context);

      Def_Context.Parent.Safe.Purge;
   exception
      when E : others =>
         Trace.Log ("G2.Search.Purge_Event: " & Trace.Report (E), Trace.Error);
   end Purge_Event;

   ------------------------------------------------------------------------
   -- Watchdog_Event                                                     --
   ------------------------------------------------------------------------
   -- Ensure that searching is attempted regularly:
   procedure Watchdog_Event (Context : Agpl.Event_Queues.Context_Type'Class)
   is
      Event        : Event_Queue.Event_Type;
      Def_Context  : Default_Context_Type :=
         Default_Context_Type (Context);
   begin
      Trace.Log ("G2.Search.Watchdog_Event", Trace.Debug);

      -- Reprogram after desired time:
      Event_Queue.Create (
         Def_Context.Parent.Events,
         Event,
         Calendar.Clock + Globals.Options.G2_Search_WatchdogPeriod,
         Watchdog_Event'Access,
         Def_Context);

      -- Reinsert startup nodes if necessary
      if Def_Context.Parent.Safe.Must_Search and then
         Def_Context.Parent.Safe.Get_Hubs < Start_Servers_Num
      then
         Def_Context.Parent.Safe.Program_Start_Nodes;
      end if;
   exception
      when E : others =>
         Trace.Log ("G2.Search.Watchdog_Event: " & Trace.Report (E), Trace.Error);
   end Watchdog_Event;

   ------------------------------------------------------------------------
   -- Query_Event                                                        --
   ------------------------------------------------------------------------
   -- Triggered when a server is to be queried
   procedure Query_Event (Context : Agpl.Event_Queues.Context_Type'Class) is
      Hub_Context : Node_Context_Type := Node_Context_Type (Context);
   begin
      Trace.Log ("G2.Search.Query_Event", Trace.Never);

      Globals.Main_Throttle.Start_Work;
      Hub_Context.Parent.Safe.Process_Query_Event (S (Hub_Context.Address));
      Globals.Main_Throttle.End_Work;
   exception
      when E : others =>
         Trace.Log ("G2.Search.Query_Event: " & Trace.Report (E), Trace.Error);
   end Query_Event;

   ------------------------------------------------------------------------
   -- Drop_Event                                                         --
   ------------------------------------------------------------------------
   -- Triggered to check if a server is to be dropped
   procedure Drop_Event (Context : Agpl.Event_Queues.Context_Type'Class) is
      Hub_Context : Node_Context_Type := Node_Context_Type (Context);
   begin
      Trace.Log ("G2.Search.Drop_Event", Trace.Never);

      Hub_Context.Parent.Safe.Process_Drop_Event (S (Hub_Context.Address));
   exception
      when E : others =>
         Trace.Log ("G2.Search.Drop_Event: " & Trace.Report (E), Trace.Error);
   end Drop_Event;

   ------------------------------------------------------------------------
   -- Priority_Event                                                     --
   ------------------------------------------------------------------------
   -- Triggered to apply a new priority to a search:
   procedure Priority_Change_Event (Context : Agpl.Event_Queues.Context_Type'Class) is
      Search_Context : Search_Context_Type := Search_Context_Type (Context);
      Priority       : Searches.Priorities := Manager.Get_Priority (Search_Context.Search);
      PDelta         : Natural :=             Manager.Get_Priority_Delta (
         Search_Context.Search);
   begin
      Trace.Log ("PRIORITY EVENT FOR " & Searches.To_String (Search_Context.Search) &
         "; New priority: " & Priority'Img & PDelta'Img, Trace.Never);
      Search_Context.Parent.Safe.Set_Priority (Search_Context.Search, Priority, PDelta);
   exception
      when E : others =>
         Trace.Log ("G2.Search.Priority_Event: " & Trace.Report (E), Trace.Error);
   end Priority_Change_Event;

   ------------------------------------------------------------------------
   -- Create_Node                                                        --
   ------------------------------------------------------------------------
   function Create_Node (
      Address     : in String;
      Last_Access : Calendar.Time := Past_Aeons;
      Last_QA     : Calendar.Time := Past_Aeons;
      Key         : Query_key     := Null_Key) return Node_Type
   is
   begin
      return (
         Address     => U (Address),
         Scheduled   => false,
         Next_QEvent => Past_Aeons,
         Last_Access => Last_Access,
         Last_QA     => Last_QA,
         Unknown_Leaves => true,
         Leaves      => 0,
         Alive       => false,
         Growing     => false,
         Key         => Key,
         Key_Time    => Calendar.Clock);
   end;

   ------------------------------------------------------------------------
   -- Is_Dropable                                                        --
   ------------------------------------------------------------------------
   function Is_Dropable (Node : access Node_Type) return Boolean is
      Now : Calendar.Time := Calendar.Clock;
      function Max (L, R : in Calendar.Time) return Calendar.Time;
      pragma Inline (Max);
      function Max (L, R : in Calendar.Time) return Calendar.Time is
      begin
         if L > R then
            return L;
         else
            return R;
         end if;
      end Max;
   begin
      return
         (not Node.Scheduled) and then
         Now - Node.Last_Access > Globals.Options.G2_Search_PurgeAge;
   end Is_Dropable;

   ------------------------------------------------------------------------
   -- Create_Search                                                      --
   ------------------------------------------------------------------------
   -- Notify the creation of a new search
   procedure Create_Search (
      This : access Object; Target : in Searches.Search_Id)
   is
      Prio : Searches.Priorities := Manager.Get_Priority (Target);
      use type Searches.Priorities;
   begin
      This.Safe.Create_Search (
         Target,
         Manager.Get_Payload (Target),
         Prio,
         Manager.Get_Priority_Delta (Target));

      -- If priority is exclusive for limited time, program event to change delta:
      if Prio > Searches.High and then Prio < Searches.Exclusive_Forever then
         declare
            Event : Event_Queue.Event_Type;
         begin
            Event_Queue.Create (
               This.Events,
               Event,
               Calendar.Clock + Searches.Priority_Delays (Prio) + 1.0,
               Priority_Change_Event'Access,
               Search_Context_Type'(Search => Target, Parent => Object_Access (This)));
         end;
      end if;
   end Create_Search;

   ------------------------------------------------------------------------
   -- Delete_Search                                                      --
   ------------------------------------------------------------------------
   procedure Delete_Search (
      This : access Object; Target : in Searches.Search_Id)
   is
   begin
      This.Safe.Delete_Search (Target);
   end Delete_Search;

   ------------------------------------------------------------------------
   -- Get_Custom_Info                                                    --
   ------------------------------------------------------------------------
   -- Intended to allow each network to provide some progress info.
   function Get_Custom_Info (
      This : access Object; Target : in Searches.Search_Id) return String is
   begin
      return This.Safe.Get_Custom_Info (Target);
   end Get_Custom_Info;

   ------------------------------------------------------------------------
   -- Get_Hubs                                                           --
   ------------------------------------------------------------------------
   function Get_Hubs (This : access Object) return Natural is
   begin
      return This.Safe.Get_Hubs;
   end Get_Hubs;
   function Get_Tracked_Hubs (This : access Object) return Natural is
   begin
      return This.Safe.Get_Tracked_Hubs;
   end Get_Tracked_Hubs;

   ------------------------------------------------------------------------
   -- Get_Latency                                                        --
   ------------------------------------------------------------------------
   -- Latency of the remote search (minimum wait locally imposed on new events).
   function Get_Latency (This : access Object) return Duration is
   begin
      return This.Latency;
   end Get_Latency;

   ------------------------------------------------------------------------
   -- Get_Leaves                                                         --
   ------------------------------------------------------------------------
   function Get_Leaves (This : access Object) return Natural is
   begin
      return This.Safe.Get_Leaves;
   end Get_Leaves;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (This : access Object; Data : out Data_Set) is
   begin
      This.Safe.Http_Report (Data);
   end Http_Report;

   ------------------------------------------------------------------------
   -- Process_Search_Packet                                              --
   ------------------------------------------------------------------------
   procedure Process_Search_Packet (
      This : access Object; Item : G2.Packet.Queue.Item_Type)
   is
      P : G2.Packet.Object renames Item.Packet;

      procedure Do_Hit is
         Load : constant String := Packet.Payload (P);
         Guid : Guid_String     := Load (Load'First + 1 .. Load'First + 16);
         Srch : Searches.Search_Id;
      begin
         Srch := This.Safe.Get_Id_From_Guid (Guid);
         declare
            Hits : G2.Hit.Object_Array := G2.Hit.Create (Item);
            Pre  : Natural             := Manager.Get_Hits (Srch);
         begin
            for I in Hits'Range loop
               Manager.Add_Hit (Srch, Hits (I));
               -- Trace.Log ("ADDING HIT: " & Hit.Get_Name (Hits (I)), Trace.Always);
            end loop;

            -- Set new priority in case it was auto:
            declare
               Priority : Searches.Priorities := Manager.Get_Priority (Srch);
               PDelta   : Natural             := Manager.Get_Priority_Delta (Srch);
            begin
               This.Safe.Set_Priority (Srch, Priority, PDelta);
            end;

            -- Pause if necessary:
            if Pre < Globals.Options.G2_Search_Priorities_Stop_Threshold and then
               Manager.Get_Hits (Srch) >=
                  Globals.Options.G2_Search_Priorities_Stop_Threshold
            then
               Manager.Pause_Search (Srch);
            end if;
         end;
      exception
         when Guid_Not_Found =>
            Trace.Log ("G2.Search.Do_Hit: Hit for unknown search", Trace.Debug);
      end Do_Hit;

   begin
      Globals.Main_Throttle.Start_Work;
      if    G2.Packet.Is_A (P, "/QKA") then
         This.Safe.Do_QKA (Item);
      elsif G2.Packet.Is_A (P, "/QA") then
         This.Safe.Do_QA (Item);
      elsif G2.Packet.Is_A (P, "/QH2") then
         -- AQUI HAY QUE ACTUALIZAR LA PRIORITY DELTA DE LA BÚSQUEDA
         Do_Hit;
      else
         Trace.Log ("G2.Search: Unknown search Packet: " & G2.Packet.To_Hex (P),
            Trace.Warning);
      end if;
      Globals.Main_Throttle.End_Work;
   exception
      when E : others =>
         Trace.Log ("G2.Search.Process_Search_Packet: " &
            Trace.Report (E), Trace.Error);
   end Process_Search_Packet;

   ------------------------------------------------------------------------
   -- Set_Paused                                                         --
   ------------------------------------------------------------------------
   procedure Set_Paused (
      This   : access Object;
      Target : in     Searches.Search_Id;
      Paused : in     Boolean := true)
   is
   begin
      This.Safe.Pause_Search (Target, Paused);
   end Set_Paused;

   ------------------------------------------------------------------------
   -- Set_Priority                                                       --
   ------------------------------------------------------------------------
   procedure Set_Priority (
      This     : access Object;
      Target   : in Searches.Search_Id;
      Priority : in Searches.Priorities)
   is
      use type Searches.Priorities;
   begin
      This.Safe.Set_Priority (Target, Priority, Manager.Get_Priority_Delta (Target));
      -- If priority is exclusive for limited time, program event to change delta:
      if Priority > Searches.High and then Priority < Searches.Exclusive_Forever then
         declare
            Event : Event_Queue.Event_Type;
         begin
            Event_Queue.Create (
               This.Events,
               Event,
               Calendar.Clock + Searches.Priority_Delays (Priority) + 1.0,
               Priority_Change_Event'Access,
               Search_Context_Type'(Search => Target, Parent => Object_Access (This)));
         end;
      end if;
   end Set_Priority;

   ------------------------------------------------------------------------
   -- Set_Queues                                                         --
   ------------------------------------------------------------------------
   -- Informs of TCP queues available for firewalled searching
   procedure Set_Queues (
      This     : access Object;
      Queues   : in Packet.Queue.Address_Queue_Array)
   is
   begin
      This.Safe.Set_Queues (Queues);
   end Set_Queues;

   ------------------------------------------------------------------------
   -- Set_Start_Nodes                                                    --
   ------------------------------------------------------------------------
   -- Informs of possible start servers (known alive hubs)
   -- Receives an array of addresses
   procedure Set_Start_Nodes (This : access Object; Nodes : Ustring_Array) is
   begin
      This.Safe.Set_Start_Nodes (Nodes);
   end Set_Start_Nodes;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Sets up the searcher
   procedure Start (
      This        : access Object;
      Sender      : in G2.Packet.Queue.Object_Access;
      Transceiver : in G2.Transceiver.Object_Access)
   is
      Event : Event_Queue.Event_Type;
   begin
      This.Sender      := Sender;
      This.Transceiver := Transceiver;
      This.Safe.Start;

      -- Create Watchdog Event
      Event_Queue.Create (
         This.Events,
         Event,
         Calendar.Clock + Globals.Options.G2_Search_WatchdogPeriod,
         Watchdog_Event'Access,
         Default_Context_Type'(
            Agpl.Event_Queues.Context_Type
            with Parent => Object_access (This)));
      -- Create Purge Event
      Event_Queue.Create (
         This.Events,
         Event,
         Calendar.Clock + Globals.Options.G2_Search_PurgePeriod,
         Purge_Event'Access,
         Default_Context_Type'(
            Agpl.Event_Queues.Context_Type
            with Parent => Object_access (This)));
      Trace.Log ("G2 search engine started", Trace.Informative);
   end Start;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown (This : in out Object) is
   begin
      Agpl.Event_Queues.Calendar.Shutdown (This.Events);
   end Shutdown;

   ------------------------------------------------------------------------
   -- Safe_Object                                                        --
   ------------------------------------------------------------------------
   protected body Safe_Object is
      -----------------
      -- Add_New_Hub --
      -----------------
      procedure Add_New_Hub (Address : in String; Just_Searched : in Boolean := false) is
         Dummy : Node_Access;
      begin
         Add_New_Hub (Address, Just_Searched, Dummy);
      end Add_New_Hub;
      procedure Add_New_Hub (
         Address        : in  String;
         Just_Searched  : in  Boolean := false;
         Hub            : out Node_Access)
      is
         use Hub_List;
         I      : Iterator_Type := Find (Hubs, Address);
         Now    : Calendar.Time := Calendar.Clock;
         Is_New : Boolean := false;
      begin
         if I = Back (Hubs) then
            Is_New := true;
            Insert (
               Hubs, Address, Create_Node (Address, Last_Access => Past_Aeons), I);
            Statistics.Object.Set (
               Stat_Hubs, Statistics.Integers.Create (Length (Hubs)));
         end if;

         Hub := Get_Access (I);
         pragma Assert (Hub /= null);
         if Is_New and then not Just_Searched then
            -- Program query:
            Program_Node_Query (Hub, Now);
         elsif Is_New and Just_Searched then
            -- Query later
            Program_Node_Query (Hub, Now + Globals.Options.G2_Search_HubRestPeriod);
         else -- Not is new
            Program_Node_Query (Hub, Now);
         end if;
      end Add_New_Hub;

      -------------------
      -- Create_Search --
      -------------------
      procedure Create_Search (
         Target   : in Adagio.Searches.Search_Id;
         Payload  : in Adagio.Searches.Payload;
         Priority : in Adagio.Searches.Priorities;
         PDelta   : in Natural)
      is
         use Search_List;
         New_Search   : G2_Search_Access := new G2_Search'(
            Hub_Accesses   => 0,
            Leaf_Accesses  => 0,
            Payload        => new Adagio.Searches.Payload'(Payload),
            Priority       => Priority,
            Priority_Delta => PDelta,
            Guid           => Guid.To_Char_Array (Guid.Create_Guid),
            Search         => Target,
            Paused         => false,
            Searched_Nodes => Null_Searched_List);
         Index : Natural := 0;
         use type Adagio.Searches.Priorities;
      begin
         -- Index as the first search entered, but not if next is Idle:
         if Priority = Adagio.Searches.Idle then
            Index := Natural'Last;
         elsif not Is_Empty (Searches) then
            Index := Key (First (Searches));
            if Index = Natural'Last then
               Index := 0;
            end if;
         end if;

         -- Add
         Insert (
            Searches,
            Index,
            New_Search);

         Guid_List.Insert (
            Guids,
            New_Search.Guid,
            New_Search);

         -- Try to start immediately:
         Program_Start_Nodes;
      end Create_Search;

      ----------------
      -- Delete_Hub --
      ----------------
      procedure Delete_Hub (Address : in String) is
         use Hub_List;
         I : Iterator_Type := Find (Hubs, Address);
      begin
         if I = Back (Hubs) then
            Trace.Log ("G2.Search.Delete_Hub: Missing hub: " & Address, Trace.Debug);
         else
            Discount_Hub_Data (Get_Access (I));
            Delete (Hubs, I);
         end if;
      end Delete_Hub;

      -------------------
      -- Delete_Search --
      -------------------
      procedure Delete_Search (Target : in Adagio.Searches.Search_Id) is
         use Search_List;
         I : Iterator_Type := First (Searches);
         use Guid_List;
         use type Search.Object_Access;
         procedure Free is new Unchecked_Deallocation (G2_Search, G2_Search_Access);
         procedure Free is new Unchecked_Deallocation (
            Adagio.Searches.Payload, Adagio.Searches.Payload_Access);
         Aux : G2_Search_Access;
      begin
         while I /= Back (Searches) loop
            if Element (I).Search = Target then
               Aux := Element (I);
               Delete (Guids, Aux.Guid);
               Free (Aux.Payload);
               Free (Aux);
               Delete (Searches, I);
               return;
            end if;
            I := Succ (I);
         end loop;
         Trace.Log ("Deleting G2 search: Not found: " &
            Adagio.Searches.To_String (Target), Trace.Warning);
      end Delete_Search;

      -----------------------
      -- Discount_Hub_Data --
      -----------------------
      procedure Discount_Hub_Data (Hub : access Node_Type) is
      begin
         Leaves     := Leaves - Hub.Leaves;
         if Hub.Alive then
            Alive_Hubs := Alive_Hubs - 1;
         end if;
      end Discount_Hub_Data;

      -----------
      -- Do_QA --
      -----------
      procedure Do_QA (Item : in G2.Packet.Queue.Item_Type) is
         use Endian;
         use Guid_List;
         use Hub_List;
         Address : String := Socket.Image (Item.Udp_Source);
         Added_Hub, Hub     : Node_Access;
         I       : Hub_List.Iterator_Type := Find (Hubs, Address);
         Ban     : Duration := 0.0;
         Now     : Calendar.Time := Calendar.Clock;
         Srch    : G2_Search_Access := null;
      begin
         if I = Back (Hubs) then
            -- Ack from unknown hub, create and proceed but nothing else:
            Add_New_Hub (Address, Just_Searched => True, Hub => Hub);
         else
            Hub := Get_Access (I);
         end if;

         Hub.Last_QA := Now;

         -- Extract ban time:
         begin
            if G2.Packet.Is_A (Item.Packet, "/QA/RA") then
               Ban := Duration (Integer'(Endian.Convert (
                  To_Byte_Array (G2.Packet.Payload (
                     G2.Packet.Get_Child (Item.Packet, "RA"))),
                  G2.Packet.Big_Endian (Item.Packet))));
               Trace.Log ("Imposed ban from " & Address & ":" & Ban'Img, Trace.Debug);
            end if;
         exception
            when others =>
               Trace.Log ("G2.Search: /QA/RA too large: " & G2.Packet.To_Hex (
                  G2.Packet.Get_Child (Item.Packet, "RA")), Trace.Warning);
         end;
         Ban := Duration'Max (Ban, Globals.Options.G2_Search_HubRestPeriod);

         -- Program new query:
         Program_Node_Query (Hub, Now + Ban);

         -- Add new hubs to continue searching:
         declare
            Hubs : G2.Packet.Object_Array := G2.Packet.Get_Children (Item.Packet, "S");
         begin
            for I in Hubs'range loop
               Add_new_hub (
                  Address => G2.To_Address (
                     G2.Packet.Payload (Hubs (I)),
                     G2.Packet.Big_Endian (Item.Packet)),
                  Hub => Added_Hub);
            end loop;
         end;

         -- Mark search as accessed, add visited hubs, add leaves:
         declare
            I    : Guid_List.Iterator_Type := Find (Guids, G2.Packet.Payload (
               Item.Packet));
            Hubs : G2.Packet.Object_Array := G2.Packet.Get_Children (Item.Packet, "D");
         begin
            if I = Back (Guids) then
               Trace.Log ("G2.Do_QA: Ack for unknown search (dropped?)",
                  Trace.Debug);
            else
               Srch := Element (I);
            end if;

            for I in Hubs'range loop
               declare
                  Payload : constant String := G2.Packet.Payload (Hubs (I));
                  Addr    : constant String := To_Address (
                     Payload (Payload'First .. Payload'Last - 2),
                     G2.Packet.Big_Endian (Item.Packet));
                  Node_Leaves : constant Natural := Endian.Convert (
                     Endian.To_Byte_Array (Payload (Payload'Last - 1 .. Payload'Last)),
                     G2.Packet.Big_Endian (Item.Packet));
               begin
                  -- Add a done hub
                  if Addr /= Address then -- not the answering one:
                  --   if Leaves >= Globals.Options.G2_Search_MinimumLeaves then
                        Add_New_Hub (Addr, Just_Searched => true, Hub => Added_Hub);
                  --   end if;
                  else
                     Added_Hub := Hub;
                  end if;
                  -- If we have added it (enough leaves) add stats:
                  if Added_Hub /= null then
                     Leaves           := Leaves + Node_Leaves;
                     Leaves           := Leaves - Added_Hub.Leaves;
                     Added_Hub.Leaves := Node_Leaves;
                     Added_Hub.Unknown_Leaves := false;
                     if not Added_Hub.Alive then
                        Alive_Hubs := Alive_Hubs + 1;
                        Added_Hub.Alive  := true;
                     end if;
                  end if;
                  if Srch /= null then
                     Srch.Hub_Accesses  := Srch.Hub_Accesses + 1;
                     Srch.Leaf_Accesses := Srch.Leaf_Accesses + Node_Leaves;
                     Searched_List.Delete (Srch.Searched_Nodes, Addr);
                     Searched_List.Insert (
                        Srch.Searched_Nodes, Addr, (Last_Reply => Now));
                  end if;
               end;
            end loop;
         end;

      end Do_QA;

      ------------
      -- Do_QKA --
      ------------
      procedure Do_QKA (Item : in G2.Packet.Queue.Item_Type) is
         use Hub_List;
         use Packet.Queue;
         Hub     : Node_Access;
         Address : Ustring;
         I       : Iterator_Type;
      begin
         -- Direct reply from a queried hub?
         if Item.Source = Listener_Udp then
            Address := U (Socket.Image (Item.Udp_Source));
         else
            -- Forwarded key via TCP
            Address := U (Socket.Image (To_Address (
               Packet.Payload (Packet.Get_Child (Item.Packet, "QNA")),
               Packet.Big_Endian (Item.Packet))));
         end if;
         I := Find (Hubs, S (Address));
         if I = Back (Hubs) then
            -- Add a new hub who is answering under a new address (?)
            -- or which is remote (no directly accesible).
            declare
               New_Hub : Node_Type := Create_Node (
                  S (Address),
                  Last_Access => Past_Aeons,
                  Key         => G2.Packet.Payload (
                                    G2.Packet.Get_Child (Item.packet, "QK")));
            begin
               Insert (Hubs, S (Address), New_Hub);
               Hub := Get_Access (Find (Hubs, S (Address)));
            end;
         else
            Hub          := Get_Access (I);
            Hub.Key      := G2.Packet.Payload (G2.Packet.Get_Child (Item.Packet, "QK"));
            Hub.Key_Time := Calendar.Clock;
         end if;

         Query_Hub (Hub);
      end Do_QKA;

      -----------------------
      -- Exists_Search_For --
      -----------------------
      function  Exists_Search_For (Hub : in Node_Access) return Boolean is
         use Search_List;
         use Searched_List;
         Candidate: Search_list.Iterator_Type;
         I        : Searched_List.Iterator_Type;
         Now      : Calendar.Time := Calendar.Clock;
         Srch     : G2_Search_Access;
      begin
         Candidate := First (Searches);
         while Candidate /= Back (Searches) loop
            Srch  := Element (Candidate);
            -- Skip if paused:

            if not Srch.Paused then
               -- Look for the hub in the list of the search:
               I := Find (Srch.Searched_Nodes, S (Hub.Address));
               exit when I = Back (Srch.Searched_Nodes) or else
                 Now - Element (I).Last_Reply > Globals.Options.G2_Search_MinimumRequeryWait;
            end if;

            -- At this point, the candidate search is rejected because queried not much ago
            Candidate := Succ (Candidate);
            Srch      := null;
         end loop;

         return Srch /= null;
      end Exists_Search_For;

      ---------------------
      -- Get_Custom_Info --
      ---------------------
      function Get_Custom_Info (Target : in Adagio.Searches.Search_Id) return String is
         use Search_List;
         I        : Iterator_Type := First (Searches);
         Srch     : G2_Search_Access;
      begin
         while I /= Back (Searches) loop
            if Element (I).Search = Target then
               Srch := Element (I);
               return
                  "Hubs:" & Srch.Hub_Accesses'Img & "; Leaves:" & Srch.Leaf_Accesses'Img;
            end if;
            I := Succ (I);
         end loop;
         return "Unknown";
      end Get_Custom_Info;

      --------------
      -- Get_Hubs --
      --------------
      function Get_Hubs return Natural is
      begin
         return Alive_Hubs;
      end Get_Hubs;

      --------------------
      -- Get_Next_Queue --
      --------------------
      -- Rotates through neighbors. May return invalid queue.
      procedure Get_Next_Queue (Queue : out Packet.Queue.Address_Queue) is
      begin
         Queue_Index := Queue_Index + 1;
         if Queue_Index > Queues'Last or else Queues (Queue_Index).Address = Null_Ustring then
            Queue_Index := 1;
         end if;
         Queue := Queues (Queue_Index);
      end Get_Next_Queue;

      ----------------------
      -- Get_Tracked_Hubs --
      ----------------------
      function Get_Tracked_Hubs return Natural is
      begin
         return Hub_List.Length (Hubs);
      end Get_Tracked_Hubs;

      ----------------------
      -- Get_Id_From_Guid --
      ----------------------
      function  Get_Id_From_Guid (Gu : in Guid_String) return Adagio.Searches.Search_Id is
         use Guid_List;
         I : Iterator_Type := Find (Guids, Gu);
      begin
         if I = Back (Guids) then
            raise Guid_Not_Found;
         else
            return Element (I).Search;
         end if;
      end Get_Id_From_Guid;

      -------------------
      -- Get_Index_For --
      -------------------
      -- Says the index that should apply if it were new:
      function Get_Index_For (Srch : in G2_Search_Access) return Natural is
         use type Adagio.Searches.Priorities;
         use Search_List;
         Index : Natural;
      begin
         if Srch.Priority = Adagio.Searches.Idle then
            Index := Natural'Last;
         else
            if not Is_Empty (Searches) then
               Index := Key (First (Searches));
               if Index = Natural'Last then
                  Index := 0;
               end if;
            end if;
         end if;
         return Index;
      end Get_Index_For;

      ----------------
      -- Get_Leaves --
      ----------------
      function Get_Leaves return Natural is
      begin
         return Leaves;
      end Get_Leaves;

      -----------------
      -- Http_Report --
      -----------------
      procedure Http_Report (Data : out Data_Set) is
         use Hub_List;
         I     : Iterator_Type := First (Hubs);
         Now   : Calendar.Time := Calendar.Clock;
         Never : Ustring       := U ("Never");
         Never_Sort  : Ustring  := RPad (Duration'(Duration'Last));
         Never_Sort2 : Ustring  := RPad (Duration'(0.0));
      begin
         while I /= Back (Hubs) loop
            declare
               Hub : Node_Access := Get_Access (I);
               Row : Data_Row;
            begin
               if Hub.Alive then
                  -- Address
                  Append (Row, (Hub.Address, Hub.Address));
                  -- Next query time
                  if Hub.Next_QEvent <= Now then
                     Append (Row, (U ("Awaiting reply"), Never_Sort2));
                  else
                     Append (Row, (
                        U (Misc.Image (Hub.Next_QEvent - Now)),
                        Rpad (Hub.Next_QEvent - Now)));
                  end if;
                  -- Last msg time
                  if Hub.Last_Access = Past_Aeons then
                     Append (Row, (Never, Never_Sort));
                  else
                     Append (Row, (
                        U (Misc.Image (Now - Hub.Last_Access)),
                        RPad (Now - Hub.Last_Access)));
                  end if;
                  -- Last answer
                  if Hub.Last_QA = Past_Aeons then
                     Append (Row, (Never, Never_Sort));
                  else
                     Append (Row, (
                        U (Misc.Image (Now - Hub.Last_QA)),
                        RPad (Now - Hub.Last_QA)));
                  end if;
                  -- Leaves
                  if Hub.Unknown_Leaves then
                     Append (Row, (
                        U ("Unknown"),
                        Rpad (0)));
                  else
                     Append (Row, (
                        U (Misc.To_String (Hub.Leaves)),
                        Rpad (Hub.Leaves)));
                  end if;
                  -- Growing
                  Append (Row, (
                     U (Hub.Growing'Img),
                     U (Hub.Growing'Img)));

                  Append (Data, Row);
               else
                  Trace.Log ("Skipping dead hub: " & S (Hub.Address), Trace.Never);
               end if;
            end;
            I := Succ (I);
         end loop;
      end Http_Report;

      -----------------
      -- Is_Neighbor --
      -----------------
      function  Is_Neighbor (Address : in String) return Boolean is
      begin
         for I in Queues'Range loop
            if Queues (I).Address = Null_Ustring then
               return false;
            elsif S (Queues (I).Address) = Address then
               return true;
            end if;
         end loop;
         return false;
      end Is_Neighbor;

      ------------------
      -- Pause_Search --
      ------------------
      procedure Pause_Search (
         Target : in Adagio.Searches.Search_Id; Paused : in Boolean)
      is
         use Search_List;
         I : Iterator_Type := First (Searches);
         Srch : G2_Search_Access;
      begin
         while I /= Back (Searches) loop
            if Element (I).Search = Target then
               if Paused then
                  Element (I).Paused := Paused;
               else
                  -- Re-index it.
                  Srch := Element (I);
                  Srch.Paused := false;
                  Delete (Searches, I);
                  Insert (Searches, Get_Index_For (Srch), Srch);
                  Program_Start_Nodes;
               end if;
               exit;
            end if;
            I := Succ (I);
         end loop;
      end Pause_Search;

      ------------------------
      -- Process_Drop_Event --
      ------------------------
      procedure Process_Drop_Event (Address : in String) is
         use Hub_List;
         I           : Iterator_Type := Find (Hubs, Address);
         Hub         : Node_Access;
      begin
         if I = Back (Hubs) then
            return; -- <-- EARLY EXIT IF NOT STORED
         end if;

         Hub := Get_Access (I);

         -- Being not scheduled means: searches are stopped or,
         -- the query round is started but no QA has still received
         if not Hub.Scheduled then
            Discount_Hub_Data (Hub);
            Trace.Log ("G2.Search: Dropping silent hub: " & Address, Trace.Debug);
            Delete (Hubs, I);
            Statistics.Object.Set (Stat_Hubs, Statistics.Integers.Create (Length (Hubs)));
         else
            -- Trace.Log ("WASTED EVENT drop ", Trace.Always);
            null;
         end if;
      end Process_Drop_Event;

      -------------------------
      -- Process_Query_Event --
      -------------------------
      procedure Process_Query_Event (Address : in String) is
         use Hub_List;
         use Network_Settings;
         P, C        : G2.Packet.Object;
         I           : Iterator_Type := Find (Hubs, Address);
         Hub         : Node_Access;
         Now         : Calendar.Time := Calendar.Clock;
      begin
         if I = Back (Hubs) then
            Trace.Log ("G2.Search.Process_Query_Event: Missing hub!", Trace.Error);
            return; -- <-- EARLY EXIT IF NOT STORED
         end if;

         Hub           := Get_Access (I);

         if Hub.Next_QEvent > Now then
            --Trace.Log ("WASTED EVENT query for " & S (Hub.Address), Trace.Always);
            --Trace.Log ("DROPPED QUERY FOR " & S (Hub.Address) & " UNTIL " &
            --   Misc.Image (Hub.Next_QEvent - Now), Trace.Always);
            return; -- <-- EARLY EXIT IF A LATER EVENT IS SCHEDULED
         end if;

         Hub.Scheduled := false;

         if not Must_Search then
            return; -- <-- EARLY EXIT IF NO RUNNING SEARCHES
         end if;

         -- If too few leaves, don't query it:
         if (not Hub.Unknown_Leaves) and then
            Hub.Leaves < Globals.Options.G2_Search_MinimumLeaves and then
            not Hub.Growing
         then
            Trace.Log ("G2.Search.Process_Query_Event: Not enough leaves for " &
               S (Hub.Address), Trace.Debug);
            Hub.Growing := true;
            Program_Node_Query (
               Hub, Now + Globals.Options.G2_Search_HubGrowingPeriod - Globals.Options.G2_Search_HubRestPeriod);
            return; -- <-- EARLY EXIT IF NO ENOUGH LEAVES
         else
            Hub.Growing := false;
         end if;

         -- Send only if searches available:
         if not Exists_Search_For (Hub) then
            Trace.Log ("QKR: No search ready for " & S (Hub.Address), Trace.Debug);
            -- Reprogram after minimum rest!
            Program_Node_Query (Hub, Now + Globals.Options.G2_Search_HubRestPeriod);
            return;  -- <-- EARLY EXIT IF NO SEARCH AVAILABLE FOR THE HUB
         end if;

         -- Mark as accessed
         Hub.Last_Access := Now;

         -- Check for antiquity of the key, go to query or else request key:
         -- If neighbor, direct query too.
         if Hub.Key /= Null_Key and then Now - Hub.Key_Time <
            Globals.Options.G2_Search_KeyDuration
         then

            -- Reuse cached kuery qey.
            Query_Hub (Hub);

         elsif Is_Neighbor (S (Hub.Address)) then

            Trace.Log ("Querying neigbor " & S (Hub.Address), Trace.Debug);
            Query_Hub (Hub);

         else

            Trace.Log ("Requesting key to " & S (Hub.Address), Trace.Debug);
            -- Create QKR
            P := G2.Packet.Create ("QKR");

            if Internet_Route >= Nat then -- Firewalled case
               declare
                  Queue : Packet.Queue.Address_Queue;
               begin
                  Get_Next_Queue (Queue);
                  if Queue.Address /= Null_Ustring then
                     -- Trace.Log ("FIREWALLED QKR TO " & S (Queues (I).Address), Trace.Always);
                     C := G2.Packet.Create ("QNA",
                        To_Char_Array (
                           S (Hub.Address),
                           G2.Packet.Big_Endian (P)));
                     G2.Packet.Add_Child (P, C);
                     Packet.Queue.Send (Queue.Queue.all, P, S (Queue.Address));
                     -- Mark as accessed
                     Hub.Last_Access := Now;
                  else
                     return;  -- <-- EARLY EXIT, NO QUEUE TO ROUTE TO
                  end if;
               end;

            else
               if Internet_Route = NatForward then
                  C := G2.Packet.Create ("RNA",
                     To_Char_Array (
                        Network_Settings.Get_NATF_Address & ":" &
                        Misc.To_String (Globals.Options.G2_port),
                        G2.Packet.Big_Endian (P)));
               else
                  C := G2.Packet.Create ("RNA",
                     To_Char_Array (
                        Socket.IP.Get_IP (Public => true) & ":" &
                        Misc.To_String (Globals.Options.G2_port),
                        G2.Packet.Big_Endian (P)));
               end if;
               G2.Packet.Add_Child (P, C);
               -- And send it (UDP)!
               G2.Packet.Queue.Send (
                  Parent.Sender.all,
                  P,
                  Socket.To_Address (S (Hub.Address)),
                  Safe => false);
            end if;
         end if;
      exception
         when E : others =>
            Trace.Log ("G2.Search.Query_Event: " & Trace.Report (E),
               Trace.Error);
      end Process_Query_Event;

      ------------------------
      -- Program_Node_Query --
      ------------------------
      -- Creates events for querying and checking if alive:
      procedure Program_Node_Query (
         Hub      : access Node_Type;
         For_Time : in     Calendar.Time)
      is
         Event : Event_Queue.Event_Type;
         Drift : Duration :=
            G2.Transceiver.Get_Outbound_Udp_Delay (Parent.Transceiver.all);
         True_For_Time : Calendar.Time := For_Time;
         Now           : Calendar.Time := Calendar.Clock;
      begin
         if True_For_Time < Next_Packet then
            True_For_Time := Next_Packet;
         end if;

         if Hub.Next_QEvent < For_Time then
            Hub.Next_QEvent := True_For_Time + Drift;
            Hub.Scheduled := true;
         else
            return; -- <-- EARLY EXIT IF LATER EVENT ALREADY PROGRAMMED
         end if;

         Next_Packet := Next_Packet + Globals.Options.G2_Search_SendingThrottle;

         if Next_Packet < Now then
            Next_Packet := Now;
            Parent.Latency := 0.0;
            Statistics.Object.Set (Stat_Latency, Statistics.Strings.Create ("0.0"));
         else
            Parent.Latency := Next_Packet - Now;
            Statistics.Object.Set (Stat_Latency, Statistics.Strings.Create (
               Misc.Image (Next_Packet - Now)));
         end if;

         -- Trace.Log ("Scheduling query for " & S (Hub.Address), Trace.Debug);

         -- Search event
         Event_Queue.Create (
            Parent.Events,
            Event,
            True_For_Time + Drift,
            Query_Event'Access,
            Node_Context_Type'(
               Agpl.Event_Queues.Context_Type with
                  Parent  => Object_Access (Parent),
                  Address => Hub.Address));
         -- Deadline event:
         Event_Queue.Create (
            Parent.Events,
            Event,
            True_For_Time + Globals.Options.G2_Search_HubTimeout + Drift,
            Drop_Event'Access,
            Node_Context_Type'(
               Agpl.Event_Queues.Context_Type with
                  Parent  => Object_Access (Parent),
                  Address => Hub.Address));
         Statistics.Object.Set (Stat_Events,
            Statistics.Integers.Create (Event_Queue.Length (parent.Events)));
      end Program_Node_Query;

      -------------------------
      -- Program_Start_Nodes --
      -------------------------
      -- Searches starters, and  if they're queriable they're programmed to
      -- be queried.
      -- Adds too regular cached hubs to complete 10 starting hubs.
      -- Tries first with the search cache and then with the global cache.
      -- The global cache is currently disabled because it causes problems
      --    with neighbor hubs.
      procedure Program_Start_Nodes is
         use Hub_List;
         I    : Iterator_Type := First (Starters);
         J    : Iterator_Type;
         Node : Node_Access;
         Now  : Calendar.Time := Calendar.Clock;
         Started : Natural := 0;
      begin
         Trace.Log ("PROGRAMMING START NODES", Trace.Never);
         while I /= Back (Starters) loop
            J := Find (Hubs, Key (I));
            if J /= Back (Hubs) then
               Node := Get_Access (J);
               if not Node.Scheduled then
                  -- Program query:
                  Program_Node_Query (Node, Now);
                  Started := Started + 1;
               end if;
            else
               -- Add (will be queried inside):
               Add_New_Hub (Key (I));
               Started := Started + 1;
            end if;
            I := Succ (I);
         end loop;

         I := First (Hubs);
         while Started < Start_Servers_Num and then I /= Back (Hubs) loop
            Node := Get_Access (I);
            if not Node.Scheduled then
               Program_Node_Query (Node, Now);
               Started := Started + 1;
            end if;
            I := Succ (I);
         end loop;

         -- From global cache:
         if false then
            if Started < Start_Servers_Num then
               declare
                  Servers : Server.Object_Access_Array :=
                     Server.List.Get_Best (Network_Id, Start_Servers_Num - Started);
               begin
                  for I in Servers'range loop
                     declare
                        Addr : constant String := Server.Id (Servers (I).all);
                     begin
                        Server.List.Check_In (Servers (I));
                        Add_New_Hub (Addr);
                     end;
                  end loop;
               end;
            end if;
         end if;
      end Program_Start_Nodes;

      ---------------
      -- Query_Hub --
      ---------------
      -- Assumes we already have a valid key for the hub.
      procedure Query_Hub (Hub : in Node_Access) is
         Srch : G2_Search_Access;
         P, C : G2.Packet.Object;
         Now  : constant Calendar.Time := Calendar.Clock;
         Queue: Packet.Queue.Address_Queue;
         use Network_Settings;
      begin
         Select_Search_For (Hub, Srch);

         if Srch = null then
            Trace.Log ("G2.Search: No candidate searches for " & S (Hub.Address),
               Trace.Debug);
            -- Reprogram it!
            if not Hub.Scheduled then
               Program_Node_Query (Hub, Now + Globals.Options.G2_Search_HubRestPeriod);
            end if;
            return;
         end if;

         -- Create query packet and send it:
         P := G2.Packet.Create ("Q2", Srch.Guid);

         if Is_Neighbor (S (Hub.Address)) then
            -- No UDP child
            null;
         elsif Internet_Route >= Nat then
            -- If firewalled, send it via neighbor:
            Get_Next_Queue (Queue);
            if Queue.Address /= Null_Ustring then
               C := G2.Packet.Create ("UDP",
                  To_Char_Array (S (Queue.Address), G2.Packet.Big_Endian (P)) &
                  Hub.Key);
               G2.Packet.Add_Child (P, C);
            else
               return; -- <-- EARLY EXIT, NO QUEUE TO ROUTE TO
            end if;
         else
            -- Regular direct reply by udp
            if Internet_Route = Nat then
               C := G2.Packet.Create ("UDP",
                  To_Char_Array (
                     Network_Settings.Get_NATF_Address & ":" &
                     Misc.To_String (Globals.Options.G2_port),
                     G2.Packet.Big_Endian (P)) & Hub.Key);
            else
               C := G2.Packet.Create ("UDP",
                  To_Char_Array (
                     Socket.IP.Get_IP (Public => true) & ":" &
                     Misc.To_String (Globals.Options.G2_port),
                     G2.Packet.Big_Endian (P)) & Hub.Key);
            end if;
            G2.Packet.Add_Child (P, C);
         end if;

         -- Get Kind and compose according child:
         case Srch.Payload.Kind is
            when Adagio.Searches.Sha1_Digest =>
               C := G2.Packet.Create ("URN", "sha1" & Nul &
                  Sha1.To_Char_Array (Srch.Payload.Digest));
            when Adagio.Searches.Keywords =>
               C := G2.Packet.Create ("DN", S (Srch.Payload.Words));
         end case;
         G2.Packet.Add_Child (P, C);

         Hub.Last_Access := Calendar.Clock;

         -- Send the packet UDP/TCP:
         if Is_Neighbor (S (Hub.Address)) then
            Send_To_Neighbor (S (Hub.Address), P);
         elsif Internet_Route > Nat then
            Packet.Queue.Send (Queue.Queue.all, P, S (Queue.Address));
         else
            G2.Packet.Queue.Send (
               Parent.Sender.all,
               P,
               Socket.To_Address (S (Hub.Address)),
               Safe => false);
         end if;

      end Query_Hub;

      -------------------------------
      -- Perform_Searches_Rollback --
      -------------------------------
      procedure Perform_Searches_Rollback is
         use Search_List;
         I : Iterator_Type := First (Searches);
         X : Iterator_Type;
      begin
         Trace.Log ("G2.Search: Starting searches rollback...", Trace.Debug);
         while I /= Back (Searches) loop
            exit when Key (I) = Natural'Last; -- Idles must remain idle.
            X := Succ (I);
            Insert (Searches, Element (I).Priority_Delta, Element (I));
            Delete (Searches, I);
            I := X;
         end loop;
         Trace.Log ("G2.Search: Finished searches rollback.", Trace.Debug);
      end Perform_Searches_Rollback;

      -----------
      -- Purge --
      -----------
      procedure Purge is
         C    : Chronos.Object;
         Now  : Calendar.Time := Calendar.Clock;
         P    : Natural := 0;
         procedure Purge_Search (Srch : access G2_Search) is
            use Searched_List;
            I : Iterator_Type := First (Srch.Searched_Nodes);
            J : Iterator_Type;
         begin
            while I /= Back (Srch.Searched_Nodes) loop
               J := Succ (I);
               if Now - Element (I).Last_Reply > Globals.Options.G2_Search_MinimumRequeryWait
               then
                  Delete (Srch.Searched_Nodes, I);
                  P := P + 1;
               end if;
               I := J;
            end loop;
         end Purge_Search;
      begin
         if not Must_Search then
            return;
         end if;

         Trace.Log ("G2.Search.Purge starting...", Trace.Debug);
         declare
            use Hub_List;
            I : Iterator_Type := First (Hubs);
            J : Iterator_Type;
         begin
            while I /= Back (Hubs) loop
               J := Succ (I);
               if Is_Dropable (Get_Access (I)) then
                  Discount_Hub_Data (Get_Access (I));
                  Delete (Hubs, I);
                  P := P + 1;
               end if;
               I := J;
            end loop;
         end;
         if P > 0 then
            Trace.Log ("G2.Search.Purge:" & P'Img & " hubs purged.", Trace.Debug);
         else
            Trace.Log ("G2.Search.Purge: 0 hubs purged.", Trace.Debug);
         end if;
         P := 0;
         declare
            use Search_List;
            I : Iterator_Type := First (Searches);
         begin
            while I /= Back (Searches) loop
               Purge_Search (Element (I));
               I := Succ (I);
            end loop;
         end;
         Trace.Log ("G2.Search.Purge:" & P'Img & " search nodes purged.", Trace.Debug);
         Trace.Log ("G2.Search.Purge done: " & Chronos.Image (C), Trace.Debug);
      end Purge;

      -----------------------
      -- Select_Search_For --
      -----------------------
      -- Gives the search to be sent to a hub, or null if no valid candidate!
      -- Updates priority counters
      procedure Select_Search_For (Hub : in Node_Access; Srch : out G2_Search_Access) is
         use Search_List;
         use Searched_List;
         Candidate: Search_list.Iterator_Type;
         I        : Searched_List.Iterator_Type;
         Now      : Calendar.Time := Calendar.Clock;
         Index    : Natural;
      begin
         Srch      := null;
         Candidate := First (Searches);
         while Candidate /= Back (Searches) loop
            Srch  := Element (Candidate);
            Index := Key     (Candidate);
            -- Skip if paused:

            if not Srch.Paused then
               -- Look for the hub in the list of the search:
               I := Find (Srch.Searched_Nodes, S (Hub.Address));
               exit when I = Back (Srch.Searched_Nodes) or else
                 Now - Element (I).Last_Reply > Globals.Options.G2_Search_MinimumRequeryWait;
            end if;

            -- At this point, the candidate search is rejected because queried not much ago
            Candidate := Succ (Candidate);
            Srch      := null;
         end loop;

         -- Move back the search:
         if Srch /= null then
            -- Check if a rollback in indexes must be done:
            -- The -1 is because we don't want a non-idle search getting the idle index.
            if Index = Natural'Last then
               null; -- An idle search.
            elsif Natural'Last - Index - 1 < Srch.Priority_Delta then
               Perform_Searches_Rollback;
            else
               -- Move back selected search:
               Trace.Log ("Moving index from" & Index'Img & " to" &
                  Natural'Image (Index + Srch.Priority_Delta), Trace.Debug);
               Index := Index + Srch.Priority_Delta;
               Insert (Searches, Index, Element (Candidate));
               Delete (Searches, Candidate);
            end if;
--            Trace.Log ("Selected search: " & Adagio.Searches.To_String (Srch.Search),
--               Trace.Always);
         end if;
      end Select_Search_For;

      --------------------
      -- Send_To_Neighbor --
      --------------------
      procedure Send_To_Neighbor (Address : in String; P : in Packet.Object) is
      begin
         for I in Queues'Range loop
            exit when Queues (I).Address = Null_Ustring;

            if S (Queues (I).Address) = Address then
               Packet.Queue.Send (Queues (I).Queue.all, P, S (Queues (I).Address));
               return;
            end if;
         end loop;
      end Send_To_Neighbor;

      --------------------
      -- Send_To_Queues --
      --------------------
      procedure Send_To_Queues (P : in Packet.Object) is
      begin
         for I in Queues'Range loop
            exit when Queues (I).Address = Null_Ustring;

            Packet.Queue.Send (Queues (I).Queue.all, P, S (Queues (I).Address));
         end loop;
      end Send_To_Queues;

      ------------------
      -- Set_Priority --
      ------------------
      procedure Set_Priority (
         Target   : in Adagio.Searches.Search_Id;
         Priority : in Adagio.Searches.Priorities;
         PDelta   : in Natural)
      is
         use Search_List;
         I : Iterator_Type := First (Searches);
         Srch : G2_Search_Access;
      begin
         while I /= Back (Searches) loop
            if Element (I).Search = Target then
               Srch := Element (I);
               Srch.Priority       := Priority;
               Srch.Priority_Delta := PDelta;
               Delete (Searches, I);
               Insert (Searches, Get_Index_For (Srch), Srch);
               exit;
            end if;
            I := Succ (I);
         end loop;
      end Set_Priority;

      ----------------
      -- Set_Queues --
      ----------------
      procedure Set_Queues (New_Queues : in Packet.Queue.Address_Queue_Array) is
      begin
         Queues (New_Queues'Range) := New_Queues;
         Queues (New_Queues'Last + 1 .. Queues'Last) := (others => (Address => Null_Ustring, Queue => null));
      end Set_Queues;

      ---------------------
      -- Set_Start_Nodes --
      ---------------------
      procedure Set_Start_Nodes (Nodes : Ustring_Array) is
         use Hub_List;
      begin
         Clear (Starters);
         for N in Nodes'Range loop
            Trace.Log ("Adding search start address: " & S (Nodes (N)),
               Trace.Never);
            Insert (Starters, S (Nodes (N)), (
               Address     => Nodes (N),
               Scheduled   => false,
               Next_QEvent => Past_Aeons,
               Last_Access => Past_Aeons,
               Last_QA     => Past_Aeons,
               Unknown_Leaves => true,
               Growing     => false,
               Leaves      => 0,
               Alive       => false,
               Key         => Null_Key,
               Key_Time    => Calendar.Clock));
         end loop;
      end Set_Start_Nodes;

      -----------
      -- Start --
      -----------
      procedure Start is
      begin
         Queues :=
            new Packet.Queue.Address_Queue_Array (
               1 .. Globals.Options.G2_ActiveServers);
      end Start;

      -----------------
      -- Must_Search --
      -----------------
      function Must_Search return Boolean is -- True if searches enqueued
         use Search_List;
         I : Iterator_Type;
      begin
         if Is_Empty (Searches) then
            return false;
         else
            I := First (Searches);
            while I /= Back (Searches) loop
               if not Element (I).Paused then
                  return true;
               else
                  I := Succ (I);
               end if;
            end loop;
         end if;

         return false;
      end Must_Search;

   end Safe_Object;

   procedure Debug_Test (This : in out Object) is
      n : Natural;
   begin
      Text_IO.Put_Line ("Search debug in");
      n := This.Safe.Get_Index_For (null);
      Text_IO.Put_Line ("Search debug out");
   exception
      when others =>
         Text_IO.Put_Line ("Search debug out ex");
   end Debug_Test;

end Adagio.G2.Search;
