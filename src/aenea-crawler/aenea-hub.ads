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
--  $Id: aenea-hub.ads,v 1.11 2004/03/22 07:14:55 Jano Exp $

with Aenea.Types;

with Agpl.Event_Queues.Calendar;
with Agpl.Geoip;
with Agpl.Http.Server.Sort_Handler.Counter_List;

with Charles.Maps.Sorted.Strings.Unbounded;

with Ada.Calendar;
use  Ada;

package Aenea.Hub is

   pragma Elaborate_Body;

   -- Temporarily necessary until Shareaza stats code is upgraded.
   Vendor_GDNA : constant String := "GDNA";
   Vendor_RAZA : constant String := "RAZA";
   Vendor_UNKN : constant String := "UNKN";

   Version_UNKN : constant STring := "Unknown";

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- A single server found
   type Object is private;
   type Object_access is access all Object;

   type Status_Type is (
      Dead,
      Gone,
      Ready,
      Unknown,
      Leaf_Waiting,
      Leaf_Checking,
      Leaf_Resting,
      Hub_Waiting,
      Hub_Checking,
      Erroneous);

   -- Explanation of status:
   -- READY: a virtual status. A newly found hub is created in ready status and
   --    immediately is sent a FOUND signal so it goes to UNKNOWN status.
   -- DEAD: can be purged. Doesn't answer and we don't want nothing to do
   --    with it.
   -- GONE: as dead, but it can't still be converted to UNKNOWN. This way
   --    we can have a buffer period to no re-add recently checked hubs.
   --    The period in this status could be made null, in fact making it moot.
   -- UNKNOWN: the hub has been reported as alive (GWebCache, Neighbor) but
   --    hasn't answered to us. If timeout, will go to GONE.
   -- WAITING:  we are waiting for the next check time, doing nothing with it.
   -- CHECKING: we are waiting for an answer from this already alive host.
   -- LEAF_RESTING: a pinged leaf, will wait some time before being pinged again.
   -- Notes about the leaves states: leaves alive are not pinged periodically.
   --    They're pinged when found in a neighbor list, and not more than once
   --    by period. Hence the need for a Leaf resting status.

   ------------------------------------------------------------------------
   -- Hub_map                                                            --
   ------------------------------------------------------------------------
   package Hub_map is new Charles.Maps.Sorted.Strings.Unbounded (
      Object_access,
      "<",
      "=");

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This    : out Object;
      Address : in String;
      Status  : in Status_Type);

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Returns a string uniquely identifying the hub (ip:port)
   function Id (This : in Object) return String;
   pragma Inline (Id);

   ------------------------------------------------------------------------
   -- Sequence                                                           --
   ------------------------------------------------------------------------
   -- The sequence for event synchrony.
   function Sequence (This : in Object) return Types.Sequences;
   pragma Inline (Sequence);

   ------------------------------------------------------------------------
   -- Status                                                             --
   ------------------------------------------------------------------------
   procedure Set_Status (This : in out Object; Status : in Status_Type);
   function  Get_Status (This : in Object) return Status_Type;

   ------------------------------------------------------------------------
   -- Set_leaves                                                         --
   ------------------------------------------------------------------------
   procedure Set_leaves (This : in out Object; Leaves : Natural);

   ------------------------------------------------------------------------
   -- Leaves                                                             --
   ------------------------------------------------------------------------
   function Leaves (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Set_max_leaves                                                     --
   ------------------------------------------------------------------------
   procedure Set_max_leaves (This : in out Object; Leaves : Natural);

   ------------------------------------------------------------------------
   -- Max_leaves                                                         --
   ------------------------------------------------------------------------
   function Max_leaves (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Add_answer                                                         --
   ------------------------------------------------------------------------
   -- Adds 1 to the answers statistic
   procedure Add_answer (This : in out Object);

   ------------------------------------------------------------------------
   -- Failures                                                           --
   ------------------------------------------------------------------------
   function Failures (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Set_failures                                                       --
   ------------------------------------------------------------------------
   procedure Set_failures (This : in out Object; Failures : in Natural := 0);

   ------------------------------------------------------------------------
   -- Relocate                                                           --
   ------------------------------------------------------------------------
   -- Re-checks the geo location, just in case the IP range has been moved.
   procedure Relocate (This : in out Object);

   ------------------------------------------------------------------------
   -- Get_country_code                                                   --
   ------------------------------------------------------------------------
   function Get_country_code (This : in Object)
      return Agpl.Geoip.Country_code;

   ------------------------------------------------------------------------
   -- Vendor data                                                        --
   ------------------------------------------------------------------------
   procedure Set_Vendor (This : in out Object; Vendor : in String);
   function Get_Vendor (This : in Object) return String;

   procedure Set_Version (This : in out Object; Version : in String);
   function Get_Version (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Nick                                                           --
   ------------------------------------------------------------------------
   function Get_Nick (This : in Object) return String;
   pragma Inline (Get_Nick);

   ------------------------------------------------------------------------
   -- Classifier packages                                                --
   ------------------------------------------------------------------------
   package By_Vendor  is new Agpl.Http.Server.Sort_Handler.Counter_List;
   package By_Version is new Agpl.Http.Server.Sort_Handler.Counter_List;
   package By_Uptime  is new Agpl.Http.Server.Sort_Handler.Counter_List;
   package By_Leaves  is new Agpl.Http.Server.Sort_Handler.Counter_List;
   package By_Status  is new Agpl.Http.Server.Sort_Handler.Counter_List;

   ------------------------------------------------------------------------
   -- Multiplier                                                         --
   ------------------------------------------------------------------------
   -- Requires the vendor is informed.
   function Compute_Multiplier (Vendor : in String) return Float;

   ------------------------------------------------------------------------
   -- Is_Alive                                                           --
   ------------------------------------------------------------------------
   -- Hub_Checking or Hub_Waiting
   function Is_Alive (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Stats                                                              --
   ------------------------------------------------------------------------
   function  Get_Last_Seen (This : in Object) return Ada.Calendar.Time;
      pragma Inline (Get_Last_Seen);
   function  Get_First_Seen (This : in Object) return Ada.Calendar.Time;
      pragma Inline (Get_First_Seen);
   procedure Add_To_Uptime (This : in Object; Amount : in Integer := 1);
   procedure Update_Uptime (This : in out Object);

   procedure Add_To_By_Leaves (This : in out Object; Amount : in Integer);
   procedure Del_To_By_Leaves (This : in out Object);
   procedure Update_By_Leaves (This : in out Object; Amount : in Integer);

   function  Stats (This : in Object) return String;

private

   use Hub_map;
   use type Ada.Calendar.Time;

   type Payloads is array (Integer range <>) of Character;

   type Object is record
      -- Status
      Status   : Status_Type;

      -- Sequence of actions
      -- Timeouts must match this (increased in every ask)
      Sequence : Types.Sequences := 0;

      -- Address in human format (includes port):
      Address : Ustring := Null_ustring;
      -- Leaves:
      Leaves, Max_leaves  : Natural := 0;
      -- Pings done after last query:
      Pings   : Natural := 0;
      -- Consecutive failures:
      Failures          : Natural := 0;

      -- Location
      Country_code : Agpl.Geoip.Country_code := "??";
      Country_name : Ustring                 := U ("Unknown");

      Nick         : Ustring                 := U ("Anonymous");

      -- Vendor
      Vendor       : Ustring := U (Vendor_UNKN);
      Version      : Ustring := U (Version_UNKN);

      -- Event (keep last one just in case)
      Event        : Agpl.Event_Queues.Calendar.Event_Type;

      -- Uptime key (for stats)
      Uptime       : Natural := 0; -- In hours

      -- Stats:
      Stat_pings            : Natural := 0;
      Stat_queries          : Natural := 0;
      Stat_answers          : Natural := 0;
      Stat_answers_in_a_row : Natural := 0;
      Stat_first_seen       : Ada.Calendar.Time := Ada.Calendar.Clock;
         -- First seen in this "aliveness"
      Stat_last_seen        : Ada.Calendar.Time := Ada.Calendar.Clock;
         -- Last seen alive
      Stat_leaves           : Natural := 0;      -- Only for histogram purposes
   end record;

end Aenea.Hub;
