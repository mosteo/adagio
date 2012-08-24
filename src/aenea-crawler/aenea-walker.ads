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
--  $Id: aenea-walker.ads,v 1.10 2004/03/09 23:46:49 Jano Exp $

with Aenea.Hub;
with Aenea.Signals;
with Aenea.Types;

with Agpl.Event_queues;
with Agpl.Http.Server.Sort_handler;
with Agpl.Protected_Values.Duration;
with Agpl.Protected_Values.Time;

with Adagio.G2.Packet.Queue;

with Ada.Calendar;

package Aenea.Walker is

   pragma Elaborate_Body;

   Unexpected_Packet : exception;

   Debug_Stop : Boolean := False;
   --  Forcing a stop, messages will be no longer sent. For debugging.

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Restores hubs
   -- Creates the first event to process received packets.
   procedure Start;

   ------------------------------------------------------------------------
   -- Event_Maintenance                                                  --
   ------------------------------------------------------------------------
   -- Periodic tasks (saving of hubs, best uptimes...)
   procedure Event_Maintenance;

   ------------------------------------------------------------------------
   -- Adder                                                              --
   ------------------------------------------------------------------------
   protected Adder is
      procedure Add_Found (Address : in String);
         -- Add if missing and send Found signal
      function  Count return Natural;
      function  Best_Uptimes (Num : in Positive := 10) return Types.Uptimes_Array;
      procedure Save_hubs;
      procedure Restore_hubs;
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
      entry Process_Item (Item : in G2.Packet.Queue.Item_type);
      procedure Process_Signal (
         Address  : in String;
         Sequence : in Types.Sequences;
         Signal   : in Signals.Object);
      procedure Obliterate;
      procedure Purge;
      procedure Shutdown;
   private
      procedure Add_Ready  (Address : in String; H : out Hub.Object_Access);

      Last_Purge : Ada.Calendar.Time := Ada.Calendar.Clock;
      Hubs       : Hub.Hub_map.Container_type;
   end Adder;

   -- Purged hosts
   Last_Number_Purged : Natural := 0;
   pragma Atomic (Last_Number_Purged);

   Last_Number_Purged_Unique : Natural := 0;
   pragma Atomic (Last_Number_Purged_Unique);

   procedure Process_item (Item : in G2.Packet.Queue.Item_type) renames
      Adder.Process_item;

   ------------------------------------------------------------------------
   -- Receive_Signal                                                     --
   ------------------------------------------------------------------------
   -- For timed events (signals)
   procedure Receive_Signal (Context : in Agpl.Event_queues.Context_access);

   ------------
   -- Report --
   ------------
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

   ------------
   -- Worker --
   ------------
   type Worker_Code is access procedure;

   task type Worker (Name : access String; Work : Worker_Code) is
      pragma Storage_Size (1024 * 1024);
   end Worker;
   --  We'll use this task type to create some threads.

   type Worker_Access is access all Worker;

end Aenea.Walker;
