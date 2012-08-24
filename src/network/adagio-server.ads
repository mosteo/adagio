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
--  $Id: adagio-server.ads,v 1.7 2004/02/05 18:31:22 Jano Exp $

with Ada.Calendar; use Ada;
with Ada.Finalization;
with Ada.Streams;

with Agpl.Http.Server.Sort_handler;

with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;

use Ada;

package Adagio.Server is

   pragma Elaborate_body;

   type Stream_access is access all Streams.Root_stream_type'Class;

   -- A server is a entry point for a network.
   -- Usually, we maintain an active connection to it.
   type Object is abstract new Finalization.Controlled with private;
   type Object_access is access all Object'Class;
   type Object_access_array is Array(Positive range <>) of Object_access;

   -- Get a unique id to identify it:
   function Id(this: in Object) return String is abstract;

   -- Get a textual description:
   function Describe (this : in Object) return String is abstract;

   -- Get network it belongs:
   function Net(this: in Object) return String is abstract;

   subtype Rating is float range 0.0 .. float'Last;

   -- Evaluate its goodness to be connected:
   function Rate(this: in Object) return Rating is abstract;

   -- True when the server is to be purged:
   function Dropable (this : in Object) return Boolean is abstract;

   -- True when the server is candidate to be selected, false otherwise.
   function Is_Ready (This : in Object) return Boolean is abstract;

   -- Establish a connection:
   procedure Connect(this: in out Object) is abstract;

   -- Disconnect:
   procedure Disconnect(this: in out Object) is abstract;

   -- Dump:
   -- Must be used in each implementor for the 'Output stream attribute
   procedure Serialize
     (Stream: access Streams.Root_stream_type'Class;
      this: in Object) is abstract;

   -- Recover:
   -- Must be used in each implementor for the 'Input stream attribute
   function Restore
     (Stream: access Streams.Root_stream_type'Class) return Object
   is abstract;

   -- Delete a pointed object:
   procedure Free (this: in out Object_access);

   -- Auxiliary type:
   type Server_slot is record
      Server        : Adagio.Server.Object_access;
      Available     : Boolean       := true;
      Last_check_in : Calendar.Time := Calendar.Clock;
      Since         : Calendar.Time := Calendar.Clock;
   end record;
   type Server_slot_access is access all Server_slot;

   -- Helper function to compare Object_access:
   function Equal(Left, Right: in Server_slot) return boolean;

   -- Hash map that we will use for servers:
   package Server_list is new Charles.Maps.Hashed.Strings.Unbounded
     (Server_slot, Charles.Hash_string, "=");

   Server_already_cached : Exception;

   -- List of servers:
   protected List is
      -- Add a server:
      -- If already there: Will be freed, This <-- null,
      --    and Server_already_cached raised
      procedure Add (
         this  : in out Object_access; 
         Since : in     Calendar.Time := Calendar.Clock);

      -- Remove a server
      -- Caller is always responsible of deletion of memory (freeing it)
      procedure Delete(this: in Object_access);

      -- Drop a server by Id. It's freed and deleted. If checked out, 
      --    success will be false. Also if not found.
      procedure Drop (This : in String; Success : out Boolean);

      -- Returns server to available mode.
      procedure Check_in(this: in Object_access);

      -- Dump:
      procedure Serialize(Stream: in Stream_access);

      -- Restore:
      procedure Restore(Stream: in Stream_access);

      -- Startup
      procedure Initialize;

      -- Saving (if needed):
      procedure Save;

      -- Purge servers with rating = 0 and not in use:
      procedure Purge;

      -- Purge servers from a network, keeping at most some quantity:
      procedure Purge(Net: String; Keep: Natural);

      -- Available servers from a given net:
      function Available(Net: String) return Natural;

      -- Obtain the N best servers for a network:
      -- These servers are marked as check-out, should be checked-in
      -- when not in use.
      function Get_best(Net: String; Quantity: Positive)
         return Object_access_array;

      -- Http_report:
      procedure Http_report (
         Data : out Agpl.Http.Server.Sort_handler.Data_set);

   private

      Servers: Server_list.Container_type;

      Dirty: boolean:= false;                -- Marks if needs saving

   end List;

private

   type Object is abstract new Finalization.Controlled 
      with null record;
--      Initialized : Boolean := false;
--   end record;

   procedure Adjust     (This : in out Object);
   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

end Adagio.Server;
