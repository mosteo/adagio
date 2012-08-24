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
--  $Id: adagio-network.ads,v 1.4 2004/02/03 22:52:15 Jano Exp $

with Adagio.Searches.Handler;
with Adagio.Searches;

with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;

with System;

package Adagio.Network is

   pragma Elaborate_Body;

   use type Searches.Handler.Object_Access;

   -- Network object
   type Object is abstract tagged limited private;
   type Object_access is access all Object'Class;

   type Network_status is (Disconnected, Connecting, Connected);
   pragma Atomic (Network_status);

   -- Gives the network identifier
   function Id(this: in Object) return String is abstract;

   -- Connect to that network. Will get servers and connect them as needed.
   -- Performs searches restauration if necessary.
   procedure Connect(this: in out Object) is abstract;

   -- Says status of the network.
   function Status(this: in Object) return Network_status is abstract;

   -- Disconnect:
   -- Saves searches if necessary.
   procedure Disconnect(this: in out Object) is abstract;

   -- Obtain search handler. Can return null if the network is not to be
   -- searched:
   function Get_Search_Handler (This : in Object) 
      return Searches.Handler.Object_Access is abstract;

   -- Auxiliary type:
   type Network_slot is record
      Network:       Adagio.Network.Object_access;
      Available:     Boolean:= true;
   end record;
   type Network_slot_access is access all Network_slot;

   -- Helper function to compare Object_access:
   function Equal(Left, Right: in Network_slot) return boolean;

   -- Hash map that we will use for servers:
   package Network_list is new Charles.Maps.Hashed.Strings.Unbounded
     (Network_slot, Charles.Hash_string, "=", Equal);
   -- Hash map for search handlers
   package Search_list is new Charles.Maps.Hashed.Strings.Unbounded
     (Searches.Handler.Object_access, Charles.Hash_String, "=", "=");

   -- List of networks:
   protected List is
      pragma Priority (System.Priority'Last);

      -- Add a network:
      procedure Add(this: Object_access);

      -- Prepares the searcher after the network is connected:
      procedure Prepare(this: Object_access);

      -- Returns null if not found
      function Get(Id: in String) return Object_access;

      -- Disconnect all:
      procedure Disconnect_all;

      -- Search related calls:
      procedure Create_Search (This : in Searches.Search_Id);
      procedure Delete_Search (This : in Searches.Search_Id);
      procedure Set_Search_Paused (
         This : in Searches.Search_Id; Paused : in Boolean := true);
      procedure Set_Search_Priority (
         This : in Searches.Search_id; Priority : Searches.Priorities);
      function Get_Custom_Info (This : in Searches.Search_Id) return String;
         
   private
      Networks  : Network_list.Container_type;
      Searchers : Search_list.Container_type;
   end List;

private

   type Object is abstract tagged limited null record;

end Adagio.Network;
