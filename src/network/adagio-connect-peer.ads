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
--  $Id: adagio-connect-peer.ads,v 1.3 2004/01/21 21:05:36 Jano Exp $

--  For general purpose connections, like chats, browses, etc.

with Ada.Finalization;
with Ada.Unchecked_deallocation;
use  Ada;

package Adagio.Connect.Peer is

   ------------------------------------------------------------------------
   -- Context_type                                                       --
   ------------------------------------------------------------------------
   -- Used for communication between peers and the peer manager.
   type Context_type is record
      -- IN  COMPONENTS
      -- OUT COMPONENTS
      Sleep   : Duration := 0.5;   -- Desired time to wait
      Is_done : Boolean  := false; -- Set it to true to indicate end.
                                   -- The manager will free the memory.
   end record;

   ------------------------------------------------------------------------
   -- Generic_Params                                                     --
   ------------------------------------------------------------------------
   -- Type used to pass params in a synchronous call to a managed peer:
   -- Used in Signal
   type Generic_Params is tagged record
      Is_Done : Boolean := false; -- Set it to true inside the Signal call.
                                  -- The manager will dispose everything.
   end record;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- Abstract base class for peer connections.
   type Object is abstract new Finalization.Limited_controlled with null record;
   type Object_access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id
   function Id (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (This : in out Object; Context : in out Context_type)
   is abstract;

   ------------------------------------------------------------------------
   -- Signal                                                             --
   ------------------------------------------------------------------------
   -- Used to call synchronously from other process (not the manager).
   -- Default implementation does nothing.
   procedure Signal (This : in out Object; Params : in out Generic_Params'Class);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object) is abstract;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   -- Delete an object from heap
   procedure Free is new Unchecked_deallocation (Object'Class, Object_access);

end Adagio.Connect.Peer;
