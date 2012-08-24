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
--  $Id: adagio-connect-peer_manager.ads,v 1.3 2004/01/21 21:05:36 Jano Exp $

--  For general purpose connections, like chats, browses, etc.

with Adagio.Connect.Peer;

with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;

package Adagio.Connect.Peer_manager is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Already_connected : exception;

   ------------------------------------------------------------------------
   -- Helper types                                                       --
   ------------------------------------------------------------------------
   type Slot_type is record
      Peer : Connect.Peer.Object_access;
   end record;

   package Slot_list is new Charles.Maps.Hashed.Strings.Unbounded (
      Slot_type, Charles.Hash_string, "=", "=");

   protected Object is

      ---------
      -- Add --
      ---------
      --  Enqueue a peer for management.
      --  May raise Already_connected;
      procedure Add (This : access Peer.Object'Class);

      --------------
      -- Contains --
      --------------
      --  Says if some peer is already there.
      function Contains (Id : in String) return Boolean;

      -------------
      -- Process --
      -------------
      --  Do the processing for a given Id (backcalled from timeout queue).
      procedure Process (Id : in String);

      ------------
      -- Signal --
      ------------
      --  "Signals" a managed peer to do some synchronous processing.
      procedure Signal (Id : in String; Params : in out Peer.Generic_Params'Class);

      ----------------
      -- Terminated --
      ----------------
      --  Forces Finalize/removing of a peer
      procedure Terminated (Id : in String);

   private
      --  List of connected peers:
      Peers : Slot_list.Container_type;
   end Object;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown;

end Adagio.Connect.Peer_manager;
