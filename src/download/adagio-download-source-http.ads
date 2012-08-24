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

with Adagio.Http.Handshaker;
with Adagio.Http.Header;
with Adagio.Socket;
with Adagio.Xml;

with Agpl.Chronos;
with Agpl.Streams.Filter;
with Agpl.Streams.Filter.Bandwidth_Throttle;
with Agpl.Streams.Filter.Buffered_Unbounded;
with Agpl.Streams.Filter.Deflate_Unbounded;

with Ada.Finalization;

--  Sources may be of many kinds, provide implementations for each one.
--  One for each protocol, that's it.

package Adagio.Download.Source.Http is

   pragma Elaborate_Body;

   type Object is new Source.Object with private;
   type Object_Access is access all Object;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object);

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Unique id for the source.
   function Get_Id (This : access Object) return Source_Id;

   ------------------------------------------------------------------------
   -- Is_Active                                                          --
   ------------------------------------------------------------------------
   -- Say if the source is active (downloading, whatever)
   function Is_Active (This : access Object) return Boolean;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- All processsing must occur here.
   -- Once source is disposable, set Finished to true
   -- Set Again_In to the delay this source request (just orientative).
   procedure Process (
      This     : access Object; 
      Finished :    out Boolean;
      Again_In :    out Duration);

   ------------------------------------------------------------------------
   -- Set_Paused                                                         --
   ------------------------------------------------------------------------
   procedure Set_Paused (This : access Object; Paused : in Boolean := true);

   ------------------------------------------------------------------------
   -- From_Xml                                                           --
   ------------------------------------------------------------------------
   function From_Xml (Node : in Xml.Node) return Object_Access;

private

   ------------------------------------------------------------------------
   -- Receive                                                            --
   ------------------------------------------------------------------------
   -- Reads from the socket if data available
   procedure Receive (This : in out Object);

   package ASFilter     renames Agpl.Streams.Filter;
   package ASFBThrottle renames Agpl.Streams.Filter.Bandwidth_Throttle;
   package ASFBuffer    renames Agpl.Streams.Filter.Buffered_Unbounded;
   package ASFDeflate   renames Agpl.Streams.Filter.Deflate_Unbounded;
   package Header       renames Adagio.Http.Header;

   type Status_Type is (
      Ready,         -- Ready to start connecting or pushing
      Waiting_Push,  -- Awaiting push connection
      Connecting,    -- Awaiting connection completion
      Disconnected,  -- Can't connect, waiting next back-off time to expire
      Requesting,    -- Awaiting handshaking completion
      Downloading,   -- Downloading valid data
      Skipping,      -- Skipping response data from a 404, 503/queue, etc...
      Queued,        -- Waiting for poll time delay expiration
      Paused,        -- Forcefully disabled
      Done           -- Nothing more to do.
      );

   ------------------------------------------------------------------------
   -- Hand_Type                                                          --
   ------------------------------------------------------------------------
   type Hand_Type (Parent : access Object) is 
   new Adagio.Http.Handshaker.Object (Max_Data => 1_000_000) with 
   record
      Stage : Positive range 1 .. 3 := 1;
   end record;
   type Hand_Access is access all Hand_Type;
   ------------------------------------------------------------------------
   -- Got_Answer                                                         --
   ------------------------------------------------------------------------
   procedure Got_Answer (
      This   : in     Hand_Type; 
      Answer : in     Header.Set;
      Reply  :    out Header.Set);
   ------------------------------------------------------------------------
   -- Request_Sent                                                       --
   ------------------------------------------------------------------------
   procedure Request_Sent (This : in out Hand_Type; Finished : out Boolean);

   type Controller (Parent : access Object) is new 
      Ada.Finalization.Limited_Controlled with null record;

   procedure Finalize   (This : in out Controller);

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Source.Object with record
      Urn         : Ustring;
      Address     : Ustring;
      Firewalled  : Boolean;
      Proxies     : Ustring_Vector.Object (First => 1); 
         -- Addresses of hubs connected to the source.
      Link        : aliased Socket.Object;
      Hand        : Hand_Access;
      Status      : Status_Type;

      -- Streams required
      Buffer      : aliased ASFBuffer.Stream_Type;
      Deflater    : aliased ASFDeflate.Stream_Access;
      Throttle    : aliased ASFBThrottle.Stream_Type;
      -- Data is available to be delivered in Buffer, if not inflating, or in
      -- Deflater, if inflating. A pointer to the one to use is set up here:
      Mouth       : Agpl.Streams.Filter.Stream_Access;

      Chunk          : Chunk_Bounds;
      Deflate        : Boolean;
      Content_Length : SECount;    -- Remaining content to be received.
      Response       : Header.Set; -- The response to our request

      Self        : Object_Access := Object'Unchecked_Access; 
         -- To pass it to the handshaker.
      Control     : Controller (Object'Access);

      Cron        : Agpl.Chronos.Object;

      Connect_Failures : Natural  := 0;
      Backoff_Period   : Duration := 600.0;
   end record;

end Adagio.Download.Source.Http;
