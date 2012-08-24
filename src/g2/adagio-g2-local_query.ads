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
--  $Id: adagio-g2-local_query.ads,v 1.5 2004/01/28 15:33:18 Jano Exp $

--  Queries received from G2

with Adagio.File;
with Adagio.G2.Packet;
with Adagio.G2.Packet.Queue;

package Adagio.G2.Local_query is

   pragma Elaborate_Body;

   Max_query_length : constant := 1024;

   -- Some stats:
   Query_hits : Natural := 0;
   pragma Atomic (Query_hits);

   ------------------------------------------------------------------------
   -- Get_concurrent_searches                                            --
   ------------------------------------------------------------------------
   function Get_concurrent_searches return Natural;

   -- Search files with given keywords
   -- The hubs are passed in readable format (i.e: "127.0.0.1:4610")
   -- Port is the port where we are listening.
   -- They are our current connected neighbours.
   procedure DN (
      Words     : in String;
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : in Packet.Queue.Object_access; 
      Queue_tcp : in Packet.Queue.Object_access; 
      Port      : in Natural;
      Hubs      : in Ustring_array);

   -- Search files by URN
   -- The hubs are passed in readable format (i.e: "127.0.0.1:4610")
   -- Port is the port where we are listening.
   -- They are our current connected neighbours.
   procedure URN (
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : in Packet.Queue.Object_access; 
      Queue_tcp : in Packet.Queue.Object_access; 
      Port      : in Natural;
      Hubs      : in Ustring_array);

   -- Search by metadata
   -- The hubs are passed in readable format (i.e: "127.0.0.1:4610")
   -- They are our current connected neighbours.
   -- Port is the port where we are listening.
   procedure MD (
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : in Packet.Queue.Object_access; 
      Queue_tcp : in Packet.Queue.Object_access; 
      Port      : in Natural;
      Hubs      : in Ustring_array);

   -- Need because of internal workers:
   -- Must be called for finalization.
   procedure Shutdown;

   ------------------------------------------------------------------------
   -- Create_hit_child                                                   --
   ------------------------------------------------------------------------
   procedure Create_hit_child (
      F          : in  File.Object; 
      Big_endian : in  Boolean; 
      Hit        : out Packet.Object);

   ------------------------------------------------------------------------
   -- Create_hit_skeleton                                                --
   ------------------------------------------------------------------------
   -- Prepares a hit to reply to a query.
   function Create_hit_skeleton (
      Query    : in Packet.Queue.Item_type;
      Our_port : Natural;
      Hubs     : Ustring_array) return Packet.Object;

   ------------------------------------------------------------------------
   -- Create_simple_hit_skeleton                                         --
   ------------------------------------------------------------------------
   -- Prepares a simple hit for browse host purposes.
   function Create_simple_hit_skeleton return Packet.Object;

   ------------------------------------------------------------------------
   -- Add_extra_children                                                 --
   ------------------------------------------------------------------------
   -- Adds browse/chat children as necessary
   procedure Add_extra_children (This : in out Packet.Object);

private

   type Query_context (Length : Natural; Num_hubs : Natural) is record
      Words     : String (1 .. Length);             -- Keywords
      Reply_to  : Packet.Queue.Item_type;           -- Q2 packet
      Queue_udp : Packet.Queue.Object_access; 
      Queue_tcp : Packet.Queue.Object_access; 
      Our_port  : Natural;                          -- Our listening port
      Hubs      : Ustring_array (1 .. Num_hubs);    -- Neighbours
   end record;

   procedure Null_procedure (Nothing : in Query_context);

   ------------------------------------------------------------------------
   -- Do_query                                                           --
   ------------------------------------------------------------------------
   procedure Do_query (On : in Query_context);

   ------------------------------------------------------------------------
   -- Send_hit                                                           --
   ------------------------------------------------------------------------
   procedure Send_hit (
      Hit       : Packet.Object;
      Query     : Packet.Queue.Item_type;
      Queue_udp : Packet.Queue.Object_access;
      Queue_tcp : Packet.Queue.Object_access
      );


end Adagio.G2.Local_query;

