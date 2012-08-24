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

with Ada.Calendar;
with Ada.Streams;
use  Ada.Streams;
use  Ada;

with Charles.Multimaps.Sorted.Strings.Unbounded;
with Charles.Sets.Sorted.Unbounded;

generic 
   type Element_type is private;             -- An alternate location.
   with function Key (E : in Element_type) return String;
                                             -- Unique key for the element.
   with function Location (E : in Element_type) return String;
                                             -- Description of the location.
   with function Better (L, R : in Element_type) return Boolean; 
                                             -- "<" semantic
   Default_cached_elements : Positive := 20; -- Default size for each key.
package Adagio.Upload.Mesh is

   pragma Elaborate_Body;

   -- Helper array:
   type Element_array is array (Positive range <>) of Element_type;

   type Slot_type is record
      Element : Element_type;
      Born    : Calendar.Time;
   end record;

   -- Container:
   package Element_list is new Charles.Multimaps.Sorted.Strings.Unbounded (
      Slot_type, "<", "=");

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   -- A mesh with given Id (for example network).
   type Object;
   type Object_access is access all Object;
   protected type Object (Id : access String) is

   ------------------------------------------------------------------------
   -- Configure                                                          --
   ------------------------------------------------------------------------
   -- Set the number of alt-sources to keep for an item and
   -- the time they will last.
   procedure Configure (
      Sources : in Positive := Default_cached_elements;
      TTL     : in Duration := 24.0 * 60.0 * 60.0);

   ------------------------------------------------------------------------
   -- Restore                                                               --
   ------------------------------------------------------------------------
   -- Restore a mesh from hard disk or stream
   procedure Restore (Path   : in     String);
   procedure Restore (Stream : access Root_stream_type'Class);

   ------------------------------------------------------------------------
   -- Save                                                               --
   ------------------------------------------------------------------------
   -- Save to a file or stream
   procedure Save (Path   : in     String);
   procedure Save (Stream : access Root_stream_type'Class);

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   -- Add an element to the mesh with a given age.
   procedure Add (
      Element : in Element_type; 
      Born    : in Calendar.Time := Calendar.Clock);

   ------------------------------------------------------------------------
   -- Remove                                                             --
   ------------------------------------------------------------------------
   -- Removes any elements for the give Key/Location.
   procedure Remove (Key : in String; Loc : in String);

   ------------------------------------------------------------------------
   -- Get an array of objects (the N youngest)                           --
   ------------------------------------------------------------------------
   function Get (
      Key    : in String;
      Amount : in Positive := Default_cached_elements) return Element_array;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a Key/Location are already present:
   function Contains (
      Key : in String; Loc : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Contains_better                                                    --
   ------------------------------------------------------------------------
   -- Says if a Element is already present and is better:
   function Contains_better (E : in Element_type) return Boolean;

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   -- Says the number of elements stored under a given key:
   function Count (Key : in String) return Natural;

   ------------------------------------------------------------------------
   -- Purge                                                              --
   ------------------------------------------------------------------------
   -- Deletes elements too old
   procedure Purge;

   private

      -- All cached elements
      Elements     : Element_list.Container_type;

      -- Max sources per key
      Max_Sources  : Positive := Default_cached_elements;

      -- Time to live of items
      Time_to_live : Duration := 24.0 * 60.0 * 60.0;

   end Object;

private
   
   package Element_set is new Charles.Sets.Sorted.Unbounded (
      Element_type, Better, "=");

end Adagio.Upload.Mesh;
