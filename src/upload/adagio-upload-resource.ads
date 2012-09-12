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
--  $Id: adagio-upload-resource.ads,v 1.3 2004/01/21 21:05:51 Jano Exp $

--  Uploadable resources.

With
Adagio.Safe_access,
Ada.Finalization,
Ada.Streams;

Use
Ada,
Ada.Streams;

package Adagio.Upload.Resource is

   type Stream_access is access all Root_stream_type'Class;

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   -- Some exceptions for resources:
   Malformed_request : exception; -- Recognized but malformed.
   Unavailable       : exception; -- Recognized but not found.
   Unknown           : exception; -- Request with unknown method.

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is abstract new Finalization.Limited_controlled with private;
   type Object_access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Handle                                                             --
   ------------------------------------------------------------------------
   -- Use this private type to hold Object'Class without need to control it.
   type Handle is private;
   function Create (This : access Object) return Handle;
   function V      (This : in Handle) return Object_access;
   function "+"    (This : in Handle) return Object_access renames V;

   Null_handle : constant Handle;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id for the resource, will identify it in queues.
   function Id (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Set_position                                                       --
   ------------------------------------------------------------------------
   -- Set the starting position for its data. Call it before Stream.
   -- First byte is at 1.
   procedure Set_position (This : in out Object; Position : in Natural)
      is abstract;

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Get an stream for the resource binary data
   procedure Stream (This : in out Object; Result : out Stream_access)
   is abstract;

   ------------------------------------------------------------------------
   -- Content_type                                                       --
   ------------------------------------------------------------------------
   -- Get the corresponding Content-Type: http header for the resource.
   function Content_type (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Name                                                               --
   ------------------------------------------------------------------------
   -- Descriptive name of the resource.
   function Name (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Size in bytes of the resource.
   function Size (This : in Object) return Natural is abstract;

   ------------------------------------------------------------------------
   -- Qualify                                                            --
   ------------------------------------------------------------------------
   -- Say if a resource qualifies according a criteria string:
   function Qualify (This : in Object; Criteria : in String) return Boolean
   is abstract;

private

   type Object is abstract new
      Finalization.Limited_controlled with null record;

   package Safe_object is new Safe_access (
      Object'Class, Object_access, "Upload.Resource");
   type Handle is record
      Internal_handle : Safe_object.Object;
   end record;

   Null_handle : constant Handle := (
      Internal_handle => Safe_object.Null_access);

end Adagio.Upload.Resource;
