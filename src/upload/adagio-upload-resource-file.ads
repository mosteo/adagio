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
--  $Id: adagio-upload-resource-file.ads,v 1.3 2004/01/21 21:05:51 Jano Exp $

--  Uploadable resources.

With
Adagio.File,
Adagio.Buffered_stream,
Adagio.Decoupled_file_stream;

package Adagio.Upload.Resource.File is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Upload.Resource.Object with private;
   type Object_access is access all Object;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation
   function Create (Target : in Adagio.File.Object) return Object_access;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id for the resource, will identify it in queues.
   function Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Set_position                                                       --
   ------------------------------------------------------------------------
   -- Set the starting position for its data. Call it before Stream.
   -- First byte is at 1.
   procedure Set_position (This : in out Object; Position : in Natural);

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Get an stream for the resource binary data
   procedure Stream (This : in out Object; Result : out Stream_access);

   ------------------------------------------------------------------------
   -- Content_type                                                       --
   ------------------------------------------------------------------------
   -- Get the corresponding Content-Type: http header for the resource.
   function Content_type (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Name                                                               --
   ------------------------------------------------------------------------
   -- Descriptive name of the resource.
   function Name (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Size in bytes of the resource.
   function Size (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Qualify                                                            --
   ------------------------------------------------------------------------
   -- Say if a resource qualifies according a criteria string:
   function Qualify (This : in Object; Criteria : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Add_upload                                                         --
   ------------------------------------------------------------------------
   -- Marks this file as having another upload.
   procedure Add_upload (This : in Object);

   ------------------------------------------------------------------------
   -- File                                                               --
   ------------------------------------------------------------------------
   -- Get the boxed file.
   function File (This : in Object) return Adagio.File.object;

private

   -- We will not allocate the buffered stream until requested to save memory.
   type Object is new Upload.Resource.Object with record
      File : Adagio.File.Object;
      BS   : Buffered_stream.Object_access;
      DS   : aliased Decoupled_file_stream.Decoupled_file_stream;
   end record;

   procedure Finalize (This : in out Object);

end Adagio.Upload.Resource.File;
