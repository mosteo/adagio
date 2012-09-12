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
--  $Id: adagio-upload-resource-file.adb,v 1.3 2004/01/21 21:05:50 Jano Exp $

--  Uploadable resources.

With
Sha1,
Adagio.Types,
Adagio.File.Criteria,
Adagio.File.Safe,
Adagio.Globals.Options,
Ada.Streams.Stream_io,
Ada.Unchecked_deallocation;

Use
Adagio.Types;

package body Adagio.Upload.Resource.File is

   Upload_buffer_size : constant File_size :=
      File_size (Globals.Options.Uploads_SendBufferSize);

   procedure Free is new Unchecked_deallocation (
      Buffered_stream.Buffered_stream, Buffered_stream.Object_access);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation
   function Create (Target : in Adagio.File.Object) return Object_access is
      O : Object_access := new Object;
   begin
      O.File := Target;
      Decoupled_file_stream.Get_decoupled_file_stream (
         O.DS, Adagio.File.Path (Target));

      return O;
   end Create;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id for the resource, will identify it in queues.
   function Id (This : in Object) return String is
   begin
      return "urn:sha1:" & Sha1.To_base32 (Adagio.File.Sha (File (This)));
   end Id;

   ------------------------------------------------------------------------
   -- Set_position                                                       --
   ------------------------------------------------------------------------
   -- Set the starting position for its data. Call it before Stream.
   procedure Set_position (This : in out Object; Position : in Natural) is
      use Ada.Streams.Stream_io;
   begin
      Decoupled_file_stream.Set_position (This.DS, Count (Position));
   end Set_position;

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Get an stream for the resource binary data
   procedure Stream (This : in out Object; Result : out Stream_access) is
      use type Buffered_stream.Object_access;
   begin
      if This.BS = null then
         This.BS := new Buffered_stream.Buffered_stream (
            Streams.Stream_element_offset (Upload_buffer_size));
         Buffered_stream.Get_buffered_stream (
            This.BS.all, This.DS'Unchecked_access);
      end if;
      Result := Stream_access (This.BS);
   end Stream;

   ------------------------------------------------------------------------
   -- Content_type                                                       --
   ------------------------------------------------------------------------
   -- Get the corresponding Content-Type: http header for the resource.
   function Content_type (This : in Object) return String is
      pragma Unreferenced (This);
   begin
      return "application/x-binary";
   end Content_type;

   ------------------------------------------------------------------------
   -- Name                                                               --
   ------------------------------------------------------------------------
   -- Descriptive name of the resource.
   function Name (This : in Object) return String is
   begin
      return Adagio.File.Name (This.File);
   end Name;

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Size in bytes of the resource.
   function Size (This : in Object) return Natural is
   begin
      return Natural (Decoupled_file_stream.Size (This.DS));
   end Size;

   ------------------------------------------------------------------------
   -- Qualify                                                            --
   ------------------------------------------------------------------------
   -- Say if a resource qualifies according a criteria string:
   function Qualify (This : in Object; Criteria : in String) return Boolean is
   begin
      return Adagio.File.Criteria.Qualify (This.File, Criteria);
   end Qualify;

   ------------------------------------------------------------------------
   -- Add_upload                                                         --
   ------------------------------------------------------------------------
   -- Marks this file as having another upload.
   procedure Add_upload (This : in Object) is
   begin
      Adagio.File.Safe.Add_upload (This.File);
   end Add_upload;

   ------------------------------------------------------------------------
   -- File                                                               --
   ------------------------------------------------------------------------
   -- Get the boxed file.
   function File (This : in Object) return Adagio.File.object is
   begin
      return This.File;
   end File;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      Free (This.BS);
   end Finalize;

end Adagio.Upload.Resource.File;
