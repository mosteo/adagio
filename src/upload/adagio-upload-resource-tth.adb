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
--  $Id: adagio-upload-resource-tth.adb,v 1.4 2004/01/29 21:47:10 Jano Exp $

--  Uploadable resources.

with Adagio.Library;
with Sha1;

with Ada.Unchecked_deallocation;

package body Adagio.Upload.Resource.TTH is

   procedure Free is new Unchecked_deallocation (
      MSU.Stream_type, MSU.Stream_access);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation
   function Create (Target : in Adagio.File.Object) return Object_access is
      O : Object_access := new Object;
   begin
      O.File   := Target;
      Library.Object.Export_TTH (Target, O.Data);
      O.Source := new MSU.Stream_type (O.Data);
      return O;
   end Create;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Unique id for the resource, will identify it in queues.
   function Id (This : in Object) return String is
   begin
      return "urn:sha1:" & 
         Sha1.To_base32 (Adagio.File.Sha (This.File));
   end Id;

   ------------------------------------------------------------------------
   -- Set_position                                                       --
   ------------------------------------------------------------------------
   -- Set the starting position for its data. Call it before Stream.
   procedure Set_position (This : in out Object; Position : in Natural) is
   begin
      MSU.Set_index (
         This.Source.all, 
         Streams.Stream_element_offset (Position));
   end Set_position;

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Get an stream for the resource binary data
   procedure Stream (This : in out Object; Result : out Stream_access) is
   begin
      Result := Stream_access (This.Source);
   end Stream;

   ------------------------------------------------------------------------
   -- Content_type                                                       --
   ------------------------------------------------------------------------
   -- Get the corresponding Content-Type: http header for the resource.
   function Content_type (This : in Object) return String is
      pragma Unreferenced (This);
   begin
      return "application/tigertree-breadthfirst";
   end Content_type;
      
   ------------------------------------------------------------------------
   -- Name                                                               --
   ------------------------------------------------------------------------
   -- Descriptive name of the resource.
   function Name (This : in Object) return String is
   begin
      return "TTH of " & Adagio.File.Name (This.File);
   end Name;

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Size in bytes of the resource.
   function Size (This : in Object) return Natural is
   begin
      return Natural (This.Data'Length);
   end Size;

   ------------------------------------------------------------------------
   -- Qualify                                                            --
   ------------------------------------------------------------------------
   -- Say if a resource qualifies according a criteria string:
   function Qualify (This : in Object; Criteria : in String) return Boolean is
      pragma Unreferenced (This);
      pragma Unreferenced (Criteria);
   begin
      return true;
   end Qualify;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin 
      Free (This.Source);
      Adagio.Streams.Free (This.Data);
   end Finalize;

end Adagio.Upload.Resource.TTH;
