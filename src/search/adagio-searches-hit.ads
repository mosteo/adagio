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
--  $Id: adagio-upload.ads,v 1.4 2004/01/21 21:05:51 Jano Exp $

--  Root package for all search packages

With
Adagio.Download.Source,
Adagio.Hash_Dictionary,
Adagio.Types,
Adagio.Xml,
Agpl.Types.Ustrings;

Use
Adagio.Types;

package Adagio.Searches.Hit is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is abstract tagged private;
   type Object_Access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Add_Hash                                                           --
   ------------------------------------------------------------------------
   -- Add a canonical hash text representation. The key should be the hash type.
   -- sha1     <base32>
   -- ed2k     hex
   -- bp       <base32>.<base32>
   -- tth      <raw binary data in standard order (breadth first)>
   procedure Add_Hash (
      This      : in out Object;
      Hash_Type : in     String;
      Value     : in     String);

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : in Object_Access) return Boolean;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   -- Deallocation
   procedure Free (This : access Object'Class);

   ------------------------------------------------------------------------
   -- Get_Extra                                                          --
   ------------------------------------------------------------------------
   -- Get extra data for the http report
   -- Default imp. returns ""
   function Get_Extra (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Hashes                                                         --
   ------------------------------------------------------------------------
   function Get_Hashes (This : in Object) return Hash_Dictionary.Object;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Should return a unique id, identifying the source (IP based or something).
   function Get_Id (This : in Object) return String is abstract;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (This : in Object) return File_Size;

   ------------------------------------------------------------------------
   -- Get_Source                                                         --
   ------------------------------------------------------------------------
   -- Constructs a Source'Class object for use in downloading corresponding
   -- to this hit.
   -- Can return Null if the source can't be built by whatever reason.
   function Get_Source (This : in Object) return Download.Source.Object_Access
   is abstract;

   ------------------------------------------------------------------------
   -- Is Firewalled                                                      --
   ------------------------------------------------------------------------
   -- Default implementation returns false
   function Is_Firewalled (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Is_New                                                             --
   ------------------------------------------------------------------------
   function Is_New (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Is_Secure                                                          --
   ------------------------------------------------------------------------
   -- Says if we have secure sources for it (freenet)
   function Is_Secure (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Set_Name                                                           --
   ------------------------------------------------------------------------
   procedure Set_Name (This : in out Object; Name : in String);

   ------------------------------------------------------------------------
   -- Set_New                                                            --
   ------------------------------------------------------------------------
   procedure Set_New (This : in out Object; Is_New : Boolean := true);

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   procedure Set_Size (This : in out Object; Size : in File_Size);

   ------------------------------------------------------------------------
   -- Create_From_Xml                                                    --
   ------------------------------------------------------------------------
   function Create_From_Xml (Node : in Xml.Node) return Object is abstract;

   ------------------------------------------------------------------------
   -- Merge_From_Xml                                                     --
   ------------------------------------------------------------------------
   -- Not to be overrided:
   procedure Merge_From_Xml (This : in out Object; Node : in Xml.Node);

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   -- Caller should deallocate the resulting node
   function To_Xml (This : in Object; Node : in Xml.Document) return Xml.Node is abstract;

   ------------------------------------------------------------------------
   -- Merge_To_Xml                                                       --
   ------------------------------------------------------------------------
   -- Not to be overrided:
   procedure Merge_To_Xml (This : in Object; Node : in out Xml.Node);

private

   type Object is abstract tagged record
      Name   : Ustring;
      Hashes : Hash_Dictionary.Object;
      Size   : File_Size;
      Is_New : Boolean := true;
   end record;

   pragma Inline (Get_Name, Get_Extra, Get_Size);

end Adagio.Searches.Hit;
