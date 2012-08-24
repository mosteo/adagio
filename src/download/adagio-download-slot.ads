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

-- Download slots refer to which is normally known as "donwloads". A slot in
-- Adagio is a regular download, with a lot of sources and stuff.
-- This package is private because is only accessed by means of the download
-- manager.

-- The slots aren't generic nor abstract. They have definite functions and
-- behavior. All genericity is encapsulated in the sources, which can be of
-- many classes, of course, and in the data consumers.

-- Intended for direct use from Download.Manager body. 

with Adagio.Download.Segments;
with Adagio.Types;
with Adagio.Xml;

with Agpl.Bmp;
with Agpl.Magnet;
with Agpl.Segmented_Thing;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Hash;

package Adagio.Download.Slot is

   type Object is limited private;
   type Object_Access is access Object;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   function Create (From : in Agpl.Magnet.Object) return Object_Access;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   function Get_Id (This : access Object) return Slot_Id;
   pragma Inline (Get_Id);
   
   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : access Object) return String;
   pragma Inline (Get_Name);

   ------------------------------------------------------------------------
   -- Get_Progress_Bmp                                                   --
   ------------------------------------------------------------------------
   function Get_Progress_Bmp (This : access Object) return Agpl.Bmp.Object;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (This : access Object) return Types.File_Size;
   pragma Inline (Get_Size);

   ------------------------------------------------------------------------
   -- Is_Size_Unknown                                                    --
   ------------------------------------------------------------------------
   function Is_Size_Unknown (This : access Object) return Boolean;
   pragma Inline (Is_Size_Unknown);

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   procedure Set_Size (This : access Object; Size : in Types.File_Size);

   ------------------------------------------------------------------------
   -- From_Xml                                                           --
   ------------------------------------------------------------------------
   -- Restores from a given node.
   function From_Xml (Node : in Xml.Node) return Object_Access;

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   -- Returns a node representing this object. Caller should free it.
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node;

   ------------------------------------------------------------------------
   -- Set_Max_Id                                                         --
   ------------------------------------------------------------------------
   -- Informs the Id generator about a Id value so the top can be raised if necessary
   procedure Set_Max_Id (Id : in Slot_Id);

   ------------------------------------------------------------------------
   -- Hash                                                               --
   ------------------------------------------------------------------------
   -- Internal use
   -- Containers related, nothig to with file hashes
   function Hash (Id : in Slot_Id) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);

private

   package Seg_Valid renames Segments.Segmented_Validities;
   package Seg_Avail renames Segments.Segmented_Availabilities;

   type Object is record
      Id            : Slot_Id;
      Name          : Ustring;

      Size_Unknown  : Boolean         := false;
      Size          : Types.File_Size;
      Size_Completed: Types.File_Size := 0;

      Status        : Slot_Status := Enabled;

      Priority      : Priorities := Medium;
      Auto_Priority : Boolean    := true;

      Availability  : Seg_Avail.Object;
      Validity      : Seg_Valid.Object;
   end record;

end Adagio.Download.Slot;
