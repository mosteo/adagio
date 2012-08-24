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
-- many classes, of course.

-- Intended for direct use from Download.Manager body. 

with Adagio.Download.Slot.Maps;
with Adagio.Trace;

with Agpl.Bmp;
with Agpl.Sequence;
with Agpl.Strings;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
use  Ada;

package body Adagio.Download.Slot is

   type Slot_Id_Num is mod Natural'Last;
   package Id_Sequences is new Agpl.Sequence (Slot_Id_Num);
   Id_Sequence : Id_Sequences.Object;

   No_Size_Bmp : Agpl.Bmp.Object;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation (Object, Object_Access);

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Create the download slot and add sources as necessary.
   function Create (From : in Agpl.Magnet.Object) return Object_Access is
      This     : Object_Access := new Object;
      Proto_Id : Slot_Id_Num;
   begin
      Id_Sequence.Get_Next (Proto_Id);

      This.Id   := To_Slot_Id (Agpl.Strings.Trim (Slot_Id_Num'Image (Proto_Id)));
      This.Name := U (Agpl.Magnet.Get_Name (From));
      if Agpl.Magnet.Get_Attribute (From, Agpl.Magnet.User_Defined_Prefix & "sz") = "" then
         This.Size_Unknown := true;
      else
         Set_Size (This, Types.File_Size'Value (
            Agpl.Magnet.Get_Attribute (From, Agpl.Magnet.User_Defined_Prefix & "sz")));
         Seg_Avail.Create (This.Availability, 1, This.Size, 0);
         Seg_Valid.Create (This.Validity, 1, This.Size, Missing);
      end if;

      return This;
   exception
      when E : others =>
         Trace.Log ("Donwload.Slot.Create: " & Trace.Report (E), Trace.Error);
         Free (This);
         return null;
   end Create;
   
   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   function Get_Id (This : access Object) return Slot_Id is
   begin
      return This.Id;
   end Get_Id;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : access Object) return String is
   begin
      return S (This.Name);
   end Get_Name;

   ------------------------------------------------------------------------
   -- Get_Progress_Bmp                                                   --
   ------------------------------------------------------------------------
   function Get_Progress_Bmp (This : access Object) return Agpl.Bmp.Object is
   begin
      if This.Size_Unknown then
         return No_Size_Bmp;
      else
         return Segments.Get_Progress_Bmp (This.Validity, This.Availability);
      end if;
   end Get_Progress_Bmp;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (This : access Object) return Types.File_Size is
   begin
      return This.Size;
   end Get_Size;

   ------------------------------------------------------------------------
   -- Is_Size_Unknown                                                    --
   ------------------------------------------------------------------------
   function Is_Size_Unknown (This : access Object) return Boolean is
   begin
      return This.Size_Unknown;
   end Is_Size_Unknown;

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   procedure Set_Size (This : access Object; Size : in Types.File_Size) is
   begin
      This.Size         := Size;
      This.Size_Unknown := false;
   end Set_Size;

   ------------------------------------------------------------------------
   -- Hash                                                               --
   ------------------------------------------------------------------------
   function Hash (Id : in Slot_Id) return Ada.Containers.Hash_Type is
   begin 
      return Ada.Strings.Hash (S (Ustring (Id)));
   end Hash;

   ------------------------------------------------------------------------
   -- From_Xml                                                           --
   ------------------------------------------------------------------------
   -- Restores from a given node.
   function From_Xml (Node : in Xml.Node) return Object_Access is
      This : Object_Access := new Object;
   begin
      This.Id := To_Slot_Id (Xml.Get_Attribute (Node, "id", "-1"));
      This.Name            := U (Xml.Get_Attribute (Node, "name", ""));
      This.Size_Unknown    := Boolean'Value (Xml.Get_Attribute (Node, "size_unknown", "false"));
      if not This.Size_Unknown then
         This.Size := Types.File_Size'Value (Xml.Get_Attribute (Node, "size", "-1"));
      end if;
      This.Size_Completed  := Types.File_Size'Value (Xml.Get_Attribute (Node, "size_completed", "-1"));
      This.Status          := Slot_Status'Value (Xml.Get_Attribute (Node, "status", "-1"));
      This.Priority        := Priorities'Value (Xml.Get_Attribute (Node, "priority", "normal"));
      This.Auto_Priority   := Boolean'Value (Xml.Get_Attribute (Node, "auto_priority", "true"));

      Seg_Avail.Create (This.Availability, 1, This.Size, 0);
      Seg_Valid.Create (This.Validity, 1, This.Size, Missing);

      return This;
   end From_Xml;

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   -- Returns a node representing this object. Caller should free it.
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node is
      Node : Xml.Node := Xml.Add (Doc, "slot");
   begin
      Xml.Set_Attribute (Node, "id", To_String (This.Id));
      Xml.Set_Attribute (Node, "name", S (This.Name));
      Xml.Set_Attribute (Node, "size_unknown", This.Size_Unknown'Img);
      if not This.Size_Unknown then
         Xml.Set_Attribute (Node, "size", Agpl.Strings.To_String (Natural (This.Size)));
      end if;
      Xml.Set_Attribute (Node, "size_completed", Agpl.Strings.To_String (Natural (This.Size_Completed)));
      Xml.Set_Attribute (Node, "status", This.Status'Img);
      Xml.Set_Attribute (Node, "priority", This.Priority'Img);
      Xml.Set_Attribute (Node, "auto_priority", This.Auto_Priority'Img);
      return Node;
   end To_Xml;

   ------------------------------------------------------------------------
   -- Set_Max_Id                                                         --
   ------------------------------------------------------------------------
   -- Informs the Id generator about a Id value so the top can be raised if necessary
   procedure Set_Max_Id (Id : in Slot_Id) is
      Val : Slot_Id_Num := Slot_Id_Num'Value (To_String (Id));
   begin
      if Val >= Id_Sequence.Peek_Next then
         Id_Sequence.Set_Next (Val + 1);
      end if;
   end Set_Max_Id;

begin
   Agpl.Bmp.Create (No_Size_Bmp, 1, 1);
end Adagio.Download.Slot;
