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

with Agpl.Strings;

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with Aws.Translator;

package body Adagio.Searches.Hit is

   ------------------------------------------------------------------------
   -- Add_Hash                                                           --
   ------------------------------------------------------------------------
   -- Add a canonical hash text representation. The key should be the hash type.
   -- sha1     <base32>
   -- ed2k     hex
   -- bp       <base32>.<base32>
   procedure Add_Hash (
      This      : in out Object; 
      Hash_Type : in     String;
      Value     : in     String)
   is
      use Hash_Dictionary;
   begin
      Add_Word (This.Hashes, Hash_Type, U (Value));
   end Add_Hash;

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : Object_Access) return Boolean is
      use Ada.Tags;
   begin
      return L.all'Tag = R.all'Tag and then Get_Id (L.all) = Get_Id (R.all);
   end Equal;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   -- Deallocation
   procedure Free (This : access Object'Class) is
      Aux : Object_Access := Object_Access (This);
      procedure Delete is new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      Delete (Aux);
   end Free;

   ------------------------------------------------------------------------
   -- Get_Extra                                                          --
   ------------------------------------------------------------------------
   -- Get extra data for the http report
   -- Default imp. returns ""
   function Get_Extra (This : in Object) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Get_Extra;

   ------------------------------------------------------------------------
   -- Get_Hashes                                                         --
   ------------------------------------------------------------------------
   function Get_Hashes (This : in Object) return Hash_Dictionary.Object is
   begin
      return This.Hashes;
   end Get_Hashes;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : in Object) return String is
   begin
      return S (This.Name);
   end Get_Name;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (This : in Object) return File_Size is
   begin
      return This.Size;
   end Get_Size;

   ------------------------------------------------------------------------
   -- Is Firewalled                                                      --
   ------------------------------------------------------------------------
   -- Default implementation returns false
   function Is_Firewalled (This : in Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return false;
   end Is_Firewalled;

   ------------------------------------------------------------------------
   -- Is_New                                                             --
   ------------------------------------------------------------------------
   function Is_New (This : in Object) return Boolean is
   begin
      return This.Is_New;
   end Is_New;

   ------------------------------------------------------------------------
   -- Is_Secure                                                          --
   ------------------------------------------------------------------------
   -- Says if we have secure sources for it (freenet)
   function Is_Secure (This : in Object) return Boolean is
      use Hash_Dictionary;
   begin
      return Contains_Key (This.Hashes, Secure_Key);
   end Is_Secure;

   ------------------------------------------------------------------------
   -- Set_Name                                                           --
   ------------------------------------------------------------------------
   procedure Set_Name (This : in out Object; Name : in String) is
   begin
      This.Name := U (Name);
   end Set_Name;

   ------------------------------------------------------------------------
   -- Set_New                                                            --
   ------------------------------------------------------------------------
   procedure Set_New (This : in out Object; Is_New : Boolean := true) is
   begin
      This.Is_New := Is_New;
   end Set_New;

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   procedure Set_Size (This : in out Object; Size : in File_Size) is
   begin
      This.Size := Size;
   end Set_Size;

   ------------------------------------------------------------------------
   -- Merge_From_Xml                                                     --
   ------------------------------------------------------------------------
   -- Hash values are stored under its hash name
   -- All hash names are indexed in "hash1", "hash2", ... attributes
   procedure Merge_From_Xml (This : in out Object; Node : in Xml.Node) is
      use Aws;
   begin
      This.Name   := U (Translator.To_String (Translator.Base64_Decode (
         Xml.Get_Attribute (Node, "name", ""))));
      This.Size   := File_Size'Value (Xml.Get_Attribute (Node, "size", "-1"));
      This.Is_New := Boolean'Value (Xml.Get_Attribute (Node, "IsNew", "false"));
      for I in 1 .. Positive'Last loop
         declare
            Key : constant String := Xml.Get_Attribute (Node, "hash" & Agpl.Strings.To_String (I), "end");
         begin
            exit when Key = "end";
            Hash_Dictionary.Add_Word (This.Hashes, Key, U (Xml.Get_Attribute (Node, Key, "")));
         end;
      end loop;
   end Merge_From_Xml;

   ------------------------------------------------------------------------
   -- Merge_To_Xml                                                       --
   ------------------------------------------------------------------------
   -- Hash values are stored under its hash name
   -- All hash names are indexed in "hash1", "hash2", ... attributes
   procedure Merge_To_Xml (This : in Object; Node : in out Xml.Node) is
      use Aws;
   begin
      Xml.Set_Attribute (Node, "name", Translator.Base64_Encode (S (This.Name)));
      Xml.Set_Attribute (Node, "size", Agpl.Strings.To_String (Natural (This.Size)));
      Xml.Set_Attribute (Node, "IsNew", This.Is_New'Img);
      Xml.Set_Attribute (Node, "tag", 
         Ada.Tags.External_Tag (Object'Class (This)'Tag));

      declare
         H : Hash_Dictionary.Pair_Array := Hash_Dictionary.Get_Contents (This.Hashes);
      begin
         for I in H'Range loop
            Xml.Set_Attribute (Node, S (H (I).Key), S (H (I).Value));
            Xml.Set_Attribute (Node, "hash" & Agpl.Strings.To_String (I), S (H (I).Key));
         end loop;
      end;
   end Merge_To_Xml;

end Adagio.Searches.Hit;
