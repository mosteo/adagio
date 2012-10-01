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

--  A family is a collection of hits with the same hashes.

With
Adagio.Download,
Adagio.Download.Manager,
Adagio.Hash_Dictionary,
Adagio.Searches.Hit,
Adagio.Types,
Adagio.Xml,
Agpl.Http.Server.Sort_Handler,
Charles.Hash_String,
Charles.Maps.Hashed.Strings.Unbounded,
Ada.Finalization;

Use
Ada,
Agpl.Http.Server.Sort_Handler;

package Adagio.Searches.Hit_Family is

   No_Such_Hash : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is limited private;
   type Object_Access is access all Object;
   type Object_Access_Array is array (Positive range <>) of Object_Access;

   type Family_Id is private;
   function To_String (Id : in Family_Id) return String;
   function To_Family_Id (Id : in String) return Family_Id;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   -- Adds a hit. It must be compatible
   procedure Add_Hit (This : in out Object; H : in Hit.Object'Class);

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Creates sources for its hits and add these to a download
   procedure Add_Sources_To_Download (This : in Object; Id : in Download.Slot_Id);

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   function Contains (This : in Object; H : in Hit.Object'Class) return Boolean;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- A seed hit is needed
   procedure Create (This : out object; From : in Hit.Object'Class);

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access);

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Get an unique id for the family (meaningless, for indexing)
   function Get_Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Link                                                           --
   ------------------------------------------------------------------------
   -- Will provide a link for the hit with secure sources if possible
   -- Will be a magnet for g2 sources and http for freenet sources
   function Get_Link (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Magnet                                                         --
   ------------------------------------------------------------------------
   -- Will try to get a magnet link
   -- Even for freenet key it will be in magnet form
   -- Raise No_Such_Hash if unable to obtain it
   -- Secure will provide a freenet hash
   -- Sources will cause the addition of known sources
   function Get_Magnet (
      This    : in Object;
      Secure  : in Boolean := false;
      Sources : in Boolean := true) return String;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (This : in Object) return Types.File_Size;

   ------------------------------------------------------------------------
   -- Has_New_Hits                                                       --
   ------------------------------------------------------------------------
   function Has_New_Hits (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Is_Compatible                                                      --
   ------------------------------------------------------------------------
   -- Says if a hit is compatible with this family
   function Is_Compatible (This : in Object; H : in Hit.Object'Class) return Boolean;

   ------------------------------------------------------------------------
   -- Is_Secure                                                          --
   ------------------------------------------------------------------------
   -- Says if we have secure sources for it (freenet)
   function Is_Secure (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Num_Firewalled_Hits                                                --
   ------------------------------------------------------------------------
   function Num_Firewalled_Hits (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Num_Hits                                                           --
   ------------------------------------------------------------------------
   function Num_Hits (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Num_New_Hits                                                       --
   ------------------------------------------------------------------------
   function Num_New_Hits (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Num_Secure_Hits                                                    --
   ------------------------------------------------------------------------
   function Num_Secure_Hits (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (This : in out Object; Expanded : in Boolean := true);

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (
      This : in out Object; Data : in out Data_Set; Srch : in Search_Id);

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node;

private

   type Family_Id is mod 2 ** 32;

   -- Index is the unique id of the hit
   package Hit_Map is new Charles.Maps.Hashed.Strings.Unbounded (
      Searches.Hit.Object_Access, Charles.Hash_String, "=", Searches.Hit.Equal);

   type Object is new Finalization.Limited_Controlled with record
      Id       : Family_Id;  -- Unique ID
      Name     : Ustring;  -- The most used name between all hits belonging to this family.
      Size     : Adagio.Types.File_Size; -- Most seen size (should be only one if no buggy clients!)
      Hashes   : Hash_Dictionary.Object;
      Hits     : Hit_Map.Container_Type;
      Expanded : Boolean := false; -- Just for the GUI
   end record;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object);

end Adagio.Searches.Hit_Family;
