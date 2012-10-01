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
--  $Id: adagio-g2-core.ads,v 1.19 2004/03/29 19:13:30 Jano Exp $

With
Adagio.Download.Source,
Adagio.G2.Packet.Queue,
Adagio.Globals.Options,
Adagio.Searches.Hit,
Adagio.Xml,
Adagio.Types;

Use
Adagio.Types;

package Adagio.G2.Hit is

   -- G2 hits store the hashes in its binary form. Translation will be performed if
   -- needed at the download request time.

   Max_Hits : Positive renames Globals.Options.G2_Search_MaxHitsPerPacket;

   Malformed_Hit : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is new Searches.Hit.Object with private;

   type Object_Array is array (Positive range <>) of Object;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Create all hits from a QH2 packet.
   function Create (Item : in Packet.Queue.Item_Type) return Object_Array;

   ------------------------------------------------------------------------
   -- Get_Address                                                        --
   ------------------------------------------------------------------------
   function Get_Address (This : in object) return String;
   pragma Inline (Get_Address);

   ------------------------------------------------------------------------
   -- Get_Extra                                                          --
   ------------------------------------------------------------------------
   function Get_Extra (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Should return a unique id, identifying the source (IP based or something).
   function Get_Id (This : in Object) return String;

   ------------------------------------------------------------------------
   -- Get_Proxies                                                        --
   ------------------------------------------------------------------------
   -- Returns the addresses of the hubs this source is connected to.
   function Get_Proxies (This : in Object) return Ustring_Array;

   ------------------------------------------------------------------------
   -- Get_Source                                                         --
   ------------------------------------------------------------------------
   -- Constructs a Source'Class object for use in downloading corresponding
   -- to this hit.
   -- Can return Null if the source can't be built by whatever reason.
   function Get_Source (This : in Object) return Download.Source.Object_Access;

   ------------------------------------------------------------------------
   -- Get_Urn                                                            --
   ------------------------------------------------------------------------
   -- Returns the string that identifies the resouce at the server
   -- I.e. what has to be written after the http GET
   function Get_Urn (This : in Object) return String;
   pragma Inline (Get_Urn);

   ------------------------------------------------------------------------
   -- Is Firewalled                                                      --
   ------------------------------------------------------------------------
   function Is_Firewalled (This : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   -- Merge two hits to get extra features from a set of hits.
   -- Merges second on first
   procedure Merge (L : in out Object; R : in Object);

   ------------------------------------------------------------------------
   -- Create_From_Xml                                                    --
   ------------------------------------------------------------------------
   function Create_From_Xml (Node : in Xml.Node) return Object;

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   -- Caller should deallocate the resulting node
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node;

private

   type Object is new Searches.Hit.Object with record
      Firewalled  : Boolean := false;
      Sender_Guid : Guid_String;
      Sender_Addr : Ustring;
      Firewalls   : Agpl.Types.Ustrings.Ustring_Vector.Object (First => 1);
         -- Addresses of the nodes that can be used to send a push to the hit.
      Nick        : Ustring;
      Vendor      : Ustring;
      Preview     : Ustring;  -- Url
      Url         : Ustring;  -- Url for requesting it, can be empty if urn-res to be used
      Alt_Sources : Natural := 0;
      Sha1        : Ustring; -- base32 textual form without "sha1:"

      Rated       : Boolean := false;
      Rating      : Natural range 0 .. 5; -- Meaningless if not Rated
      Comment     : Ustring;
      Browsable   : Boolean := false;
      Chatable    : Boolean := false;

      Busy        : Boolean := false;
      Bandwidth   : Speed   := 0;
   end record;

   pragma Inline (Is_Firewalled);

end Adagio.G2.Hit;
