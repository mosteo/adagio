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

with Adagio.Download;
with Adagio.Hash_Dictionary;
with Adagio.Globals.Options;
with Adagio.Searches.Hit;
with Adagio.Searches.Hit_Family;
with Adagio.Xml;
with Sha1;

with Agpl.Http.Server.Sort_Handler;
use  Agpl.Http.Server.Sort_Handler;

with Charles.Hash_String;
with Charles.Multimaps.Hashed.Strings.Unbounded;

with Ada.Calendar;
with Ada.Finalization;
use  Ada;

package Adagio.Searches.Search is

   -- Each search has a collection of indexes to hit families, for each hash type in the
   -- families. This way, a hit can be immediately allocated to its family in O(1) time.
   -- For the moment no family merging is implemented.

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object (<>) is limited private;
   type Object_Access is access all Object;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   procedure Add_Hit (This : access Object; New_Hit : in Hit.Object'Class);

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Check its families to find compatible hits
   procedure Add_Sources_To_Download (
      This : access Object;
      Hash : in     Hash_Dictionary.Object;
      Id   : in     Download.Slot_Id);

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a hit already is in the search
   function Contains (This : access Object; New_Hit : in Hit.Object'Class) return Boolean;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Allocates a new object
   function Create (
      Target   : in     String;
      Priority : in     Priorities) return Object_Access;

   ------------------------------------------------------------------------
   -- Create_From_XML                                                    --
   ------------------------------------------------------------------------
   -- Gets a Search Node (formed like the one generated for saving) and creates an Object
   function Create_From_XML (Srch : in Xml.Node) return Object_Access;

   ------------------------------------------------------------------------
   -- Destroy                                                            --
   ------------------------------------------------------------------------
   -- This will be null after destruction
   procedure Destroy (This : in out Object_access);

   ------------------------------------------------------------------------
   -- Get_Payload                                                        --
   ------------------------------------------------------------------------
   -- Returns the searched thing
   function Get_Payload (This : in Object_Access) return Payload;

   ------------------------------------------------------------------------
   -- Pause                                                              --
   ------------------------------------------------------------------------
   procedure Pause (This : access Object);

   ------------------------------------------------------------------------
   -- Resume                                                             --
   ------------------------------------------------------------------------
   procedure Resume (This : access Object);

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (
      This : access Object; Family : in String; Expanded : in Boolean := true);

   ------------------------------------------------------------------------
   -- To_XML                                                             --
   ------------------------------------------------------------------------
   -- Returns a freshly allocated and created XML node <search/>
   -- Caller should deallocate.
   function To_Xml (
      This      : access Object; 
      Doc       :        in Xml.Document; 
      With_Hits :        in Boolean) return Xml.Node;
   
   ------------------------------------------------------------------------
   -- "="                                                                --
   ------------------------------------------------------------------------
   function "=" (L, R : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Get_xxxxxx                                                         --
   ------------------------------------------------------------------------
   function Get_Id (This : access Object) return Search_Id;
   function Get_Family (This : access Object; Family : in Hit_Family.Family_Id) return Hit_Family.Object_Access;
   function Get_Firewalled_Hits (This : access Object) return Natural;
   function Get_Hits (This : access Object) return Natural;
   function Get_Kind (This : access Object) return Kinds;
   function Get_New_Hits (This : access Object) return Natural;
   function Get_Secure_Hits (This : access Object) return Natural;
   -- Priority Delta that should now apply:
   function Get_Priority_Delta (This : access Object) return Natural;
   -- Priority at creation time:
   function Get_Priority (This : access Object) return Priorities;
   -- Priority applied (distinct from the creation priority only if auto):
   function Get_Effective_Priority (This : access Object) return Priorities;
   -- Descriptive target description:
   function Get_Paused (This : access Object) return Boolean;
   function Get_Target (This : access Object) return String;

   ------------------------------------------------------------------------
   -- Set_xxxxxx                                                         --
   ------------------------------------------------------------------------
   procedure Set_Priority (This : access Object; Priority : in Priorities);

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (This : access Object; Data : in out Data_Set);

private

   use type Calendar.time;

   Priority_values : array (Priorities) of Natural := (
      Auto              => 0,
      Idle              => Integer'Last,
      Low               => Globals.Options.G2_Search_Priorities_Low,
      Medium            => Globals.Options.G2_Search_Priorities_Medium, 
      High              => Globals.Options.G2_Search_Priorities_High, 
      Exclusive5m       => 0, 
      Exclusive15m      => 0,
      Exclusive30m      => 0,
      Exclusive60m      => 0, 
      Exclusive_Forever => 0);

   -- The index will be urn:hash
   -- For example sha1:blabla, ed2k:blabla, etc
   -- The only possibility of multple elements for a hash is if two families
   -- have incompatible hashsets, which will mean some client hashing incorrectly.
   -- We'll consider that possibility however and not risk losing hits.
   package Hit_Family_Map is new Charles.Multimaps.Hashed.Strings.Unbounded (
      Hit_Family.Object_Access, 
      Charles.Hash_String, 
      "=", 
      Searches.Hit_Family."=");

   type Object (Kind : Kinds) is new Finalization.Limited_Controlled with record
      Id       : Search_Id;
      Priority : Priorities    := Auto;
      Paused   : Boolean       := false;
      Started  : Calendar.Time := Calendar.Clock;
      Changed  : Calendar.Time := Calendar.Clock; -- For exclusives timeouts
      Hits     : Hit_Family_Map.Container_Type; -- By hash, may contain duplicate accesses
      Ids      : Hit_Family_Map.Container_Type; -- By id, no duplicates
      case Kind is
         when Keywords =>
            Words : Ustring; -- Keywords to search for
         when SHA1_digest =>
            Digest      : Sha1.Digest; -- Digest
            Digest_text : Ustring;     -- Beautified digest
            Name        : Ustring;     -- Name, if available
      end case;
   end record;

   ------------------------------------------------------------------------
   -- Add_Hashes                                                         --
   ------------------------------------------------------------------------
   -- Add missing indexes to a family
   procedure Add_Hash_Indexes (
      This : access Object; F : in Hit_Family.Object_Access; H : in Hit.Object'Class);

   ------------------------------------------------------------------------
   -- Construct_Pair                                                     --
   ------------------------------------------------------------------------
   -- Gets a "key" and "value" and returns "key:value"
   function Construct_Pair (K, V : in Ustring) return String;
   pragma Inline (Construct_Pair);

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object);

   ------------------------------------------------------------------------
   -- Get_Families                                                       --
   ------------------------------------------------------------------------
   -- Get families compatible with a hit
   -- No duplicate families are reported
   function Get_Families (This : access Object; H : in Hit.Object'Class) 
      return Hit_Family.Object_Access_Array;
   -- Or families compatible with a hash_dictionary
   function Get_Families (This : access Object; H : in Hash_Dictionary.Object)
      return Hit_Family.Object_Access_Array;

   -----------------------------
   -- Determine_Auto_Priority --
   -----------------------------
   -- Computes priority from hits thresholds
   function Determine_Auto_Priority (Hits : in Natural) return Priorities;

end Adagio.Searches.Search;
