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

With
Adagio.Searches.Hit.Factory,
Adagio.Trace,
Agpl.Magnet,
Agpl.Strings.Fields,
Ada.Unchecked_deallocation;

package body Adagio.Searches.Search is

   use type Hit_Family.Object_Access;

   ------------------------------------------------------------------------
   -- Add_Hashes                                                         --
   ------------------------------------------------------------------------
   -- Add missing indexes to a family who contains a hit
   procedure Add_Hash_Indexes (
      This : access Object; F : in Hit_Family.Object_Access; H : in Hit.Object'Class)
   is
      use Hit_Family_Map;
      Hashes : Hash_Dictionary.Pair_Array :=
         Hash_Dictionary.Get_Contents (Hit.Get_Hashes (H));

      ------------
      -- Exists --
      ------------
      function Exists (Hash : in String) return Boolean is
         First, Back : Iterator_Type;
      begin
         Equal_Range (This.Hits, Hash, First, Back);
         while First /= Back loop
            if Element (First) = F then
               return true;
            end if;
            First := Succ (First);
         end loop;
         return false;
      end Exists;

   begin
      pragma Assert (Hit_Family.Contains (F.all, H));
      for I in Hashes'Range loop
         declare
            Index : constant String := Construct_Pair (Hashes (I).Key, Hashes (I).Value);
         begin
            if not Exists (Index) then
               -- Trace.Log ("INSERTING NEW INDEX: " & Index & " for " & Hit.Get_Name (H),
               --   Trace.Always);
               Insert (This.Hits, Index, F);
            end if;
         end;
      end loop;
   end Add_Hash_Indexes;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   -- Search in current families.
   -- If found add missing hashes if they exist.
   -- If not found create new family.
   procedure Add_Hit (This : access Object; New_Hit : in Hit.Object'Class) is
      use Hit_Family_Map;
      In_Some_Family : Boolean                        := false;
      Fams           : Hit_Family.Object_Access_Array := Get_Families (This, New_Hit);
   begin
      for I in Fams'Range loop
         if Hit_Family.Contains (Fams (I).all, New_Hit) then
            In_Some_Family := true;
            -- Trace.Log ("ALREADY KNOWN HIT: " & Hit.Get_Name (New_Hit), Trace.Always);
            Add_Hash_Indexes (This, Fams (I), New_Hit);
         elsif Hit_Family.Is_Compatible (Fams (I).all, New_Hit) then
            In_Some_Family := true;
            --Trace.Log ("MERGED COMPATIBLE HIT: " & Hit.Get_Name (New_Hit), Trace.Always);
            Hit_Family.Add_Hit (Fams (I).all, New_Hit);
            Add_Hash_Indexes (This, Fams (I), New_Hit);
         end if;
      end loop;
      if not In_Some_Family then
         -- Create a new family for this hit
         declare
            Fam     : Hit_Family.Object_Access := new Hit_Family.Object;
         begin
            -- Trace.Log ("CREATED NEW HIT: " & Hit.Get_Name (New_Hit), Trace.Always);
            Hit_Family.Create (Fam.all, New_Hit);
            -- To ids
            Insert (This.Ids, Hit_Family.Get_Id (Fam.all), Fam);
            -- To indexes
            Add_Hash_Indexes (This, Fam, New_Hit);
         end;
      end if;
   end Add_Hit;

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Check its families to find compatible hits
   procedure Add_Sources_To_Download (
      This : access Object;
      Hash : in     Hash_Dictionary.Object;
      Id   : in     Download.Slot_Id)
   is
      Fams : Hit_Family.Object_Access_Array :=
         Get_Families (This, Hash);
   begin
      Trace.Log ("Searches.Search: Found" & Natural'Image (Fams'Length) &
         " compatible hit families for the " &
         "download " & Download.To_String (Id), Trace.Always);
      for I in Fams'Range loop
         Hit_Family.Add_Sources_To_Download (Fams (I).all, Id);
      end loop;
   end Add_Sources_To_Download;

   ------------------------------------------------------------------------
   -- Construct_Pair                                                     --
   ------------------------------------------------------------------------
   -- Gets a "key" and "value" and returns "key:value"
   function Construct_Pair (K, V : in Ustring) return String
   is
   begin
      return S (K) & ":" & S (V);
   end Construct_Pair;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a hit already is in the search
   function Contains (This : access Object; New_Hit : in Hit.Object'Class) return Boolean is
      Fams : Hit_Family.Object_Access_Array := Get_Families (This, New_Hit);
   begin
      for I in Fams'Range loop
         if Hit_Family.Contains (Fams (I).all, New_Hit) then
            return true;
         end if;
      end loop;
      return false;
   end Contains;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   function Create (
      Target   : in     String;
      Priority : in     Priorities) return Object_Access
   is
      use Agpl;
      New_search : Object_access;
   begin
      -- For now, we'll not detect magnets:
      if not Agpl.Magnet.Is_Magnet (Target) then
         New_search       := new Object (Kind => Keywords);
         New_search.Words := U (Target);
         New_search.Id    := From_String (Target);
      else
         New_Search       := new Object (Kind => Sha1_Digest);
         declare
            Mg        : Magnet.Object  := Magnet.Create (Target);
            Hash_Type : String         := Magnet.Get_Hash_Type (Mg);
         begin
            if Hash_Type = "sha1" then
               New_Search.Digest_Text := U (Magnet.Get_Hash_Value (Mg, Hash_Type));
            elsif Hash_Type = "bitprint" or else Hash_Type = "bp" then
               New_Search.Digest_Text :=
                  U (Agpl.Strings.Fields.Select_Field (Magnet.Get_Hash_Value (Mg, Hash_Type), 1, '.'));
            elsif Hash_Type = "several" then
               if Magnet.Get_Hash_Value (Mg, "sha1") /= "" then
                  New_Search.Digest_Text := U (Magnet.Get_Hash_Value (Mg, "sha1"));
               elsif Magnet.Get_Hash_Value (Mg, "bitprint") /= "" then
                  New_Search.Digest_Text :=
                     U (Agpl.Strings.Fields.Select_Field (Magnet.Get_Hash_Value (Mg, "bitprint"), 1, '.'));
               elsif Magnet.Get_Hash_Value (Mg, "bp") /= "" then
                  New_Search.Digest_Text :=
                     U (Agpl.Strings.Fields.Select_Field (Magnet.Get_Hash_Value (Mg, "bp"), 1, '.'));
               else
                  New_Search.Digest_Text := U (Magnet.Get_Hash_Value (Mg));
               end if;
            end if;
            New_Search.Digest := Sha1.From_Base32 (S (New_Search.Digest_Text));
            New_Search.Name   := U (Magnet.Get_Name (Mg));
            New_search.Id     := From_String (Get_Target (New_Search));
         end;
      end if;
      New_search.Priority := Priority;

      return New_search;
   end Create;

   ------------------------------------------------------------------------
   -- Create_From_XML                                                    --
   ------------------------------------------------------------------------
   -- Gets a Search Node (formed like the one generated for saving) and creates an Object
   function Create_From_XML (Srch : in Xml.Node) return Object_Access is
      Res   : Object_Access;
      Child : Xml.Node;
      Hitn  : Xml.Node;
      use type Xml.Node;
   begin
      if Xml.Get_Attribute (Srch, "kind", Kinds'Image (Keywords)) = Kinds'Image (Keywords)
      then
         Res         := new Object (Kind => Keywords);
         Res.Words   := U (Xml.Get_Attribute (Srch, "id", "error"));
         Res.Id      := From_String (S (Res.Words));
      else
         Res         := new Object (Kind => Sha1_Digest);
         declare
            Hash     : String := Xml.Get_Attribute (Srch, "sha1", "error");
         begin
            Res.Digest      := Sha1.From_Base32 (Hash);
            Res.Digest_Text := U (Hash);
            Res.Name        := U (Xml.Get_Attribute (Srch, "name", ""));
            Res.Id          := From_String (Get_Target (Res));
         end;
      end if;
      Res.Priority := Priorities'Value (Xml.Get_Attribute (Srch, "priority", "error"));
      Res.Paused   := Boolean'Value    (Xml.Get_Attribute (Srch, "paused", "false"));

      -- Hit families
      for N in 1 .. Positive'Last loop
         Child := Xml.Get ("family", Srch, N);
         exit when Child = null;

         -- Hits
         for M in 1 .. Positive'Last loop
            Hitn := Xml.Get ("hit", Child, M);
            exit when Hitn = null;

            declare
               H : Hit.Object'Class := Hit.Factory.Create_From_Xml (Hitn);
            begin
               Add_Hit (Res, H);
            end;
         end loop;
      end loop;

      return Res;
   end Create_From_XML;

   ------------------------------------------------------------------------
   -- Destroy                                                            --
   ------------------------------------------------------------------------
   -- This will be null after destruction
   procedure Destroy (This : in out Object_access) is
      procedure Free is new Unchecked_deallocation (Object, Object_Access);
   begin
      Free (This);
   end Destroy;

   -----------------------------
   -- Determine_Auto_Priority --
   -----------------------------
   -- Computes priority from hits thresholds
   function Determine_Auto_Priority (Hits : in Natural) return Priorities is
   begin
      if Hits <= Globals.Options.G2_Search_Priorities_Medium_Threshold then
         return High;
      elsif Hits <= Globals.Options.G2_Search_Priorities_Low_Threshold then
         return Medium;
      else
         return Low;
      end if;
   end Determine_Auto_Priority;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
      use Hit_Family_Map;
      I   : Iterator_Type := First (This.Ids);
      Aux : Hit_Family.Object_Access;
   begin
      while I /= Back (This.Ids) loop
         Aux := Element (I);
         Hit_Family.Free (Aux);
         I := Succ (I);
      end loop;
   end Finalize;

   ------------------------------------------------------------------------
   -- Get_Families                                                       --
   ------------------------------------------------------------------------
   -- Get families compatible with a hit
   -- Without duplicate families
   -- There can be at most N! families with the same hash and being distinct ones.
   -- Hence the fixed array size of candidates.
   function Get_Families (This : access Object; H : in Hit.Object'Class)
      return Hit_Family.Object_Access_Array is
   begin
      return Get_Families (This, Hit.Get_Hashes (H));
   end Get_Families;

   -- Without duplicate families
   -- There can be at most N! families with the same hash and being distinct ones.
   -- Hence the fixed array size of candidates.
   function Get_Families (This : access Object; H : in Hash_Dictionary.Object)
      return Hit_Family.Object_Access_Array
   is
      Fams : Hit_Family.Object_Access_Array (1 .. 120);
      Num  : Natural := 0;
      use Hit_Family_Map;
      First, Back : Iterator_Type;
      Hashes      : Hash_Dictionary.Pair_Array :=
         Hash_Dictionary.Get_Contents (H);

      -- Add if not present
      procedure Add_Fam (Fam : in Hit_Family.Object_Access) is
      begin
         for I in 1 .. Num loop
            if Fams (I) = Fam then
               return;
            end if;
         end loop;
         Num := Num + 1;
         Fams (Num) := Fam;
      end Add_Fam;

   begin
      for I in Hashes'Range loop
         Equal_Range (This.Hits, Construct_Pair (Hashes (I).Key, Hashes (I).Value), First, Back);
         while First /= Back loop
            Add_Fam (Element (First));
            First := Succ (First);
         end loop;
      end loop;
      return Fams (1 .. Num);
   end Get_Families;

   ------------------------------------------------------------------------
   -- Get_Payload                                                        --
   ------------------------------------------------------------------------
   -- Returns the searched thing
   function Get_Payload (This : in Object_Access) return Payload is
      Target : Payload (Kind => This.Kind);
   begin
      case This.Kind is
         when Keywords =>
            Target.Words := This.Words;
         when Sha1_Digest =>
            Target.Digest   := This.Digest;
      end case;
      return Target;
   end Get_Payload;

   ------------------------------------------------------------------------
   -- Pause                                                              --
   ------------------------------------------------------------------------
   procedure Pause (This : access Object) is
   begin
      This.Paused := true;
   end Pause;

   ------------------------------------------------------------------------
   -- Resume                                                             --
   ------------------------------------------------------------------------
   procedure Resume (This : access Object) is
   begin
      This.Paused := false;
   end Resume;

   ------------------------------------------------------------------------
   -- To_XML                                                             --
   ------------------------------------------------------------------------
   -- Returns a freshly allocated and created XML node <search/>
   -- Caller should deallocate.
   function To_Xml (
      This      : access Object;
      Doc       :        in Xml.Document;
      With_Hits :        in Boolean) return Xml.Node
   is
      Srch : Xml.Node := Xml.Create_Child (Doc, "search");
      use Hit_Family_Map;
      I : Iterator_Type := First (This.Ids);
   begin
      Xml.Set_Attribute (Srch, "id",       To_String (This.Id));
      Xml.Set_Attribute (Srch, "priority", This.Priority'Img);
      Xml.Set_Attribute (Srch, "paused",   This.Paused'Img);
      Xml.Set_Attribute (Srch, "kind",     This.Kind'Img);
      if This.Kind = Sha1_Digest then
         Xml.Set_Attribute (Srch, "sha1", Sha1.To_Base32 (This.Digest));
         Xml.Set_Attribute (Srch, "name", S (This.Name));
      end if;

      -- Add hit families as child nodes:
      if With_Hits then
         while I /= Back (This.Ids) loop
            Xml.Add (Srch, Hit_Family.To_Xml (Element (I).all, Doc));
            I := Succ (I);
         end loop;
      end if;

      return Srch;
   end To_Xml;

   ------------------------------------------------------------------------
   -- "="                                                                --
   ------------------------------------------------------------------------
   function "=" (L, R : in Object) return Boolean is
      use type Sha1.Digest;
   begin
      if L.Kind = R.Kind then
         case L.Kind is
            when Keywords    => return L.Words = R.Words;
            when SHA1_digest => return L.Digest = R.Digest;
         end case;
      else
         return false;
      end if;
   end "=";

   ------------------------------------------------------------------------
   -- Get_xxxxxx                                                         --
   ------------------------------------------------------------------------
   function Get_Id (This : access Object) return Search_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Family (This : access Object; Family : in Hit_Family.Family_Id) return Hit_Family.Object_Access is
      use Hit_Family_Map;
   begin
      return Element (Find (This.Ids, Hit_Family.To_String (Family)));
   end Get_Family;

   function Get_Firewalled_Hits (This : access Object) return Natural is
      use Hit_Family_Map;
      I   : Iterator_Type := First (This.Ids);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Ids) loop
         Num := Num + Hit_Family.Num_Firewalled_Hits (Element (I).all);
         I := Succ (I);
      end loop;
      return Num;
   end Get_Firewalled_Hits;

   function Get_Hits (This : access Object) return Natural is
      use Hit_Family_Map;
      I   : Iterator_Type := First (This.Ids);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Ids) loop
         Num := Num + Hit_Family.Num_Hits (Element (I).all);
         I := Succ (I);
      end loop;
      return Num;
   end Get_Hits;

   function Get_New_Hits (This : access Object) return Natural is
      use Hit_Family_Map;
      I   : Iterator_Type := First (This.Ids);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Ids) loop
         Num := Num + Hit_Family.Num_New_Hits (Element (I).all);
         I := Succ (I);
      end loop;
      return Num;
   end Get_New_Hits;

   function Get_Secure_Hits (This : access Object) return Natural is
      use Hit_Family_Map;
      I   : Iterator_Type := First (This.Ids);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Ids) loop
         Num := Num + Hit_Family.Num_Secure_Hits (Element (I).all);
         I := Succ (I);
      end loop;
      return Num;
   end Get_Secure_Hits;

   function Get_Kind (This : access Object) return Kinds is
   begin
      return This.Kind;
   end Get_Kind;

   -- Priority Delta that should now apply:
   function Get_Priority_Delta (This : access Object) return Natural is
   begin
      return Priority_values (Get_Effective_Priority (This));
   end Get_Priority_Delta;

   -- Priority at creation time:
   function Get_Priority (This : access Object) return Priorities is
   begin
      -- Check priority change:
      if This.Priority >= Exclusive5m then
         if Calendar.Clock - This.Changed >= Priority_Delays (This.Priority) then
            This.Priority := Auto;
         end if;
      end if;

      return This.Priority;
   end Get_Priority;

   -- Priority applied (distinct from the creation priority only if auto):
   function Get_Effective_Priority (This : access Object) return Priorities is
      Prio : Priorities := Get_Priority (This);
   begin
      if Prio = Auto then
         return Determine_Auto_Priority (Get_Hits (This));
      else
         return Prio;
      end if;
   end Get_Effective_Priority;

   function Get_Paused (This : access Object) return Boolean is
   begin
      return This.Paused;
   end Get_Paused;

   -- Descriptive target description:
   function Get_Target (This : access Object) return String is
   begin
      case This.Kind is
         when Keywords    => return S (This.Words);
         when Sha1_Digest =>
            if This.Name /= Null_Ustring then
               return S (This.Digest_Text) & " [" & S (This.Name) & "]";
            else
               return S (This.Digest_Text) & " [Anon.]";
            end if;
      end case;
   end Get_Target;

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (
      This : access Object; Family : in String; Expanded : in Boolean := true)
   is
      use Hit_Family_Map;
      I : Iterator_Type := Find (This.Ids, Family);
   begin
      Hit_Family.Set_Expanded (Element (I).all, Expanded);
   end Set_Expanded;

   ------------------------------------------------------------------------
   -- Set_xxxxxx                                                         --
   ------------------------------------------------------------------------
   procedure Set_Priority (This : access Object; Priority : in Priorities) is
   begin
      This.Changed  := Calendar.Clock;
      This.Priority := Priority;
   end Set_Priority;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (This : access Object; Data : in out Data_Set) is
      use Hit_Family_Map;
      I : Iterator_Type := First (This.Ids);
   begin
      while I /= Back (This.Ids) loop
         Hit_Family.Http_Report (Element (I).all, Data, This.Id);
         I := Succ (I);
      end loop;
   end Http_Report;

end Adagio.Searches.Search;
