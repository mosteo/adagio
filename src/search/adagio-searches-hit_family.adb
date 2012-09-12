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
Adagio.Convert,
Adagio.Download.Source,
Adagio.Exceptions,
Adagio.G2.Hit,
Adagio.Misc,
Adagio.Trace,
Agpl.Counter.Multi,
Agpl.Magnet,
Agpl.Sequence,
Agpl.Strings,
Ada.Tags,
Ada.Unchecked_Deallocation,
Adagio.Types;

Use
Ada.Tags,
Adagio.Types;

package body Adagio.Searches.Hit_Family is

   package Id_Sequence is new Agpl.Sequence (Family_Id);
   Ids : Id_Sequence.Object;

   function To_String (Id : in Family_Id) return String is
   begin
      return Misc.To_String (Natural (Id));
   end To_String;

   function To_Family_Id (Id : in String) return Family_Id is
   begin
      return Family_Id'Value (Id);
   end To_Family_Id;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   -- Adds a hit. It must be compatible
   procedure Add_Hit (This : in out Object; H : in Hit.Object'Class) is
      New_Hit : Hit.Object_Access := new Hit.Object'Class'(H);
      Success : Boolean;
      J       : Hit_Map.Iterator_Type;
   begin
      -- Merge hits just in case
      Hash_Dictionary.Merge (This.Hashes, Hit.Get_Hashes (H));
      -- Add the hit
      Hit_Map.Insert (This.Hits, Hit.Get_Id (H), New_Hit, J, Success);
      if not Success then
         Hit.Free (New_Hit);
         Trace.Log ("FAILED HIT ADDITION: " & Hit.Get_Id (H), Trace.Error);
      end if;
   exception
      when E : others =>
         Trace.Log ("Hit_Family.Add_Hit: " & Trace.Report (E), Trace.Error);
         Hit.Free (New_Hit);
   end Add_Hit;

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Creates sources for its hits and add these to a download
   procedure Add_Sources_To_Download (This : in Object; Id : in Download.Slot_Id) is
      use Hit_Map;
      I : Iterator_Type := First (This.Hits);
   begin
      while I /= Back (This.Hits) loop
         declare
            Src : Download.Source.Object_Access :=
               Hit.Get_Source (Element (I).all);
         begin
            Download.Manager.Add_Source (Id, Src);
         end;
         I := Succ (I);
      end loop;
   end Add_Sources_To_Download;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   function Contains (This : in Object; H : in Hit.Object'Class) return Boolean is
      use Hit_Map;
   begin
      return Is_In (Hit.Get_Id (H), This.Hits);
   end;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- A seed hit is needed
   procedure Create (This : out object; From : in Hit.Object'Class) is
      New_Hit : Hit.Object_Access := new Hit.Object'Class'(From);
   begin
      Ids.Get_Next (This.Id);
      This.Name   := U (Hit.Get_Name (From));
      This.Size   := Hit.Get_Size (From);
      This.Hashes := Hit.Get_Hashes (From);
      Hit_Map.Insert (This.Hits, Hit.Get_Id (From), New_Hit);
   exception
      when E : others =>
         Trace.Log ("Hit_Family.Create: " & Trace.Report (E), Trace.Error);
         Hit.Free (New_Hit);
   end Create;

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : in Object) return Boolean is
   begin
      return L.Id = R.Id;
   end Equal;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access) is
      procedure Del is new Unchecked_Deallocation (Object, Object_Access);
   begin
      Del (This);
   end Free;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
      use Hit_Map;
      I : Iterator_Type := First (This.Hits);
   begin
      while I /= Back (This.Hits) loop
         Hit.Free (Element (I));
         I := Succ (I);
      end loop;
   end Finalize;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Get an unique id for the family (meaningless, for indexing)
   function Get_Id (This : in Object) return String is
   begin
      return To_String (This.Id);
   end Get_Id;

   ------------------------------------------------------------------------
   -- Get_Link                                                           --
   ------------------------------------------------------------------------
   -- Will provide a link for the hit
   function Get_Link (This : in Object) return String is
   begin
      if Is_Secure (This) then
         raise Exceptions.Unimplemented;
         return "http://127.0.0.1:8888/CHK@"; -- Unimplemented
      else
         return Get_Magnet (This, Secure => false, Sources => false);
      end if;
   end Get_Link;

   ------------------------------------------------------------------------
   -- Get_Magnet                                                         --
   ------------------------------------------------------------------------
   -- Will try to get a magnet link for sha1 hashes
   -- Raise No_Such_Hash if unable to obtain it
   function Get_Magnet (
      This    : in Object;
      Secure  : in Boolean := false;
      Sources : in Boolean := true) return String
   is
      use Agpl;
      use Hash_Dictionary;
      Hashes : Pair_Array := Get_Contents (This.Hashes);
      Result : Magnet.Object;
   begin
      if Sources then
         pragma Unimplemented;
         raise Exceptions.Unimplemented;
      end if;
      Magnet.Add_Attribute (Result, Magnet.Name_Attr, Get_Name (This));
      Magnet.Add_Attribute (
         Result, Magnet.User_Defined_Prefix & "sz",
         Agpl.Strings.Trim (Types.File_Size'Image (Get_Size (This))));
      for I in Hashes'Range loop
         if S (Hashes (I).Key) = "sha1" and not Secure then
            Magnet.Add_Attribute (Result, Magnet.Uri_Attr,
               "urn:sha1:" & S (Hashes (I).Value));
            return Magnet.To_String (Result);
         elsif S (Hashes (I).Key) = Secure_Key and Secure then
            raise Exceptions.Unimplemented;
         end if;
      end loop;
      raise No_Such_Hash;
   end Get_Magnet;

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
   function Get_Size (This : in Object) return Types.File_Size is
   begin
      return This.Size;
   end Get_Size;

   ------------------------------------------------------------------------
   -- Has_New_Hits                                                       --
   ------------------------------------------------------------------------
   function Has_New_Hits (This : in Object) return Boolean is
      use Hit_Map;
      I : Iterator_Type := First (This.Hits);
   begin
      while I /= Back (This.Hits) loop
         if Hit.Is_New (Element (I).all) then
            return true;
         end if;
         I := Succ (I);
      end loop;
      return false;
   end Has_New_Hits;

   ------------------------------------------------------------------------
   -- Is_Compatible                                                      --
   ------------------------------------------------------------------------
   -- Says if a hit is compatible with this family
   function Is_Compatible (This : in Object; H : in Hit.Object'Class) return Boolean is
   begin
      return Hash_Dictionary.Are_Compatible (
         This.Hashes,
         Hit.Get_Hashes (H));
   end Is_Compatible;

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
   -- Num_Firewalled_Hits                                                --
   ------------------------------------------------------------------------
   function Num_Firewalled_Hits (This : in Object) return Natural is
      use Hit_Map;
      I   : Iterator_Type := First (This.Hits);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Hits) loop
         if Hit.Is_Firewalled (Element (I).all) then
            Num := Num + 1;
         end if;
         I := Succ (I);
      end loop;
      return Num;
   end Num_Firewalled_Hits;

   ------------------------------------------------------------------------
   -- Num_Hits                                                           --
   ------------------------------------------------------------------------
   function Num_Hits (This : in Object) return Natural is
   begin
      return Hit_Map.Length (This.Hits);
   end Num_Hits;

   ------------------------------------------------------------------------
   -- Num_New_Hits                                                       --
   ------------------------------------------------------------------------
   function Num_New_Hits (This : in Object) return Natural is
      use Hit_Map;
      I   : Iterator_Type := First (This.Hits);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Hits) loop
         if Hit.Is_New (Element (I).all) then
            Num := Num + 1;
         end if;
         I := Succ (I);
      end loop;
      return Num;
   end Num_New_Hits;

   ------------------------------------------------------------------------
   -- Num_Secure_Hits                                                    --
   ------------------------------------------------------------------------
   function Num_Secure_Hits (This : in Object) return Natural is
      use Hit_Map;
      I   : Iterator_Type := First (This.Hits);
      Num : Natural       := 0;
   begin
      while I /= Back (This.Hits) loop
         if Hit.Is_Secure (Element (I).all) then
            Num := Num + 1;
         end if;
         I := Succ (I);
      end loop;
      return Num;
   end Num_Secure_Hits;

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (This : in out Object; Expanded : in Boolean := true) is
   begin
      This.Expanded := Expanded;
   end Set_Expanded;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   procedure Http_Report (
      This : in out Object; Data : in out Data_Set; Srch : in Search_Id)
   is
      use Hit_Map;
      I      : Iterator_Type   := First (This.Hits);
      NHits  : Natural := Length (This.Hits);
      Extra  : Ustring;
      use ASU;
   begin
      if NHits = 1 then
         Extra := U (Hit.Get_Extra (Element (First (This.Hits)).all));
      end if;
      -- Get common name, size, extras:
      declare
         Names, Sizes : Agpl.Counter.Multi.Object;
         I            : Iterator_Type := First (This.Hits);
         package MCounter renames Agpl.Counter.Multi;
         Aux_Hit      : G2.Hit.Object;
         Inited       : Boolean := false;
      begin
         while I /= Back (This.Hits) loop
            -- Merge extra info
            if Element (I).all'Tag = G2.Hit.Object'Tag then
               if Inited then
                  G2.Hit.Merge (Aux_Hit, G2.Hit.Object (Element (I).all));
               else
                  Aux_Hit := G2.Hit.Object (Element (I).all);
                  Inited  := true;
               end if;
            end if;

            -- Name & Size
            MCounter.Add (Names, Hit.Get_Name (Element (I).all));
            MCounter.Add (Sizes, File_Size'Image (Hit.Get_Size (Element (I).all)));
            I := Succ (I);
         end loop;
         This.Name := U (MCounter.Max_Key (Names));
         This.Size := File_Size'Value (MCounter.Max_Key (Sizes));
         if Extra = Null_Ustring and then Inited then
            Extra := U (G2.Hit.Get_Extra (Aux_Hit));
         end if;
      end;
      -- Firstly, the family header
      declare
         Row : Data_Row;
         Prefix : constant String := S (Rpad (Natural (This.Id))) & "1";
      begin
         -- Hits
         Append (Row, (
            U (Misc.To_String (Num_Hits (This))),
            U (S (Rpad (Num_Hits (This))) & Prefix)));
         -- New hits
         Append (Row, (
            U (Misc.To_String (Num_New_Hits (This))),
            U (S (Rpad (Num_New_Hits (This))) & Prefix)));
         -- Fwd hits
         Append (Row, (
            U (Misc.To_String (Num_Firewalled_Hits (This))),
            U (S (Rpad (Num_Firewalled_Hits (This))) & Prefix)));
         -- Name
         Append (Row, (
            This.Name,
            U (S (This.Name) & Prefix)));
         -- Size
         Append (Row, (
            U (Convert.To_Size (Natural (This.Size))),
            U (S (Rpad (Natural (This.Size))) & Prefix)));
         -- Extra
         Append (Row, (Extra, Extra & U (Prefix)));
         -- Hit?
         Append (Row, (
            U (Boolean'Image (false)),
            U (Boolean'Image (false) & Prefix)));
         -- Expanded?
         Append (Row, (
            U (Boolean'Image (This.Expanded)),
            U (Boolean'Image (This.Expanded) & Prefix)));
         -- Id
         Append (Row, (
            U (Misc.To_String (Natural (This.Id))),
            U (Misc.To_String (Natural (This.Id)) & Prefix)));
         -- Magnet
         Append (Row, (
            U (Get_Link (This)),
            U (Get_Link (This) & Prefix)));
         -- Srch id
         Append (Row, (
            U (To_String (Srch)),
            U (To_String (Srch) & Prefix)));
         -- Secure
         Append (Row, (
            U (Misc.To_String (Num_Secure_Hits (This))),
            U (S (Rpad (Num_Secure_Hits (This))) & Prefix)));

         Append (Data, Row);
      end;
      -- Hits
      declare
         Prefix : constant String  := S (Rpad (Natural (This.Id))) & "0";
         Nums   : array (Boolean) of Natural := (false => 0, true => 1);
      begin
         while I /= Back (This.Hits) loop
            declare
               Row    : Data_Row;
               H      : Hit.Object'Class renames Element (I).all;
            begin
               if This.Expanded then
                  -- Hits
                  Append (Row, (
                     Null_Ustring,
                     U (S (Rpad (Num_Hits (This))) & Prefix)));
                  -- New hits
                  Append (Row, (
                     U (Misc.To_String (Nums (Hit.Is_New (H)))),
                     U (S (Rpad (Num_New_Hits (This))) &
                        Prefix & S (Rpad (Nums (Hit.Is_New (H)), 2)))));
                  -- Fwd hits
                  Append (Row, (
                     U (Misc.To_String (Nums (Hit.Is_Firewalled (H)))),
                     U (S (Rpad (Num_Firewalled_Hits (This))) &
                        Prefix & S (Rpad (Nums (Hit.Is_Firewalled (H)), 2)))));
                  -- Name
                  Append (Row, (
                     U (Hit.Get_Name (H)),
                     U (S (This.Name) & Prefix & Hit.Get_Name (H))));
                  -- Size
                  Append (Row, (
                     U (Convert.To_Size (Natural (Hit.Get_Size (H)))),
                     U (S (Rpad (Natural (This.Size))) & Prefix & S (Rpad (Natural (Hit.Get_Size (H)))))));
                  -- Extra
                  Append (Row, (
                     U (Hit.Get_Extra (H)),
                     Extra & Prefix & Hit.Get_Extra (H)));
                  -- Hit?
                  Append (Row, (
                     U (Boolean'Image (true)),
                     U (Boolean'Image (false) & Prefix & Boolean'Image (true))));
                  -- Expanded?
                  Append (Row, (
                     U (Boolean'Image (This.Expanded)),
                     U (Boolean'Image (This.Expanded) & Prefix &
                        Boolean'Image (This.Expanded))));
                  -- Id
                  Append (Row, (
                     U (Misc.To_String (Natural (This.Id))),
                     U (Misc.To_String (Natural (This.Id)) & Prefix)));
                  -- Magnet
                  Append (Row, (
                     U (Get_Link (This)),
                     U (Get_Link (This) & Prefix)));
                  -- Srch id
                  Append (Row, (
                     U (To_String (Srch)),
                     U (To_String (Srch) & Prefix)));
                  -- Secure?
                  Append (Row, (
                     U (Misc.To_String (Nums (Hit.Is_Secure (H)))),
                     U (S (Rpad (Num_Secure_Hits (This))) &
                        Prefix & S (Rpad (Nums (Hit.Is_Secure (H)), 2)))));

                  Append (Data, Row);
               end if;
               Hit.Set_New (H, false);
            end;
            I := Succ (I);
         end loop;
      end;
   end Http_Report;

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node is
      Fam : Xml.Node := Xml.Create_Child (Doc, "family");
      H   : Xml.Node;
      use Hit_Map;
      I : Iterator_Type := First (This.Hits);
   begin
      Xml.Set_Attribute (Fam, "id", Get_Id (This));

      while I /= Back (This.Hits) loop
         H := Hit.To_Xml (Element (I).all, Doc);
         Hit.Merge_To_Xml (Hit.Object (Element (I).all), H); -- Save common things
         Xml.Add (Fam, H);
         I := Succ (I);
      end loop;

      return Fam;
   end;

end Adagio.Searches.Hit_Family;
