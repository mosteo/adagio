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
Adagio.Convert,
Adagio.Download.Source.Http.Create,
Adagio.G2.Packet,
Adagio.Misc,
Adagio.Network.Endian,
Adagio.Socket,
Adagio.Socket.IP,
Adagio.Trace,
Adagio.Unicode,
Adagio.Xml.Utils,
Sha1,
Agpl.Strings,
Aws.Translator,
Aws.Url;

package body Adagio.G2.Hit is

   package Endian renames Adagio.Network.Endian;

   use type Types.File_Size;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Create all hits from a QH2 packet.
   function Create (Item : in Packet.Queue.Item_Type) return Object_Array is
      H        : Packet.Object renames Item.Packet;
      Result   : Object_Array (1 .. Max_Hits);
      Num_Hits : Natural             := 0;
      Hits     : Packet.Object_Array := Packet.Get_Children (H, "H");
      Groups   : Packet.Object_Array := Packet.Get_Children (H, "HG");
      Big_Endian : Boolean := Packet.Big_Endian (H);

      type Group_Type is record
         Id   : Natural;
         Busy : Boolean;
         BW   : Speed;
      end record;
      Group_Data : array (Groups'Range) of Group_Type;
      use type Socket.Ip.Address_Type;
   begin
      -- Prepare groups:
      for I in Groups'Range loop
         declare
            Pay : constant String := Packet.Payload (Packet.Get_Child (Groups (I), "SS"));
         begin
            Group_Data (I).Id   :=
               Endian.Convert (
                  Endian.To_Byte_Array (Packet.Payload (Groups (I))),
                  Big_Endian);
            Group_Data (I).Busy :=
               Endian.Convert (
                  Endian.To_Byte_Array (Pay (Pay'First .. Pay'First + 1)),
                  Big_Endian) >=
               Endian.Convert (
                  Endian.To_Byte_Array (Pay (Pay'First + 2 .. Pay'First + 2)),
                  Big_Endian);
            Group_Data (I).BW := File_Size (
               Endian.Convert (
                  Endian.To_Byte_Array (Pay (Pay'First + 3 .. Pay'First + 6)),
                  Big_Endian));
         end;
      end loop;
      -- Create Hits
      for I in Hits'Range loop
         Num_Hits   := Num_Hits + 1;
         -- Sender Guid
         Result (Num_Hits).Sender_Guid := Packet.Payload (Packet.Get_Child (H, "GU"));
         -- Sender Address
         if Packet.Is_A (H, "/QH2/NA") then
            Result (Num_Hits).Sender_Addr := U (G2.To_Address (
               Packet.Payload (Packet.Get_Child (H, "NA")),
               Big_Endian));
            if S (Result (Num_Hits).Sender_Addr) /= Socket.Image (Item.Udp_Source)
               and then not Socket.IP.Is_Public (S (Result (Num_Hits).Sender_Addr))
               and then not (Socket.IP.Kind (S (Result (Num_Hits).Sender_Addr)) =
                  Socket.IP.Local)
            then
               -- Mark firewalled
               Result (Num_Hits).Firewalled := true;
            end if;
         else
            Result (Num_Hits).Sender_Addr := U (Socket.Image (Item.Udp_Source));
         end if;
         if Result (Num_Hits).Firewalled then
            -- Add the packet source public address as a firewall too
            -- Add firewalled sources HERE from NA
            null;
         end if;
         -- Vendor code
         if Packet.Is_A (H, "/QH2/V") then
            Result (Num_Hits).Vendor := U (Packet.Payload (Packet.Get_Child (H, "V")));
         end if;
         -- Browsable
         Result (Num_Hits).Browsable := Packet.Is_A (H, "/QH2/BUP");
         -- Chatable
         Result (Num_Hits).Chatable := Packet.Is_A (H, "/QH2/PCT");
         -- Get groups info:
         declare
            Group : Integer;
         begin
            if Packet.Is_A (Hits (I), "/H/G") then
               Group := Endian.Convert (
                  Endian.To_Byte_Array (Packet.Payload (Packet.Get_child (Hits (I), "G"))),
                  Big_Endian);
            else
               Group := 0;
            end if;
            for K in Group_Data'Range loop
               if Group = Group_Data (K).Id then
                  Result (Num_Hits).Busy      := Group_Data (K).Busy;
                  Result (Num_Hits).Bandwidth := Group_Data (K).BW;
                  exit;
               end if;
            end loop;
         end;
         -- Add the URNs
         declare
            Urns : Packet.Object_Array := Packet.Get_Children (Hits (I), "URN");
         begin
            for J in Urns'Range loop
               declare
                  Pay  : String := Packet.Payload (Urns (J));
                  Urn  : String := Misc.Get_C_String (Pay);
                  Hash : String := Pay (Pay'First + Urn'Length + 1 .. Pay'Last);
               begin
                  if Urn = "sha1" then
                     Result (Num_Hits).Sha1 :=
                        U (Sha1.To_Base32 (Sha1.From_Char_Array (Hash)));
                     Add_Hash (Result (Num_Hits), urn,
                        Sha1.To_Base32 (Sha1.From_Char_Array (Hash)));
--                     Trace.Log ("G2.Hit.Create: Adding hash for family " & Urn,
--                        Trace.Always);
                  elsif Urn = "bp" then
                     Result (Num_Hits).Sha1 :=
                        U (Sha1.To_Base32 (Sha1.From_Char_Array (
                           Hash (Hash'First .. Hash'First + 19))));
                     Add_Hash (Result (Num_Hits), "sha1",
                        Sha1.To_Base32 (Sha1.From_Char_Array (
                           Hash (Hash'First .. Hash'First + 19))));
--                     Trace.Log ("G2.Hit.Create: Adding hash for family " & Urn,
--                        Trace.Always);
                  else
                     null;
--                     Trace.Log ("G2.Hit.Create: Discarding hash for family " & Urn,
--                        Trace.Always);
                  end if;
               end;
            end loop;
         end;
         -- Add URL if available
         if Packet.Is_A (Hits (I), "/H/URL") then
            Result (Num_Hits).Url :=
               U (Packet.Payload (Packet.Get_Child (Hits (I), "URL")));
--            Trace.Log ("G2.Hit.Create: Adding URL: " & S (Result (Num_Hits).Url),
--               Trace.Always);
         end if;
         -- Add Name skipping the 4-bytes for size if necessary
         if Packet.Is_A (Hits (I), "/H/DN") then
            if Packet.Is_A (Hits (I), "/H/SZ") then
               declare
                  Pay : String := Packet.Payload (Packet.Get_Child (Hits (I), "DN"));
                  Bad : Boolean := false;
               begin
                  -- Check for spurious sizes in names:
                  for K in Pay'First .. Pay'First + 3 loop
                     if Pay (K) < ' ' then
                        Bad := true;
                        exit;
                     end if;
                  end loop;
                  if Bad then
                     Set_Name (Result (Num_Hits), Agpl.Strings.Trim (Unicode.G2_To_String (
                        Pay (Pay'first + 4 .. Pay'Last),
                        Big_Endian)));
                     Trace.Log ("Correcting hit name: " & Pay, Trace.Debug);
                  else
                     Set_Name (Result (Num_Hits), Agpl.Strings.Trim (Unicode.G2_To_String (
                        Pay,
                        Big_Endian)));
                  end if;
               end;
            else
               declare
                  Pay : String := Packet.Payload (Packet.Get_Child (Hits (I), "DN"));
               begin
                  Set_Name (Result (Num_Hits), Agpl.Strings.Trim (Unicode.G2_To_String (
                     Pay (Pay'First + 4 .. Pay'Last),
                     Big_Endian)));
               end;
            end if;
         else
            Trace.Log ("G2.Hit.Create: Unnamed hit (xml?)", Trace.Warning);
            Set_Name (Result (Num_Hits), "(No data)");
         end if;
         -- Set Size:
         if Packet.Is_A (Hits (I), "/H/SZ") then
            Set_Size (Result (Num_Hits), File_Size (Endian.Convert_L (
               Endian.To_Byte_Array (Packet.Payload (Packet.Get_Child (Hits (I), "SZ"))),
               Big_Endian)));
         elsif Packet.Is_A (Hits (I), "/H/DN") then
            declare
               Pay : String := Packet.Payload (Packet.Get_Child (Hits (I), "DN"));
            begin
               Set_Size (Result (Num_Hits), File_Size (Endian.Convert (
                  Endian.To_Byte_Array (Pay (Pay'First .. Pay'First + 3)),
                  Big_Endian)));
            end;
         end if;
         -- Set Alt Sources
         if Packet.Is_A (Hits (I), "/H/CSC") then
            Result (Num_Hits).Alt_Sources := Endian.Convert (
               Endian.To_Byte_Array (Packet.Payload (Packet.Get_Child (Hits (I), "CSC"))),
               Big_Endian);
         end if;
         -- Get User comments
         if Packet.Is_A (Hits (I), "/H/COM") then
            declare
               Xmlstr : String := "<dummy>" & Unicode.G2_To_String (
                  Packet.Payload (Packet.Get_Child (Hits (I), "COM")),
                  Big_Endian) & "</dummy>";
               Doc    : Xml.Document := Xml.From_String (Xmlstr);
            begin
               Result (Num_Hits).Comment := U (Xml.Get_Attribute ("", "comment", Doc, ""));
               begin
                  Result (Num_Hits).Rating  :=
                     Xml.Utils.Get_Num ("comment", "rating", Doc, -1);
                  Result (Num_Hits).Rated   := true;
               exception
                  when Constraint_Error =>
                     null; -- No rating present
               end;
               Xml.Delete (Doc);
            end;
         end if;
         -- Get Preview URL
         if Packet.Is_A (Hits (I), "/H/PVU") then
            Result (Num_Hits).Preview := U (Unicode.G2_To_String (
               Packet.Payload (Packet.Get_Child (Hits (I), "PVU")),
               Big_Endian));
            if Result (Num_Hits).Preview = Null_Ustring then
               Result (Num_Hits).Preview := "http://" &
                  Result (Num_Hits).Sender_Addr & "/gnutella/preview/v1?urn:" &
                  S (Result (Num_Hits).Sha1);
            end if;
            -- Trace.log ("ADDING HIT WITH PREVIEW: " & S (Result (Num_Hits).Preview),
            --   Trace.Always);
         end if;
         -- Get sender nick
         if Packet.Is_A (H, "/QH2/UPROD/NICK") then
            Result (Num_Hits).Nick := U (Unicode.G2_To_String (
               Packet.Payload (Packet.Get_Child (H, "UPROD/NICK")),
               Big_Endian));
         end if;
      end loop;

      return Result (1 .. Num_Hits);
   exception
      when E : others =>
         Trace.Log ("G2.Hit.Create: " & Trace.Report (E), Trace.Warning);
         raise Malformed_Hit;
   end Create;

   ------------------------------------------------------------------------
   -- Get_Address                                                        --
   ------------------------------------------------------------------------
   function Get_Address (This : in object) return String is
   begin
      return S (This.Sender_Addr);
   end Get_Address;

   ------------------------------------------------------------------------
   -- Get_Extra                                                          --
   ------------------------------------------------------------------------
   function Get_Extra (This : in Object) return String is
      R : Ustring;
      use ASU;
   begin
      if This.Busy then
         Append (R, "<img src=""busy.png"">;");
      else
         Append (R, "<img src=""ready.png"">;");
      end if;
      if This.Rated then
         Append (R, "<img src=""star" & Misc.To_String (This.Rating) & ".png"">;");
      end if;
      if This.Comment /= Null_Ustring then
         Append (R, "Comment: " & This.Comment & ";");
      end if;
--      if This.Preview /= Null_Ustring then
--         Append (R, "<a href=""" & This.Preview & """>Preview</a>;");
--      end if;
      if This.Alt_Sources > 0 then
         Append (R, "Alt+" & Misc.To_String (This.Alt_Sources) & ";");
      end if;
      if This.Bandwidth > 0 then
         begin
            Append (R, "BW:" & Convert.To_Size (Natural (This.Bandwidth) * 1024) & "/s;");
         exception
            when Constraint_Error =>
               Append (R, "BW: Unlimited;");
         end;
      end if;
      if This.Nick /= Null_Ustring then
         Append (R, This.Nick);
         if This.Vendor /= Null_Ustring then
            Append (R, " using " & This.Vendor & ";");
         else
            Append (R, ";");
         end if;
      elsif This.Vendor /= Null_Ustring then
         Append (R, This.Vendor & ";");
      end if;

      Return Slice (R, 1, Length (R) - 1); -- Removing last ';'
   end Get_Extra;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Should return a unique id, identifying the source (IP based or something).
   function Get_Id (This : in Object) return String is
   begin
      return This.Sender_Guid;
   end Get_Id;

   ------------------------------------------------------------------------
   -- Get_Proxies                                                        --
   ------------------------------------------------------------------------
   -- Returns the addresses of the hubs this source is connected to.
   function Get_Proxies (This : in Object) return Ustring_Array is
      use Ustring_Vector;
   begin
      return Ustring_Array (This.Firewalls.Vector (1 .. Last (This.Firewalls)));
   end Get_Proxies;

   ------------------------------------------------------------------------
   -- Get_Source                                                         --
   ------------------------------------------------------------------------
   -- Constructs a Source'Class object for use in downloading corresponding
   -- to this hit.
   -- Can return Null if the source can't be built by whatever reason.
   function Get_Source (This : in Object) return Download.Source.Object_Access is
   begin
      return Download.Source.Object_Access (
         Download.Source.Http.Create.From_G2_Hit (This));
   end Get_Source;

   ------------------------------------------------------------------------
   -- Get_Urn                                                            --
   ------------------------------------------------------------------------
   -- Returns the string that identifies the resouce at the server
   function Get_Urn (This : in Object) return String is
   begin
      if This.Url /= Null_Ustring then
         declare
            Url : constant Aws.Url.Object := Aws.Url.Parse (S (This.Url));
         begin
            return Aws.Url.Pathname_And_Parameters (Url);
         end;
      else
         Trace.Log ("Hit has Sha1: " & S (This.Sha1), Trace.Always);
         return "/uri-res/N2R?urn:sha1:" & S (This.Sha1);
      end if;
   exception
      when E : others =>
         Trace.Log ("G2.Hit.Get_Urn: " & S (This.Url) & " is not valid URL: " &
            Trace.Report (E), Trace.Warning);
         return "/uri-res/N2R?urn:sha1:" & S (This.Sha1);
   end Get_Urn;

   ------------------------------------------------------------------------
   -- Is Firewalled                                                      --
   ------------------------------------------------------------------------
   function Is_Firewalled (This : in Object) return Boolean is
   begin
      return This.Firewalled;
   end Is_Firewalled;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   -- Merge two hits to get extra features from a set of hits.
   -- Merges second on first
   procedure Merge (L : in out Object; R : in Object) is
   begin
      L.Vendor      := Null_Ustring;
      L.Nick        := Null_Ustring;
      L.Firewalled  := L.Firewalled and R.Firewalled;
      L.Alt_Sources := L.Alt_Sources + R.Alt_Sources;
      if L.Rated then
         if R.Rated then
            L.Rating := Natural'Min (L.Rating, R.Rating);
         end if;
      elsif R.Rated then
         L.Rated  := true;
         L.Rating := R.Rating;
      end if;
      L.Busy := L.Busy and R.Busy;
      if not R.Busy then
         L.Bandwidth := L.Bandwidth + R.Bandwidth;
         if L.Bandwidth >= Speed'Last / 1024 then
            L.Bandwidth := Speed'Last / 1024;
         end if;
      end if;
   end Merge;

   ------------------------------------------------------------------------
   -- Create_From_Xml                                                    --
   ------------------------------------------------------------------------
   function Create_From_Xml (Node : in Xml.Node) return Object is
      H : Object;
      use Ustring_Vector;
   begin
      H.Firewalled   := Boolean'Value (Xml.Get_Attribute (Node, "firewalled", "false"));
      H.Sender_Guid  := Aws.Translator.To_String (Aws.Translator.Base64_Decode (
         Xml.Get_Attribute (Node, "SenderGuid", H.Sender_Guid)));
      H.Sender_Addr  := U (Xml.Get_Attribute (Node, "SenderAddr", "0.0.0.0:0"));
      for N in 1 .. Positive'Last loop
         declare
            Fwl : constant String := Xml.Get_Attribute (Node, "firewall" & Agpl.Strings.To_String (N), "end");
         begin
            exit when Fwl = "end";
            Append (H.Firewalls, U (Fwl));
         end;
      end loop;
      H.Nick         := U (Xml.Get_Attribute (Node, "nick", ""));
      H.Vendor       := U (Xml.Get_Attribute (Node, "vendor", ""));
      H.Preview      := U (Xml.Get_Attribute (Node, "preview", ""));
      H.Url          := U (Xml.Get_Attribute (Node, "url", ""));
      H.Alt_Sources  := Natural'Value (Xml.Get_Attribute (Node, "AltSources", "0"));
      H.Rated        := Boolean'Value (Xml.Get_Attribute (Node, "rated", "false"));
      if H.Rated then
         H.Rating    := Natural'Value (Xml.Get_Attribute (Node, "rating", "0"));
      end if;
      H.Comment      := U (Xml.Get_Attribute (Node, "comment", ""));
      H.Browsable    := Boolean'Value (Xml.Get_Attribute (Node, "browsable", "false"));
      H.Chatable     := Boolean'Value (Xml.Get_Attribute (Node, "chatable", "false"));
      H.Busy         := Boolean'Value (Xml.Get_Attribute (Node, "busy", "false"));
      H.Bandwidth    := Speed'Value (Xml.Get_Attribute (Node, "bandwidth", "0"));

      H.Sha1         := U (Xml.Get_Attribute (Node, "sha1", ""));

      return H;
   end;

   ------------------------------------------------------------------------
   -- To_Xml                                                             --
   ------------------------------------------------------------------------
   -- Caller should deallocate the resulting node
   function To_Xml (This : in Object; Doc : in Xml.Document) return Xml.Node is
      Node : Xml.Node := Xml.Create_Child (Doc, "hit");
      use Ustring_Vector;
   begin
      Xml.Set_Attribute (Node, "firewalled", This.Firewalled'Img);
      Xml.Set_Attribute (
         Node, "SenderGuid", Aws.Translator.Base64_Encode (This.Sender_Guid));
      Xml.Set_Attribute (Node, "SenderAddr", S (This.Sender_Addr));

      for N in 1 .. Last (This.Firewalls) loop
         Xml.Set_Attribute (Node, "firewall" & Agpl.Strings.To_String (N), S (This.Firewalls.Vector (N)));
      end loop;

      Xml.Set_Attribute (Node, "nick",       S (This.Nick));
      Xml.Set_Attribute (Node, "vendor",     S (This.Vendor));
      Xml.Set_Attribute (Node, "preview",    S (This.Preview));
      Xml.Set_Attribute (Node, "url",        S (This.Url));
      Xml.Set_Attribute (Node, "AltSources", Agpl.Strings.To_String (This.Alt_Sources));
      Xml.Set_Attribute (Node, "rated",      This.Rated'Img);
      if This.Rated then
         Xml.Set_Attribute (Node, "rating",  Agpl.Strings.To_String (This.Rating));
      end if;
      Xml.Set_Attribute (Node, "comment",    S (This.Comment));
      Xml.Set_Attribute (Node, "browsable",  This.Browsable'Img);
      Xml.Set_Attribute (Node, "chatable",   This.Chatable'Img);
      Xml.Set_Attribute (Node, "busy",       This.Busy'Img);
      Xml.Set_Attribute (Node, "bandwidth",  Agpl.Strings.To_String (Natural (This.Bandwidth)));

      return Node;
   end To_Xml;

end Adagio.G2.Hit;
