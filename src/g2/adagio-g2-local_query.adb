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
--  $Id: adagio-g2-local_query.adb,v 1.17 2004/02/05 18:31:20 Jano Exp $

--  Queries received from G2

With
Adagio.Constants,
Adagio.Ed2k,
Adagio.File.Safe,
Adagio.G2.Mesh,
Adagio.Globals.Options,
Adagio.Guid,
Adagio.Library,
Adagio.Library.Query,
Adagio.Misc,
Adagio.Network.Endian,
Adagio.Network_settings,
Adagio.Query.Incoming,
Adagio.Socket.IP,
Adagio.Trace,
Adagio.Unicode,
Adagio.Upload,
Adagio.Upload.Queue.Manager,
Adagio.Workers,
Adagio.Xml,
Sha1,
Sha1.Strings,
--Strings.Fields,
Agpl.Strings.Fields,
Strings.Utils,
TigerTree,
Adagio.Types,
Aenea.Countries,
Agpl.Context_controllers,
Agpl.Geoip,
Dom.Core,
Dom.Core.Nodes,
Ada.Calendar;

Use
Ada,
--Strings.Fields,
Agpl.Strings.Fields,
Strings.Utils,
Adagio.Constants;

package body Adagio.G2.Local_query is

   Nul : Character renames Adagio.Constants.Nul;

   use type Types.File_Size;

   Accept_browse : Boolean renames Globals.Options.Library_AllowBrowse;
   Accept_chat   : Boolean renames Globals.Options.Chat_Enabled;

   package Query_workers is new Workers (
      Query_context,
      Do_query,
      Null_procedure,
      Globals.Options.Library_MaxSearches,
      Globals.Options.Library_MaxPendingSearches);

   Search_timeout : Duration renames Globals.Options.Library_SearchTimeout;

   ------------
   -- Concur --
   ------------
   protected Concur is
      procedure Add;
      procedure Sub;
      procedure Get (Concurrent : out Natural);
   private
      Concurrent_searches : Natural := 0;
   end Concur;

   ------------------------------------------------------------------------
   -- Get_concurrent_searches                                            --
   ------------------------------------------------------------------------
   function Get_concurrent_searches return Natural is
      Conc : Natural;
   begin
      Concur.Get (Conc);
      return Conc;
   end Get_concurrent_searches;

   protected body Concur is
      procedure Add is
      begin
         Concurrent_searches := Concurrent_searches + 1;
      end Add;
      procedure Sub is
      begin
         Concurrent_searches := Concurrent_searches - 1;
      end Sub;
      procedure Get (Concurrent : out Natural) is
      begin
         Concurrent := Concurrent_searches;
      end Get;
   end Concur;

   procedure Add is
   begin
      Concur.Add;
   end Add;
   procedure Sub is
   begin
      Concur.Sub;
   end Sub;

   procedure Create_special_hit_child (
      Big_endian : in Boolean; Hit : out Packet.Object);

   ------------------------------------------------------------------------
   -- Add_country                                                        --
   ------------------------------------------------------------------------
   procedure Add_country (IP : in String) is
      Code : Agpl.Geoip.Country_code;
   begin
      begin
         Code := Agpl.Geoip.Country_code_from_addr (IP);
      exception
         when others =>
            Code := "??";
      end;
      Aenea.Countries.Sum_hub (Code, 1);
   end Add_country;

   ------------------------------------------------------------------------
   -- Get_IP                                                             --
   ------------------------------------------------------------------------
   --  Get the reply IP from a Q2 packet if it has a UDP child
   function Get_IP (This : in Packet.Object) return String is
   begin
      if not Packet.Is_a (This, "/Q2/UDP") then
         return "Local from hub";
      else
         return To_address (
            Packet.Payload (Packet.Get_child (This, "UDP")),
            Packet.Big_endian (This));
      end if;
   exception
      when E : others =>
         return "?.?.?.?:? (exception: " & Trace.Report (E) & ")";
   end Get_IP;
   ------------------------------------------------------------------------
   -- DN                                                                 --
   ------------------------------------------------------------------------
   --  Search files with given keywords
   procedure DN (
      Words     : in String;
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : Packet.Queue.Object_access;
      Queue_tcp : Packet.Queue.Object_access;
      Port      : in Natural;
      Hubs      : in Ustring_array) is
      Started   : Boolean;
   begin
      if Words'Length > Max_query_length then
         Trace.Log ("G2.Local_query.DN : Dropping query too large: " & Words,
            Trace.Informative);
      else
         Query_workers.Start (
            (Length   => Words'Length,
             Num_hubs => Hubs'Length,
             Words    => Words,
             Reply_to => Reply_to,
             Queue_tcp=> Queue_tcp,
             Queue_udp=> Queue_udp,
             Our_port => Port,
             Hubs     => Hubs),
            Search_timeout,
            Started);
         if not Started then
            Trace.Log ("G2.Local_query.DN: Too many queries, query dropped",
               Trace.Informative);
         end if;
      end if;
   end DN;

   procedure Null_procedure (Nothing : in Query_context) is
      pragma Unreferenced (Nothing);
   begin
      null;
   end Null_procedure;

   ------------------------------------------------------------------------
   -- Do_query                                                           --
   ------------------------------------------------------------------------
   procedure Do_query (On : in Query_context) is
      use Calendar;
      Start   : Time := Clock;
      use Library.File_set;
      Results : Container_type;
      I       : Iterator_type;
      Hit, H  : Packet.Object;
      Hitb    : Packet.Object;
      Counter : Agpl.Context_controllers.Simple_controller (
         Beginning => Add'Access, Ending => Sub'Access);
      pragma Unreferenced (Counter);
   begin
      --  Out on empty queries:
      if On.Words = "" then
         return;
      end if;
      --  Do the query:
      Library.Query.Multiple_words (On.Words, Results);
      Query_hits := Query_hits + Length (Results);
      --  Enqueue results:
      if Length (Results) > 0 then

         Hit := Create_hit_skeleton (On.Reply_to, On.Our_port, On.Hubs);

         --  We'll split results in packages of 8 hits:
         I := First (Results);
         for M in 0 .. (Length (Results) - 1) / 8 loop

            Hitb := Packet.Clone (Hit);

            for N in 1 .. 8 loop
               exit when I = Back (Results);
               Create_hit_child (Element (I), Packet.Big_endian (Hitb), H);
               Packet.Add_child (Hitb, H);
               File.Safe.Add_hit (Element (I));
               I := Succ (I);
            end loop;

            --  Final send:
            Send_hit (Hitb, On.Reply_to, On.Queue_udp, On.Queue_tcp);

         end loop;

         Library.Object.Mark_dirty;
      end if;

      --  Special hit for crawling:
      if Globals.Options.Debug_CrawlerAllowed and then
         On.Words = "accioagio"
      then
         Hit := Create_hit_skeleton (On.Reply_to, On.Our_port, On.Hubs);
         Create_special_hit_child (Packet.Big_endian (Hit), H);
         Packet.Add_child (Hit, H);
         Send_hit (Hit, On.Reply_to, On.Queue_udp, On.Queue_tcp);
      end if;

      --  Logging
      declare
         Level : Trace.Warning_level;
         Name  : Ustring := U (Misc.To_string (Length (Results)));
      begin
         if Length (Results) > 0 then
            Level := Trace.Debug;
         else
            Level := Trace.Debug;
         end if;
         Trace.Log ("Query: " &
            On.Words &
            "; Hits:" & Natural'Image (Length (Results)) &
            "; Latency: " & Misc.To_string (Float (Clock - Start), 5) &
            "; IP: " & Get_IP (On.Reply_to.Packet),
            Level);

         if Length (Results) = 1 then
            Name := U (File.Name (Element (First (Results))));
         end if;

         Query.Incoming.Add ((
            Arrival => Start,
            Kind    => U ("Words"),
            Terms   => U (On.Words),
            Hits    => Name,
            From    => U (Get_IP (On.Reply_to.Packet)),
            Latency => Clock - Start));
      end;

      --  Countries
      Add_country (Get_IP (On.Reply_to.Packet));
   exception
      when E : others =>
         Trace.Log ("G2.Local_query: " & Trace.Report (E),
            Trace.Error);
         raise;
   end Do_query;

   ------------------------------------------------------------------------
   -- URN                                                                --
   ------------------------------------------------------------------------
   --  Search files by URN
   --  The hubs are passed in readable format (i.e: "127.0.0.1:4610")
   --  Port is the port where we are listening.
   --  They are our current connected neighbours.
   procedure URN (
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : Packet.Queue.Object_access;
      Queue_tcp : Packet.Queue.Object_access;
      Port      : in Natural;
      Hubs      : in Ustring_array) is

      use Ada.Calendar;
      Start    : Calendar.Time := Calendar.Clock;
      Hit, H   : Packet.Object;
      Found    : File.Object;
      Urns     : Packet.Object_array := Packet.Get_children (
         Reply_to.Packet, "URN");
      use type File.Object;
      use type Packet.Object;
   begin
      for N in Urns'Range loop
         declare
            Search   : String := Packet.Payload (Urns (N));
            Family   : String := Select_field (Search, 1, Character'Val (0));
            Hash     : String :=
               Search (Search'First + Family'Length + 1 .. Search'Last);
            Hashdesc : Ustring;
            Num_hits : Natural := 0;
         begin
            Trace.Log ("G2.Local_query: urn:" & Family);

            if Family = "sha1" then
               Found := Library.Object.Query_sha1 (
                  Sha1.To_base32 (Sha1.From_char_array (Hash)));
               Hashdesc := U (Sha1.To_base32 (Sha1.From_char_array (Hash)));
            elsif Family = "bitprint" or Family = "bp" then
               Found := Library.Object.Query_sha1 (
                  Sha1.To_base32 (
                     Sha1.From_char_array (
                        Hash (Hash'First .. Hash'First + 19))));
               Hashdesc := U (Sha1.To_base32 (Sha1.From_char_array (
                  Hash (Hash'First .. Hash'First + 19))));
            elsif Family = "ed2k" then
               Found := Library.Object.Query_ed2k (
                  Ed2k.Hash_as_hex (
                     Ed2k.Hash_from_char_array (
                        Hash (Hash'First .. Hash'First + 15))));
               Hashdesc := U (Ed2k.Hash_as_hex (Ed2k.Hash_from_char_array (
                  Hash (Hash'First .. Hash'First + 15))));
            else
               Trace.Log (
                  "G2.Local_query: Discarded query for unknown family: " &
                  Family);
               Query.Incoming.Add ((
                  Arrival => Start,
                  Kind    => U ("URN"),
                  Terms   => U (Family & ":" & "Unknown family"),
                  Hits    => U ("0"),
                  From    => U (Get_IP (Reply_to.Packet)),
                  Latency => Calendar.Clock - Start));
               return;
            end if;

            if Found /= File.Null_file then
               Num_hits := 1;
               Query_hits := Query_hits + Num_hits;

               Hit := Create_hit_skeleton (Reply_to, Port, Hubs);
               Create_hit_child (Found, Packet.Big_endian (Hit), H);
               Packet.Add_child (Hit, H);

               File.Safe.Add_hit (Found);
               Library.Object.Mark_dirty;

               --  Final send:
               Send_hit (Hit, Reply_to, Queue_udp, Queue_tcp);
            end if;

            --  Logging:
            declare
               Level : Trace.Warning_level;
               Name  : Ustring;
            begin
               if Num_hits > 0 then
                  Level := Trace.Debug;
                  Name  := U (File.Name (Found));
               else
                  Level := Trace.Debug;
                  Name  := U ("0");
               end if;
               Trace.Log ("Query: family: " & Family &
                     "; Hits:" & Natural'Image (Num_hits) &
                     "; Latency: " &
                        Misc.To_string (Float (Calendar.Clock - Start), 5) &
                     "; IP: " & Get_IP (Reply_to.Packet),
                     Level);
               Query.Incoming.Add ((
                  Arrival => Start,
                  Kind    => U ("URN"),
                  Terms   => U (Family & ":" & S (Hashdesc)),
                  Hits    => Name,
                  From    => U (Get_IP (Reply_to.Packet)),
                  Latency => Calendar.Clock - Start));
            end;
         end;
      end loop;
      --  Countries
      Add_country (Get_IP (Reply_to.Packet));
   end URN;

   ------------------------------------------------------------------------
   --  MD
   ------------------------------------------------------------------------
   procedure MD (
      Reply_to  : in Packet.Queue.Item_type;
      Queue_udp : in Packet.Queue.Object_access;
      Queue_tcp : in Packet.Queue.Object_access;
      Port      : in Natural;
      Hubs      : in Ustring_array) is
      MD_Child  : Packet.Object := Packet.Get_child (
         Reply_to.Packet, "MD");
      Metadata  : constant String := Packet.Payload (MD_Child);
      Words     : Ustring;
      Doc       : Xml.Document;
      Attrs     : Dom.Core.Named_node_map;
      package DCN renames Dom.Core.Nodes;
      use Dom.Core;
   begin
      Doc := Xml.From_string (Metadata);
      declare
         Children  : Xml.Node_array := Xml.Get_all (Doc);
      begin
         for N in Children'Range loop
            Attrs := DCN.Attributes (Children (N));
            for M in 0 .. DCN.Length (Attrs) - 1 loop
               if M /= 0 then
                  ASU.Append (Words, " ");
               end if;
               ASU.Append (Words, DCN.Node_value (DCN.Item (Attrs, M)));
            end loop;
         end loop;
         Xml.Delete (Doc);
         --  Simple query
         Trace.Log ("Query: [from metadata] " & S (Words));
         if S (Words) /= "" then
            DN (S (Words), Reply_to, Queue_udp, Queue_tcp, Port, Hubs);
         end if;
      end;
   exception
      when others =>
         if Doc /= null then
            Xml.Delete (Doc);
         end if;
   end MD;

   ------------------------------------------------------------------------
   -- Create_hit_child                                                   --
   ------------------------------------------------------------------------
   procedure Create_hit_child (
      F : in File.Object; Big_endian : in Boolean; Hit : out Packet.Object) is

      H     : Packet.Object := Packet.Create ("H");
      Child : Packet.Object;

      package Conv renames Network.Endian;

   begin
      --  Add bitprint hit
      Child := Packet.Create ("URN", "bp" & Nul &
         Sha1.To_char_array (File.Sha (F)) &
         TigerTree.To_char_array (File.TTH (F)));
      Packet.Add_child (H, Child);

      --  Add ed2k hit
      Child := Packet.Create ("URN", "ed2k" & Nul &
         Ed2k.Hash_as_char_array (File.Ed2k (F)));
      Packet.Add_child (H, Child);

      --  Add URL flag
      Child := Packet.Create ("URL");
      Packet.Add_child (H, Child);

      --  Size
      Child := Packet.Create ("SZ",
         Conv.To_string (Conv.Convert (File.Size (F), 8, Big_endian)));
      Packet.Add_child (H, Child);

      --  Name
      Child := Packet.Create ("DN", Unicode.To_utf8(File.Name (F)));
      Packet.Add_child (H, Child);

      --  Cached source count:
      if Use_mesh then
         declare
            Cached_sources : Natural :=
               Mesh.Object.Count ("urn:sha1:" & Sha1.To_base32 (
                  File.Sha (F)));
         begin
            if Cached_sources > 0 then
               Child := Packet.Create ("CSC", Conv.To_string (
                  Conv.Convert (Cached_sources, 2, Big_endian)));
               Packet.Add_child (H, Child);
            end if;
         end;
      end if;

      Hit := H;
   end Create_hit_child;

   procedure Create_special_hit_child (
      Big_endian : in Boolean; Hit : out Packet.Object)
   is
      H     : Packet.Object := Packet.Create ("H");
      Child : Packet.Object;

      package Conv renames Network.Endian;
   begin
      --  Add bitprint hit with encrypted GUID to preserve anonymity
      --    but maintain uniqueness.
      Child := Packet.Create ("URN", "sha1" & Nul &
         Sha1.To_char_array (Sha1.Strings.Hash (
            Guid.To_char_array (Guid.My_guid))));
      Packet.Add_child (H, Child);

      --  Size
      Child := Packet.Create ("SZ",
         Conv.To_string (Conv.Convert (Integer'(1), 8, Big_endian)));
      Packet.Add_child (H, Child);

      --  Name
      Child := Packet.Create ("DN",
         Unicode.To_utf8 (Replace (User_agent, ".", " ")));
      Packet.Add_child (H, Child);

      Hit := H;
   end Create_special_hit_child;

   ------------------------------------------------------------------------
   -- Create_hit_skeleton                                                --
   ------------------------------------------------------------------------
   function Create_hit_skeleton (
      Query    : in Packet.Queue.Item_type;
      Our_port : Natural;
      Hubs     : Ustring_array) return Packet.Object is

      Hit      : Packet.Object := Packet.Create (
         "QH2", Character'Val (1) & Packet.Payload (Query.Packet));
      Child    : Packet.Object;
      Child2   : Packet.Object;

      package Conv renames Adagio.Network.Endian;
      use Network_settings;

      --  Says if the UDP child is internal, or false if not present
      function UDP_is_internal return Boolean is
         use Socket.IP;
      begin
         if not Packet.Is_a (Query.Packet, "/Q2/UDP") then
            return False;
         end if;
         declare
            sAddr : String :=
               To_address (Packet.Payload (Packet.Get_child (
                  Query.Packet, "UDP")),
                  Packet.Big_endian (Query.Packet));
         begin
            return Socket.IP.Kind (sAddr) /= Public;
         end;
      end UDP_is_internal;

   begin
      Child := Packet.Create ("GU", Guid.To_char_array (Guid.My_guid));
      Packet.Add_child (Hit, Child);

      Child := Packet.Create ("V", G2.Vendor_code);
      Packet.Add_child (Hit, Child);

      --  Our address. Will report one or other selon the network configuration
      Child := Packet.Create ("NA",
         To_char_array (Network_Settings.Get_Reachable_IP & ":" & Misc.To_string (Our_port),
         Packet.Big_endian (Hit)));
      Packet.Add_child (Hit, Child);

      --  Neighbours
      --  Only needed if we are not directly reachable:
      if Internet_route > NatForward and then not UDP_is_internal then
         for N in Hubs'Range loop
            Child := Packet.Create ("NH",
               To_char_array (S (Hubs (N)), Packet.Big_endian (Hit)));
            Packet.Add_child (Hit, Child);
         end loop;
      end if;

       --  Profile
      Child  := Packet.Create ("UPRO");
      Child2 := Packet.Create ("NICK",
            Xml.Get_attribute ("identity/handle", "primary",
               Globals.My_profile, ""));
      if Packet.Payload (Child2) /= "" then
         Packet.Add_child (Child, Child2);
         Packet.Add_child (Hit, Child);
      end if;

      --  Queue usage:
      Child  := Packet.Create ("HG", (1 => Character'Val (0)));
      declare
         Length  : String := To_string (
            Conv.Convert (Upload.Queue.Manager.Object.Max_length, 2,
               Packet.Big_endian (Hit)));
         Uploads : String := (1 => Character'Val (
            Natural'Min (
               Character'Pos (Character'Last),
               Upload.Queue.Manager.Object.Max_active_length)));
         Speed   : String := To_string (
            Conv.Convert (Natural (Globals.Options.Uploads_bandwidth) / 1024 * 8, 4,
               Packet.Big_endian (Hit)));
      begin
         Child2 := Packet.Create ("SS", Length & Uploads & Speed);
         Packet.Add_child (Child, Child2);
         Packet.Add_child (Hit, Child);
      end;

      Add_extra_children (Hit);

      return Hit;
   end Create_hit_skeleton;

   ------------------------------------------------------------------------
   -- Create_simple_hit_skeleton                                         --
   ------------------------------------------------------------------------
   function Create_simple_hit_skeleton return Packet.Object is
      Hit      : Packet.Object := Packet.Create (
         "QH2", Character'Val (1) & Guid.To_char_array (Guid.My_guid));
      Child    : Packet.Object;
      Child2   : Packet.Object;

      package Conv renames Adagio.Network.Endian;
      use Network_settings;

   begin
      Child := Packet.Create ("GU", Guid.To_char_array (Guid.My_guid));
      Packet.Add_child (Hit, Child);

      Child := Packet.Create ("V", G2.Vendor_code);
      Packet.Add_child (Hit, Child);

      --  Our address. Will report one or other selon the network configuration
      declare
         Our_port    : Natural renames Globals.Options.G2_port;
      begin
         Child := Packet.Create ("NA",
            To_char_array (Network_Settings.Get_Reachable_IP & ":" & Misc.To_string (Our_port),
            Packet.Big_endian (Hit)));
         Packet.Add_child (Hit, Child);
      end;

       --  Profile
      Child  := Packet.Create ("UPRO");
      Child2 := Packet.Create ("NICK",
            Xml.Get_attribute ("identity/handle", "primary",
               Globals.My_profile, ""));
      if Packet.Payload (Child2) /= "" then
         Packet.Add_child (Child, Child2);
         Packet.Add_child (Hit, Child);
      end if;

      --  Queue usage:
      Child  := Packet.Create ("HG", (1 => Character'Val (0)));
      declare
         Length  : String := To_string (
            Conv.Convert (Upload.Queue.Manager.Object.Max_length, 2,
               Packet.Big_endian (Hit)));
         Uploads : String := (1 => Character'Val (
            Upload.Queue.Manager.Object.Max_active_length));
         Speed   : String := To_string (
            Conv.Convert (Natural (Globals.Options.Uploads_bandwidth) / 1024 * 8, 4,
               Packet.Big_endian (Hit)));
      begin
         Child2 := Packet.Create ("SS", Length & Uploads & Speed);
         Packet.Add_child (Child, Child2);
         Packet.Add_child (Hit, Child);
      end;

      Add_extra_children (Hit);

      return Hit;
   end Create_simple_hit_skeleton;

   ------------------------------------------------------------------------
   -- Add_extra_children                                                 --
   ------------------------------------------------------------------------
   --  Adds browse/chat children as necessary
   procedure Add_extra_children (This : in out Packet.Object) is
      Child : Packet.Object;
   begin
      if Accept_chat then
         Child := Packet.Create ("PCH");
         Packet.Add_child (This, Child);
      end if;
      if Accept_browse then
         Child := Packet.Create ("BUP");
         Packet.Add_child (This, Child);
      end if;
   end Add_extra_children;

   ------------------------------------------------------------------------
   -- Send_hit                                                           --
   ------------------------------------------------------------------------
   procedure Send_hit (
      Hit   : Packet.Object;
      Query : Packet.Queue.Item_type;
      Queue_udp : Packet.Queue.Object_access;
      Queue_tcp : Packet.Queue.Object_access)
   is
      AuxP  : Packet.Object;
      AuxH  : Packet.Object := Hit;
      use Network_settings;
   begin
      if Packet.Is_a (Query.Packet, "/Q2/UDP") and then
         Internet_route /= none then
         --  Direct reply via UDP:
         --  We'll assume a IPv4 address:
         declare
            sAddr   : String := Packet.Payload (
               Packet.Get_child (Query.Packet, "UDP"));
            Address : Socket.Sock_addr_type := To_address (
               sAddr (sAddr'First .. sAddr'First + 5),
               Packet.Big_endian (Query.Packet));
         begin
            Packet.Queue.Send (
               Queue_udp.all,
               Hit,
               Address,
               Safe => True,
               In_response_to => Query.Packet);
         end;
      else
         --  Direct reply via TCP
         --  If the packet has a UDP child we must reply with a redirector TO
         --    if it's not for the own server
         if Packet.Is_a (Query.Packet, "/Q2/UDP") and then
            S (Query.Tcp_id) /=
               To_address (Packet.Payload (Packet.Get_child (
                  Query.Packet, "UDP")),
                  Packet.Big_endian (Query.Packet))
         then
            AuxP := Packet.Create ("TO", Packet.Payload (Query.Packet));
            Packet.Add_child (Hit, AuxP);
         end if;
         --  Sending
         Packet.Queue.Send (
            Queue_tcp.all,
            Hit,
            S (Query.Tcp_id),
            In_response_to => Query.Packet);
         --  Experimental -- Sent it without the TO
         Packet.Queue.Send (
            Queue_tcp.all,
            AuxH,
            S (Query.Tcp_id),
            In_response_to => Query.Packet);
      end if;
   end Send_hit;

   ------------------------------------------------------------------------
   -- Shutdown                                                           --
   ------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Query_workers.Shutdown;
   end Shutdown;

end Adagio.G2.Local_query;
