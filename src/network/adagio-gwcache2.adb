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
--  $Id: adagio-gwcache2.adb,v 1.7 2004/02/04 16:20:14 Jano Exp $

with Adagio.Chronos;
with Adagio.Http;
with Adagio.Misc;
with Adagio.Trace;
with Adagio.Traffic;
with Adagio.Xml;

with Agpl.Strings;


with Charles.Hash_string;
with Charles.Maps.Hashed.Strings.Unbounded;

with Aws.Client;
with Aws.Messages;
with Aws.Net;
with Aws.Response;
with Aws.Url;

package body Adagio.GWCache2 is

   -- Helpers
   Client_id : UString := U (
      "AGIO" & User_agent (User_agent'First + 7 .. User_agent'Last));

   procedure Set_client_id (
      Acronym : in Acronyms := "AGIO";
      Name    : in String   :=
         User_agent (User_agent'First + 7 .. User_agent'Last)) is
   begin
      Client_id := U (Acronym & Name);
   end Set_client_id;

   -- Local test:
   Local_test : Boolean renames Globals.Options.GWC2_LocalTest;

   use type Aws.Messages.Status_code;
   use type Calendar.Time;
   use type Server.Object_access;

   -- Local list of targetted networks:
   package Network_list is new Charles.Maps.Hashed.Strings.Unbounded(
      Network_access, Charles.Hash_string, "=", "=");

   Networks: Network_list.Container_type;

   -- Say if we can query a server:
   function Is_acceptable(this: in Server_type) return boolean;

   task body Inquirer is
      Net             : Ustring;
      Parent          : Network_access;
      Connect_timeout : Integer;
      Answer_timeout  : Integer;
      New_server      : Server_access;

      -----------
      -- Parse --
      -----------
      -- This function process a string extracting hosts and caches
      procedure Parse(s: String; Success : out Boolean) is
         First, Last, Mid : Integer := s'First;
         Host: Network_node;
         -- End of parse
         function End_of_parse return boolean is
         begin
            return First >= s'Last;
         end End_of_parse;
         -- Skip_line
         procedure Skip_line is
         begin
            while First <= s'Last and then s(First) /= Http.LF loop
               First := First + 1;
            end loop;
            First := First + 1;
            Last := First;
            while Last <= s'Last and then s(Last) /= Http.LF loop
               Last:= Last + 1;
            end loop;
            Mid := First + 2;
         end Skip_line;
      begin
         Success := true;
         if S'Length = 0 then
            Trace.Log ("GWebCache2.Parse: Empty response");
            Success := false;
            return;
         end if;
         if S'length >= 5 and then
            S (S'first .. S'first + 4) = "ERROR"
         then
            Success := false;
         end if;
         Mid := First + 2; -- Skip first '|'
         while not End_of_parse loop
         begin
            case Misc.To_lower (s(First)) is
               -------------------
               -- New hostcache --
               when 'u' =>
                  -- Skip forward until '|' or CR
                  while Mid <= s'Last and then
                     s(Mid) /= '|' and then
                     s(Mid) /= Http.CR and then
                     s(Mid) /= Http.LF loop
                     Mid := Mid + 1;
                  end loop;
                  -- Create a new webcache and add it:
                  if Agpl.Strings.Starts (s (First + 2 .. Mid - 1), "http://") then
                     if Mid <= s'Last then
                        Adding: begin
                           New_server := Create(s(First + 2 .. Mid - 1));
                           Server.List.Add(Server.Object_access(New_server));
                        exception
                           when Server.Server_already_cached =>
                              null;  -- Server discarded
                        end Adding;
                     end if;
                  end if;
               --------------
               -- New host --
               when 'h' =>
                  -- Skip forward until ':'
                  while s(Mid) /= ':' loop
                     Mid := Mid + 1;
                  end loop;
                  -- Create the network_node and add it to local cache:
                  Host.Address := To_ustring(s(First + 2 .. Mid - 1));
                  -- Get the port:
                  First := Mid + 1;
                  while Mid <= s'Last and then
                     s(Mid) /= Http.CR and then
                     s(Mid) /= '|' and then
                     s(Mid) /= Http.LF loop
                     Mid := Mid + 1;
                  end loop;
                  Host.Port := Natural'Value(s(First .. Mid - 1));
                  Parent.Nodes.Put(Host);
               ----------------------
               -- Informative line --
               when 'i' =>
                  Trace.Log ("GWCache2: informative line: " &
                     S (First .. Last));
                  if S (Last) = Http.CR then
                     Last := Last - 1;
                  end if;
                  if S (Mid .. Last) = "net-not-supported" then
                     Success := false;
                     return;
                  end if;
               --------------
               -- Unknowns --
               when others =>
                  Trace.Log("GWCache2: unknown line: " & s(First .. Last),
                     Trace.Debug);
                  Success := false;
                  return;
            end case;
            Skip_line;
         exception
            when others =>
               Trace.Log(
                  "GWCache2 parsing failed for line: " & s(First .. Last),
                  Trace.Warning);
               Success := false;
               return;
         end;
         end loop;
      end Parse;

      --------------
      -- Do_query --
      --------------
      Cron_acc_log : Chronos.Object;
      procedure Do_query is
         -- Get proxy info:
         proxy: String renames S (Globals.Options.Network_proxy);
         Url: Aws.Url.Object:= Aws.Url.Parse(proxy);
         host: String:= Aws.Url.Host(Url) & ":" & Aws.Url.Port(Url);
         user: String:= Aws.Url.User(Url);
         pass: String:= Aws.Url.Password(Url);
         -- Get better cache:
         cache: Server.Object_access_array:= Server.List.Get_best
           (Network_id, 20);
         gwcache : Server_access:= null;
         Response: Aws.Response.Data;
         Status: Aws.Messages.Status_code;
         Ask: String:= "?get=1&net=" &
            Misc.To_lower (Parent.Target_network.all) &
            "&client=" & Aws.Url.Encode(S (Client_id));
      begin
         -- Get timeouts
         Connect_timeout:= Integer (Globals.Options.GWC2_ConnectTimeout);
         Answer_timeout := Integer (Globals.Options.GWC2_AnswerTimeout);
         -- Try first acceptable:
         for n in cache'Range loop
            if Is_acceptable(Server_access(cache(n)).all) then
               gwcache:= Server_access(cache(n));
            end if;
         end loop;
         -- Check in unused servers:
         for n in cache'Range loop
            if cache (n) /= Server.Object_access (gwcache) then
               Server.List.Check_in(cache(n));
            end if;
         end loop;
         if gwcache /= null then
            -- Can query it?
            if Is_acceptable(gwcache.all) then
               -- Mark traffic
               Traffic.Add ((
                  Arrival  => Calendar.Clock,
                  Protocol => U ("HTTP"),
                  Way      => Traffic.Outgoing,
                  From     => gwcache.Url,
                  Name     => U ("Request"),
                  Data     => Null_ustring));
               -- Mark last access:
               gwcache.Last_access := Calendar.Clock;
               -- Get it!
               Trace.Log(
                  "GWebCache2 querying: " & To_string(gwcache.Url) & "...",
                  Trace.Informative);
               Trace.Log(
                  "GWebCache2 query: " & To_string(gwcache.Url) & Ask,
                  Trace.Debug);
               begin
                  if Host(Host'First) /= ':' then
                     Response := Aws.Client.Get(
                        Url                 => To_string(gwcache.Url) & Ask,
                        Proxy               => Host,
                        Proxy_user          => User,
                        Proxy_pwd           => Pass,
                        Timeouts            => (Connect_timeout,
                                                Answer_timeout),
                        Follow_redirection  => true);
                  else
                     Response := Aws.Client.Get(
                        Url                 => To_string(gwcache.Url) & Ask,
                        Timeouts            => (Connect_timeout,
                                                Answer_timeout),
                        Follow_redirection  => true);
                  end if;
               exception
                  when Aws.Net.Socket_error =>
                     Trace.Log ("GWebCache2: Can't connect with " &
                        To_string (gwcache.Url), Trace.Informative);
                     Server.List.Check_in(Server.Object_access(gwcache));
                     return;
               end;
               Status:= Aws.Response.Status_code(Response);
               if Status > Aws.Messages.s307 then
                  gwcache.Failures := gwcache.Failures + 1;
               else
                  gwcache.Successes:= gwcache.Successes + 1;
               end if;
               -- Parse result:
               if Status <= Aws.Messages.s307 then
                  declare
                     Success : Boolean;
                  begin
                     Parse(Aws.Response.Message_body(Response), Success);
                     if not Success then
                        gwcache.Failures := gwcache.Failures + 1;
                        gwcache.Successes:= gwcache.Successes - 1;
                     end if;
                  exception
                     when others =>
                        gwcache.Failures := gwcache.Failures + 1;
                        gwcache.Successes:= gwcache.Successes - 1;
                  end;
               end if;
               -- Check in used server:
               Trace.Log("GWebCache2 query done: " &
                  Aws.Messages.Image(Status) & " (" &
                  Aws.Messages.Reason_phrase(Status) & ")");
               end if;
            Server.List.Check_in(Server.Object_access(gwcache));
         elsif Chronos.Elapsed (Cron_acc_log) > 5.0 then
            Chronos.Reset (Cron_acc_log);
            Trace.Log ("GWebCache2: No acceptable caches found");
         end if;
      exception
         when E : others =>
            Trace.Log ("Gwcache2.Do_query: " & Trace.Report (E),
               Trace.Warning);
            -- Check in the selected server:
            if gwcache /= null then
               Server.List.Check_in(Server.Object_access(gwcache));
            end if;
      end Do_query;

   begin
      loop
      begin
         select
            accept Query_any(this: Network_access; Net: String) do
               Inquirer.Net:= To_ustring (Net);
               Parent:= this;
            end Query_any;
            -- Do query:
            Do_query;
         or
            terminate;
         end select;
      exception
         when E: others =>
            Trace.Log("Gwcache2.Inquirer [loop]: " & Trace.Report(E),
               Trace.error);
      end;
      end loop;
   end Inquirer;

   -- Function to obtain a few nodes for a given network:
   -- Will query any GWCache following its internal criterion
   -- We'll return 20 each time at most
   function Query_any(Network_id: in String; Desired: Integer := 20)
      return Network_node_array is
      Result: Network_node_array(1 .. Desired);
      Num: Natural:= 0;
      Net: Network_access:= Network_list.Element(
         Network_list.Find(Networks, Network_id));
   begin
      if Local_test then
         return (1 => (U ("127.0.0.1"), Globals.Options.GWC2_LocalTest_port));
      end if;
      if Desired < 1 then
         return Result (1 .. 0);
      end if;
      if Net.Nodes.Length < Result'Length then
         -- Pre-fetch some more:
         select
            Net.The_task.Query_any(Net, Network_id);
         else
            null; -- Go ahead;
         end select;
      end if;
      while Num < Result'Last and not Net.Nodes.Is_empty loop
         Num:= Num + 1;
         Net.Nodes.Get (Result(Num));
      end loop;
      return Result (1 .. Num);
   end Query_any;

   -------------------
   -- Network stuff --
   -------------------

   -- Gives the network identifier
   function Id(this: in Network_type) return String is
      pragma Unreferenced (This);
   begin
      return Network_id;
   end Id;

   -- Connect to that network. Will get servers and connect them as needed.
   procedure Connect(this: in out Network_type) is
      pragma Unreferenced (This);
   begin
      null;
   end Connect;

   -- Disconnect:
   procedure Disconnect(this: in out Network_type) is
      pragma Unreferenced (This);
   begin
      null;
   end Disconnect;

   -- Says status of the network.
   function Status(this: in Network_type) return Network.Network_status is
      pragma Unreferenced (This);
   begin
      return Network.Connected;
   end Status;

   -- Obtain search handler. Can return null if the network is not to be
   -- searched:
   function Get_Search_Handler (This : in Network_Type)
      return Searches.Handler.Object_Access
   is
      pragma Unreferenced (This);
   begin
      return null;
   end Get_Search_Handler;

   ------------------
   -- Server stuff --
   ------------------

   -- Creation from URL, returns allocated and initialized server:
   function Create(Url : String; Is_root : Boolean := false)
      return Server_access is
      s : Server_access;
   begin
      s := new Server_type;
      s.Url := To_ustring(Url);
      s.Last_access := Past_aeons;
      s.Is_root := Is_root;
      return s;
   end Create;

   -- Says if we can query a server:
   function Is_acceptable(this: in Server_type) return boolean is
   begin
      -- Not more that once per hour.
      return Calendar.Clock - this.Last_access > Sleep_time;
   end Is_acceptable;

   -- Get a unique id to identify it:
   function Id(this: in Server_type) return String is
   begin
      return To_string(this.Url);
   end Id;

   -- Get network it belongs:
   function Net(this: in Server_type) return String is
      pragma Unreferenced (this);
   begin
      return Network_id;
   end Net;

   -- Evaluate its goodness to be connected:
   function Rate(this: in Server_type) return Server.Rating is
      Elapsed: float :=
         float'Max(float(Calendar.Clock - this.Last_access), 1.0);
   begin
      if not this.Is_root then
         if this.Failures >= 2 then
            return Server.Rating'First;
         end if;
      end if;

      -- Prioritize least used caches:
      return
         Server.Rating (30_000_000.0 - (30_000_000.0 / Elapsed));
   end Rate;

   ------------------------------------------------------------------------
   -- Dropable                                                           --
   ------------------------------------------------------------------------
   function Dropable (this : in Server_type) return Boolean is
   begin
      return (not this.Is_root) and then this.Failures >= 2;
   end Dropable;

   -- Ready to connect:
   function Is_Ready (This : in Server_Type) return Boolean is
   begin
      return Is_Acceptable (This);
   end Is_Ready;

   -- Establish a connection:
   procedure Connect(this: in out Server_type) is
   begin
      null;
   end Connect;

   -- Disconnect:
   procedure Disconnect(this: in out Server_type) is
   begin
      null;
   end Disconnect;

   -- Dump:
   procedure Serialize
     (Stream: access Streams.Root_stream_type'Class;
      this: in Server_type) is
   begin
      String'Output(Stream, To_string(this.Url));
      Natural'Output(Stream, this.Successes);
      Natural'Output(Stream, this.Failures);
      Calendar.Time'Output(Stream, this.Last_access);
      Boolean'Output(Stream, this.Is_root);
   end Serialize;

   -- Recover:
   function Restore
     (Stream: access Streams.Root_stream_type'Class) return Server_type is
      s: Server_type;
   begin
      s.Url         := To_ustring(String'Input(Stream));
      s.Successes   := Natural'Input(Stream);
      s.Failures    := Natural'Input(Stream);
      s.Last_access := Calendar.Time'Input(Stream);
      s.Is_root     := Boolean'Input (Stream);
      return s;
   end Restore;

   -- Only targetted network at the moment:
   Gnutella2: aliased Network_type(new String'("Gnutella2"));

begin

   Hardcoded_servers:
      declare
         Serv  : Server_access;
         Nodes : Xml.Node_array := Xml.Get_all ("network/GWebCache2/root",
            Globals.Config);
      begin
         for N in Nodes'Range loop
            Serv := Create (
               Xml.Get_attribute (Nodes (N), "url", ""),
               Is_root => true);
            Server.List.Add (Server.Object_access (Serv));
         end loop;
      end Hardcoded_servers;

   Network_list.Insert(Networks, "Gnutella2", Gnutella2'Access);

end Adagio.GWCache2;
