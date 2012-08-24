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
--  $Id: adagio-server.adb,v 1.4 2004/02/05 18:31:22 Jano Exp $
with Adagio.Globals;
with Adagio.Misc;
with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Trace;

with Agpl.Strings;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar;   use Ada;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_deallocation;

with Gnat.Os_lib;    use Gnat;

with Charles.Multimaps.Sorted.Unbounded;

package body Adagio.Server is

   Stat_cached_hubs : constant String := "Servers - Cached";

   use type Server_list.Iterator_type;
   use type Ada.Calendar.Time;

   Minimum_idle_period : constant Duration := 91.0;

   Stat_allocated_servers : constant String := "Network - Allocated servers";

   -- Helper function to compare Object_access:
   function Equal(Left, Right: in Server_slot) return Boolean is
   begin
      return Id(Left.Server.all) = Id(Right.Server.all);
   end Equal;

   package Sorted_list is new Charles.Multimaps.Sorted.Unbounded(
      Rating, Server_slot_access, ">", "=");

   use type Sorted_list.Iterator_type;

   -- Direct access to slots in use:
   function Get_ref is new
      Server_list.Generic_element(Server_slot_access);

   -- Delete a pointed object:
   procedure Free (this: in out Object_access) is
      procedure Free_local is new
         Unchecked_deallocation(Object'Class, Object_access);
   begin
      Free_local (this);
   end Free;

   ------------------------------------------------------------------------
   -- Controlled procedures                                              --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Object) is
      pragma Unreferenced (This);
   begin
--      This.Initialized := true;
--      Trace.Log ("====>");
--      Statistics.Object.Update (
--         Stat_allocated_servers,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (1));
      null;
   end Initialize;

   procedure Adjust     (This : in out Object) is
      pragma Unreferenced (This);
   begin
--      Trace.Log ("=***=");
      null;
   end Adjust;

   procedure Finalize   (This : in out Object) is
      pragma Unreferenced (This);
   begin
--      if This.Initialized then
--         This.Initialized := false;
--         Statistics.Object.Update (
--            Stat_allocated_servers,
--            Statistics.Integers.Increment'Access,
--            Statistics.Integers.Create (-1));
--      end if;
--      Trace.Log ("<====");
      null;
   end Finalize;

   -- List of servers:
   protected body List is

      -- Add
      procedure Add (
         This  : in out Object_access;
         Since : in     Calendar.Time := Calendar.Clock)
      is
      begin
         -- If present, error:
         if Server_list.Is_in (Id(this.all), Servers) then
            Free (This);
            raise Server_already_cached;
         else
            Server_list.Insert(
               Servers, Id(this.all),
               (this, true, Calendar.Clock, Since));
--          Trace.Log("Server.List.Add: " & Id(this.all) & " added to cache");
         end if;
         Dirty:= true;
         Statistics.Object.Set (Stat_cached_hubs,
            Statistics.Integers.Create (Server_list.Length (Servers)));
      end Add;

      -- Remove a server
      -- Freeing it is responsability of the caller!
      procedure Delete(this: in Object_access) is
      begin
         Server_list.delete(Servers, Id(this.all));
         Statistics.Object.Set (Stat_cached_hubs,
            Statistics.Integers.Create (Server_list.Length (Servers)));
      end Delete;

      -- Drop a server by Id. It's freed and deleted. If checked out,
      --    success will be false. Also if not found.
      procedure Drop (This : in String; Success : out Boolean) is
         use Server_list;
         Server : Object_access;
         Pos    : Iterator_type := Find (Servers, This);
      begin
         Success := Pos /= Back (Servers);
         if Success then
            Success := Element (Pos).Available;
            if Success then
               Server := Element (Pos).Server;
               Delete (Server);
               Free (Server);
            end if;
         end if;
      end Drop;

      -- Returns server to available mode.
      procedure Check_in(this: in Object_access) is
         Serv: Server_slot_access renames
            Get_ref(Server_list.Find(Servers, Id(this.all)));
      begin
         if Serv.Available then
            raise Constraint_error;
         else
            Serv.Available:= true;
            Serv.Last_check_in := Calendar.Clock;
         end if;
      end Check_in;

      -- Dump:
      procedure Serialize(Stream: in Stream_access) is
         procedure Check(Item: Server_slot) is
         begin
            Calendar.Time'Output (Stream, Item.Since);
            Object'Class'Output(Stream, Item.Server.all);
         end Check;
         procedure Do_check is new Server_list.Generic_select_elements(Check);
      begin
         -- Dump number:
         Integer'Write(Stream, Server_list.Length(Servers));
         Do_check (Server_list.First(Servers), Server_list.Back(Servers));
      end Serialize;

      -- Restore:
      procedure Restore(Stream: in Stream_access) is
         Num   : Integer;
         Since : Calendar.Time;
      begin
         -- Read number:
         Integer'Read(Stream, Num);
         -- Restore servers:
         for n in 1 .. Num loop
            Since := Calendar.Time'Input (Stream);
            declare
               Serv: Object_access:=
                  new Object'Class'(Object'Class'Input(Stream));
            begin
               Add (Serv, Since);
--               Server_list.Insert(
--                  Servers, Id(Serv.all), (Serv, true, Calendar.Clock));
            exception
               when Server_already_cached =>
                  null;
            end;
         end loop;
         Statistics.Object.Set (Stat_cached_hubs,
            Statistics.Integers.Create (Server_list.Length (Servers)));
      end Restore;

      -- Startup
      procedure Initialize is
      use Streams.Stream_IO;
         Location: String:= S (Globals.Data_folder) & "hostcache.dat";
         F       : File_type;
         T       : Calendar.Time:= Calendar.Clock;
         Success : Boolean;
         use type Calendar.Time;
      begin
         if Os_lib.Is_regular_file (Location & ".tmp") and not
            Os_lib.Is_regular_file (Location) then
            Os_lib.Rename_file (Location & ".tmp", Location, Success);
         end if;
         if Os_lib.Is_regular_file(Location) then
            Open(F, Name => Location, Mode => In_file);
            Restore(Stream_access(Stream(F)));
            Close(F);
         else
            Trace.Log("Server.List.Initialize: " & Location &
               " doesn't exists.");
            Dirty:= false;
            return;
         end if;
         Trace.Log("Server.List.Initialize: " & Location &
           " loaded correctly (" & Duration'Image(Calendar.Clock - T) & "s)");
         Dirty:= false;
      exception
         when E: others =>
            if Is_open(F) then
               Close(F);
               Trace.Log
                 ("Server.List.Initialize (loading): " & Trace.Report(E),
                  Trace.Error);
            end if;
            Server_list.Clear (Servers);
      end Initialize;

      -- Saving (if needed):
      procedure Save is
         use Streams.Stream_IO;
         F: File_type;
         T: Calendar.Time:= Calendar.Clock;
         Success: Boolean;
         Location: String:= S (Globals.Data_folder) & "hostcache.dat";
         use type Calendar.Time;
      begin
         if not Dirty then
            return;
         end if;
         -- Try to delete a temp failed:
         if Os_lib.Is_regular_file(Location & ".tmp") then
            Os_lib.Delete_file(Location & ".tmp", Success);
            if not Success then
               Exceptions.Raise_exception
                 (Storage_error'identity, "Cannot delete temp file.");
            end if;
         end if;
         Create(F, Name => Location & ".tmp");
         Serialize(Stream_access(Stream(F)));
         Close(F);
         -- Delete old hostcache
         if Os_lib.Is_regular_file(Location) then
            Os_lib.Delete_file(Location, Success);
            if not Success then
               Exceptions.Raise_exception
                 (Storage_error'identity, "Cannot delete old hostcache.");
            end if;
         end if;
         Os_lib.Rename_file(Location & ".tmp", Location, Success);
         if not Success then
            Exceptions.Raise_exception
              (Storage_error'identity, "Cannot rename saved temp file.");
         end if;
         Trace.Log("Server.List.Save: " & Location & " saved correctly (" &
            Duration'Image(Calendar.Clock - T) & "s)");
         Dirty:= false;
      exception
         when E: others =>
            if Is_open(F) then
               Close(F);
            end if;
            Trace.Log("Server.List.Save: " & Trace.Report(E), Trace.Error);
      end Save;

      -- Purge servers dropables and not in use:
      procedure Purge is
         Pos: Server_list.Iterator_type:= Server_list.First(Servers);
         Serv: Object_access;
      begin
         while Pos /= Server_list.Back(Servers) loop
            if Server_list.Element(Pos).Available and then
               Dropable (Server_list.Element(Pos).Server.all) and then
               Calendar.Clock - Server_list.Element(Pos).Last_check_in >
                  Minimum_idle_period
               then
               -- Get reference:
               Serv:= Server_list.Element(Pos).Server;
               -- Advance to safe position:
               Pos:= Server_list.Succ (Pos);
               Trace.Log("Server.List.Purge: " & Id(Serv.all) & " purged");
               -- Remove:
               Server_list.Delete(Servers, Id(Serv.all));
               -- Free:
               Free (Serv);
            else
               Pos:= Server_list.Succ (Pos);
            end if;
         end loop;
         Dirty:= true;
         Statistics.Object.Set (Stat_cached_hubs,
            Statistics.Integers.Create (Server_list.Length (Servers)));
      exception
         when E: others =>
            Trace.Log("Server.List.Purge: " & Trace.Report(E), Trace.Error);
      end Purge;

      -- Purge servers from a particular network,
      --    keeping at most some quantity:
      procedure Purge (Net: String; Keep: Natural) is
         Sorted : Sorted_list.Container_type;
         Pos    : Server_list.Iterator_type:= Server_list.First(Servers);
         Poss   : Sorted_list.Iterator_type;
         Pos_num: Natural:= 0;
         SS     : Object_access;
      begin
         if Available(Net) <= Keep then
            return;
         end if;
         -- Do a ordered copy:
         Trace.Log ("Server.Purge (Keep) starting for " & Net);
         while Pos /= Server_list.Back(Servers) loop
            declare
               Serv: Server_slot_access:= Get_ref(Pos);
            begin
               if Serv.Available and then
                  Server.Net(Serv.Server.all) = Net and then
                  Calendar.Clock - Serv.Last_check_in > Minimum_idle_period
               then
                  -- Copy it into targetted servers:
                  Sorted_list.Insert(Sorted, Rate(Serv.Server.all), Serv);
               end if;
            end;
            Pos:= Server_list.Succ (Pos);
         end loop;
         Trace.Log ("Server.Purge (Keep) in progress for " & Net);
         -- Delete any excess servers:
         Poss:= Sorted_list.First(Sorted);
         while Poss /= Sorted_list.Back(Sorted) loop
            Pos_num:= Pos_num + 1;
            if Pos_num > Keep then
               -- Get server
               SS := Sorted_list.Element (Poss).Server;
               -- Delete from original list
               List.Delete(SS);
               -- Free it:
               Free (SS);
            end if;
            Poss:= Sorted_list.Succ (Poss);
         end loop;
         Trace.Log("Server.Purge (Keep) done for network " & Net &
            ", dropped" & Integer'Image (
            Integer'Max (0, Pos_num - Keep)) & " servers.");
         Dirty:= true;
         Statistics.Object.Set (Stat_cached_hubs,
            Statistics.Integers.Create (Server_list.Length (Servers)));
      exception
         when E: others =>
            Trace.Log("Server.Purge(Keep): " & Trace.Report(E), Trace.Error);
      end Purge;

      -- Available servers from a given net:
      function Available(Net: String) return Natural is
         Total: Natural:= 0;
         procedure Check(Item: Server_slot) is
         begin
            if Server.Net(Item.Server.all) = Net then
               Total:= Total + 1;
            end if;
         end Check;
         procedure Do_check is new Server_list.Generic_select_elements(Check);
      begin
         Do_check(Server_list.First(Servers), Server_list.Back(Servers));
         return Total;
      end Available;

      -- Obtain the N best servers for a network:
      function Get_best(Net: String; Quantity: Positive)
         return Object_access_array is
         -- We'll sort the servers by inserting it in a ordered list.
         Null_result: Object_access_array(1 .. 0);
         Sorted: Sorted_list.Container_type;
         procedure Copy(Item: in Server_slot_access) is
         begin
            if Item.Available and then Server.Net(Item.Server.all) = Net
               and then Is_Ready (Item.Server.all)
            then
               Sorted_list.Insert(Sorted, Rate(Item.Server.all), Item);
            end if;
         end Copy;
         Pos: Server_list.Iterator_type:= Server_list.First(Servers);
         use type Server_list.Iterator_type;
      begin
         -- Do sort:
         while Pos /= Server_list.Back(Servers) loop
            Copy(Get_ref(Pos));
            Pos:= Server_list.Succ(Pos);
         end loop;

         -- Nothing found?
         if Sorted_list.Is_empty(Sorted) then
            return Null_result;
         else
            declare
               Pos: Sorted_list.Iterator_type:= Sorted_list.First(Sorted);
               use type Sorted_list.Iterator_type;
               Result: Object_access_array(1 .. Quantity);
               Len: Natural:=
                  Natural'Min(Result'last, Sorted_list.Length(Sorted));
            begin
               for n in 1 .. Len loop
                  -- Check out server:
                  Sorted_list.Element(Pos).Available:= false;
                  Result(n):= Sorted_list.Element(Pos).Server;
                  Pos:= Sorted_list.succ(Pos);
               end loop;
               return Result(1 .. Len);
            exception
               when E: others =>
                  Trace.Log("Server.Get_best: Error building result (1): " &
                     Trace.Report(E), Trace.Error);
                  return Null_result;
            end;
         end if;
      exception
         when E: others =>
            Trace.Log("Server.Get_best: Error building result (2): " &
               Trace.Report(E), Trace.Error);
            return Null_result;
      end Get_best;

      -- Http_report:
      procedure Http_report (
         Data : out Agpl.Http.Server.Sort_handler.Data_set)
      is
         use Agpl.Http.Server.Sort_Handler;
         use Agpl.Http.Server;
         use Ada.Calendar;
         use Server_list;
         I : Iterator_type := First (Servers);
      begin
         while I /= Back (Servers) loop
            declare
               Row  : Data_row;
               Serv : Object_access renames Element (I).Server;
               Uid  : Ustring := U (Id (Serv.all));
               Netw : Ustring := U (Net (Serv.all));
               Rat  : Float   := Rate (Serv.all);
               Drop : Ustring := U (Boolean'Image (Dropable (Serv.all)));
               Used : Ustring := U (Boolean'Image (not Element(I).Available));
               Sinc : Duration:= Clock - Element (I).Since;
               Redy : Ustring := U (Boolean'Image (Is_Ready (Serv.all)));
            begin
               -- Address
               Append (Row, Item => (Uid, Uid));
               -- Network
               Append (Row, Item => (Netw, Netw));
               -- Rating
               Append (Row, Item => (
                  U (Agpl.Strings.To_string (Rat, 2)),
                  Rpad (Rat, 16)));
               -- In use
               Append (Row, Item => (Used, Used));
               -- Dropable
               Append (Row, Item => (Drop, Drop));
               -- Since
               Append (Row, Item => (
                  U (Misc.Image (Sinc)),
                  Rpad (Float (Sinc), 16)));
               -- Ready
               Append (Row, Item => (Redy, Redy));

               Append (Data, Row);
            end;
            I := Succ (I);
         end loop;
      end Http_report;

   end List;

begin
   null;
--   Statistics.Object.Set (
--      Stat_allocated_servers,
--      Statistics.Integers.Create (0));
end Adagio.Server;
