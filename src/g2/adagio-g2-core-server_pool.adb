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
--  $Id: adagio-g2-core-server_pool.adb,v 1.3 2004/01/21 21:05:26 Jano Exp $

separate (Adagio.G2.Core)
   protected body Server_pool is

      -- Add a server:
      -- Initially connecting!
      procedure Add (this : in Server_access) is
      begin
         if this = null then
            raise Constraint_error;
         end if;
         for N in Slots'Range loop
            if not Slots (N).In_use then
               Slots (N).In_use      := true;
               Slots (N).Server      := this;
               Slots (N).Status      := Connecting;
               Slots (N).Slot.Index  := N;

               -- Server reference
               this.Slot := Slots (N).Slot'Unrestricted_access;

               -- Done.
               return;
            end if;
         end loop;

         raise Storage_exhausted;
      end Add;

      -- Get a server by Id.
      procedure Get (
         Id     : in  String;
         Status : in  Server_status;
         Server : out Server_access) is
	pragma Unreferenced (Status);
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then Core.Id (Slots (N).Server.all) = id
            then
               Server := Slots (N).Server;
               return;
            end if;
         end loop;
         Server := null;
      end Get;
      procedure Get (
         Id     : in  UString;
         Status : in  Server_status;
         Server : out Server_access) is
      begin
         Get (S (Id), Status, Server);
      end Get;

      -- Get a connected server by Id or null if not found:
      function Get (Id : in UString; Status : in Server_status)
         return Server_access is
	pragma Unreferenced (Status);
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then Core.Id (Slots (N).Server.all) = id
            then
               return Slots (N).Server;
            end if;
         end loop;

         return null;
      end Get;

      -- Get all servers in a given status as array
      function Get_array (Status : in Server_status) return Server_array is
         Result : Server_array (1 .. Status_count (Status));
         Pos : Natural := Result'First;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then
               Slots (N).Status = Status
            then
               Result (Pos) := Slots (N).Server;
               Pos := Pos + 1;
            end if;
         end loop;
         return Result;
      end;

      -- Get all queues of connected servers but one (optional)
      function Get_queues (But : in Server_access := null) return
         Packet.Queue.Object_array is
         Result : Packet.Queue.Object_array (1 .. Status_count (Connected));
         Pos : Natural := Result'First;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then
               Slots (N).Status = Connected and then
               Slots (N).Server /= But
            then
               Result (Pos) := Slots (N).Slot.Outbound'Unrestricted_Access;
               Pos := Pos + 1;
            end if;
         end loop;
         return Result (1 .. Pos - 1);
      end Get_queues;

      -- As previous but with addresses too:
      function Get_Queues_And_Addresses (But : in Server_Access := null)
         return Packet.Queue.Address_Queue_Array
      is
         Result : Packet.Queue.Address_Queue_Array (1 .. Status_count (Connected));
         Pos : Natural := Result'First;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then
               Slots (N).Status = Connected and then
               Slots (N).Server /= But
            then
               Result (Pos).Queue   := Slots (N).Slot.Outbound'Unrestricted_Access;
               Result (Pos).Address := Slots (N).Server.Address & ":" & Misc.To_String (Slots (N).Server.Port);
               Pos := Pos + 1;
            end if;
         end loop;
         return Result (1 .. Pos - 1);
      end Get_queues_And_Addresses;

      -- Get next server:
      -- Will return null if no more:
      -- will check-in the current server.
      -- Will skip checked-out servers.
      procedure Get_next (Server : in out Server_access) is
      begin
         if Server = null or else
            Server.Slot = null or else
            Server.Slot.Index not in Try_range
         then
            Get_first (Server);
            return;
         end if;
         for N in Server.Slot.Index + 1 .. Try_range'Last loop
            if Slots (N).In_use then
               Server := Slots (N).Server;
               return;
            end if;
         end loop;
         Server := null;
      end Get_next;

      -- Get first server:
      -- Will return null if no more:
      procedure Get_first (Server : out Server_access) is
      begin
         for N in Try_range'Range loop
            if Slots (N).In_use then
               Server := Slots (N).Server;
               return;
            end if;
         end loop;
         Server := null;
      end Get_first;

      -- Get newest connected server:
      procedure Get_newest (Server : out Server_access) is
         Newest         : Natural := 0;
         Connect_time   : Calendar.Time := Calendar.Time_of (1976, 9, 6);
      begin
         for N in Try_range'Range loop
            if Slots (N).In_use and then Slots (N).Status = Connected then
               if Slots (N).Server.Connection_start > Connect_time then
                  Newest := N;
                  Connect_time := Slots (N).Server.Connection_start;
               end if;
            end if;
         end loop;
         if Newest = 0 then
            Server := null;
         else
            Server := Slots (Newest).Server;
         end if;
      end Get_newest;

      -- Count valid servers in any active (not disconnected) status:
      function Active_count return Natural is
         Result : Natural := 0;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then Slots (N).Status /= Disconnected then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end;

      -- Count used slots:
      function Count return Natural is
         Result : Natural := 0;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count;

      -- Count valid servers in a given status:
      function Status_count (Status : in Server_status) return Natural is
         Result : Natural := 0;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then Slots (N).Status = Status then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Status_count;

      -- Remove a server.
      procedure Remove (Server : in out Server_access) is
         P : Natural renames Server.Slot.Index;
      begin
         if not Slots (P).In_use then
            raise Constraint_error;
         end if;
         Slots (P).In_use := false;
         Server.Slot      := null;
         Server           := null;
      end Remove;

      -- Disconnect all servers, without pity, regardeless of checking!
      procedure Disconnect_all is
      begin
         for N in Slots'Range loop
            if Slots (N).In_use then
               Socket.Close (Slots (N).Slot.Socket);
            end if;
         end loop;
      end Disconnect_all;

       -- Read status:
      function Status (Server : in Server_access) return Server_status is
      begin
         return Slots (Server.Slot.Index).Status;
      exception
         when others =>
            return Disconnected;
      end Status;

      -- Set status:
      procedure Set_status (
         Server : in Server_access;
         Status : in Server_status) is
      begin
         Slots (Server.Slot.Index).Status := Status;
      exception
         when E : others =>
            Trace.Log ("G2.Core.Server_pool.Set_status: " & Trace.Report (E),
               Trace.Debug);
      end Set_status;

      -- Get an array of readable addresses of connected neighbours:
      function Address_list return Ustring_array is
         Result : Ustring_array (1 .. Status_count (Connected));
         Pos    : Natural := 1;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use and then Slots (N).Status = Connected then
               Result (Pos) :=
                  Slots (N).Server.Address & ":" &
                  Misc.To_string (Slots (N).Server.Port);
               Pos := Pos + 1;
            end if;
         end loop;
         return Result;
      end Address_list;

      -- Get a report of all servers:
      function Report return Report_array is
         Result : Report_array (1 .. Count);
         Pos    : Positive := 1;
      begin
         for N in Slots'Range loop
            if Slots (N).In_use then
               Result (Pos).Uptime := Natural (
                  Calendar.Clock - Slots (N).Server.Connection_start);
               Result (Pos).Status := U (
                  Server_status'Image (Slots (N).Status));
               Result (Pos).Id     := U (Describe (Slots (N).Server.all));
               Pos := Pos + 1;
            end if;
         end loop;

         return Result;
      end Report;

   end Server_pool;
