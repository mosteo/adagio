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
--  $Id: adagio-upload-client_data.adb,v 1.3 2004/01/21 21:05:49 Jano Exp $

--  Data to be remembered about all clients (credit system).

with Adagio.Globals;
with Adagio.Os;
with Adagio.Statistics;
with Adagio.Statistics.Integers;
with Adagio.Trace;
--with Adagio.Xml;

with Gnat.Os_lib; use Gnat;

with Ada.Streams.Stream_io; use Ada.Streams.Stream_io;

package body Adagio.Upload.Client_data is

   use type Types.File_Size;
   use type Agpl.Types.Ustrings.Ustring;

   Stat_clients : constant String := "Uploads - Known clients";

   use Data_list;

   Location : Ustring := Globals.Data_folder;

   protected body List is
      ----------
      -- Init --
      ----------
      procedure Init is
         Loc : String := S (Location);
         F   : File_type;
         Str : Stream_access;
         C   : Object_access;
      begin
         Statistics.Object.Set (Stat_clients,
            Statistics.Integers.Create (0));

         if Loc (Loc'Last) /= Os.Folder_separator then
            Location := Location & U (String'(1 => Os.Folder_separator));
         end if;

         Location := Location & U ("clients.dat");

         if Os_lib.Is_regular_file (S (Location)) then
            Open (F, Mode => In_file, Name => S (Location));
            Str := Stream (F);
            while not End_of_file (F) loop
               C := new Object'(Object'Input (Str));
               Insert (Data, S (C.Client_id), C);
            end loop;
            Close (F);
         end if;

         Statistics.Object.Set (Stat_clients,
            Statistics.Integers.Create (Length (Data)));

      exception
         when E : others =>
            Trace.Log ("Upload.Client_data.Init: Cannot load from " & 
               S (Location) & ": " & Trace.Report (E), Trace.Error);
            if Is_open (F) then
               Close (F);
            end if;
      end Init;

      ----------
      -- Save --
      ----------
      procedure Save is
         Pos : Iterator_type := First (Data);
         F   : File_type;
         Str : Stream_access;
      begin
         Create (F, Mode => Out_file, Name => S (Location));
         Str := Stream (F);
         while Pos /= Back (Data) loop
            Trace.Log ("Upload.Client_data.Save: Client " &
               S (Element (Pos).Client_id) & " sent" & 
               Element (Pos).Sent'Img, Trace.Never);
            Object'Output (Str, Element (Pos).all);
            Pos := Succ (Pos);
         end loop;
         Close (F);
         Trace.Log ("Upload.Client_data.Save: Client data saved correctly.");
      exception
         when E : others =>
            Trace.Log ("Upload.Client_data.Shutdown: " & Trace.Report (E),
               Trace.Error);
            if Is_open (F) then
               Close (F);
            end if;
      end Save;

      --------------
      -- Add_sent --
      --------------
      procedure Add_sent (Client_id : in String; Amount : in File_size) is
         C : Object_access;
      begin
         if not Is_in (Client_id, Data) then
            C := new Object'(
               Client_id    => U (Client_id), 
               Sent         => 0,
               Last_request => Past_aeons);
            Insert (Data, Client_id, C);
         else
            C := Element (Find (Data, Client_id));
         end if;

         begin
            C.Sent := C.Sent + Amount;
         exception
            when Constraint_error =>
               C.Sent := File_size'Last;
         end;

         Statistics.Object.Set (Stat_clients,
            Statistics.Integers.Create (Length (Data)));
      end Add_sent;

      --------------
      -- Get_sent --
      --------------
      function Get_sent (Client_id : in String) return File_size is
         I : Iterator_type := Find (Data, Client_id);
      begin
         if I /= Back (Data) then
            return Element (I).Sent;
         else
            return 0;
         end if;
      end Get_sent;

      ----------------------
      -- Set_last_request --
      ----------------------
      procedure Set_last_request (
         Client_id : in String; 
         Been      : in Calendar.Time := Calendar.Clock) is
         C : Object_access;
      begin
         if not Is_in (Client_id, Data) then
            C := new Object'(
               Client_id    => U (Client_id), 
               Sent         => 0,
               Last_request => Been);
            Insert (Data, Client_id, C);
         else
            C := Element (Find (Data, Client_id));
         end if;

         C.Last_request := Been;

         Statistics.Object.Set (Stat_clients,
            Statistics.Integers.Create (Length (Data)));
      end Set_last_request;

      ----------------------
      -- Get_last_request --
      ----------------------
      -- Will return Past_aeons if unknown
      function Get_last_request (Client_id : in String) return Calendar.Time
      is
         I : Iterator_type := Find (Data, Client_id);
      begin
         if I /= Back (Data) then
            return Element (I).Last_request;
         else
            return Past_aeons;
         end if;
      end Get_last_request;

   end List;

end Adagio.Upload.Client_data;









