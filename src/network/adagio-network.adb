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
--  $Id: adagio-network.adb,v 1.3 2004/01/21 21:05:37 Jano Exp $

with Adagio.Trace;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Adagio.Network is

   use type Network_list.Iterator_type;

    -- Helper function to compare Object_access:
   function Equal(Left, Right: in Network_slot) return boolean is
   begin
      return Id(Left.Network.all) = Id(Right.Network.all);
   end Equal;

   ----------
   -- List --
   ----------

   -- Function to get a reference to the real item:
   function Elem is new Network_list.Generic_element(Network_slot_access);

   protected body List is

      -- Add a network, initially checked-in
      procedure Add(this: Object_access) is
      begin
         Network_list.Insert(Networks, Id(this.all), (this, true));
         Trace.Log("Network.List.Add: Added network: " & Id(this.all));
      end Add;

      -- Prepares the searcher after the network is connected:
      procedure Prepare(this: Object_access) is
         Searcher : Searches.Handler.Object_access;
      begin
         Searcher := Get_Search_Handler (This.all);
         if Searcher /= null then
            Search_List.Insert (Searchers, Id (This.all), Searcher);
            Trace.Log("Network.List.Add: Added searcher for network: " & 
            Id(this.all));
         end if;
      end Prepare;

      -- Search for a network:
      function Get(Id: in String) return Object_access is
      begin
         if Network_list.Is_in (Id, Networks) then
            return Elem (Network_list.Find (Networks, Id)).Network;
         else
            Trace.Log
              ("Network.List.Get: Network missing: " & Id, Trace.Warning);
            return null;
         end if;
      exception
         when Constraint_error =>
            raise;
         when others =>
            Trace.Log
              ("Network.List.Get: Network missing: " & Id, Trace.Warning);
            return null;
      end Get;

      -- Disconnect all:
      procedure Disconnect_all is
         Pos: Network_list.Iterator_type:= Network_list.First(Networks);
      begin
         while Pos /= Network_list.Back(Networks) loop
            declare
               Net: Object_access renames Network_list.Element(Pos).Network;
            begin
               if Status(Net.all) /= Disconnected then
                  Disconnect(Net.all);
                  Trace.Log("Disconnected from " & Id(Net.all));
               end if;
            exception
               when E: others =>
                  Trace.Log("Network.List.Disconnect_all: " & Id(Net.all) &
                     ": " & Trace.Report(E), Trace.Error);
            end;
            Pos:= Network_list.Succ (Pos);
         end loop;
      end Disconnect_all;

      -- Search related calls:
      procedure Create_Search (This : in Searches.Search_Id) is
         use Search_List;
         I : Iterator_Type := First (Searchers);
      begin
         while I /= Back (Searchers) loop
            Searches.Handler.Create_Search (Element (I), This);
            I := Succ (I);
         end loop;
      end Create_Search;

      procedure Delete_Search (This : in Searches.Search_Id) is
         use Search_List;
         I : Iterator_Type := First (Searchers);
      begin
         while I /= Back (Searchers) loop
            Searches.Handler.Delete_Search (Element (I), This);
            I := Succ (I);
         end loop;
      end Delete_Search;

      procedure Set_Search_Paused (
         This : in Searches.Search_Id; Paused : in Boolean := true)
      is
         use Search_List;
         I : Iterator_Type := First (Searchers);
      begin
         while I /= Back (Searchers) loop
            Searches.Handler.Set_Paused (Element (I), This, Paused);
            I := Succ (I);
         end loop;
      end Set_Search_Paused;

      procedure Set_Search_Priority (
         This : in Searches.Search_id; Priority : Searches.Priorities)
      is
         use Search_List;
         I : Iterator_Type := First (Searchers);
      begin
         while I /= Back (Searchers) loop
            Searches.Handler.Set_Priority (Element (I), This, Priority);
            I := Succ (I);
         end loop;
      end Set_Search_Priority;

      function Get_Custom_Info (This : in Searches.Search_Id) return String is
         use Search_List;
         I      : Iterator_Type := First (Searchers);
         Result : Ustring;
      begin
         while I /= Back (Searchers) loop
            Asu.Append (Result, Searches.Handler.Get_Custom_Info (Element (I), This));
            I := Succ (I);
            if I /= Back (Searchers) then
               Asu.Append (Result, "; ");
            end if;
         end loop;
         return S (Result);
      end Get_Custom_Info;


   end List;

end Adagio.Network;
