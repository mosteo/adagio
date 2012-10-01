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
--  $Id: aenea-hub.adb,v 1.10 2004/03/22 07:14:55 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Gui.Events;

with Agpl.Http.Server.Sort_Handler;

package body Aenea.Hub is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This    : out Object; 
      Address : in String; 
      Status  : in Status_Type)
   is
   begin
      Gui.Events.Add ("[+] Create - " & Address, 2);
      This.Address    := U (Address);
      This.Status     := Status;
      This.Leaves     := 0;
      This.Max_leaves := 0;
      This.Pings      := Globals.Options.Walk_Pings; -- To start with a CRAWLR
      This.Failures   := 0;

      --
      This.Stat_First_seen := Calendar.Clock;
      This.Stat_Last_seen  := Calendar.Clock;

      -- Location:
      This.Country_code := Agpl.Geoip.Country_code_from_addr (Address);
      This.Country_name := U (Agpl.Geoip.Country_name_from_addr (Address));
   end Create;

   ------------------------------------------------------------------------
   -- Id                                                                 --
   ------------------------------------------------------------------------
   -- Returns a string uniquely identifying the hub (ip:port)
   function Id (This : in Object) return String is
   begin
      return S (This.Address);
   end Id;

   ------------------------------------------------------------------------
   -- Sequence                                                           --
   ------------------------------------------------------------------------
   -- The sequence for event synchrony.
   function Sequence (This : in Object) return Types.Sequences is
   begin
      return This.Sequence;
   end Sequence;

   ------------------------------------------------------------------------
   -- Status                                                             --
   ------------------------------------------------------------------------
   procedure Set_Status (This : in out Object; Status : in Status_Type) is
   begin
      This.Status := Status;
   end Set_Status;

   function  Get_Status (This : in Object) return Status_Type is
   begin
      return This.Status;
   end Get_Status;

   ------------------------------------------------------------------------
   -- Stats                                                              --
   ------------------------------------------------------------------------
   function Stats (This : in Object) return String is
   begin
      return 
         This.Status'Img &
         This.Stat_queries'Img & 
         This.Stat_pings'Img & 
         This.Stat_answers'Img;
   end Stats;

   ------------------------------------------------------------------------
   -- Set_leaves                                                         --
   ------------------------------------------------------------------------
   procedure Set_leaves (This : in out Object; Leaves : Natural) is
   begin
      This.Leaves := Leaves;
   end Set_leaves;

   ------------------------------------------------------------------------
   -- Leaves                                                             --
   ------------------------------------------------------------------------
   function Leaves (This : in Object) return Natural is
   begin
      return This.Leaves;
   end Leaves;

   ------------------------------------------------------------------------
   -- Set_max_leaves                                                     --
   ------------------------------------------------------------------------
   procedure Set_max_leaves (This : in out Object; Leaves : Natural) is
   begin
      This.Max_leaves := Leaves;
   end Set_max_leaves;

   ------------------------------------------------------------------------
   -- Max_leaves                                                         --
   ------------------------------------------------------------------------
   function Max_leaves (This : in Object) return Natural is
   begin
      return This.Max_leaves;
   end Max_leaves;

   ------------------------------------------------------------------------
   -- Add_answer                                                         --
   ------------------------------------------------------------------------
   -- Adds 1 to the answers statistic
   procedure Add_answer (This : in out Object) is
   begin
      This.Stat_answers          := This.Stat_answers + 1;
      This.Stat_answers_in_a_row := This.Stat_answers_in_a_row + 1;
      This.Stat_Last_seen        := Calendar.Clock;
   end Add_answer;

   ------------------------------------------------------------------------
   -- Failures                                                           --
   ------------------------------------------------------------------------
   function Failures (This : in Object) return Natural is
   begin
      return This.Failures;
   end Failures;

   ------------------------------------------------------------------------
   -- Set_failures                                                       --
   ------------------------------------------------------------------------
   procedure Set_failures (This : in out Object; Failures : in Natural := 0)
   is
   begin
      This.Failures := Failures;
   end Set_failures;

   ------------------------------------------------------------------------
   -- Relocate                                                           --
   ------------------------------------------------------------------------
   -- Re-checks the location, just in case the IP range has been moved.
   procedure Relocate (This : in out Object) is
   begin
      This.Country_code := 
         Agpl.Geoip.Country_code_from_addr (Id (This));
      This.Country_name := 
         U (Agpl.Geoip.Country_name_from_addr (Id (This)));
   end Relocate;

   ------------------------------------------------------------------------
   -- Get_country_code                                                   --
   ------------------------------------------------------------------------
   function Get_country_code (This : in Object) 
      return Agpl.Geoip.Country_code is
   begin
      return This.Country_code;
   end Get_country_code;

   ------------------------------------------------------------------------
   -- Vendor data                                                        --
   ------------------------------------------------------------------------
   procedure Set_Vendor (This : in out Object; Vendor : in String) is
   begin
      This.Vendor := U (Vendor);
   end Set_Vendor;

   function Get_Vendor (This : in Object) return String is
   begin
      return S (This.Vendor);
   end Get_Vendor;

   procedure Set_Version (This : in out Object; Version : in String) is
   begin
      This.Version := U (Version);
   end Set_Version;

   function Get_Version (This : in Object) return String is
   begin
      return S (This.Version);
   end Get_Version;

   ------------------------------------------------------------------------
   -- Get_Nick                                                           --
   ------------------------------------------------------------------------
   function Get_Nick (This : in Object) return String is
   begin
      return S (This.Nick);
   end Get_Nick;

   ------------------------------------------------------------------------
   -- Multiplier                                                         --
   ------------------------------------------------------------------------
   function Compute_Multiplier (Vendor : in String) return Float is
   begin
      return 0.5;

      --  This was wrong. Very wrong.
      if Vendor = Vendor_GDNA then
         return 1.0;
      elsif Vendor = Vendor_RAZA then
         return 0.5;
      else
         return 0.5;
      end if;
   end Compute_Multiplier;

   ------------------------------------------------------------------------
   -- Is_Alive                                                           --
   ------------------------------------------------------------------------
   -- Hub_Checking or Hub_Waiting
   function Is_Alive (This : in Object) return Boolean is
   begin
      return This.Status = Hub_Waiting or else This.Status = Hub_Checking;
   end Is_Alive;

   ------------------------------------------------------------------------
   -- Stats                                                              --
   ------------------------------------------------------------------------
   function Get_Last_Seen (This : in Object) return Ada.Calendar.Time is
   begin
      return This.Stat_Last_Seen;
   end Get_Last_Seen;

   function  Get_First_Seen (This : in Object) return Ada.Calendar.Time is
   begin
      return This.Stat_First_Seen;
   end Get_First_Seen;

   function Uptime_Keys (Uptime : in Natural) return String is
      use Agpl.Http.Server.Sort_Handler;
   begin
      return S (Rpad (Uptime, 3)) & "-" &
         S (Rpad (Uptime + 1, 3)) & " h";
   end Uptime_Keys;

   procedure Add_To_Uptime (This : in Object; Amount : in Integer := 1) is
   begin
      By_Uptime.Sum_Key (Uptime_Keys (This.Uptime), Amount);
   end Add_To_Uptime;

   procedure Update_Uptime (This : in out Object) is
      use Ada.Calendar;
      New_Uptime : constant Natural := Natural (
         Float'Floor (Float (
            (Clock - This.Stat_First_Seen) / Duration'(1.0 * 60.0 * 60.0))));
   begin
      if New_Uptime /= This.Uptime then
         By_Uptime.Sum_Key (Uptime_Keys (This.Uptime), -1);
         This.Uptime := New_Uptime;
         By_Uptime.Sum_Key (Uptime_Keys (This.Uptime), 1);
      end if;
   end Update_Uptime;

   function Leaves_Key (Leaves : in Natural) return String is
      use Agpl.Http.Server.Sort_Handler;
      Idx : Natural := Leaves / 25;
   begin
      return S (Rpad (Idx * 25, 4)) & "-" &
             S (Rpad ((Idx + 1) * 25 - 1, 4));
   end Leaves_Key;

   procedure Add_To_By_Leaves (This : in out Object; Amount : in Integer) is
   begin
      By_Leaves.Sum_Key (Leaves_Key (Amount), 1);
      This.Stat_Leaves := Amount;
   end Add_To_By_Leaves;

   procedure Del_To_By_Leaves (This : in out Object) is
   begin
      By_Leaves.Sum_Key (Leaves_Key (This.Stat_Leaves), -1);
   end Del_To_By_Leaves;

   procedure Update_By_Leaves (This : in out Object; Amount : in Integer) is
   begin
      if Amount /= This.Stat_Leaves then
         Del_To_By_Leaves (This);
         Add_To_By_Leaves (This, Amount);
      end if;
   end Update_By_Leaves;

end Aenea.Hub;
