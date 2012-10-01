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
--  $Id: adagio-gui-graphs.adb,v 1.4 2004/03/29 19:13:31 Jano Exp $

with Aenea.Debug;
with Aenea.Trace;

with Adagio.BW_usage;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Os.Memory;

with Agpl.Chronos;
with Agpl.Constants;
with Agpl.Graph;
with Agpl.Types.UStrings; use Agpl.Types.UStrings;

with Ada.Calendar;
use  Ada;

package body Aenea.Gui.Graphs is

   Width  : constant Natural := 600;
   Height : constant Natural := 70;

   ------------------------------------------------------------------------
   -- Grapher                                                            --
   ------------------------------------------------------------------------
   task Grapher is
      entry Start;
      entry Clear;
      entry Get_graph (
         Graph : in Available_graphs; Image : out Agpl.Bmp.Object);
      entry Set_time_range (New_range : in Duration);
   end Grapher;

   task body Grapher is
      Next       : Calendar.Time := Calendar.Clock;
      Period     : Duration := Adagio.Globals.Options.Gui_GraphPeriod;
      Wait_slots : Natural :=
         Natural (Adagio.Globals.Options.Gui_GraphPeriod) / Width;
      Current_waited : Natural := 0;

      -- Graphs
      BW_udp : Agpl.Graph.Object (Series => 4, Samples => Width);
      Memory : Agpl.Graph.Object (Series => 1, Samples => Width);
      -- 1, 2 => out/in session
      -- 3, 4 => out/in 5 min
      use type Calendar.Time;
   begin
      -- Prepare graphs:
      -- Memory
      Agpl.Graph.Set_colors (
         Memory,
         Bgcolor => Agpl.Constants.Gainsboro,
         Fgcolor => (1 => Agpl.Constants.Navy));
      Agpl.Graph.Set_scale_min (Memory, 0.0);
      Agpl.Graph.Set_YAxis (
         Memory,
         Float (8 * 2**20), Color => Agpl.Constants.White, Repeat => true);
      -- BW_udp
      Agpl.Graph.Set_colors (
         BW_udp,
         Bgcolor => Agpl.Constants.Gainsboro,
         Fgcolor => (
            1 => Agpl.Constants.Soft_red,
            2 => Agpl.Constants.Soft_green,
            3 => Agpl.Constants.Red,
            4 => Agpl.Constants.Green));
      Agpl.Graph.Set_scale_min (BW_udp, 0.0);
      Agpl.Graph.Set_YAxis (BW_udp,
         8.0 * 1024.0, Color => Agpl.Constants.White, Repeat => true);
      Agpl.Graph.Set_YAxis (BW_udp,
         Float (Adagio.Globals.Options.G2_UdpBandwidthIn),
         Color => Agpl.Constants.Gray, Repeat => false);
      Agpl.Graph.Set_YAxis (BW_udp,
         Float (Adagio.Globals.Options.G2_UdpBandwidthOut),
         Color => Agpl.Constants.Gray, Repeat => false);

      -- Wait for launch:
      select
         accept Start;
      or
         terminate;
      end select;
      Trace.Log ("Grapher started, 1 update every" & Wait_slots'Img &
         " seconds.", Trace.Informative);
      while not Adagio.Globals.Requested_exit loop
         declare
            In1, In5, InS, Out1, Out5, OutS : Float;
            Watchdog : Agpl.Chronos.Object;
         begin
            -- Rendez-vouses:
            loop
               if Watchdog.Elapsed > 3.0 then
                  Debug.Overtimed_Task.Set (+"Grapher");
                  Debug.Overtime.Set (Watchdog.Elapsed);
               end if;
               Watchdog.Reset;
               select
                  accept Get_graph (
                     Graph : in Available_graphs; Image : out Agpl.Bmp.Object)
                  do
                     case Graph is
                        when BW_usage_udp =>
                           Image := Agpl.Graph.Get_bmp (BW_udp, Height * 2);
                        when Memory_usage =>
                           Image := Agpl.Graph.Get_bmp (Memory, Height);
                        when others =>
                           Agpl.Bmp.Create (Image, 1, 1);
                     end case;
                  end Get_graph;
               or
                  accept Set_time_range (New_range : in Duration) do
                     Period     := New_range;
                     Wait_slots := Natural (New_range) / Width;
                  end Set_time_range;
               or
                  accept Clear do
                     Agpl.Graph.Reset (BW_udp);
                     Agpl.Graph.Reset (Memory);
                  end Clear;
               or
                  delay until Next;
                  Next := Next + 1.0;
                  exit;
               end select;
            end loop;

            -- Add samples if necessary:
            Current_waited := Current_waited + 1;
            if Current_waited >= Wait_slots then
               Current_waited := 0;
               -- Memory usage
               Agpl.Graph.Add_sample
                 (Memory,
                  Serie  => 1,
                  Sample => Float (Adagio.Os.Memory.Heap_usage));
               -- Bandwidths UDP
               begin
                  Adagio.BW_usage.Get_in (
                     In1, In5, InS, Adagio.BW_usage.UDP);
                  Adagio.BW_usage.Get_out (
                     Out1, Out5, OutS, Adagio.BW_usage.UDP);
               exception
                  when Tasking_Error =>
                     if Adagio.Globals.Requested_exit then
                        exit;
                     end if;
               end;
               Agpl.Graph.Add_sample (BW_udp, Serie => 1, Sample => OutS);
               Agpl.Graph.Add_sample (BW_udp, Serie => 2, Sample => InS);
               Agpl.Graph.Add_sample (BW_udp, Serie => 3, Sample => Out5);
               Agpl.Graph.Add_sample (BW_udp, Serie => 4, Sample => In5);
            end if;
         exception
            when E : others =>
               Trace.Log ("Grapher: " & Trace.Report (E), Trace.Error);
         end;
      end loop;
   end Grapher;

   ------------------------------------------------------------------------
   -- Get_graph                                                          --
   ------------------------------------------------------------------------
   function Get_graph (Graph : in Available_graphs) return Agpl.Bmp.Object is
      Result : Agpl.Bmp.Object;
   begin
      select
         Grapher.Get_graph (Graph, Result);
         return Result;
      or
         delay 10.0;
         Agpl.Bmp.Create (Result, Width => 1, Height => 1);
         return Result;
      end select;
   end Get_graph;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   procedure Start is
   begin
      Grapher.Start;
   end Start;

   ------------------------------------------------------------------------
   -- Set_time_range                                                     --
   ------------------------------------------------------------------------
   procedure Set_time_range (Time_range : in Duration) is
   begin
      select
         Grapher.Set_time_range (Duration'Max (Duration (Width), Time_range));
      or
         delay 2.0;
      end select;
   end Set_time_range;

   ------------------------------------------------------------------------
   -- Clear                                                              --
   ------------------------------------------------------------------------
   procedure Clear is
   begin
      select
         Grapher.Clear;
      or
         delay 2.0;
      end select;
   end Clear;

end Aenea.Gui.Graphs;
