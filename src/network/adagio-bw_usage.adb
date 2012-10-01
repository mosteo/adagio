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
--  $Id: adagio-bw_usage.adb,v 1.2 2004/03/29 19:13:32 Jano Exp $

with Adagio.Globals;
with Adagio.Trace;

with Ada.Calendar;
use  Ada;

package body Adagio.Bw_usage is

   ------------------------------------------------------------------------
   -- Div                                                                --
   ------------------------------------------------------------------------
   function Div (L : in Float; R : in Integer) return Float is
   begin
      return L / Float (R);
   end Div;

   ------------------------------------------------------------------------
   -- Adder                                                              --
   ------------------------------------------------------------------------
   protected Adder is
      procedure Add_out     (Size : in Natural);
      procedure Add_in      (Size : in Natural);
      procedure Add_out_udp (Size : in Natural);
      procedure Add_in_udp  (Size : in Natural);
      procedure Fetch_reset (Total_in : out Natural; Total_out : out Natural);
      procedure Fetch_reset_udp (
         Total_in : out Natural; Total_out : out Natural);
   private
      Acum_in  : Natural := 0;
      Acum_out : Natural := 0;
      Acum_in_udp  : Natural := 0;
      Acum_out_udp : Natural := 0;
   end Adder;
   protected body Adder is
      procedure Add_out     (Size : in Natural) is
      begin
         Acum_out := Acum_out + Size;
      end Add_out;
      procedure Add_in      (Size : in Natural) is
      begin
         Acum_in := Acum_in + Size;
      end Add_in;
      procedure Add_out_udp (Size : in Natural) is
      begin
         Acum_out_udp := Acum_out_udp + Size;
      end Add_out_udp;
      procedure Add_in_udp  (Size : in Natural) is
      begin
         Acum_in_udp := Acum_in_udp + Size;
      end Add_in_udp;
      procedure Fetch_reset (Total_in : out Natural; Total_out : out Natural)
      is
      begin
         Total_in  := Acum_in;
         Acum_in   := 0;
         Total_out := Acum_out;
         Acum_out  := 0;
      end Fetch_reset;
      procedure Fetch_reset_udp (
         Total_in : out Natural; Total_out : out Natural)
      is
      begin
         Total_in     := Acum_in_udp;
         Acum_in_udp  := 0;
         Total_out    := Acum_out_udp;
         Acum_out_udp := 0;
      end Fetch_reset_udp;
   end Adder;

   ------------------------------------------------------------------------
   -- Add_out                                                            --
   ------------------------------------------------------------------------
   procedure Add_out (Size : in Natural; Kind : in Kinds) is
   begin
      if Kind = Tcp then
         Adder.Add_out (Size);
      else
         Adder.Add_out_udp (Size);
      end if;
   end Add_out;

   ------------------------------------------------------------------------
   -- Add_in                                                             --
   ------------------------------------------------------------------------
   procedure Add_in  (Size : in Natural; Kind : in Kinds) is
   begin
      if Kind = Tcp then
         Adder.Add_in (Size);
      else
         Adder.Add_in_udp (Size);
      end if;
   end Add_in;

   ------------------------------------------------------------------------
   -- Updater                                                            --
   ------------------------------------------------------------------------
   task Updater is 
      entry Get_in  (I1Min, I5Min, ISMin : out Float; Kind : in Kinds);
      entry Get_out (O1Min, O5Min, OSMin : out Float; Kind : in Kinds);
   end Updater;
   task body Updater is
      use Calendar;
      Next : Time := Clock;
   begin
      while not Globals.Requested_exit loop
         declare
            Q_in, Q_out : Natural;
         begin
            loop
               select
                  accept Get_in (
                     I1Min, I5Min, ISMin : out Float; Kind : in Kinds) 
                  do
                     if Kind = Tcp then
                        I1Min := Averages.Average (In_1min);
                        I5Min := Averages.Average (In_5min);
                        ISMin := In_ses / Samples;
                     else
                        I1Min := Averages.Average (In_1min_u);
                        I5Min := Averages.Average (In_5min_u);
                        ISMin := In_ses_u / Samples;
                     end if;
                  end Get_in;
               or
                  accept Get_out (
                     O1Min, O5Min, OSMin : out Float; Kind : in Kinds) 
                  do
                     if Kind = Tcp then
                        O1Min := Averages.Average (Out_1min);
                        O5Min := Averages.Average (Out_5min);
                        OSMin := Out_ses / Samples;
                     else
                        O1Min := Averages.Average (Out_1min_u);
                        O5Min := Averages.Average (Out_5min_u);
                        OSMin := Out_ses_u / Samples;
                     end if;
                  end Get_out;
               or
                  delay until Next;
                  exit;
               end select;
            end loop;
            Next := Next + 1.0;
            Adder.Fetch_reset (Total_in => Q_in, Total_out => Q_out);
            Averages.Push (In_1min, Float (Q_in));
            Averages.Push (In_5min, Float (Q_in));
            Averages.Push (Out_1min, Float (Q_out));
            Averages.Push (Out_5min, Float (Q_out));
            In_ses  := In_ses + Float (Q_in);
            Out_ses := Out_ses + Float (Q_out);

            Adder.Fetch_reset_udp (Total_in => Q_in, Total_out => Q_out);
            Averages.Push (In_1min_u, Float (Q_in));
            Averages.Push (In_5min_u, Float (Q_in));
            Averages.Push (Out_1min_u, Float (Q_out));
            Averages.Push (Out_5min_u, Float (Q_out));
            In_ses_u  := In_ses_u + Float (Q_in);
            Out_ses_u := Out_ses_u + Float (Q_out);

            Samples := Samples + 1.0;
         exception
            when E : others => 
               Trace.Log ("BW_usage.Updater: " & Trace.Report (E), 
                  Trace.Error);
         end;
      end loop;
   end Updater;

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   procedure Get_in  (I1Min, I5Min, ISMin : out Float; Kind : in Kinds) is
   begin
      select 
         Updater.Get_in (I1Min, I5Min, ISMin, Kind);
      or
         delay 1.0;
         I1Min := 0.0;
         I5Min := 0.0;
         ISMin := 0.0;
      end select;
   end Get_in;

   procedure Get_out (O1Min, O5Min, OSMin : out Float; Kind : in Kinds) is
   begin
      select 
         Updater.Get_out (O1Min, O5Min, OSMin, Kind);
      or
         delay 1.0;
         O1Min := 0.0;
         O5Min := 0.0;
         OSMin := 0.0;
      end select;
   end Get_out;

end Adagio.Bw_usage;
