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
--  $Id: adagio-bw_usage.ads,v 1.2 2004/03/29 19:13:33 Jano Exp $

with Average_queue;

package Adagio.Bw_usage is

   pragma Elaborate_body;

   type Kinds is (TCP, UDP);

   procedure Add_out (Size : in Natural; Kind : in Kinds);
   procedure Add_in  (Size : in Natural; Kind : in Kinds);
   procedure Get_in  (I1Min, I5Min, ISMin : out Float; Kind : in Kinds);
   procedure Get_out (O1Min, O5Min, OSMin : out Float; Kind : in Kinds);

private

   function Div (L : in Float; R : in Integer) return Float;

   package Averages is new Average_queue (Float, "+", Div);

   In_1min  : Averages.Object (60);
   In_5min  : Averages.Object (300);
   Out_1min : Averages.Object (60);
   Out_5min : Averages.Object (300);
   In_1min_u  : Averages.Object (60);
   In_5min_u  : Averages.Object (300);
   Out_1min_u : Averages.Object (60);
   Out_5min_u : Averages.Object (300);

   In_ses   : Float;
   Out_ses  : Float;
   In_ses_u   : Float;
   Out_ses_u  : Float;

   Samples  : Float;

end Adagio.Bw_usage;
