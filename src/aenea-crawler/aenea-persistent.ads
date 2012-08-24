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
--  $Id: aenea-os.ads,v 1.3 2004/01/21 21:05:25 Jano Exp $

--  All data in this package will be saved and restored on request

with Aenea.Types;

package Aenea.Persistent is
   
   pragma Elaborate_Body;

   protected Object is
      ------------------------------------------------------------------------
      -- Restore                                                            --
      ------------------------------------------------------------------------
      -- Restores from disk
      procedure Restore;
      ------------------------------------------------------------------------
      -- Save                                                               --
      ------------------------------------------------------------------------
      -- Causes the saving.
      procedure Save;

      function  Get_Longest_Uptime_Time return Duration;
      procedure Set_Longest_Uptime_Time (Dur   : in Duration);

      function  Get_Longest_Uptime_Nick return String;
      procedure Set_Longest_Uptime_Nick (Nick  : in Ustring);

      function  Get_Top_Ten_Uptimes          return Types.Uptimes_Array;
      procedure Set_Top_Ten_Uptimes     (Times : in Types.Uptimes_Array);

   private

      Longest_Uptime_Time : Duration;
      Longest_Uptime_Nick : Ustring;

      -- Top ten uptimes:
      Top_Ten_Uptimes : Types.Uptimes_Array (1 .. 10);
      Top_Ten_Last    : Natural := 0;

   end Object;


end Aenea.Persistent;
