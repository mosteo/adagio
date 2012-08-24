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
--  $Id: adagio-statistics.ads,v 1.6 2004/01/29 21:47:09 Jano Exp $

--  Helper package for miscelaneous informative data

with Agpl.Http.Server.Sort_handler;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Charles.Maps.Sorted.Strings.Unbounded;

package Adagio.Statistics is

   pragma Elaborate_Body;

   Value_not_defined : Exception;

   -- UTF8 encoded!
   type Stat_record is record
      Key, 
      Value : Ustring;
   end Record;

   -- Values are managed as a polymorphic collection:
   type Values is abstract tagged null record;
   type Value_access is access all Values'Class;
   function Image (This : access Values) return String is abstract;
   function Image (This : in Values'Class) return String;

   type Stat_array is array (Positive range <>) of Stat_record;

   package Stat_list is new Charles.Maps.Sorted.Strings.Unbounded (
      Value_access, "<", "=");

   -- For atomic processing.
   -- The function will be called with the actual value and with an
   --    extra value supplied, inside the critical region. It must 
   --    return the new desired value for the statistic.
   type Process_function is access
      function (Actual_value, Extra_value : in Values'Class) 
         return Values'Class;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected Object is

      procedure Set (Key : in String; Value : in Values'Class);

      -- Raise Value_not_defined if not found
      function Get (Key : in String) return Values'Class;

      procedure Update (
         Key     : in String; 
         Process : in Process_function;
         Extra   : in Values'Class);

      function Report return Stat_array;

   private

      Data : Stat_list.Container_type;

   end Object;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set);

end Adagio.Statistics;
