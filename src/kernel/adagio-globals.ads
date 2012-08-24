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
--  $Id: adagio-globals.ads,v 1.4 2004/03/10 23:50:01 Jano Exp $

with Adagio.Throttler;
with Adagio.User_profile;
with Adagio.Xml;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar; use Ada;

package Adagio.Globals is

   -- Elaborate body. Config is read while elaboration.
   -- -f <file> is loaded.
   -- A default data/adagio.xml file is tried.
   pragma Elaborate_body;

   ------------------------------------------------------------------------
   -- Load_config                                                        --
   ------------------------------------------------------------------------
   -- Reloads the configuration file
   procedure Load_config;

   -- Global flag to exit (initially false).
   Requested_exit : Boolean := false;
   pragma Atomic (Requested_exit);

   -- XML configuration:
   Config: aliased Adagio.XML.Document;

   -- Global throttle aimed at 90%, configurable via adagio.xml
   Main_throttle: aliased Throttler.Object(Target_usage => 90);

   -- Hashing throttle aimed at 50%, configurable via adagio.xml
   Hash_throttle: aliased Throttler.Object(Target_usage => 50);

   -- My user profile:
   My_profile : User_profile.Object;

   -- For uptime:
   Adagio_start : Calendar.Time := Calendar.Clock;

   -- Persistent data folder:
   function Data_folder return String;
   function Data_folder return UString;

   ------------------------------------------------------------------------
   -- Prepare_GUID                                                       --
   ------------------------------------------------------------------------
   -- Inserts the GUID child in the profile
   procedure Prepare_GUID;

   ------------------------------------------------------------------------
   -- Remove_lock                                                        --
   ------------------------------------------------------------------------
   -- Removes the lock file for aesthetic purposes
   procedure Remove_lock;

end Adagio.Globals;
