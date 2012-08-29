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
--  $Id: adagio-trace.adb,v 1.5 2004/02/05 18:31:21 Jano Exp $

-- Package for help in tracing events

with Adagio.Debug;
with Adagio.Event_log;
-- with Adagio.Globals; -- Forbidden, circularities arise
with Adagio.Globals.Options;
with Adagio.Misc;
with Adagio.OS;

with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;

package body Adagio.Trace is

   General_File : constant String := S (Globals.Options.Debug_Logfile);
   Network_File : constant String := S (Globals.Options.Debug_NetLogfile);

   -- Minimum debug level
   -- See elaboration for additional initializing.
   Minimum_Level : Warning_Level := Warning_level'Value (
      S (Globals.Options.Debug_loglevel));

   procedure Check_changed_level is
   begin
      Minimum_Level := Warning_level'Value (
         S (Globals.Options.Debug_loglevel));
   end Check_changed_level;

   -- Instance vars for debugging.
   -- To avoid continuous polling in the tracing function:
   Debug_on      : boolean renames Globals.Options.Debug_active;

   subtype Warn_Prefix is String (1 .. 4);
   type Prefix is array (All_levels) of Warn_Prefix;

   Event_equiv : constant array (All_levels) of Event_log.Levels := (
      Never       => Event_log.Debug,
      Debug       => Event_log.Debug,
      Informative => Event_log.Normal,
      Error       => Event_log.Error,
      Warning     => Event_log.Warning,
      Always      => Event_log.Normal);

   -- Logs a text to log file. Slow, thread safe.
   -- Timestamp automatically prepended.
   procedure Log(
      Text    : String;
      Warning : All_levels := Debug;
      File    : String     := "")
   is
      Msg     : Ustring;
   begin
      if not Debug_on then  -- Earliest exit
         return;
      elsif Warning < Minimum_level then
         return;
      end if;

      Msg := U ("[" & Adagio.Misc.Timestamp & "] " & Text);

      -- To web log
      Event_log.Add ((
         Arrival  => Calendar.Clock,
         Level    => Event_equiv (Warning),
         Text     => U (Text)));

      if File = "" or else File = General_File then
         General.Log ("[" & Adagio.Misc.Timestamp & "] " & Text, Agpl.Trace.All_Levels (Warning));
      elsif File = Network_File then
         Network.Log ("[" & Adagio.Misc.Timestamp & "] " & Text, Agpl.Trace.All_Levels (Warning));
      else
         Other.Log   ("[" & Adagio.Misc.Timestamp & "] " & Text, Agpl.Trace.All_Levels (Warning));
      end if;

      if Warning >= Error then
         Agpl.Trace.Console_Tracer.Log ("[" & Adagio.Misc.Timestamp & "] " & Text, Agpl.Trace.All_Levels (Warning));
         Other.Log ("[" & Adagio.Misc.Timestamp & "] " & Text, Agpl.Trace.All_Levels (Warning));
      end if;

   exception
      when E : others =>
         Os.Message_box ("Trace.Log", Exception_information (E));
   end Log;

   -- Displays a box with info about some exception
   procedure Report(e: Ada.Exceptions.Exception_occurrence;
                    Context: String:= "") is
   begin
      OS.Message_box
        ("Adagio " & Context,
         "Error: " & Ada.Exceptions.Exception_name(e) & ": " &
                     Ada.Exceptions.Exception_message(e));
   end Report;

   -- Constructs a error string upon exception:
   function Report(e: Ada.Exceptions.Exception_occurrence) return String is
   begin
--         return Ada.Exceptions.Exception_name(e) & ": " &
--                Ada.Exceptions.Exception_message(e) &
           return     Exception_information (e);
   end Report;

   -- Get pending logs
   function Get_logs return Ustring_array is
      Result : constant Ustring_Array (1 .. 0) := (others => Null_Ustring);
   begin
      return Result;
   end Get_logs;

begin
   --  Global settings
   Agpl.Trace.Set_Level (Agpl.Trace.Debug);

   -- Prepare debug objects:
   General.Set_File (S (Globals.Options.Debug_Logfile));
   General.Set_Level (Agpl.Trace.Debug);

--     Agpl.Trace.Create (
--        General,
--        Agpl.Trace.Debug,
--        Globals.Options.Debug_ConsoleEcho,
--        S (Globals.Options.Debug_Logfile),
--        Globals.Options.Debug_Active,
--        Globals.Options.Debug_PurgeOnStartup);

   if S (Globals.Options.Debug_NetLogfile) /= S (Globals.Options.Debug_Logfile) then
      Network.Set_File (S (Globals.Options.Debug_NetLogfile));
      Network.Set_Level (Agpl.Trace.Debug);
--        Agpl.Trace.Create (
--           Network,
--           Agpl.Trace.Debug,
--           Globals.Options.Debug_ConsoleEcho,
--           S (Globals.Options.Debug_NetLogfile),
--           Globals.Options.Debug_Active,
--           Globals.Options.Debug_PurgeOnStartup);
   end if;

   Other.Set_File ("errors.log");
   Other.Set_Level (Agpl.Trace.Debug);

--     Agpl.Trace.Create (
--        Other,
--        Agpl.Trace.Debug,
--        not Globals.Options.Debug_ConsoleEcho, -- So only general or this is seen.
--        "errors.log",
--        Globals.Options.Debug_Active,
--        Globals.Options.Debug_PurgeOnStartup);

   Adagio.Debug.Tracing_finished := true; -- Remains of former implementation
end Adagio.Trace;
