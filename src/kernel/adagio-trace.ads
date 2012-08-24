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
--  $Id: adagio-trace.ads,v 1.4 2004/01/21 21:05:27 Jano Exp $

-- Package for help in tracing events

with Agpl.Debug;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Exceptions;          use Ada;
with Ada.Streams;

package Adagio.Trace is

   pragma Elaborate_body;

   type All_levels       is new Agpl.Trace.All_Levels;
   subtype Warning_level is All_levels range Debug..Error;

   procedure Check_changed_level;

   -- Logs a text to log file. Slow, thread safe.
   -- Timestamp automatically prepended.
   -- To default file if not specified, other otherwise.
   procedure Log(
      Text:    String;
      Warning: All_levels := Debug;
      File:    String:= "");

   -- Displays a box with info about some exception
   procedure Report(e: Ada.Exceptions.Exception_occurrence;
                    Context: String:= "");

   -- Constructs a error string upon exception:
   function Report(e: Ada.Exceptions.Exception_occurrence) return String;

   -- Returns next N characters from a stream as hex
   function Debug_stream (
      Stream      : access Streams.Root_stream_type'Class;
      N           : Positive := 8;
      Separator   : String := ":") return String
      renames Agpl.Debug.Hex_Dump_From_Stream;

   -- Get pending logs
   function Get_logs return Ustring_array;

   ------------------------------------------------------------------------
   -- Debug Objects                                                      --
   ------------------------------------------------------------------------
   General : aliased Agpl.Trace.Object;
   Network : aliased Agpl.Trace.Object;
   Other   : aliased Agpl.Trace.Object;

end Adagio.Trace;
