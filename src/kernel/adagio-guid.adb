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
--  $Id: adagio-guid.adb,v 1.3 2004/01/21 21:05:27 Jano Exp $

with Adagio.Misc;
with Adagio.Monitor;

with Gnat.Os_lib;    use Gnat;

with Text_io; use Text_io;

with Ada.Numerics.Discrete_random;

package body Adagio.GUID is

   ------------------------------------------------------------------------
   -- To_char_array                                                      --
   ------------------------------------------------------------------------
   -- Return it as a series of 16 chars.
   function To_char_array (This : Adagio.GUID.GUID) return String is
   begin
      return String (This);
   end To_char_array;

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   procedure Init is
      F    : File_type;
      S    : String (1 .. 100);
      Last : Integer;
      File : Constant String := "adagio.guid";
   begin
      -- Check existence:
      if Os_lib.Is_regular_file (File) then
         -- Load it
         Open (F, Name => File, Mode => In_file);
         Get_line (F, S, Last);
         My_GUID := To_GUID (S (1 .. Last));
      else
         -- Create it
         My_GUID := Create_GUID;
         Create (F, Name => File, Mode => Out_file);
         Put_line (F, To_string (My_GUID));
      end if;
      Close (F);
   exception
      when others =>
         if Is_open (F) then
            Close (F);
         end if;
         raise;
   end Init;

   ------------------------------------------------------------------------
   -- To_hex                                                             --
   ------------------------------------------------------------------------
   -- Returns the hex representation of the raw GUID.
   function To_hex (this : Adagio.GUID.GUID) return String is
      Raw : String := To_char_array (this);
      S   : String (1 .. Raw'Length * 2);
   begin
      for N in Raw'Range loop
         S ((N - 1) * 2 + 1 .. N * 2) := Misc.To_hex (Raw (N));
      end loop;
      return S;
   end To_hex;

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   -- Returns the standard string representation of a GUID
   -- {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
   function To_string (this : in Adagio.GUID.GUID) return String is
      S : constant String := String (This);
   begin
      return
         "{" &
         Misc.To_hex (S (S'First .. S'First + 3)) & "-" &
         Misc.To_hex (S (S'First + 4 .. S'First + 5)) & "-" &
         Misc.To_hex (S (S'First + 6 .. S'First + 7)) & "-" &
         Misc.To_hex (S (S'First + 8 .. S'First + 9)) & "-" &
         Misc.To_hex (S (S'First + 10 .. S'First + 15)) &
         "}";
   end To_string;

   ------------------------------------------------------------------------
   -- To_GUID                                                            --
   ------------------------------------------------------------------------
   -- Converts a standard GUID string representation to a guid
   function To_GUID (this : in String) return Adagio.GUID.GUID is
      X : String renames This;
      function V (S : in String) return String renames Misc.From_hex;
   begin
      return
         Adagio.GUID.GUID (
            V (X (X'First + 1 .. X'First + 8)) &
            V (X (X'First + 10 .. X'First + 13)) &
            V (X (X'First + 15 .. X'First + 18)) &
            V (X (X'First + 20 .. X'First + 23)) &
            V (X (X'First + 25 .. X'First + 36)));
   end To_GUID;

   package Char_rand is new Ada.Numerics.Discrete_random (Character);

   Char_generator : Char_rand.Generator;

   ------------------------------------------------------------------------
   -- Create_GUID                                                        --
   ------------------------------------------------------------------------
   Mutex : aliased Monitor.Semaphore;
   function Create_GUID return Adagio.GUID.GUID is
      Result : Adagio.GUID.GUID;
      M      : Monitor.Object (Mutex'Access);
      pragma Unreferenced (M);
   begin
      for N in Result'Range loop
         Result (N) := Char_rand.Random (Char_generator);
      end loop;

      return Result;
   end Create_GUID;

   ------------------------------------------------------------------------
   -- Init_rand                                                          --
   ------------------------------------------------------------------------
   procedure Init_rand is
   begin
      -- Careful! This initialization time dependent should be replaced with
      -- a stronger seed generation:
      Char_rand.Reset (Char_generator);
   end Init_rand;

begin
   Init_rand;
end Adagio.GUID;
