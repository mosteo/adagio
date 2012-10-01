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
--  $Id: adagio-statistics.adb,v 1.6 2004/01/29 21:47:09 Jano Exp $

--  Helper package for miscelaneous informative data

with Adagio.Unicode;

with Ada.Exceptions;

package body Adagio.Statistics is

   use Stat_list;

   ------------------------------------------------------------------------
   -- Image                                                              --
   ------------------------------------------------------------------------
   function Image (This : in Values'Class) return String is
      Local : aliased Values'Class := This;
   begin
      return Image (Local'Access);
   end Image;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is

      procedure Set (Key : in String; Value : in Values'Class) is
         New_value : Value_access;
      begin
         if Is_in (Key, Data) then
            Element (Find (Data, Key)).all := Value;
         else
            New_value := new Values'Class'(Value);
            Insert (Data, Key, New_value);
         end if;
      end;

      function Get (Key : in String) return Values'Class is
      begin
         if not Is_in (Key, Data) then
            raise Value_not_defined;
         else
            return Element (Find (Data, Key)).all;
         end if;
      end Get;

      procedure Update (
         Key     : in String;
         Process : in Process_function;
         Extra   : in Values'Class) is
         Result  : Values'Class := Process (Get (Key), Extra);
      begin
         Set (Key, Result);
      end Update;

      function Report return Stat_array is
         Result : Stat_array (1 .. Length (Data));
         I : Iterator_type := First (Data);
      begin
         for N in Result'Range loop
            begin
               Result (N) := (
                  Key   => U (Unicode.To_utf8 (Key (I))),
                  Value => U (Unicode.To_utf8 (Image (Element (I)))));
            exception
               when E : others =>
                  Result (N) := (
                     Key   => U (Unicode.To_utf8 (Key (I))),
                     Value => U (Unicode.To_utf8 (Ada.Exceptions.Exception_Information (E))));
            end;
            I := Succ (I);
         end loop;

         return Result;
      end;

   end Object;

   ------------------------------------------------------------------------
   -- Http_report                                                        --
   ------------------------------------------------------------------------
   procedure Http_report (Data : out Agpl.Http.Server.Sort_handler.Data_set)
   is
      Stats : Stat_array := Object.Report;
   begin
      for N in Stats'Range loop
         declare
            use Agpl.Http.Server.Sort_handler;
            Row : Data_row;
         begin
            Append (Row, (Stats (N).Key,   Stats (N).Key));
            Append (Row, (Stats (N).Value, Stats (N).Value));

            Append (Data, Row);
         end;
      end loop;
   end Http_report;

end Adagio.Statistics;
