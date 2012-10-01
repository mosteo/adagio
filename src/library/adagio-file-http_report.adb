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
--  $Id: adagio-file-http_report.adb,v 1.1 2004/01/29 21:47:08 Jano Exp $

with Adagio.Ed2k;
with Adagio.Library;
with Adagio.Misc;
with Sha1;
with TigerTree;

procedure Adagio.File.Http_report (Data : out Data_set) is
   Files : Library.File_list.Container_type;
   use Library.File_list;
   I     : Iterator_type;
begin
   Library.Object.Get_all_files (Files);
   I := First (Files);
   while I /= Back (Files) loop
      declare
         Row : Data_row;
         F   : Object      := Element (I);
         Fa  : File_access := V (F);
         DN  : Ustring     := U (Dir_name (F));
         Sh  : Ustring     := U (Sha1.To_base32 (fa.Sha));
         TT  : Ustring     := U (TigerTree.To_base32 (fa.TTH));
         Ed  : Ustring     := U (Adagio.Ed2k.Hash_as_hex (Fa.Ed2k));
      begin
         -- Path
         Append (Row, (DN, DN));
         -- Name
         Append (Row, (U (Name (F)), U (Name (F))));
         -- Size
         Append (Row, (
            U (Misc.To_string (Natural (Fa.Size))),
            Rpad (Natural (Fa.Size), 15)));
         -- Ups today
         Append (Row, (
            U (Misc.To_string (Fa.Uploads_session)),
            Rpad (Fa.Uploads_session, 15)));
         -- Ups total
         Append (Row, (
            U (Misc.To_string (Fa.Uploads)),
            Rpad (Fa.Uploads, 15)));
         -- Hits today
         Append (Row, (
            U (Misc.To_string (Fa.Hits_session)),
            Rpad (Fa.Hits_session, 15)));
         -- Hits total
         Append (Row, (
            U (Misc.To_string (Fa.Hits_total)),
            Rpad (Fa.Hits_total, 15)));
         -- Sha1
         Append (Row, (Sh, Sh));
         -- TTH
         Append (Row, (Tt, Tt));
         -- Ed2k
         Append (Row, (Ed, Ed));

         Append (Data, Row);
      end;
      I := Succ (I);
   end loop;
end Adagio.File.Http_report;
