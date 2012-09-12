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
--  $Id: adagio-library-query.adb,v 1.4 2004/01/21 21:05:28 Jano Exp $

with
Adagio.File,
Adagio.Misc,
Adagio.Trace,
Strings.Utils,
Charles.Algorithms.Generic_set_intersection;

package body Adagio.Library.Query is

   ------------------------------------------------------------------------
   -- Multiple_words                                                     --
   ------------------------------------------------------------------------
   -- Will return an array with all the files matching ALL the keywords.
   procedure Multiple_words (
      Words : in String; Files : in out File_set.Container_type)
   is
      Simple_words : String :=
         Strings.Utils.Simplify (
            Strings.Utils.Positivize (Words));
      Indexes   : Strings.Utils.Index_array :=
         Strings.Utils.Tokenize (Simple_words);
      Results   : File_set.Container_type;
      Common    : File_set.Container_type;
      Intersect : File_set.Container_type;

      -- Copy intersected items to intersection set:
      procedure Process (I : File_set.Iterator_type) is
      begin
         File_set.Insert (Intersect, File_set.Element (I));
      end Process;
      procedure Succ (I : in out File_set.Iterator_type) is
      begin
         I := File_set.Succ (I);
      end Succ;
      function Is_less (L, R : File_set.Iterator_type) return Boolean is
         use File_set;
      begin
         return File.Path (Element (L)) < File.Path (Element (R));
      end Is_less;
      procedure Do_intersection is new
         Charles.Algorithms.Generic_set_intersection (
            File_set.Iterator_type,
            Succ,
            Process,
            Is_less,
            File_set."=");
      use File_set;
   begin
      for N in Indexes'Range loop
         if Simple_words (Indexes (N).First) /= '-' and then
            Indexes (N).Last - Indexes (N).First + 1 >= 3
         then

            Library.Object.Query_word (
               Simple_words (Indexes (N).First .. Indexes (N).Last), Results);

            -- Remove unshared ones
            declare
               I : Iterator_type := First (Results);
            begin
               while I /= Back (Results) loop
                  if not (File.Shared (Element (I)) and
                          file.Folder_shared (Element (I))) then
                     Delete (Results, I);
                  else
                     I := Succ (I);
                  end if;
               end loop;
            end;

            -- First keyword queried, copy all results:
            if File_Set.Is_Empty (Common) then
               Copy (Common, Results);
            else
               File_set.Clear (Intersect);
               -- Do intersection
               Do_intersection (First (Results), Back (Results),
                                First (Common), Back (Common));
               -- Assign:
               Copy (Common, Intersect);
            end if;
            -- There is no point in proceeding if intersection
            --  is already empty:
            exit when File_set.Is_empty (Common);

         end if;
      end loop;

      -- Construct the result vector and return it:
      declare
         I      : File_set.Iterator_type := File_set.First (Common);
         Add    : Boolean;
      begin
         File_set.Clear (Files);
         while I /= File_set.Back (Common) loop
            Add := true;
            -- Check against negative words:
            for N in Indexes'Range loop
               if Simple_words (Indexes (N).First) = '-' and then
                  Misc.Contains (
                     Misc.To_lower (File.Name (File_set.Element (I))),
                     Simple_words (Indexes (N).First + 1 .. Indexes (N).Last))
               then
                  Add := false;
                  exit;
               end if;
            end loop;
            -- Add if still valid:
            if Add then
               File_set.Insert (Files, File_set.Element (I));
            end if;
            I := File_set.Succ (I);
         end loop;
      end;
   exception
      when E : others =>
         Trace.Log ("Library.Query.Multiple_words: " & Words & ": " &
            Trace.Report (E), Trace.Error);
   end Multiple_words;

end Adagio.Library.Query;
