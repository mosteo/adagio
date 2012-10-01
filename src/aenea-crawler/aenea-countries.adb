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
--  $Id: aenea-countries.adb,v 1.3 2004/03/09 23:46:48 Jano Exp $

--  Keep track of hubs in each country:

with Agpl.Strings;

with Charles.Maps.Sorted.Unbounded;

package body Aenea.Countries is

   package Country_index is new Charles.Maps.Sorted.Unbounded
     (Agpl.Geoip.Country_code, Natural, "<", "=");

   type Iterate_Code is access procedure (Key   : in String;
                                          Count : in Integer);

   ------------------------------------------------------------------------
   -- Counter                                                            --
   ------------------------------------------------------------------------
   protected Counter is
      procedure Add (Code : Country_code; Amount : in Integer := 1);
      function  Get (Code : Country_code) return Natural;
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set);
      function  Total_hubs return Natural;
      procedure Iterate (Process : Iterate_Code);
   private
      Table : Country_index.Container_type;
   end Counter;
   protected body Counter is
      ---------
      -- Add --
      ---------
      procedure Add (Code : Country_code; Amount : in Integer := 1) is
         use Country_index;
         I         : Iterator_type := Find (Table, Code);
         New_value : Natural;
      begin
         if I /= Back (Table) then
            New_value := Element (I) + Amount;
            Delete (Table, I);
            if New_value > 0 then
               Insert (Table, Code, New_value);
            end if;
         else
            Insert (Table, Code, Amount);
         end if;
      end Add;

      ---------
      -- Get --
      ---------
      function  Get (Code : Country_code) return Natural is
         use Country_index;
         I : constant Iterator_type := Find (Table, Code);
      begin
         if I /= Back (Table) then
            return Element (I);
         else
            return 0;
         end if;
      end Get;

      ------------
      -- Report --
      ------------
      procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
         use Agpl.Http.Server.Sort_handler;
         use Country_index;
         I      : Iterator_type := First (Table);
         Total  : Natural := 0;
         FTotal : Float;
         FMax   : Float := 1.0;
      begin
         -- Get total number:
         while I /= Back (Table) loop
            Total := Total + Element (I);
            I     := Succ (I);
         end loop;
         FTotal := Float (Total);
         -- Rows:
         I := First (Table);
         while I /= Back (Table) loop
            declare
               Row : Data_row;
            begin
               -- Num
               Append (Row, (
                  Value       => U (
                     Agpl.Strings.Trim (Natural'Image (Element (I)))),
                  Order_value => Rpad (Element (I), 10)));
               -- Code
               if Key (I) /= "??" then
                  Append (Row, (
                     Value       => U (Key (I)),
                     Order_value => U (Key (I))));
               else
                  Append (Row, (
                     Value       => U (Key (I)),
                     Order_value => U ("ZZ")));
               end if;
               -- Name
               if Key (I) /= "??" then
                  Append (Row, (
                     Value       => U (
                        Agpl.Geoip.Country_name_from_code (Key (I))),
                     Order_value => U (
                        Agpl.Geoip.Country_name_from_code (Key (I)))));
               else
                  Append (Row, (
                     Value       => U ("Unknown"),
                     Order_value => U ("ZZZZZZZ")));
               end if;
               -- Flag
               if Key (I) /= "??" then
                  Append (Row, (
                     Value       => U (Agpl.Strings.L (Key (I))),
                     Order_value => U (Key (I))));
               else
                  Append (Row, (
                     Value       => U ("unknown"),
                     Order_value => U ("ZZ")));
               end if;
               -- Percents
               Append (Row, (
                  Value       => U (Agpl.Strings.Trim (
                     Agpl.Strings.To_string (
                        Float (Element (I)) * 100.0 / FTotal, 1))),
                  Order_value => Null_Ustring));
               -- Rounded percents upped to a minimum of 1
               declare
                  Percent : Float := Float (Element (I)) * 100.0 / FTotal;
               begin
                  Percent := Float'Max (Percent, 1.0);
                  Append (Row, (
                     Value  => U (Agpl.Strings.To_string (Natural (Percent))),
                     Order_value => Null_Ustring));
               end;

               -- ROW
               Append (Data, Row);

               Fmax := Float'Max (Fmax, Float (Element (I)));
            end;
            I := Succ (I);
         end loop;
         -- Create last value getting normalized values across 1 -- 100 (ints)
         begin
            I := First (Table);
            for N in 1 .. Last (Data) loop
               Append (Data.Vector (N), (
                  U (Agpl.Strings.To_string (Natural'Max (1,
                     Natural (
                        Float (Element (I)) * 100.0 / FMax)))),
                  Null_ustring));
               I := Succ (I);
            end loop;
         end;
      end Report;
      ----------------
      -- Total_hubs --
      ----------------
      function Total_hubs return Natural is
         use Country_index;
         Total : Natural       := 0;
         I     : Iterator_type := First (Table);
      begin
         -- Get total number:
         while I /= Back (Table) loop
            Total := Total + Element (I);
            I     := Succ (I);
         end loop;

         return Total;
      end Total_hubs;

      -------------
      -- Iterate --
      -------------
      procedure Iterate (Process : Iterate_Code)
      is
         use Country_Index;
         I : Iterator_Type := First (Table);
         begin
            while I /= Back (Table) loop
               Process (Key (I), Element (I));
               I := Succ (I);
            end loop;
      end Iterate;

   end Counter;

   ------------------------------------------------------------------------
   -- Sum_hub                                                            --
   ------------------------------------------------------------------------
   -- Add or remove a hub from a country:
   -- Use Inc => -1 to remove a hub.
   procedure Sum_hub (Country : in Country_code; Inc : in Integer := 1) is
   begin
      Counter.Add (Country, Inc);
   end Sum_hub;

   ------------------------------------------------------------------------
   -- Hubs_in_country                                                    --
   ------------------------------------------------------------------------
   function Hubs_in_country (Country : in Country_code) return Natural is
   begin
      return Counter.Get (Country);
   end Hubs_in_country;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   -- Generates the data for the html report.
   procedure Report (Data : out Agpl.Http.Server.Sort_handler.Data_set) is
   begin
      Counter.Report (Data);
   end Report;

   ------------------------------------------------------------------------
   -- Total_hubs                                                         --
   ------------------------------------------------------------------------
   -- SINGLE1 <-- num of hubs
   function Total_hubs return Templates_parser.Translate_table is
      use Templates_parser;
   begin
      return (1 => Assoc ("SINGLE1", Counter.Total_hubs));
   end Total_hubs;

   -------------
   -- Iterate --
   -------------
   procedure Iterate
     (Process :   not null access procedure (Key   : in String;
                                             Count : in Integer))
   is
   begin
      Counter.Iterate (Process);
   end Iterate;

end Aenea.Countries;
