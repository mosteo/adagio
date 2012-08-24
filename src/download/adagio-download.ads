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

with Adagio.Globals.Options;

with Agpl.Bandwidth_Throttle;
with Agpl.Types;
with Agpl.Types.Constants;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Streams;

package Adagio.Download is

   pragma Elaborate_Body;

   package Color renames Agpl.Types.Constants;

   -- Download BW manager
   Bandwidth : aliased Agpl.Bandwidth_Throttle.Object (
      Bandwidth => Globals.Options.Download_bandwidth,
      Period    => Natural (Globals.Options.Globals_TimeUnit * 1000.0));

   -- Ids for different entities are strings.
   -- We define new types just to avoid unintentional mixing.
   type Consumer_Id is new String;
   type Source_Id   is new Ustring;
   type Slot_Id     is new Ustring;

   function To_String    (Id : in Source_Id) return String;
   function To_String    (Id : in Slot_Id)   return String;
   function To_Source_Id (Id : in String)    return Source_Id;
   function To_Slot_Id   (Id : in String)    return Slot_Id;

   Null_Source : constant Source_Id;
   Null_Slot   : constant Slot_Id;  

   -- Priorities for downloads
   type Priorities is (Low, Medium, High, Critical);

   type Slot_Status is   (Paused, Enabled, Finished_Unverified, Finished_Verifying, Finished_Verified);
   type Source_Status is (Paused, Enabled);

   -- Chunk info
   type Chunk_Bounds is record
      First, Last : Ada.Streams.Stream_Element_Offset;
   end record;

   -- Chunk status:
   type Chunk_Validity_Status is (Missing, Unknown, Invalid, Valid);
   type Chunk_Validity_Status_Count_Array is array (Chunk_Validity_Status) of Ada.Streams.Stream_Element_Count;
   type Chunk_Validity_Status_Color_Array is array (Chunk_Validity_Status) of Agpl.Types.RGB_Triplet;

   CVS_Colors : Chunk_Validity_Status_Color_Array := (
      Missing => Color.Gainsboro,
      Unknown => Color.Blue,
      Invalid => Color.Red,
      Valid   => Color.Green
      );

   -- Concesion to lazyness:
   subtype SECount  is Ada.Streams.Stream_Element_Count;
   subtype SEOffset is Ada.Streams.Stream_Element_Offset;
   subtype SEArray  is Ada.Streams.Stream_Element_Array;

private

   Null_Source : constant Source_Id := Source_Id (Null_Ustring);
   Null_Slot   : constant Slot_Id   := Slot_Id   (Null_Ustring);

end Adagio.Download;
