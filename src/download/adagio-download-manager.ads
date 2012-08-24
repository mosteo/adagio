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

with Adagio.Download.Consumer;
with Adagio.Download.Data;
with Adagio.Download.Source;
with Adagio.Types;

with Agpl.Bmp;
with Agpl.Http.Server.Sort_Handler;
with Agpl.Magnet;

-- Central, thread-safe point of control for all download operations.
-- Each download has two disk files:
--    <name>.partial contains data downloaded till the moment.
--    <name>.xml     contains metadata about the download/sources/etc.

package Adagio.Download.Manager is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Add_Consumer                                                       --
   ------------------------------------------------------------------------
   -- Adds a consumer for a slot. Only one of each kind is accepted, others
   -- are discarded.
   procedure Add_Consumer (
      Id : in Slot_Id; Consumer : access Download.Consumer.Object'Class);

   ------------------------------------------------------------------------
   -- Add_Source                                                         --
   ------------------------------------------------------------------------
   -- Adds a source for a slot
   -- Repeated Source_Ids are discarded.
   procedure Add_Source (
      Id : in Slot_Id; Source : in Download.Source.Object_Access);

   ------------------------------------------------------------------------
   -- Create_Slot                                                        --
   ------------------------------------------------------------------------
   -- Begins a new download slot
   -- Returns the id for the newly created slot
   procedure Create_Slot (
      Hash   : in     Agpl.Magnet.Object; 
      Secure : in     Boolean;
      Id     :    out Slot_Id);

   ------------------------------------------------------------------------
   -- Get_Chunk_To_Request                                               --
   ------------------------------------------------------------------------
   -- Gives the next chunk that should be requested for a file.
   -- To be called by sources.
   function Get_Chunk_To_Request (Id : in Slot_Id) return Chunk_Bounds;

   ------------------------------------------------------------------------
   -- Get_Progress_Bmp                                                   --
   ------------------------------------------------------------------------
   -- Returns a BMP graph with progress status
   function Get_Progress_Bmp (Id : in Slot_Id) return Agpl.Bmp.Object;

   ------------------------------------------------------------------------
   -- Get_Speed_Bar                                                      --
   ------------------------------------------------------------------------
   -- Returns a BMP graph with data rates
   function Get_Speed_Bar (Id : in Slot_Id) return Agpl.Bmp.Object;

   ------------------------------------------------------------------------
   -- Http_Report_Downloads                                              --
   ------------------------------------------------------------------------
   procedure Http_Report_Downloads (Data : out Agpl.Http.Server.Sort_Handler.Data_Set);

   ------------------------------------------------------------------------
   -- Remove_Slot                                                        --
   ------------------------------------------------------------------------
   procedure Remove_Slot (Id : in Slot_Id);

   ------------------------------------------------------------------------
   -- Remove_Source                                                      --
   ------------------------------------------------------------------------
   procedure Remove_Source (Source : in Source_Id);

   ------------------------------------------------------------------------
   -- Restore                                                            --
   ------------------------------------------------------------------------
   -- Restores downloads from disk
   procedure Restore;

   ------------------------------------------------------------------------
   -- Saves the downloads to disk                                        --
   ------------------------------------------------------------------------
   -- Optionally a slot id can be provided to only save that one.
   procedure Save (Id : in Slot_Id := Null_Slot);

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   -- Allows a source to provide the size for a slot. If it's unknown it will
   -- be recorded, and if not the size will be checked and in case of mismatch
   -- the source will be dropped.
   procedure Set_Size (Id : in Slot_Id; From : in Source_Id; Size : in Types.File_Size);

   ------------------------------------------------------------------------
   -- Store_Data                                                         --
   ------------------------------------------------------------------------
   -- Store/Process some received data.
   -- To be called by the sources.
   procedure Store_Data (
      Id       : in Slot_Id; 
      Data     : in Download.Data.Object'Class);

end Adagio.Download.Manager;
