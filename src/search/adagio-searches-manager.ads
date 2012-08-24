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

with Adagio.Hash_Dictionary;
with Adagio.Download;
with Adagio.Searches.Hit;
with Adagio.Searches.Hit_Family;
with Adagio.Types;

with Agpl.Http.Server.Sort_Handler;
with Templates_Parser;

package Adagio.Searches.Manager is

   pragma Elaborate_Body;

   -- This package is thread-safe

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Search_Not_Found : exception;

   ------------------------------------------------------------------------
   -- Renamings                                                          --
   ------------------------------------------------------------------------
   package Sort_Handler renames Agpl.Http.Server.Sort_handler;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   procedure Add_Hit (Id : in Search_Id; New_Hit : in Searches.Hit.Object'Class);

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Will perform a search of all the available hits, and create sources
   -- for the compatible ones with the supplied magnet.
   procedure Add_Sources_To_Download (
      Hashes : in Hash_Dictionary.Object;
      Id     : in Download.Slot_Id);

   ------------------------------------------------------------------------
   -- Create_search                                                      --
   ------------------------------------------------------------------------
   procedure Create_search (Target   : in String; Priority : in Priorities := Auto);

   ------------------------------------------------------------------------
   -- Delete_Search                                                      --
   ------------------------------------------------------------------------
   -- No error if non-existant
   procedure Delete_Search (Id : in Search_Id);

   ------------------------------------------------------------------------
   -- Get_Hits                                                           --
   ------------------------------------------------------------------------
   -- Number of hits for the search.
   function Get_Hits (Id : in Search_Id) return Natural;

   ------------------------------------------------------------------------
   -- Get_Magnet                                                         --
   ------------------------------------------------------------------------
   -- Get a magnet string
   -- If not available returns ""
   -- Includes name (dn) and size (x.sz) fields.
   function Get_Magnet (
      Id     : in Search_Id;
      Family : in Hit_Family.Family_Id;
      Secure : in Boolean) return String;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (Id : in Search_Id; Family : in Hit_Family.Family_Id) return String;

   ------------------------------------------------------------------------
   -- Get_Payload                                                        --
   ------------------------------------------------------------------------
   -- Returns the search payload
   -- May raise Search_Not_Found
   function Get_Payload (Id : in Search_Id) return Payload;

   ------------------------------------------------------------------------
   -- Get_Priority                                                       --
   ------------------------------------------------------------------------
   -- May raise Search_Not_Found
   function Get_Priority (Id : in Search_Id) return Priorities;

   ------------------------------------------------------------------------
   -- Get_Priority_Delta                                                 --
   ------------------------------------------------------------------------
   -- Returns the priority delta for a given search:
   -- May raise Search_Not_Found
   function Get_Priority_Delta (Id : in Search_Id) return Natural;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (Id : in Search_Id; Family : in Hit_Family.Family_Id) return Types.File_Size;

   ------------------------------------------------------------------------
   -- Pause_Search                                                       --
   ------------------------------------------------------------------------
   -- No error if already paused
   procedure Pause_Search (Id : in Search_Id);

   ------------------------------------------------------------------------
   -- Resume_Search                                                      --
   ------------------------------------------------------------------------
   -- No error if already running
   procedure Resume_Search (Id : in Search_Id);

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (
      Id : in Search_Id; Family : in String; Expanded : in Boolean := true);

   ------------------------------------------------------------------------
   -- Set_Priority                                                       --
   ------------------------------------------------------------------------
   -- No error if non-existant
   procedure Set_Priority (Id : in Search_Id; Priority : in Priorities);

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Start to run the whole thing
   procedure Start;

   ------------------------------------------------------------------------
   -- Stop                                                               --
   ------------------------------------------------------------------------
   procedure Stop;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   -- Creates the Http dataset.
   procedure Http_Report (Data : out Sort_Handler.Data_Set);

   ------------------------------------------------------------------------
   -- Http_Report_Search                                                 --
   ------------------------------------------------------------------------
   -- Http report for the selected search
   procedure Http_Report_Search (Data : out Sort_Handler.Data_Set);
   function  Http_Report_Search return Templates_Parser.Translate_Table;

   ------------------------------------------------------------------------
   -- Http_Report_Set_Search                                             --
   ------------------------------------------------------------------------
   -- Set the search to be reported in subsequent calls
   procedure Http_Report_Set_Search (Id : in Search_Id);

   ------------------------------------------------------------------------
   -- Misc info                                                          --
   ------------------------------------------------------------------------
   function Get_Running_Searches return Natural;
   function Get_Paused_Searches  return Natural;
   function Get_New_Hits         return Natural;

end Adagio.Searches.Manager;
