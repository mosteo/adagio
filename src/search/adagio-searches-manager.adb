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
--  $Id: adagio-upload.ads,v 1.4 2004/01/21 21:05:51 Jano Exp $

With
Adagio.Chronos,
Adagio.Globals,
Adagio.Globals.Options,
Adagio.Misc,
Adagio.Network,
Adagio.Searches.Search,
Adagio.Trace,
Adagio.Xml,
Strings.Utils,
Agpl.Magnet,
Agpl.Safe_File,
Agpl.Strings,
Charles.Hash_string,
Charles.Maps.Hashed.Strings.Unbounded,
Ada.Calendar,
Ada.Streams.Stream_IO,
Adagio.Constants;

Use
Ada;

package body Adagio.Searches.Manager is

   use type Calendar.Time;

   use type Searches.Search.Object;

   Save_File      : constant String := "searches.xml";

   function Same_Search (L, R : Searches.Search.Object_Access) return Boolean
   is
   begin
      return L.all = R.all;
   end Same_Search;

   package Search_list is new Charles.Maps.Hashed.Strings.Unbounded (
      Searches.Search.Object_Access,
      Charles.Hash_string,
      "=",
      Same_Search);

   -- Indexed by Search_Id, which must be equal to the Target Words/URN

   use Search_List;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected Object is
      -------------
      -- Add_Hit --
      -------------
      procedure Add_Hit (Id : in Search_Id; H : in Searches.Hit.Object'Class);
      -----------------------------
      -- Add_Sources_To_Download --
      -----------------------------
      procedure Add_Sources_To_Download (Hashes : in Hash_Dictionary.Object; Id : in Download.Slot_Id);
      --------------
      -- Contains --
      --------------
      -- Says if the search already exists
      function Contains (This : in String) return Boolean;
      -------------------
      -- Create_Search --
      -------------------
      procedure Create_search (
         Target : in String; Priority : in Searches.Priorities; Id : out Search_Id);
      -------------------
      -- Delete_Search --
      -------------------
      procedure Delete_Search (Id : in Search_Id);
      --------------
      -- Get_Hits --
      --------------
      function Get_Hits (Id : in Search_Id) return Natural;
      -------------------
      -- Get_Last_Save --
      -------------------
      function Get_Last_Save return Calendar.Time;
      ----------------
      -- Get_Magnet --
      ----------------
      function Get_Magnet (Id : in Search_Id; Family : in Hit_Family.Family_Id; Secure : in Boolean) return String;
      --------------
      -- Get_Name --
      --------------
      function Get_Name (Id : in Search_Id; Family : in Hit_Family.Family_Id) return String;
      -----------------
      -- Get_Payload --
      -----------------
      function Get_Payload (Id : in Search_Id) return Payload;
      ------------------
      -- Get_Priority --
      ------------------
      function Get_Priority (Id : in Search_Id) return Priorities;
      ------------------------
      -- Get_Priority_Delta --
      ------------------------
      function Get_Priority_Delta (Id : in Search_Id) return Natural;
      ----------------
      -- Get_Search --
      ----------------
      -- Can raise Search_Not_Found;
      function Get_Search (Id : in Search_Id) return Search.Object_Access;
      --------------
      -- Get_Size --
      --------------
      function Get_Size (Id : in Search_Id; Family : in Hit_Family.Family_Id) return Types.File_Size;
      -------------------
      -- Insert_Search --
      -------------------
      -- Inserts a search object in the list.
      procedure Insert_Search (Srch: in Search.Object_Access);
      ------------------
      -- Pause_Search --
      ------------------
      procedure Pause_Search (Id : in Search_Id);
      -------------------
      -- Resume_Search --
      -------------------
      procedure Resume_Search (Id : in Search_Id);
      ----------
      -- Save --
      ----------
      procedure Save (With_Hits : in Boolean);
      ------------------
      -- Set_Expanded --
      ------------------
      procedure Set_Expanded (
         Id : in Search_Id; Family : in String; Expanded : in Boolean := true);
      ------------------
      -- Set_Priority --
      ------------------
      procedure Set_Priority (Id : in Search_Id; Priority : in Priorities);
      ------------
      -- Report --
      ------------
      procedure Report (Data : out Sort_Handler.Data_Set);
      ------------------------
      -- Http_Report_Search --
      ------------------------
      procedure Http_Report_Search (Data : out Sort_Handler.Data_Set);
      function  Http_Report_Search return Templates_Parser.Translate_Table;
      ----------------------------
      -- Http_Report_Set_Search --
      ----------------------------
      procedure Http_Report_Set_Search (Id : in Search_Id);
      ---------------
      -- Misc info --
      ---------------
      function Get_Running_Searches return Natural;
      function Get_Paused_Searches  return Natural;
      function Get_New_Hits         return Natural;
   private
      List      : Search_list.Container_type;
      Target    : Search_Id;
      Last_Save : Calendar.Time := Adagio.Constants.Past_Aeons;
   end Object;

   protected body Object is

      -------------
      -- Add_Hit --
      -------------
      procedure Add_Hit (Id : in Search_Id; H : in Searches.Hit.Object'Class) is
         I    : Iterator_Type := Find (List, To_String (Id));
         Srch : Search.Object_Access;
      begin
         if I /= Back (List) then
            Srch := Element (I);
            Search.Add_Hit (Srch, H);
         end if;
      end Add_Hit;

      -----------------------------
      -- Add_Sources_To_Download --
      -----------------------------
      procedure Add_Sources_To_Download (
         Hashes : in Hash_Dictionary.Object;
         Id     : in Download.Slot_Id)
      is
         I : Iterator_Type := First (List);
      begin
         while I /= Back (List) loop
            Search.Add_Sources_To_Download (Element (I), Hashes, Id);
            I := Succ (I);
         end loop;
      end Add_Sources_To_Download;

      --------------
      -- Contains --
      --------------
      -- Says if the search already exists
      function Contains (This : in String) return Boolean is
      begin
         return Search_List.Is_In (This, List);
      end Contains;

      -------------------
      -- Create_Search --
      -------------------
      procedure Create_search (Target : in String; Priority : in Searches.Priorities; Id : out Search_Id)
      is
         New_Search : Searches.Search.Object_access :=
               Searches.Search.Create (Target, Priority);
      begin
         if not Contains (Target) then
            Search_list.Insert (List, To_String (Search.Get_Id (New_Search)), New_search);
            Save (With_Hits => false);
            Id := Search.Get_Id (New_Search);
         end if;
      end Create_search;
      -------------------
      -- Delete_Search --
      -------------------
      procedure Delete_Search (Id : in Search_Id) is
         I : Iterator_Type := Find (List, To_String (Id));
         S : Search.Object_access;
      begin
         if I /= Back (List) then
            S := Element (I);
            Delete (List, I);
            Search.Destroy (S);
            Save (With_Hits => false);
         end if;
      end Delete_Search;
      --------------
      -- Get_Hits --
      --------------
      function Get_Hits (Id : in Search_Id) return Natural is
      begin
         return Search.Get_Hits (Get_Search (Id));
      end Get_Hits;
      -------------------
      -- Get_Last_Save --
      -------------------
      function Get_Last_Save return Calendar.Time is
      begin
         return Last_Save;
      end Get_Last_Save;
      ----------------
      -- Get_Magnet --
      ----------------
      function Get_Magnet (Id : in Search_Id; Family : in Hit_Family.Family_Id; Secure : in Boolean) return String
      is
      begin
         return Hit_Family.Get_Magnet (
            Search.Get_Family (Get_Search (Id), Family).all,
            Secure,
            Sources => false);
      end Get_Magnet;
      --------------
      -- Get_Name --
      --------------
      function Get_Name (Id : in Search_Id; Family : in Hit_Family.Family_Id) return String is
      begin
         return Hit_Family.Get_Name (Search.Get_Family (Get_Search (Id), Family).all);
      end Get_Name;
      -----------------
      -- Get_Payload --
      -----------------
      function Get_Payload (Id : in Search_Id) return Payload is
      begin
         return Search.Get_Payload (Get_Search (Id));
      end Get_Payload;
      ------------------
      -- Get_Priority --
      ------------------
      function Get_Priority (Id : in Search_Id) return Priorities is
      begin
         return Search.Get_Priority (Get_Search (Id));
      end Get_Priority;
      ------------------------
      -- Get_Priority_Delta --
      ------------------------
      function Get_Priority_Delta (Id : in Search_Id) return Natural is
      begin
         return Search.Get_Priority_Delta (Get_Search (Id));
      end Get_Priority_Delta;
      ----------------
      -- Get_Search --
      ----------------
      -- Can raise Search_Not_Found;
      function Get_Search (Id : in Search_Id) return Search.Object_Access is
         I : Iterator_Type := Find (List, To_String (Id));
      begin
         if I /= Back (List) then
            return Element (I);
         else
            raise Search_Not_Found;
         end if;
      end Get_Search;
      --------------
      -- Get_Size --
      --------------
      function Get_Size (Id : in Search_Id; Family : in Hit_Family.Family_Id) return Types.File_Size is
      begin
         return Hit_Family.Get_Size (Search.Get_Family (Get_Search (Id), Family).all);
      end Get_Size;
      -------------------
      -- Insert_Search --
      -------------------
      -- Inserts a search object in the list.
      procedure Insert_Search (Srch: in Search.Object_Access) is
      begin
         Search_list.Insert (List, To_String (Search.Get_Id (Srch)), Srch);
      end Insert_Search;
      ------------------
      -- Pause_Search --
      ------------------
      procedure Pause_Search (Id : in Search_Id) is
         I : Iterator_Type := Find (List, To_String (Id));
      begin
         if I /= Back (List) then
            Search.Pause (Element (I));
            Network.List.Set_Search_Paused (Id, true);
            Save (With_Hits => false);
         end if;
      end Pause_Search;
      -------------------
      -- Resume_Search --
      -------------------
      procedure Resume_Search (Id : in Search_Id) is
         I : Iterator_Type := Find (List, To_String (Id));
      begin
         if I /= Back (List) then
            Search.Resume (Element (I));
            Network.List.Set_Search_Paused (Id, false);
            Save (With_Hits => false);
         end if;
      end Resume_Search;
      ----------
      -- Save --
      ----------
      procedure Save (With_Hits : in Boolean) is
         Doc : Xml.Document  := Xml.From_String ("<searches/>");
         I   : Iterator_Type := First (List);
         Cron: Chronos.Object;
      begin
         while I /= Back (List) loop
            Xml.Add (Doc, Search.To_XML (Element (I), Doc, With_Hits));
            I := Succ (I);
         end loop;
         declare
            use Ada.Streams.Stream_IO;
            use Agpl;
            F : File_Type;
         begin
            Safe_File.Open (F, Out_File, Name => Globals.Data_Folder & Save_File);
            String'Write (Stream (F), Xml.To_String (Doc));
            Safe_File.Close (F);
         end;
         Xml.Delete (Doc);
         Trace.Log ("Searches saved (hits: " & With_Hits'Img & ") in " & Chronos.Image (Cron), Trace.Debug);
         Last_Save := Calendar.Clock;
      exception
         when E : others =>
            Trace.Log ("Searches.Manager.Save: " & Trace.Report (E), Trace.Error);
            Xml.Delete (Doc);
      end Save;
      ------------------
      -- Set_Expanded --
      ------------------
      procedure Set_Expanded (
         Id : in Search_Id; Family : in String; Expanded : in Boolean := true)
      is
      begin
         Search.Set_Expanded (Get_Search (Id), Family, Expanded);
      end Set_Expanded;
      ------------------
      -- Set_Priority --
      ------------------
      procedure Set_Priority (Id : in Search_Id; Priority : in Priorities)
      is
         I : Iterator_Type := Find (List, To_String (Id));
      begin
         if I /= Back (List) then
            Search.Set_Priority (Element (I), Priority);
            Network.List.Set_Search_Priority (Id, Priority);
            Save (With_Hits => False);
         end if;
      end Set_Priority;
      ------------
      -- Report --
      ------------
      procedure Report (Data : out Sort_Handler.Data_Set) is
         use Sort_Handler;
         I : Iterator_type := First (List);
      begin
         while I /= Back (List) loop
            declare
               Row  : Data_Row;
               Srch : Searches.Search.Object_access renames Element (I);
               use Searches.Search;
            begin
               -- Target
               Append (Row, (U (Get_Target (Srch)), U (Get_Target (Srch))));

               -- Hits
               Append (Row, (
                  U (Misc.To_string (Get_Hits (Srch))),
                  Rpad (Get_Hits (Srch))));

               -- New Hits
               Append (Row, (
                  U (Misc.To_string (Get_New_Hits (Srch))),
                  Rpad (Get_New_Hits (Srch))));

               -- Firewalled Hits
               Append (Row, (
                  U (Misc.To_string (Get_Firewalled_Hits (Srch))),
                  Rpad (Get_Firewalled_Hits (Srch))));

               -- Priority
               Append (Row, (
                  U (Priorities'Image (Get_Priority (Srch))),
                  U (Misc.To_string (Priorities'Pos (Get_Priority (Srch))))));

               -- Effective Priority
               Append (Row, (
                  U (Priorities'Image (Get_Effective_Priority (Srch))),
                  U (Misc.To_string (
                     Priorities'Pos (Get_Effective_Priority (Srch))))));

               -- Paused?
               Append (Row, (
                  U (Boolean'Image (Get_Paused (Srch))),
                  U (Boolean'Image (Get_Paused (Srch)))));

               -- Secure hits
               Append (Row, (
                  U (Misc.To_string (Get_Secure_Hits (Srch))),
                  Rpad (Get_Secure_Hits (Srch))));

               -- Custom Progress Info
               declare
                  Custom : constant String :=
                     Network.List.Get_Custom_Info (Get_Id (Srch));
               begin
                  Append (Row, (U (Custom), U(Custom)));
               end;

               Append (Data, Row);
            end;
            I := Succ (I);
         end loop;
      end Report;
      ------------------------
      -- Http_Report_Search --
      ------------------------
      procedure Http_Report_Search (Data : out Sort_Handler.Data_Set) is
         Srch : Search.Object_Access := Get_Search (Target);
      begin
         Search.Http_Report (Srch, Data);
      end Http_Report_Search;
      function  Http_Report_Search return Templates_Parser.Translate_Table is
         Srch : Search.Object_Access := Get_Search (Target);
         use Templates_Parser;
      begin
         return (
            Assoc ("SINGLE1", Search.Get_Target (Srch)),
            Assoc ("SINGLE2", Search.Get_Paused (Srch)));
      end Http_Report_Search;
      ----------------------------
      -- Http_Report_Set_Search --
      ----------------------------
      procedure Http_Report_Set_Search (Id : in Search_Id) is
      begin
         Target := Id;
      end Http_Report_Set_Search;
      ---------------
      -- Misc info --
      ---------------
      -- Get_Running_Searches --
      function Get_Running_Searches return Natural is
         I : Iterator_Type := First (List);
         N : Natural       := 0;
      begin
         while I /= Back (List) loop
            if not Search.Get_Paused (Element (I)) then
               N := N + 1;
            end if;
            I := Succ (I);
         end loop;
         return N;
      end Get_Running_Searches;
      -- Get_Paused_Searches --
      function Get_Paused_Searches  return Natural is
      begin
         return Search_List.Length (List) - Get_Running_Searches;
      end Get_Paused_Searches;
      -- Get_New_Hits --
      function Get_New_Hits         return Natural is
         I : Iterator_Type := First (List);
         N : Natural       := 0;
      begin
         while I /= Back (List) loop
            N := N + Search.Get_New_Hits (Element (I));
            I := Succ (I);
         end loop;
         return N;
      end Get_New_Hits;
   end Object;

   ----------------
   -- Hits_Saver --
   ----------------
   task Hits_Saver;

   task body Hits_Saver is
      Last_Save : Calendar.Time := Adagio.Constants.Past_Aeons;
   begin
      loop
         exit when Globals.Requested_Exit;
         delay 1.0;
         if Globals.Options.G2_Search_SaveHits then
            declare
               Now  : constant Calendar.Time := Calendar.Clock;
               Last : constant Calendar.Time := Object.Get_Last_Save;
            begin
               if Last > Last_Save and then Now - Last > Globals.Options.G2_Search_SaveHitsDelay then
                  Object.Save (With_Hits => true);
                  Last_Save := Calendar.Clock;
                  -- Can't be Now because we want Last_Save > Object.Get_Last_Save,
                  --    which has been just updated in Object.Save !!
               end if;
            exception
               when E : others =>
                  Trace.Log ("Searches.Manager.Hits_Saver: " & Trace.Report (E), Trace.Error);
            end;
         end if;
      end loop;
      Trace.Log ("Hits_Saver exited.", Trace.Informative);
   end Hits_Saver;

   ------------------------------------------------------------------------
   -- Add_Hit                                                            --
   ------------------------------------------------------------------------
   procedure Add_Hit (Id : in Search_Id; New_Hit : in Searches.Hit.Object'Class) is
   begin
      Object.Add_Hit (Id, New_Hit);
   end Add_Hit;

   ------------------------------------------------------------------------
   -- Add_Sources_To_Download                                            --
   ------------------------------------------------------------------------
   -- Will perform a search of all the available hits, and create sources
   -- for the compatible ones with the supplied magnet.
   procedure Add_Sources_To_Download (
      Hashes : in Hash_Dictionary.Object;
      Id     : in Download.Slot_Id) is
   begin
      Object.Add_Sources_To_Download (Hashes, Id);
   end Add_Sources_To_Download;

   ------------------------------------------------------------------------
   -- Create_search                                                      --
   ------------------------------------------------------------------------
   procedure Create_search (
      Target   : in String;
      Priority : in Priorities := Searches.Auto)
   is
      New_Target : constant String :=
         Agpl.Strings.To_Lower (Strings.Utils.Simplify (Target));
      Id         : Search_Id;
   begin
      if Agpl.Magnet.Is_Magnet (Target) then
         Object.Create_search (Target, Priority, Id);
         Network.List.Create_Search (Id);
      elsif New_Target = "" then
         Trace.Log ("Creating search: dropping incorrect search: " & Target,
            Trace.Informative);
      elsif Object.Contains (New_Target) then
         Trace.Log ("Creating search: dropping duplicated search: " & Target,
            Trace.Informative);
      else
         Object.Create_search (New_Target, Priority, Id);
         Network.List.Create_Search (Id);
      end if;
   end Create_search;

   ------------------------------------------------------------------------
   -- Delete_Search                                                      --
   ------------------------------------------------------------------------
   -- No error if non-existant
   procedure Delete_Search (Id : in Search_Id) is
   begin
      Network.List.Delete_Search (Id);
      Object.Delete_Search (Id);
   end Delete_Search;

   ------------------------------------------------------------------------
   -- Get_Hits                                                           --
   ------------------------------------------------------------------------
   -- Number of hits for the search.
   function Get_Hits (Id : in Search_Id) return Natural is
   begin
      return Object.Get_Hits (Id);
   end Get_Hits;

   ------------------------------------------------------------------------
   -- Get_Magnet                                                         --
   ------------------------------------------------------------------------
   -- Get a magnet string
   -- If not available returns ""
   function Get_Magnet (
      Id     : in Search_Id;
      Family : in Hit_Family.Family_Id;
      Secure : in Boolean) return String is
   begin
      return Object.Get_Magnet (Id, Family, Secure);
   exception
      when others =>
         return "";
   end Get_Magnet;

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_Name (Id : in Search_Id; Family : in Hit_Family.Family_Id) return String is
   begin
      return Object.Get_Name (Id, Family);
   end Get_Name;

   ------------------------------------------------------------------------
   -- Get_Payload                                                        --
   ------------------------------------------------------------------------
   -- Returns the search payload
   function Get_Payload (Id : in Search_Id) return Payload is
   begin
      return Object.Get_Payload (Id);
   end Get_Payload;

   ------------------------------------------------------------------------
   -- Get_Priority                                                       --
   ------------------------------------------------------------------------
   -- May raise Search_Not_Found
   function Get_Priority (Id : in Search_Id) return Priorities is
   begin
      return Object.Get_Priority (Id);
   end Get_Priority;

   ------------------------------------------------------------------------
   -- Get_Priority_Delta                                                 --
   ------------------------------------------------------------------------
   -- Returns the priority delta for a given search:
   -- May raise Search_Not_Found
   function Get_Priority_Delta (Id : in Search_Id) return Natural is
   begin
      return Object.Get_Priority_Delta (Id);
   end Get_Priority_Delta;

   ------------------------------------------------------------------------
   -- Get_Size                                                           --
   ------------------------------------------------------------------------
   function Get_Size (Id : in Search_Id; Family : in Hit_Family.Family_Id) return Types.File_Size is
   begin
      return Object.Get_Size (Id, Family);
   end Get_Size;

   ------------------------------------------------------------------------
   -- Pause_Search                                                       --
   ------------------------------------------------------------------------
   -- No error if already paused
   procedure Pause_Search (Id : in Search_Id) is
   begin
      Object.Pause_Search (Id);
   end Pause_Search;

   ------------------------------------------------------------------------
   -- Restore                                                            --
   ------------------------------------------------------------------------
   procedure Restore is
      use Ada.Streams.Stream_IO;
      use Agpl;
      Cron : Chronos.Object;
      Doc  : Xml.Document;
   begin
      if Safe_File.Exists_For_Reading (Globals.Data_Folder & Save_File) then
         Doc := Xml.Parse (Safe_File.Get_Real_Name (Globals.Data_Folder & Save_File));
         declare
            Searches   : Xml.Node_Array := Xml.Get_All (Doc, "search");
            New_Search : Search.Object_Access;
         begin
            for I in Searches'Range loop
               New_Search := Search.Create_From_XML (Searches (I));
               Object.Insert_Search (New_Search);
               Network.List.Create_Search (Search.Get_Id (New_Search));
               if Search.Get_Paused (New_Search) then
                  Network.List.Set_Search_Paused (Search.Get_Id (New_Search), true);
               end if;
            end loop;
         end;
         Trace.Log ("Searches restored in " & Chronos.Image (Cron), Trace.Debug);
      end if;
   end Restore;

   ------------------------------------------------------------------------
   -- Resume_Search                                                      --
   ------------------------------------------------------------------------
   -- No error if already running
   procedure Resume_Search (Id : in Search_Id) is
   begin
      Object.Resume_Search (Id);
   end Resume_Search;

   ------------------------------------------------------------------------
   -- Set_Expanded                                                       --
   ------------------------------------------------------------------------
   procedure Set_Expanded (
      Id : in Search_Id; Family : in String; Expanded : in Boolean := true) is
   begin
      Object.Set_Expanded (Id, Family, Expanded);
   end Set_Expanded;

   ------------------------------------------------------------------------
   -- Set_Priority                                                       --
   ------------------------------------------------------------------------
   procedure Set_Priority (Id : Search_Id; Priority : in Priorities) is
   begin
      Object.Set_priority (Id, Priority);
   end Set_Priority;

   ------------------------------------------------------------------------
   -- Start                                                              --
   ------------------------------------------------------------------------
   -- Start to run the whole thing
   procedure Start is
   begin
      Restore;
   end Start;

   ------------------------------------------------------------------------
   -- Stop                                                               --
   ------------------------------------------------------------------------
   procedure Stop is
   begin
      Object.Save (With_Hits => Globals.Options.G2_Search_SaveHits);
   end Stop;

   ------------------------------------------------------------------------
   -- Http_Report                                                        --
   ------------------------------------------------------------------------
   -- Creates the Http dataset.
   procedure Http_Report (Data : out Sort_Handler.Data_Set) is
   begin
      Object.Report (Data);
   end Http_Report;

   ------------------------------------------------------------------------
   -- Http_Report_Search                                                 --
   ------------------------------------------------------------------------
   -- Http report for the selected search
   procedure Http_Report_Search (Data : out Sort_Handler.Data_Set) is
   begin
      Object.Http_Report_Search (Data);
   end Http_Report_Search;

   function  Http_Report_Search return Templates_Parser.Translate_Table is
      use Templates_Parser;
   begin
      return Object.Http_Report_Search;
   end Http_Report_Search;

   ------------------------------------------------------------------------
   -- Http_Report_Set_Search                                             --
   ------------------------------------------------------------------------
   -- Set the search to be reported in subsequent calls
   procedure Http_Report_Set_Search (Id : in Search_Id) is
   begin
      Object.Http_Report_Set_Search (Id);
   end Http_Report_Set_Search;

   ------------------------------------------------------------------------
   -- Misc info                                                          --
   ------------------------------------------------------------------------
   -- Get_Running_Searches --
   function Get_Running_Searches return Natural is
   begin
      return Object.Get_Running_Searches;
   end Get_Running_Searches;

   -- Get_Paused_Searches --
   function Get_Paused_Searches  return Natural is
   begin
      return Object.Get_Paused_Searches;
   end Get_Paused_Searches;

   -- Get_New_Hits --
   function Get_New_Hits         return Natural is
   begin
      return Object.Get_New_Hits;
   end Get_New_Hits;

end Adagio.Searches.Manager;
