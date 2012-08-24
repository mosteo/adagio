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

with Adagio.Chronos;
with Adagio.Convert;
with Adagio.Download.Consumer.Sets;
with Adagio.Download.Slot;
with Adagio.Download.Source.Factory;
with Adagio.Download.Source.Maps;
with Adagio.Exceptions;
with Adagio.Globals;
with Adagio.Globals.Options;
with Adagio.Misc;
with Adagio.Trace;
with Adagio.Xml;
with Adagio.Xml.Utils;

with Agpl.Safe_File;
with Agpl.Strings;
with Agpl.Types.Ustrings;

with Gnat.Directory_Operations.Iteration;

with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Streams;
with Ada.Streams.Stream_Io;
with Ada.Strings.Hash;
use  Ada;

package body Adagio.Download.Manager is

   use type Source.Object_Access;

   Null_Bmp : Agpl.Bmp.Object;

   ------------------------------------------------------------------------
   -- Aggregator Type                                                    --
   ------------------------------------------------------------------------
   -- Aggregates all info about a download: Slot, Sources, Consumers.
   type Aggregator is new Ada.Finalization.Limited_Controlled with record
      Slot      : Download.Slot.Object_Access;
      Consumers : Download.Consumer.Sets.Set;
      Sources   : Download.Source.Maps.Map;
   end record;
   type Aggregator_Access is access all Aggregator;

   ------------------------------------------------------------------------
   -- Source_Prioritizer                                                 --
   ------------------------------------------------------------------------
   -- Should be given in least - greatest period order.
   type Source_Check_Priorities is (High, Medium, Low);
   type Source_Map_Array    is array (Source_Check_Priorities) of Download.Source.Maps.Map;
   type Source_Cursor_Array is array (Source_Check_Priorities) of Download.Source.Maps.Cursor;
   Source_Check_Periods :      array (Source_Check_Priorities) of Duration := (
      Low    => 60.0,
      Medium => 10.0,
      High   => 1.0);

   procedure Finalize (This : in out Aggregator) is
   begin
      raise Program_Error;
   end Finalize;

   package Aggregator_Maps is new Ada.Containers.Hashed_Maps (
      Slot_Id, Aggregator_Access, Slot.Hash, "=", "=");

   ------------------------------------------------------------------------
   -- Safe                                                               --
   ------------------------------------------------------------------------
   protected Safe is
      procedure Add_Source (Id : in Slot_Id; Source : in Download.Source.Object_Access);
      procedure Create_Slot (Hash : in Agpl.Magnet.Object; Secure : in Boolean; Id : out Slot_Id);
      function  Get_Active_Sources_Count (Agg : access Aggregator) return Natural;
      function  Get_Chunk_To_Request (Id : in Slot_Id) return Chunk_Bounds;
      function  Get_Sources_Count (Priority : in Source_Check_Priorities) return Natural;
      function  Get_Progress_Bmp (Id : in Slot_Id) return Agpl.Bmp.Object;
      procedure Http_Report_Downloads (
         Data : out Agpl.Http.Server.Sort_Handler.Data_Set);
      -- Get the next source in a priority to be processed:
      procedure Get_Source_To_Process (
         Priority  : in     Source_Check_Priorities;
         Completed :    out Boolean;
         Src       :    out Source.Object_Access);
      -- Report on the processing of a source.
      procedure Reclassify_Source (
         Src      : in Source.Object_Access;
         Priority : in Source_Check_Priorities;
         Again_In : in Duration;
         Done     : in Boolean);
      procedure Restore;
      procedure Save (Id : in Slot_Id := Null_Slot);
      procedure Switch_Source_Check_Priority (
         Id          : in Source_Id;
         Src         : in Source.Object_Access;
         Prio_Before : in Source_Check_Priorities;
         Prio_After  : in Source_Check_Priorities);
   private
      Slots          : Aggregator_Maps.Map;
      Sources        : Source_Map_Array;
      Source_Cursors : Source_Cursor_Array := (others => Download.Source.Maps.No_Element);
   end Safe;
   protected body Safe is
      ----------------
      -- Add_Source --
      ----------------
      procedure Add_Source (Id : in Slot_Id; Source : in Download.Source.Object_Access) is
         use Aggregator_Maps;
         use Download.Source.Maps;
         use type Download.Source.Object_Access;
         I      : constant Aggregator_Maps.Cursor := Find (Slots, Id);
         Aux    : Download.Source.Object_Access   := Source;
         Src    : Download.Source.Object_Access   := Source;
      begin
         if Source = null then
            Trace.Log ("Download.Manager.Add_Source: " &
               "Dropping null source for download " & To_String (Id),
               Trace.Warning);
         elsif I = Aggregator_Maps.No_Element then
            Trace.Log ("Manager.Add_Source: Missing download: " &
               To_String (Id), Trace.Warning);
            Download.Source.Free (Aux);
         elsif Contains (Element (I).Sources, Download.Source.Get_Id (Src)) then
            Trace.Log ("Manager.Add_Source: Dropping already known source: " &
               To_String (Download.Source.Get_Id (Src)), Trace.Warning);
            Download.Source.Free (Aux);
         else
            -- Insert in the aggregator and in the prioritized queues
            Download.Source.Set_Slot_Id (Source, Id);
            declare
               Pos    : Download.Source.Maps.Cursor;
               Ok     : Boolean;
               Agg    : Aggregator renames Element (I).all;
               Src_Id : constant Source_Id := Download.Source.Get_Id (Source);
            begin
               Insert (Agg.Sources, Src_Id, Source, Pos, Ok);
               pragma Assert (Ok);
               Insert (Sources (High), Src_Id, Source, Pos, Ok);
               pragma Assert (Ok);
            end;
         end if;
      end Add_Source;

      -----------------
      -- Create_Slot --
      -----------------
      procedure Create_Slot (
         Hash   : in     Agpl.Magnet.Object;
         Secure : in     Boolean;
         Id     :    out Slot_Id)
      is
         use Aggregator_Maps;
         New_Agg  : Aggregator_Access  := new Aggregator;
         I        : Cursor;
         Success  : Boolean;
      begin
         New_Agg.Slot := Slot.Create (From => Hash);
         Insert (
            Slots,
            Slot.Get_Id (New_Agg.Slot),
            New_Agg,
            I, Success);
         Id := Slot.Get_Id (New_Agg.Slot);
         pragma Assert (Success);
      end Create_Slot;

      ------------------------------
      -- Get_Active_Sources_Count --
      ------------------------------
      function  Get_Active_Sources_Count (Agg : access Aggregator) return Natural
      is
         use Download.Source.Maps;
         I : Cursor  := First (Agg.Sources);
         N : Natural := 0;
      begin
         while I /= No_Element loop
            if Download.Source.Is_Active (Element (I)) then
               N := N + 1;
            end if;
            Next (I);
         end loop;
         return N;
      end Get_Active_Sources_Count;

      --------------------------
      -- Get_Chunk_To_Request --
      --------------------------
      function  Get_Chunk_To_Request (Id : in Slot_Id) return Chunk_Bounds is
         use Aggregator_Maps;
         I : constant Cursor := Find (Slots, Id);
      begin
         return (First => 1, Last => Slot.Get_Size (Element (I).Slot));
      end Get_Chunk_To_Request;

      ----------------------
      -- Get_Progress_Bmp --
      ----------------------
      function  Get_Progress_Bmp (Id : in Slot_Id) return Agpl.Bmp.Object is
         use Aggregator_Maps;
         I : constant Cursor := Find (Slots, Id);
      begin
         if I = No_Element then
            return Null_Bmp;
         else
            return Slot.Get_Progress_Bmp (Element (I).Slot);
         end if;
      end Get_Progress_Bmp;

      -----------------------
      -- Get_Sources_Count --
      -----------------------
      function  Get_Sources_Count (Priority : in Source_Check_Priorities) return Natural is
         use Download.Source.Maps;
      begin
         return Natural (Length (Sources (Priority)));
      end Get_Sources_Count;

      ---------------------------
      -- Http_Report_Downloads --
      ---------------------------
      procedure Http_Report_Downloads (
         Data : out Agpl.Http.Server.Sort_Handler.Data_Set)
      is
         use Aggregator_Maps;
         use Agpl.Http.Server.Sort_Handler;
         I : Cursor := First (Slots);
      begin
         while I /= No_Element loop
            declare
               Agg : Aggregator_Access renames Element (I);
               Elm : Slot.Object_Access renames Element (I).Slot;
               Row : Data_Row;
            begin
               -- Id
               Append (Row, (
                  U (To_String (Slot.Get_Id (Elm))),
                  U (To_String (Slot.Get_Id (Elm)))));
               -- Name
               Append (Row, (
                  U (Slot.Get_Name (Elm)),
                  U (Slot.Get_Name (Elm))));
               -- Size
               if Slot.Is_Size_Unknown (Elm) then
                  Append (Row, (
                     U ("Unknown"),
                     Rpad (0)));
               else
                  Append (Row, (
                     U (Convert.To_Size (Natural (Slot.Get_Size (Elm)))),
                     Rpad (Natural (Slot.Get_Size (Elm)))));
               end if;
               -- Progress
               Append (Row, (
                  U ("/cgi-bmp?bmp=download_progress&slot_id=" & To_String (Slot.Get_Id (Elm))),
                  U ("")));
               --
               Append (Row, (Null_Ustring, Null_Ustring));
               --
               Append (Row, (Null_Ustring, Null_Ustring));
               -- Sources
               Append (Row, (
                  U (
                     Misc.To_String (Get_Active_Sources_Count (Agg)) & "/" &
                     Agpl.Strings.Trim (
                     Containers.Count_Type'Image (
                        Download.Source.Maps.Length (Agg.Sources)))),
                  Rpad (
                     Natural (
                        Download.Source.Maps.Length (Agg.Sources)))));

               -- Full Row
               Append (Data, Row);
            end;
            Next (I);
         end loop;
      end Http_Report_Downloads;

      ---------------------------
      -- Get_Source_To_Process --
      ---------------------------
      procedure Get_Source_To_Process (
         Priority  : in     Source_Check_Priorities;
         Completed :    out Boolean;
         Src       :    out Source.Object_Access)
      is
         use Download.Source.Maps;

      begin
         Src := null;
         if Source_Cursors (Priority) = No_Element then
            Completed := true;
            Source_Cursors (Priority) := First (Sources (Priority));

            if Source_Cursors (Priority) /= No_Element then
               Src := Element (Source_Cursors (Priority));
               Next (Source_Cursors (Priority));
            end if;
         else
            Completed := false;
            Src := Element (Source_Cursors (Priority));
            Next (Source_Cursors (Priority));
         end if;
      end Get_Source_To_Process;

      -----------------------
      -- Reclassify_Source --
      -----------------------
      procedure Reclassify_Source (
         Src      : in Source.Object_Access;
         Priority : in Source_Check_Priorities;
         Again_In : in Duration;
         Done     : in Boolean)
      is
         use Download.Source.Maps;

         -- New_Priority --
         function New_Priority (This : in Duration) return Source_Check_Priorities;
         pragma Inline (New_Priority);
         function New_Priority (This : in Duration) return Source_Check_Priorities
         is
         begin
            for I in Source_Check_Priorities loop
               if Source_Check_Periods (I) > This then
                  return I;
               end if;
            end loop;
            return Source_Check_Priorities'Last;
         end New_Priority;

      begin
         if Done then
            Remove_Source (Source.Get_Id (Src));
         else
            Trace.Log ("Switching source to priority " &
               Source_Check_Priorities'Image (New_Priority (Again_In)),
               Trace.Always);
            Switch_Source_Check_Priority (
               Source.Get_Id (Src), Src, Priority, New_Priority (Again_In));
         end if;
      end Reclassify_Source;

      -------------
      -- Restore --
      -------------
      procedure Restore is
         package Dirs renames Gnat.Directory_Operations.Iteration;
         use Aggregator_Maps;
         use type Xml.Node;
         Num : Natural := 0;
         procedure Restore_One (Item : in String; Index : in Positive; Quit : in out Boolean) is
            Doc : Xml.Document;
            Agg : Aggregator_Access;
            Ok  : Boolean;
            Pos : Cursor;
         begin
            Doc := Xml.Parse (Item);
            Agg := new Aggregator;

            -- SLOT
            Agg.Slot := Slot.From_Xml (Xml.Get ("slot", Doc));
            Slot.Set_Max_Id (Slot.Get_Id (Agg.Slot));

            Insert (Slots, Slot.Get_Id (Agg.Slot), Agg, Pos, Ok);
            pragma Assert (Ok);

            -- SOURCES
            declare
               Node : Xml.Node;
               I    : Positive := 1;
               Src  : Source.Object_Access;
            begin
               loop
                 Node := Xml.Get ("source", Doc, I);
                 exit when Node = null;
                 I    := I + 1;
                 Src  := Source.Factory.From_Xml (Node);
                 Safe.Add_Source (Slot.Get_Id (Agg.Slot), Src);
               end loop;
            end;

            Xml.Delete (Doc);
            Num := Num + 1;
         exception
            when E : others =>
               Trace.Log ("Download.Manager: Restoring " & Item & ": " & Trace.Report (E), Trace.Warning);
         end Restore_One;
         procedure Loader is new Dirs.Wildcard_Iterator (Restore_One);
      begin
         Trace.Log ("Restoring downloads...", Trace.Debug);
         Loader (S (Globals.Options.Download_Incomplete) & "*.xml");
         Trace.Log ("Download.Manager:" & Num'Img & " downloads restored.", Trace.Informative);
      end Restore;

      ----------
      -- Save --
      ----------
      procedure Save (Id : in Slot_Id := Null_Slot) is
         use Aggregator_Maps;

         procedure Save_Element (I : in Cursor) is
            use Ada.Streams.Stream_Io;
            F   : File_Type;
            Doc : Xml.Document := Xml.From_String ("<download/>");
         begin
            if I = No_Element then
               return;
            end if;

            -- Build XML doc.
            Xml.Add (Doc, Slot.To_Xml (Element (I).Slot.all, Doc));

            -- Saving!
            Agpl.Safe_File.Open (F,
               Name =>
                  S (Globals.Options.Download_Incomplete) & Slot.Get_Name (Element (I).Slot) & "." &
                  To_String (Slot.Get_Id (Element (I).Slot)) &
                  ".xml",
               Mode => Out_File);
            Agpl.Types.Ustrings.Write_To_Stream (Xml.To_Ustring (Doc), Stream (F));
            Agpl.Safe_File.Close (F);

         exception
            when E : others =>
               if Is_Open (F) then
                  Agpl.Safe_File.Close (F);
               end if;
               Trace.Log ("Download.Manager.Save: Saving " & Slot.Get_Name (Element (I).Slot) & ": " &
                  Trace.Report (E), Trace.Error);
         end Save_Element;

      begin
         if Id = Null_Slot then
            Iterate (Slots, Save_Element'Unrestricted_Access);
         else
            Save_Element (Find (Slots, Id));
         end if;
      end Save;

      ----------------------------------
      -- Switch_Source_Check_Priority --
      ----------------------------------
      procedure Switch_Source_Check_Priority (
         Id          : in Source_Id;
         Src         : in Source.Object_Access;
         Prio_Before : in Source_Check_Priorities;
         Prio_After  : in Source_Check_Priorities)
      is
         use Source.Maps;
         Pos     : Cursor;
         Success : Boolean;
      begin
         if Prio_Before /= Prio_After then
            -- Prevent leaving a dangling iterator:
            if Source_Cursors (Prio_Before) = Find (Sources (Prio_Before), Id) then
               Next (Source_Cursors (Prio_Before));
            end if;

            Delete (Sources (Prio_Before), Id);
            Insert (Sources (Prio_After), Id, Src, Pos, Success);
            pragma Assert (Success);
         end if;
      end Switch_Source_Check_Priority;
   end Safe;

   ------------------------------------------------------------------------
   -- Reporter                                                           --
   ------------------------------------------------------------------------
   -- Safe object used to serialize access to sources
   protected Reporter is
      procedure Process (This : in Source.Object_Access; Priority : in Source_Check_Priorities);
      procedure Http_Report_Downloads (Data : out Agpl.Http.Server.Sort_Handler.Data_Set);
   end Reporter;
   protected body Reporter is

      -------------
      -- Process --
      -------------
      procedure Process (This : in Source.Object_Access; Priority : in Source_Check_Priorities) is
         Again_in : Duration;
         Done     : Boolean;
      begin
         Source.Process (This, Done, Again_In);
         Safe.Reclassify_Source (
            This, Priority, Done => Done, Again_In => Again_In);
      exception
         when E : others =>
            Trace.Log ("Download.Manager.Reporter.Process: " &
               Trace.Report (E), Trace.Error);
            Remove_Source (Source.Get_Id (This));
      end Process;

      ---------------------------
      -- Http_Report_Downloads --
      ---------------------------
      procedure Http_Report_Downloads (Data : out Agpl.Http.Server.Sort_Handler.Data_Set) is
      begin
         Safe.Http_Report_Downloads (Data);
      end Http_Report_Downloads;

   end Reporter;

   ------------------------------------------------------------------------
   -- Add_Consumer                                                       --
   ------------------------------------------------------------------------
   -- Adds a consumer for a slot. Only one of each kind is accepted, others
   -- are discarded.
   procedure Add_Consumer (
      Id : in Slot_Id; Consumer : access Download.Consumer.Object'Class)
   is
   begin
      null;
   end;

   ------------------------------------------------------------------------
   -- Add_Source                                                         --
   ------------------------------------------------------------------------
   -- Adds a source for a slot
   -- Repeated Source_Ids are discarded.
   procedure Add_Source (
      Id : in Slot_Id; Source : in Download.Source.Object_Access)
   is
   begin
      Safe.Add_Source (Id, Source);
   end;

   ------------------------------------------------------------------------
   -- Create_Slot                                                        --
   ------------------------------------------------------------------------
   -- Begins a new download slot
   procedure Create_Slot (
      Hash   : in     Agpl.Magnet.Object;
      Secure : in     Boolean;
      Id     :    out Slot_Id)
   is
   begin
      Safe.Create_Slot (Hash, Secure, Id);
      Trace.Log ("Added download: " & Agpl.Magnet.To_String (Hash), Trace.Debug);
   end Create_Slot;

   ------------------------------------------------------------------------
   -- Get_Chunk_To_Request                                               --
   ------------------------------------------------------------------------
   -- Gives the next chunk that should be requested for a file.
   -- To be called by sources.
   function Get_Chunk_To_Request (Id : in Slot_Id) return Chunk_Bounds
   is
   begin
      return Safe.Get_Chunk_To_Request (Id);
   end;

   ------------------------------------------------------------------------
   -- Get_Progress_Bmp                                                   --
   ------------------------------------------------------------------------
   -- Returns a BMP graph with progress status
   function Get_Progress_Bmp (Id : in Slot_Id) return Agpl.Bmp.Object
   is
   begin
      return Safe.Get_Progress_Bmp (Id);
   end;

   ------------------------------------------------------------------------
   -- Get_Speed_Bar                                                      --
   ------------------------------------------------------------------------
   -- Returns a BMP graph with data rates
   function Get_Speed_Bar (Id : in Slot_Id) return Agpl.Bmp.Object
   is
   begin
      return Safe.Get_Progress_Bmp (Id);
   end;

   ------------------------------------------------------------------------
   -- Http_Report_Downloads                                              --
   ------------------------------------------------------------------------
   procedure Http_Report_Downloads (Data : out Agpl.Http.Server.Sort_Handler.Data_Set) is
   begin
      Reporter.Http_Report_Downloads (Data);
   end Http_Report_Downloads;

   ------------------------------------------------------------------------
   -- Remove_Slot                                                        --
   ------------------------------------------------------------------------
   procedure Remove_Slot (Id : in Slot_Id)
   is
   begin
      null;
   end;

   ------------------------------------------------------------------------
   -- Remove_Source                                                      --
   ------------------------------------------------------------------------
   procedure Remove_Source (Source : in Source_Id)
   is
   begin
      null;
   end;

   ------------------------------------------------------------------------
   -- Restore                                                            --
   ------------------------------------------------------------------------
   -- Restores downloads from disk
   procedure Restore is
   begin
      Safe.Restore;
   end Restore;

   ------------------------------------------------------------------------
   -- Saves the downloads to disk                                        --
   ------------------------------------------------------------------------
   -- Optionally a slot id can be provided to only save that one.
   procedure Save (Id : in Slot_Id := Null_Slot) is
   begin
      Safe.Save (Id);
   end Save;

   ------------------------------------------------------------------------
   -- Set_Size                                                           --
   ------------------------------------------------------------------------
   -- Allows a source to provide the size for a slot. If it's unknown it will
   -- be recorded, and if not the size will be checked and in case of mismatch
   -- the source will be dropped.
   procedure Set_Size (Id : in Slot_Id; From : in Source_Id; Size : in Types.File_Size) is
   begin
      null;
   end Set_Size;

   ------------------------------------------------------------------------
   -- Store_Data                                                         --
   ------------------------------------------------------------------------
   -- Store/Process some received data.
   -- To be called by the sources.
   procedure Store_Data (
      Id       : in Slot_Id;
      Data     : in Download.Data.Object'Class)
   is
   begin
      null;
   end;

   ------------------------------------------------------------------------
   -- Task                                                               --
   ------------------------------------------------------------------------
   -- This task performs the checking of sources according to due delays.
   -- It compresses the cycle time to try to minimize CPU usage.
   task type Source_Checker (Priority : Source_Check_Priorities);
   type Source_Checker_Access is access all Source_Checker;
   Checkers : array (Source_Check_Priorities) of Source_Checker_Access;

   task body Source_Checker is
      Current_Delay        : Duration := 0.1;
      Repetitions          : Natural  := 0;  -- Times to do delay before processing.
      Check_Period         : Chronos.Object;
   begin
      -- Main loop
      loop
         loop
            delay Current_Delay;
            exit when Repetitions = 0 or else Globals.Requested_Exit;
            Repetitions := Repetitions - 1;
         end loop;
         exit when Globals.Requested_Exit;
         declare
            Done : Boolean;
            Src  : Source.Object_Access;
         begin
            Safe.Get_Source_To_Process (Priority, Done, Src);
            -- Re-compute delay:
            if Done or else Chronos.Elapsed (Check_Period) > 1.0 then
               Current_Delay :=
                  Source_Check_Periods (Priority) /
                  Duration (Natural'Max (1, Safe.Get_Sources_Count (Priority)));
               if Current_Delay > 1.0 then
                  Repetitions   := Natural (Current_Delay) - 1;
                  Current_Delay := 1.0;
               end if;
            end if;
            -- Process it if valid source:
            if Src /= null then
               Reporter.Process (Src, Priority);
            end if;
         exception
            when E : others =>
               Trace.Log ("Download.Manager.Checker " & Priority'Img & ": " &
                  Trace.Report (E), Trace.Error);
         end;
      end loop;
      Trace.Log ("Download.Manager.Checker " &
         Agpl.Strings.To_Lower (Priority'Img) & " exited.", Trace.Informative);
   end Source_Checker;

begin

   -- Overriden Source Check periods:
   declare
      Nodes : Xml.Node_Array := Xml.Get_All ("download/SourceCheckPeriod", Globals.Config);
   begin
      for I in Nodes'Range loop
         Source_Check_Periods
            (Source_Check_Priorities'Value (
               Xml.Get_Attribute (Nodes (I), "priority", "error"))) :=
         Misc.Parse_Duration (Xml.Get_Value (Nodes (I), "error"));
      end loop;
   end;

   -- Create source checkers:
   if Globals.Options.Download_Active then
      for I in Source_Check_Priorities loop
         Checkers (I) := new Source_Checker (I);
      end loop;
   end if;
end Adagio.Download.Manager;
