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

--  Facilities to pack separate entities as a single file.

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Gnat.Os_lib; use Gnat.Os_lib;

package body Filepack is

   use Entry_map;

   type File_entry_access is access all File_entry;

   function Element_ref is new Generic_element (File_entry_access);

   procedure Check_opened (This : in Object);

   -- Overriden primitives:
   procedure Read(
      Stream : in out Stream_type;
      Item   : out    Stream_Element_Array;
      Last   : out    Stream_Element_Offset) is
   begin
      Check_opened (Stream.Parent.all);
      if Stream.Parent.Mode /= Read and then Stream.Parent.Mode /= Both then
         raise Access_mode_error;
      end if;
      if Stream.Parent.Remaining = 0 then
         raise Index_out_of_bounds;
      end if;
      Read (
         Stream.Parent.FS.all,
         Item (
            Item'First ..
            Stream_element_offset'Min (
               Item'Last,
               Item'First + Stream_element_offset (
                  Stream.Parent.Remaining - 1))),
         Last);
   end Read;

	procedure Write(
      Stream : in out Stream_type;
      Item   : in     Stream_Element_Array) is
   begin
      Check_opened (Stream.Parent.all);
      if Stream.Parent.Mode /= Write and then Stream.Parent.Mode /= Both then
         raise Access_mode_error;
      end if;
      Write (Stream.Parent.FS.all, Item);
      Stream.Parent.Total_data := Stream.Parent.Total_data + Item'Length;
      Stream.Parent.Current.Size := Stream.Parent.Current.Size + Item'Length;
   end Write;

   ------------------------------------------------------------------------
   -- Check_opened                                                        --
   ------------------------------------------------------------------------
   procedure Check_opened (This : in Object) is
   begin
      if not This.Bound then
         raise Filepack_not_bound;
      end if;
      if not This.Found then
         raise File_not_open;
      end if;
   end Check_opened;

   ------------------------------------------------------------------------
   -- Create_index                                                       --
   ------------------------------------------------------------------------
   procedure Create_index (This : in out Object) is
      use Stream_io;
      Stream : Stream_access := Stream_io.Stream (This.F);
      procedure Read_entry is
         E      : File_entry;
      begin
--         Text_io.Put_line ("Index: " & Stream_io.Count'Image (Index (This.F)));
--         Text_io.Put_line ("Size : " & Stream_io.Count'Image (Size (This.F)));
         E.Header_offset := Positive (Index (This.F));
         Unbounded_string'Read (Stream, E.Name);
--         Text_io.Put_line ("Read: " & To_string (E.Name));
         Natural'Read (Stream, E.Size);
--         Text_io.Put_line ("Size:" & E.Size'Img);
         Boolean'Read (Stream, E.Deleted);
--         Text_io.Put_line ("Del :" & E.Deleted'Img);
         E.Offset := Positive (Index (This.F));
         -- Skip data:
         if E.Size > 0 then
            Set_index (This.F, Index (This.F) + Positive_Count (E.Size));
         end if;
         This.Total_data := This.Total_data + E.Size;
         if not E.Deleted then
            Insert (This.Index, To_string (E.Name), E);
         else
            This.Wasted_data :=
               This.Wasted_data + E.size;
         end if;
      end Read_entry;
   begin
      This.Wasted_data := 0;
      while not End_of_file (This.F) loop
         Read_entry;
      end loop;
   end Create_index;

   ------------------------------------------------------------------------
   -- Bind                                                               --
   ------------------------------------------------------------------------
   -- Opens or creates a filepack. It's opened always in Append mode.
   procedure Bind (This : out Object; Name : in String) is
      use Stream_io;
      Success : Boolean;
   begin
      if not Is_regular_file (Name) and then
         Is_regular_file (Name & ".tmp")
      then
         Rename_file (Name & ".tmp", Name, Success);
         if not Success then
            raise Unknown_error;
         end if;
      end if;
      if Is_regular_file (Name) then
         Open (This.F, Name => Name, Mode => In_file);
      else
         Create (This.F, Name => Name, Mode => Out_file);
         Close (This.F);
         Open (This.F, Name => Name, Mode => In_file);
      end if;
      Create_index (This);
      Close (This.F);
      This.Bound := true;
      This.Found := false;
      This.Name  := To_unbounded_string (Name);
      This.Stream_access := This.Stream'Unchecked_Access;
   end Bind;

   ------------------------------------------------------------------------
   -- Unbind                                                             --
   ------------------------------------------------------------------------
   -- Cuts the link with a filepack.
   procedure Unbind (This : in out Object) is
      use Stream_io;
   begin
      if This.Bound then
         if this.Found then
            Close (This);
         end if;
         Clear (This.Index);
         This.Bound := false;
      end if;
   end Unbind;

   ------------------------------------------------------------------------
   -- Open                                                               --
   ------------------------------------------------------------------------
   -- Opens a file inside the filepack. May raise File_not_found
   -- Opening a new file don't need to close the previous.
   procedure Open (This : in out Object; Name : in String) is
      I : Iterator_type;
      use Stream_io;
   begin
      if not This.Bound then
         raise Filepack_not_bound;
      end if;
      if This.Found then
         Close (This);
      end if;
      I := Find (This.Index, Name);
      if I = Back (This.Index) then
         raise File_not_found;
      else
         Open (This.F, Name => To_string (This.Name), Mode => In_file);
         This.Current   := Element (I);
         This.Mode      := Read;
         This.Found     := true;
         This.Remaining := This.Current.Size;
         Set_index (This.F, Positive_count (This.Current.Offset));
         This.FS := Stream (This.F);
      end if;
   end Open;

   ------------------------------------------------------------------------
   -- Write_entry                                                        --
   ------------------------------------------------------------------------
   -- Writes a file entry at current stream position:
   procedure Write_entry (
      Stream : in Stream_io.Stream_access; E : in File_entry) is
   begin
      Unbounded_string'Write (Stream, E.Name);
      Natural'Write (Stream, E.Size);
--      Text_io.Put_line ("Writing Size:" & E.Size'Img);
      Boolean'write (Stream, E.Deleted);
--      Text_io.Put_line ("Writing Dele:" & E.Deleted'Img);
   end Write_entry;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Create a new file inside the filepack.
   -- May raise File_already_exists.
   -- The created file remains opened.
   procedure Create (This : in out Object; Name : in String) is
      use Stream_io;
   begin
      if not This.Bound then
         raise Filepack_not_bound;
      end if;
      -- Close another previous open.
      if This.Found then
         Close (This);
      end if;
      if Is_in (Name, This.Index) then
         raise File_already_exists;
      end if;
      Open (This.F, Name => To_string (This.Name), Mode => Append_file);
      This.Current.Name    := To_unbounded_string (Name);
      This.Current.Deleted := false;
      This.Current.size    := 0;
      Set_index (This.F, Size (This.F) + 1);
      This.Current.Header_offset := Natural (Index (This.F));
      Write_entry (Stream (This.F), This.Current);
      This.Current.Offset := Natural (Index (This.F));
      This.Mode  := Write;
      This.Found := true;
      This.FS    := Stream (This.F);
      Insert (This.Index, Name, This.Current);
   end Create;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   -- Marks the opened file as deleted.
   procedure Delete (This : in out Object) is
      use Stream_io;
   begin
      Check_opened (This);
      This.Current.Deleted := true;
      Close (This.F);
      Open  (This.F, Name => To_string (This.Name), Mode => Append_file);
      Set_index (This.F, Positive_count (This.Current.Header_offset));
      Write_entry (Stream (This.F), This.Current);
      Close (This.F);
      This.Found := false;
      This.Wasted_data := This.Wasted_data + This.Current.Size;
      Delete (This.Index, To_string (This.Current.Name));
   end Delete;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   -- Marks the named file as deleted.
   procedure Delete (This : in out Object; Name : in String) is
   begin
      Open   (This, Name);
      Delete (This);
   end Delete;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   -- Commits changes in a file opened for writing.
   procedure Close (This : in out Object) is
      use Stream_io;
   begin
      Check_opened (This);
      if This.Mode = Write or else This.Mode = Both then
         Set_index (This.F, Positive_count (This.Current.Header_offset));
         Write_entry (Stream (This.F), This.Current);
         Flush (This.F);
         Insert (This.Index, To_string (This.Current.Name), This.Current);
      end if;
      Close (This.F);
      This.Found := false;
   end Close;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   -- Says if a certain file is in the filepack
   function Contains (This : in Object; Name : in String) return Boolean is
   begin
      return Is_in (Name, This.Index);
   end Contains;

   ------------------------------------------------------------------------
   -- Is_open                                                            --
   ------------------------------------------------------------------------
   -- Says if some file is open
   function Is_open (This : in Object) return Boolean is
   begin
      return This.Found;
   end Is_open;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   -- Sets the starting position. A file must have been opened.
   -- May raise Index_out_of_bounds or File_not_open
   procedure Set_index (This : in out Object; Index : in Positive) is
      use Stream_io;
   begin
      Check_opened (This);
      if This.Mode /= Read then
         raise Access_mode_error;
      end if;
      if Index > This.Current.Size + 1 then
         raise Index_out_of_bounds;
      end if;
      Set_index (This.F, Positive_count (This.Current.Offset + Index - 1));
   end Set_index;

   ------------------------------------------------------------------------
   -- Size                                                               --
   ------------------------------------------------------------------------
   -- Gets the size of the currently opened file (not the filepack)
   -- May raise File_not_open
   function Size (This : in Object) return Natural is
   begin
      Check_opened (This);
      return This.Current.Size;
   end Size;

   ------------------------------------------------------------------------
   -- End_of_file                                                        --
   ------------------------------------------------------------------------
   -- Says if the current opened underlying file has reached EOF
   function End_of_file (This : in Object) return Boolean is
      use Stream_io;
   begin
      Check_opened (This);
      return Index (This.F) >
         Positive_count (This.Current.Offset + This.Current.Size - 1);
   end End_of_file;

   ------------------------------------------------------------------------
   -- Stream                                                             --
   ------------------------------------------------------------------------
   -- Gets an stream for the current opened file.
   -- If the file was opened, only read is allowed.
   -- If the file was created, only writing is allowed.
   function Stream (This : in Object) return
      Ada.Streams.Stream_io.Stream_access is
   begin
      Check_opened (This);
      return This.Stream_access;
   end Stream;

   ------------------------------------------------------------------------
   -- Wasted                                                             --
   ------------------------------------------------------------------------
   -- Returns the per one wasted proportion due to deleted files
   function Wasted (This : in Object) return Wasted_percent is
   begin
      if not This.Bound then
         raise Filepack_not_bound;
      end if;
      if This.Total_data = 0 then
         return 1.0;
      else
         return Float (This.Wasted_data) / Float (This.Total_data);
      end if;
   end Wasted;

   ------------------------------------------------------------------------
   -- Purge                                                              --
   ------------------------------------------------------------------------
   -- Recreates the filepack to achieve a 0.0 of wasted space.
   -- The current file will be closed.
   procedure Purge (This : in out Object) is
      use Stream_io;
      Success : Boolean;
      Alias   : Object;
   begin
      if not This.Bound then
         raise Filepack_not_bound;
      end if;
      if This.Found then
         close (This);
      end if;
      declare
         Alias_name : String := To_string (This.Name) & ".tmp";
         F      : File_type;
         Sin    : Stream_access;
         Sout   : Stream_access;
         I      : Iterator_type := First (This.Index);
         E      : File_entry;
         Buffer : Stream_element_array (1 .. 1024);
         Last   : Stream_element_offset;
		Pragma Unreferenced( F );
      begin
         if Is_regular_file (Alias_name) then
            Delete_file (Alias_name, Success);
            if not Success then
               raise Unknown_error;
            end if;
         end if;
         Bind (Alias, Alias_name);
         while I /= Back (This.Index) loop
            E := Element (I);
            Open   (This, To_string (E.Name));
            Create (Alias, To_string (E.Name));
            Sin  := Stream (This);
            Sout := Stream (Alias);
            while not End_of_file (This) loop
               Read  (Sin.all,  Buffer, Last);
               Write (Sout.all, Buffer (Buffer'First .. Last));
            end loop;
            I := Succ (I);
         end loop;
         Unbind (Alias);
         Unbind (This);
         Delete_file (To_string (This.Name), Success);
         if not Success then
            raise Unknown_error;
         end if;
         Rename_file (Alias_name, To_string (This.Name), Success);
         if not Success then
            raise Unknown_error;
         end if;
      end;
      -- The end: rebinding
      Bind (This, To_string (This.Name));
   end Purge;

   ------------------------------------------------------------------------
   -- Mark_deletable                                                     --
   ------------------------------------------------------------------------
   -- To do a selective purge, this procedure marks files as deletables.
   -- No name implies all files.
   -- See Mark_not_deletable
   procedure Mark_deletable (This : in out Object; Name : in String := "") is
      I : Iterator_type := First (This.Index);
   begin
      if Name /= "" then
         I := Find (This.Index, Name);
         Element_ref (I).Deletable := true;
      else
         while I /= Back (This.Index) loop
            Element_ref (I).Deletable := true;
            I := Succ (I);
         end loop;
      end if;
   end Mark_deletable;

   ------------------------------------------------------------------------
   -- Mark_not_deletable                                                 --
   ------------------------------------------------------------------------
   -- Marks a file (or all) as not candidate for deleting
   procedure Mark_not_deletable (
      This : in out Object; Name : in String := "") is
      I : Iterator_type := First (This.Index);
   begin
      if Name /= "" then
         I := Find (This.Index, Name);
         Element_ref (I).Deletable := false;
      else
         while I /= Back (This.Index) loop
            Element_ref (I).Deletable := false;
            I := Succ (I);
         end loop;
      end if;
   end Mark_not_deletable;

   ------------------------------------------------------------------------
   -- Delete_marked                                                      --
   ------------------------------------------------------------------------
   -- This effectively deletes all marked files.
   procedure Delete_marked (This : in out Object) is
      I : Iterator_type := First (This.Index);
      N : Iterator_type;
   begin
      while I /= Back (This.Index) loop
         N := Succ (I);
         if Element (I).Deletable then
            Delete (This, To_string (Element (I).Name));
         end if;
         I := N;
      end loop;
   end Delete_marked;

   ------------------------------------------------------------------------
   -- Export                                                             --
   ------------------------------------------------------------------------
   -- Exports a file from the filepack, giving an open for read File_type.
   -- If no name is supplied, the file will be temporary.
   procedure Export (
      This    : in out Object;
      Name    : in     String;
      To      : in out Stream_io.File_type;
      To_name : in String := "") is
      use Stream_io;
      Buffer  : Stream_element_array (1 .. 1024);
      Last    : Stream_element_offset;
      Sin,
      Sout    : Stream_access;
   begin
      Open (This, Name);
      Open (To, Mode => Out_file, Name => To_name);
      Sin  := Stream (This);
      Sout := Stream (To);
      while not End_of_file (This) loop
         Read  (Sin.all, Buffer, Last);
         Write (Sout.all, Buffer (1 .. Last));
      end loop;
      Close     (This);
      Set_mode  (To, In_file);
      Set_index (To, 1);
   end Export;

end Filepack;
