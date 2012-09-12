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
--  $Id: adagio-xml.adb,v 1.3 2004/01/21 21:05:42 Jano Exp $

-- Helper functions for the XML/Ada DOM Component

with Adagio.Misc;
with Adagio.Os;

with Dom.Core;
with Dom.Core.Documents;
with Dom.Core.Elements;
with Dom.Core.Nodes.Output;
with Dom.Readers;
with Input_sources.File;
with Input_sources.Strings;
with Sax.Readers;
with Unicode.CES.Basic_8bit; use Unicode.CES;
with Unicode.CES.Utf8;

with Ada.Exceptions; use Ada.Exceptions;

package body Adagio.XML is

   package DCD renames Dom.Core.Documents;
   package DCE renames Dom.Core.Elements;
   package DCN renames Dom.Core.Nodes;

   function L (this : in String) return String
      renames Misc.To_lower;

   use type Dom.Core.Node;
   use type Dom.Core.Node_types;

   -- Read a XML file and stores it in memory;
   function Parse(File: String) return Document is
      Tree        : Dom.Readers.Tree_reader;
      File_handle : Input_sources.File.File_input;
      N           : Node;
   begin
      -- Needed for namespaces:
      Sax.Readers.Set_feature (
         Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_prefixes_feature, True);

      Input_sources.File.Open (File, File_handle);
      Input_sources.File.Set_encoding (File_handle,
         Basic_8bit.Basic_8bit_encoding);
      Dom.Readers.Parse      (Tree, File_handle);
      Input_sources.File.Close (File_handle);
      N := DCD.Get_element    (Dom.Readers.Get_tree (Tree));
      DCN.Normalize (N);
      return N;
   exception
      when E : others =>
         Os.Message_box ("Syntax error in XML file: " & File,
            Exception_information (E));
         raise;
   end Parse;

   ------------------------------------------------------------------------
   -- From_string                                                        --
   ------------------------------------------------------------------------
   -- Parses and XML string (Latin1 accepted)
   function From_string (Data : in String) return Document is
      Tree          : Dom.Readers.Tree_reader;
      String_handle : Input_sources.Strings.String_input;
      N             : Node;
   begin
      -- Needed for namespaces:
      Sax.Readers.Set_feature (
         Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_prefixes_feature, True);

      Input_sources.Strings.Open (
         Data'Unrestricted_Access,
         Basic_8bit.Basic_8bit_encoding,
         String_handle);
      Dom.Readers.Parse          (Tree, String_handle);
      Input_sources.Strings.Close (String_handle);
      N := DCD.Get_element        (Dom.Readers.Get_tree (Tree));
      DCN.Normalize (N);
      return N;
   end From_string;

   -- Converts a document to a string representation.
   function To_String (Doc : in Document) return String is
   begin
      return S (To_Ustring (Doc));
   end To_String;

   function To_ustring(Doc : in Document) return UString is
      Ustr : Ustring := U ("<?xml version=""1.0""?>");
   begin
      DCN.Output.Print (Doc, Ustr, Print_XML_PI => false);
      return Ustr;
   end To_ustring;

   -- Compress a string representation stripping excess whitespaces.
   function Compress (this : in String) return String is
      S     : UString := U (this);
      N     : Integer := 1;
      Cut   : Boolean := True;
   begin
      while N <= ASU.Length (S) loop
         if (ASU.Element (S, N) = ' ' and Cut) or else
            Character'Pos (ASU.Element (S, N)) < 31
         then
            -- Any control char is also replaced:
            ASU.Delete (S, N, N);
         else
            Cut := ASU.Element (S, N) = '>';
            N := N + 1;
         end if;
      end loop;

      return To_string (S);
   end Compress;

   function Get
     (Path: String;
      Parent: Node;
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return Node is
      Head: String:= String_head(Path);
      Tail: String:= String_tail(Path);
   begin
      if Path = "" then
         return Parent;
      end if;
      if Parent.Node_type /= Dom.Core.Element_node then
         raise Storage_error;
      end if;
      declare
         Nodes: Node_array:= Get_all(Parent, Head);
      begin
         if Tail = "" then -- Lower level reached
            if Unique and then Nodes'Length > 1 then
               raise Constraint_error;
            elsif Nodes'Length < Pos then
               return Null_node;
            else
               return Nodes(Pos);
            end if;
         end if;

         -- Whe should continue descending the tree if possible:
         if Nodes'Length = 0 then
            return Null_node;
         elsif Nodes'Length > 1 then
            raise Constraint_error;
         end if;

         -- Recursive call:
         return Get(Tail, Nodes(1), Pos, Unique);
      end;
   end Get;

   -- Returns childrens with given name (first is 1):
   -- * means any name.
   function Get_all(Parent: Node; Name: String:= "*") return Node_array is
      num: Natural:= 0;
      Children: DOM.Core.Node_list;
   begin
      if Parent = Null_node then
         return Node_array'((1 .. 0 => Null_node));
      end if;
      -- Let's see how many children this node has:
      Children:= DCN.Child_nodes(Parent);
      for n in 0 .. DCN.Length(Children) - 1 loop
         if DCN.Item(Children, n).Node_type = DOM.Core.Element_node and then
          (Name = "*" or else
            L (DCN.Node_name(DCN.Item(Children, n))) = L (Name))
         then
            num:= num + 1;
         end if;
      end loop;
      -- Now let's create the vector and return it:
      declare
         Result: Node_array(1..num);
         pos: Positive:= 1;
         Item: Node;
      begin
         for n in 0 .. DCN.Length(Children) - 1 loop
            Item:= DCN.Item(Children, n);
            if Item.Node_type = DOM.Core.Element_node and then
              (Name = "*" or else
                  L (DCN.Node_name(Item)) = L (Name)) then
               Result(pos):= Item;
               pos:= pos + 1;
            end if;
         end loop;
         return Result;
      end;
   end Get_all;

   function Get_all(Path: String; Parent: Node) return Node_array is
      New_parent : Node;
   begin
      if Parent = Null_node then
         return Node_array'((1 .. 0 => Null_node));
      end if;
      if Path = "" then
         return Node_array'((1 => Parent));
      end if;
      declare
         Head: String:= String_tail_reverse(Path);
         Tail: string:= String_head_reverse(Path);
      begin
         if Head = "" then
            return Get_all(Parent, Tail);
         else
            New_parent := Get (Head, Parent, Unique => true);
            if New_parent = Null_node then
               return Node_array'((1 .. 0 => Null_node));
            else
               return Get_all(New_parent, Tail);
            end if;
         end if;
      end;
   end Get_all;

 -- Insertion functions:
   -- They return the inserted node.
   function Add(Parent: Node; Name: String) return Node is
   begin
      return DCN.Append_child
        (Parent, DCD.Create_element(DCN.Owner_document(Parent), Name));
   end Add;

   function Add(Parent: Node; Path: String; Name: String)
      return Node is
      Inmediate_parent: Node;
   begin
      Inmediate_parent:= Get(Path, Parent, Unique => true);
      return Add(Inmediate_parent, Name);
   end Add;

   -- Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node) is
      Dummy : Node := DCN.Append_Child (Parent, Child);
	Pragma Unreferenced( Dummy );
   begin
      null;
   end Add;

   -- Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node is
   begin
      return DCD.Create_element (DCN.Owner_document (Parent), Name);
   end Create_Child;

   -- Deletion:
   procedure Delete(Item : in Node) is
      Dummy : Node;
   begin
      -- If it's the root element, we free everything:
      if DCD.Get_element (DCN.Owner_document (Item)) = Item then
         Dummy := DCN.Owner_document (Item);
      else
         Dummy := DCN.Remove_child (DCN.Parent_node (Item), Item);
      end if;
      DCN.Free (Dummy, Deep => true);
   end Delete;

   -- This function returns the number of nodes found at the given level
   function Length(Path: String; Parent: Node) return Natural is
   begin
      if Path = "" then
         return 1;
      else
         declare
            Nodes: Node_array:= Get_all(Path, Parent);
         begin
            return Nodes'length;
         exception
            when Constraint_error =>
               return 0;
         end;
      end if;
   end Length;

   function Length(Parent: Node; Name: String:= "*") return Natural is
   begin
      if Name = "" then
         raise Constraint_error;
      end if;
      declare
          Nodes: Node_array:= Get_all(Parent, Name);
       begin
          return Nodes'length;
       exception
          when Constraint_error =>
             return 0;
       end;
   end Length;

   -- Value
   function Get_value(Item: Node; Default_value: String) return String is
      Nodes: Dom.Core.Node_list:= DCN.Child_nodes(Item);
   begin
      if DCN.Length(Nodes) = 0 then
         return Default_value;
      end if;
      for n in 0 .. DCN.Length(Nodes) - 1 loop
         if DCN.Item(Nodes, n).Node_type = DOM.Core.Text_node then
            return To_Latin1 (DCN.Node_value(DCN.Item(Nodes, n)));
         end if;
      end loop;
      return Default_value;
   end Get_value;

   function Get_attribute
     (Path: String;
      Attr: String;
      Parent: Node;
      Default_value: String;
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return String is
      Item: Node;
   begin
      if Parent = null then
         return Default_value;
      end if;
      Item:= Get (Path, Parent, Pos, Unique);
      declare
         Result: String:= Get_attribute(Item, Attr, Default_value);
      begin
         return To_Latin1 (Result);
      end;
   exception
      when others =>
         return Default_value;
   end Get_attribute;

   function Get_numeric_attribute_from_path
     (Path: String;
      Attr: String;
      Parent: Node;
      Default_value: Number;
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return Number is
   begin
      return Number'Value(
         Get_attribute(Path, Attr, Parent, Number'Image(Default_value),
            Pos, Unique));
   exception
      when others =>
         return Default_value;
   end Get_numeric_attribute_from_path;

   -- Read an attribute from a node:
   function Get_attribute
     (Item: Node;
      Attr: String;
      Default_value: String)
      return String is
   begin
      declare
         Result: String:= DCE.Get_attribute(Item, Attr);
      begin
         if Result /= "" then
            return To_latin1 (Result);
         end if;
         -- Let's try to get a child element with value as attr:
         declare
            Child: Node:= Get (Attr, Item, Unique => true);
         begin
            return Get_value(Child, Default_value);
         end;
      end;
   exception
      when others =>
         return Default_value;
   end Get_attribute;

   function Get_numeric_attribute_from_node
     (Item: Node;
      Attr: String;
      Default_value: Number)
      return Number is
   begin
      return Number'Value(
         Get_attribute(Item, Attr, Number'Image(Default_value)));
   exception
      when others =>
         return Default_value;
   end Get_numeric_attribute_from_node;

   procedure Set_attribute(Item: Node; Attr, Value: String) is
   begin
      DCE.Set_attribute(Item, Attr, Value);
   end Set_attribute;

   procedure Set_attribute(Path: String; Attr, Value: String; Parent: Node) is
      Item: Node:= Get(Path, Parent, Unique => true);
   begin
      if Item = Null_node then
         raise Constraint_error;
      end if;
      Set_attribute(Item, Attr, Value);
   end Set_attribute;

   -- Tokenizes a string, returning the first token.
   -- All string is returned if tokenizer is not found.
   function String_head(s: String; Separator: Character:= '/')
      return String is
   begin
      if s = "" then
         return s;
      end if;
      if s(s'first) = Separator then
         return "";
      end if;
      for n in s'range loop
         if s(n) = Separator then
            return s(s'first .. n - 1);
         end if;
      end loop;
      return s;
   end String_head;

   -- Returns the head or "" if no tokenizer found.
   function String_tail(s: String; Separator: Character:= '/')
      return String is
   begin
      if s = "" then
         return s;
      end if;
      for n in s'range loop
         if s(n) = Separator and n < s'last then
            return s(n + 1 .. s'last);
         end if;
      end loop;
      return "";
   end String_tail;

   -- These are like above, but from right to left:
   -- I.e: Tail(abc/de/fg) = abc/de ; Head = fg
   function String_head_reverse(s: String; Separator: Character:= '/')
      return String is
   begin
      return Reverse_string(String_head(Reverse_string(s), Separator));
   end String_head_reverse;

   function String_tail_reverse(s: String; Separator: Character:= '/')
      return String is
   begin
      return Reverse_string(String_tail(Reverse_string(s), Separator));
   end String_tail_reverse;

   -- Reverses a string:
   function Reverse_string(s: String) return String is
      r : String (s'Range);
   begin
      for n in s'range loop
         r (r'last + (s'first - n)) := s (n);
      end loop;
      return r;
   end Reverse_string;

  -- Converts a utf32 string into Latin1
   function To_latin1 (this : in String) return String is
   begin
      return Basic_8bit.From_utf32 (Utf8.To_utf32 (this));
   end To_Latin1;


end Adagio.XML;
