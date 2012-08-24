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
--  $Id: adagio-xml.ads,v 1.3 2004/01/21 21:05:43 Jano Exp $
-- Helper functions for the XML/Ada DOM Component

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with DOM.Core.Nodes;

package Adagio.XML is

   pragma Elaborate_body;

   subtype Node is Dom.Core.Node;
   subtype Document is Node; -- NOT the document. The ROOT element.
   type Node_access is access all Node;
   type Document_access is access all Document;

   type Node_array is array(integer range <>) of Node;

   Null_node: Constant Node:= null;

   -- Read a XML file and stores it in memory;
   function Parse (File : String) return Document;

   -- Parses and XML string (Latin1 accepted)
   function From_string (Data : in String) return Document;

   -- Converts a document to a string representation.
   -- Latin1 format
   function To_string (Doc : in Document) return String;
   function To_Ustring (Doc : in Document) return Ustring;

   -- Compress a string representation stripping whitespaces and control chars
   function Compress (this : in String) return String;

   -- Retrieve the nth element in the hierarchy that matches the path.
   -- Separator is '/'
   -- Path example: "skins/html/default"
   -- Paths must be relatives to parent
   -- If unique is true, then an exception is raised if more than a value 
   --    is found at the lower level.
   -- An exception is raised if any intermediate level has multiple matches.
   -- If ain't enough leaves, or doesn't exists some intermediate level,
   --    Null_node is returned.
   -- Null_node can be returned if some node along the path is non-existant.
   function Get
     (Path: String; 
      Parent: Node; 
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return Node;

   -- Returns childrens with given name (first is 1):
   -- * means any name.
   function Get_all(Parent: Node; Name: String:= "*") return Node_array;
   function Get_all(Path: String; Parent: Node) return Node_array;

   -- Insertion functions:
   -- They return the inserted node.
   function Add(Parent: Node; Name: String) return Node;
   function Add(Parent: Node; Path: String; Name: String) 
      return Node;

   -- Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node);

   -- Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node;

   -- Deletion:
   -- Removes the Node from its location in a doc, and frees all memory
   -- Children are also removed.
   procedure Delete (Item : in Node);

   -- Name
   function Get_name(Item: Node) return String
      renames Dom.Core.Nodes.Node_name;

   -- Value
   function Get_value(Item: Node; Default_value: String) return String;

   -- This function returns the number of nodes found at the given level
   -- If some intermediate floor is multiple, then exception is raised.
   function Length(Path: String; Parent: Node) return Natural;

   -- Returns the number of childs of a node with given name.
   -- * means any name.
   function Length(Parent: Node; Name: String:= "*") return Natural;

   -- Functions to read attributes.
   -- If they can't find the attribute, will search it as a child text node.
   -- If that fails too, default value will be returned.
   -- I.e. <http port="80" /> is equal to
   --  <http><port>80</port></http>
      
   -- Read an attribute
   -- Semantix like above, if don't defined return default.
   function Get_attribute
     (Path: String; 
      Attr: String;
      Parent: Node; 
      Default_value: String;
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return String;

   generic
      type Number is range <>;
   function Get_numeric_attribute_from_path
     (Path: String; 
      Attr: String;
      Parent: Node; 
      Default_value: Number;
      Pos: Positive:= 1;
      Unique: boolean:= false)
      return Number;

   -- Read an attribute from a node:
   function Get_attribute
     (Item: Node; 
      Attr: String; 
      Default_value: String) 
      return String;

   generic
      type Number is range <>;
   function Get_numeric_attribute_from_node
     (Item: Node; 
      Attr: String; 
      Default_value: Number) 
      return Number;

   -- Set an attribute
   procedure Set_attribute(Item: Node; Attr, Value: String);
   -- Will fail if lower node isn't unique.
   procedure Set_attribute(Path: String; Attr, Value: String; Parent: Node);

   -- Converts a utf8 string into Latin1
   -- This function starts from the encoding used by the Sax.Reader.
   -- If that is changed, this should change accordingly.
   function To_latin1 (this : in String) return String;

private

   -- Tokenizes a string, returning the first token.
   -- All string is returned if tokenizer is not found.
   function String_head(s: String; Separator: Character:= '/')
      return String;

   -- Returns the head or "" if no tokenizer found.
   function String_tail(s: String; Separator: Character:= '/')
      return String;

   -- These are like above, but from right to left:
   -- I.e: Tail(abc/de/fg) = abc/de ; Head = fg
   function String_head_reverse(s: String; Separator: Character:= '/')
      return String;

   function String_tail_reverse(s: String; Separator: Character:= '/')
      return String;

   -- Reverses a string:
   function Reverse_string(s: String) return String;

end Adagio.XML;
