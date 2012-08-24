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
--  $Id: adagio-http-header.ads,v 1.3 2004/01/21 21:05:37 Jano Exp $

with Ada.Streams;

with Charles.Deques.Unbounded;

use Ada;

package Adagio.Http.Header is

   pragma Elaborate_Body;

   CR: Constant Character:= Character'val(13);
   LF: Constant Character:= Character'val(10);
   HT: Constant Character:= Character'val(9);
   SP: Constant Character:= ' ';

   CRLF: Constant String:= (CR, LF);

   -- Max num of headers allowed:
   Max_headers: Constant:= 1000;
   -- Max length of a header (name + value) allowed:
   Max_header_length: Constant:= 1000;

   -- A single header:
   type Object is 
      record
         Name: UString;
         Value: UString;
      end record;

   type Object_array is array(Positive range <>) of Object;

   -- A complete header set
   type Set is private;

   -- Delete all members:
   procedure Clear(this: out Set);

   -- Add a pair:
   -- If it already exists, will get replaced:
   -- Case insensitive
   procedure Add(this: in out Set; Name, Value: String);

   -- Remove a pair:
   -- Case insensitive
   procedure Delete(this: in out Set; Name: String);

   function Empty_Set return Set;
   pragma Inline (Empty_Set);

   -- Get a value:
   -- Returns "" if not present
   -- Case insensitive
   function Get(this: Set; Name: String) return String;

   -- Get number of headers:
   function Count(this: in Set) return Natural;

   function Is_empty(this: in Set) return boolean;

   -- Get all values as array:
   function Headers(this: Set) return Object_array;

   -- Get/Set the response string:
   function Get_response (this : Set) return String;
   function Get_Response_Code (This : Set) return Natural;
   procedure Set_response (this : in out Set; Response : String);

   -- Parse from a Stream
   -- Will read an initial response line if requested
   -- Will end after reading empty line
   -- Will concatenate line-splitted headers
   -- Will join as comma separated list multiple occurrences of a header
   -- Optionally, will erase any previous headers:
   procedure Parse
     (this          : in out Set;
      Stream        : access Ada.Streams.Root_stream_type'Class;
      Read_response : Boolean := false;
      Clean         : Boolean := false);

   -- Send headers to some stream:
   -- The trailing empty line CRLF is *not* send by default.
   procedure Write
     (this          : in Set;
      Stream        : in out Ada.Streams.Root_stream_type'Class;
      Send_response : Boolean := true;
      Send_crlf     : Boolean := false);

   -- Write headers to some string
   -- The trailing empty line CRLF is not written.
   function Write(
      this          : in Set;
      Send_response : Boolean := true;
      Send_crlf     : Boolean := false) return String;

   function To_String (
      this          : in Set;
      Send_response : Boolean := true;
      Send_crlf     : Boolean := false) return String renames Write;

private 

   Package Set_list is new
      Charles.Deques.Unbounded(Natural, Object, "=");

   type Set is record
      Response : Ustring;                 -- The response
      Data     : Set_list.Container_type; -- The headers
   end record;

end Adagio.Http.Header;
