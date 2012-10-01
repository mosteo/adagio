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
--  $Id: adagio-http-header.adb,v 1.3 2004/01/21 21:05:37 Jano Exp $

with Ada.Characters.Handling;
with Ada.Streams;

use Ada;

package body Adagio.Http.Header is

   package ACH renames Ada.Characters.Handling;

   -- Delete all members:
   procedure Clear(this: out Set) is
   begin
      this.Response := Null_ustring;
      Set_list.Clear(this.Data);
   end Clear;

   -- Add a pair:
   -- If it already exists, will get replaced:
   procedure Add(this: in out Set; Name, Value: String) is
   begin
      if Get(this, Name) = "" then
         -- Addition
         if Count(this) = Max_headers then
            raise Constraint_error;
         end if;
         Set_list.Append (this.Data,
           (Name => To_UString(Name), Value => To_UString(Value)));
      else
         -- Replacement
         for i in Set_list.First(this.Data) .. Set_list.Last(this.Data) loop
            declare
               Target: String renames ACH.To_lower
                 (To_string(Set_list.Element(this.Data, i).Name));
            begin
               if Target = ACH.To_lower(Name) then
                  Set_list.Replace_element(this.Data, i, 
                    (Name => To_ustring(Name), Value => To_ustring(Value)));
                  exit;
               end if;
            end;
         end loop;
      end if;
   end Add;

   -- Remove a pair:
   procedure Delete(this: in out Set; Name: String) is
   begin
      for Pos in Set_list.First(this.Data) .. Set_list.Last(this.Data) loop
         declare
            E: Object renames Set_list.Element(this.Data, Pos);
         begin
            if ACH.To_lower(To_string(E.Name)) = ACH.To_lower(Name) then
               Set_list.Delete(this.Data, Pos);
               exit;
            end if;
         end;
      end loop;
   end Delete;

   -- Get a value:
   function Get(this: Set; Name: String) return String is
   begin
      for Pos in Set_list.First(this.Data) .. Set_list.Last(this.Data) loop
         declare
            E: Object renames Set_list.Element(this.Data, Pos);
         begin
            if ACH.To_lower(To_string(E.Name)) = ACH.To_lower(Name) then
               return To_string(E.Value);
            end if;
         end;
      end loop;
      return "";
   end Get;

   -- Get number of headers:
   function Count(this: in Set) return Natural is
   begin
      return Set_list.Length(this.Data);
   end Count;

   Empty : Set;
   function Empty_Set return Set is
   begin
      return Empty;
   end Empty_Set;

   -- Any header?:
   function Is_empty(this: in Set) return boolean is
   begin
      return This.Response = Null_Ustring and then Count(this) = 0;
   end Is_empty;

   -- Get all values as array:
   function Headers(this: Set) return Object_array is
      Result: Object_array(1 .. Count(this));
      i: integer:= 1;
   begin
      for Pos in Set_list.First(this.Data) .. Set_list.Last(this.Data) loop
         Result(i):= Set_list.Element(this.Data, Pos);
         i:= i + 1;
      end loop;

      return Result;
   end Headers;

   -- Parse from a Stream
   -- Will end after reading empty line
   -- Will concatenate line-splitted headers
   -- Will join as comma separated list multiple occurrences of a header
   -- Optionally, will erase any previous headers:
   procedure Parse
     (this          : in out Set;
      Stream        : access Ada.Streams.Root_stream_type'Class;
      Read_response : Boolean := false;
      Clean         : Boolean := false) is
      Pair: Object;
      c, dummy: Character;
      -- Will return the look-ahead character
      function Peek_char return Character is
      begin
         return c;
      end Peek_char;
      -- Will return the next char and pre-fetch the next.
      -- Take that into account to not read the ending character!
      function Pop_char return Character is
         aux: constant Character:= c;
      begin
         Character'Read(Stream, c);
         return aux;
      end Pop_char;
      -- Will skip any whitespaces / continuation lines
      procedure Skip_whitespaces is
      begin
         loop
            if Peek_char = SP or else Peek_char = HT then
               dummy:= Pop_char;
            elsif Peek_char = CR then
               dummy:= Pop_char;
               if Pop_char /= LF then
                  raise Constraint_error;
               end if;
               if Peek_char /= SP and then Peek_char /= HT then
                  raise Constraint_error;
               end if;
            else
               return;
            end if;
         end loop;
      end Skip_whitespaces;
   begin
      if Clean then
         Clear(this);
      end if;
      -- Get response:
      if Read_response then
         this.Response := U (Get_line (Stream));
      end if;
      -- Init look-ahead char:
      Character'Read(Stream, c);
      -- Skip leading CRLFs
      while Peek_char = CR loop
         dummy:= Pop_char;
         if Pop_char /= LF then
            raise Constraint_error;
         end if;
      end loop;
      -- Main processing
      Main: loop
         -- Cleaning:
         Pair.Name:= To_UString("");
         Pair.Value:= To_UString("");
         -- Complete a header name:
         Name: loop
            exit when Peek_char = ':';
            if ASU.Length(Pair.Name) > Max_header_length then
               raise Constraint_error;
            end if;
            ASU.Append(Pair.Name, Pop_char);
         end loop Name;
         -- Skip ':'
         dummy:= Pop_char;
         -- Skip whitespaces
         Skip_whitespaces;
         -- Read value
         Value: loop
            if Peek_char = CR then
               dummy:= Pop_char; -- CR
               if Pop_char /= LF then
                  raise Constraint_error;
               end if;
               -- Is a continuation line or value completed?
               if Peek_char = SP or else Peek_char = HT then
                  Skip_whitespaces;
               else -- completed value:
                  if Get(this, To_string(Pair.Name)) /= "" then
                     Add(this, To_string(Pair.Name), 
                         Get(this, To_string(Pair.Name)) & ',' & 
                             To_string(Pair.Value));
                  else
                     Add(this, To_string(Pair.Name), To_string(Pair.Value));
                  end if;
                  exit Main when Peek_char = CR;-- Second consecutive empty line
                  exit Value;
               end if;
            end if;
            ASU.Append(Pair.Value, Pop_char);
         end loop Value;
      end loop Main;
      -- Skip the trailing CRLF
      dummy:= Pop_char;
      if Peek_char /= LF then -- Not pop, last character already read
         raise Constraint_error;
      end if;
   end Parse;

   -- Send headers to some stream:
   procedure Write
     (this          : in Set;
      Stream        : in out Ada.Streams.Root_stream_type'Class;
      Send_response : Boolean := true;
      Send_crlf     : Boolean := false) is
   begin
      String'Write (Stream'Access, Write (This, Send_response, Send_crlf));
   end Write;

   -- Write headers to some string
   -- The trailing empty line CRLF is not written.
   function Write(
      this          : in Set;
      Send_response : Boolean := true;
      Send_crlf     : Boolean := false) return String is
      Result: Ustring;
   begin
      if Send_response then
         Result := U (Get_response (This) & CRLF);
      end if;
      for Pos in Set_list.First(this.Data) .. Set_list.Last(this.Data) loop
         declare
            E: Object renames Set_list.Element(this.Data, Pos);
         begin
            Result:= Result & E.Name & ": " & E.Value & CRLF;
         end;
      end loop;

      if Send_crlf then
         Result := Result & CRLF;
      end if;

      return To_string(Result);
   end Write;

   -- Get the response string:
   function Get_Response (this : Set) return String is
   begin
      return S (this.Response);
   end Get_response;

   function Get_Response_Code (This : Set) return Natural is
   begin
      return Natural'Value (Get_Response (This));
   end Get_Response_Code;

   procedure Set_response (this : in out Set; Response : String) is
   begin
      this.Response := U (Response);
   end Set_response;

end Adagio.Http.Header;
