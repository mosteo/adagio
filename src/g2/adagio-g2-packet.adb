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
--  $Id: adagio-g2-packet.adb,v 1.7 2004/02/29 20:36:42 Jano Exp $

with Adagio.Debug;
with Adagio.Exceptions; use Adagio.Exceptions;
with Adagio.Memory_stream_constrained;
with Adagio.Misc;
with Adagio.Network.Endian;
with Adagio.Socket;
with Adagio.Trace;
with Adagio.XML;

with Agpl.Protected_Values.Integers;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Unchecked_conversion;
with Ada.Unchecked_deallocation;

with Adagio.Unicode;

package body Adagio.G2.Packet is

   use type Agpl.Types.Ustrings.Ustring;

   Stat_num_children : constant String := "Network - G2 - Alive packets";

   use type Safe_child.Object;

   package Child_vector is new Agpl.Dynamic_vector (Object);

   -- Lazyness:
   function V (this : in Object) return Child_access
      renames Safe_child.Value;

   function Length (this : in Children_vector.Object) return Integer
      renames Children_vector.Length;

   -- Returns the head kind of a packet : Head ("/PI/PO") returns "PI"
   function Head (S : String) return String;
   -- Returns the tail kind of a packet : Head ("/PI/PO") returns "/PO"
   function Tail (S : String) return String;

   -- Create a packet with given name and payload:
   -- Returns an allocated object
   function Create (Name : in String; Payload : in String := "")
      return Object is
      C : Child_access := new Child;
      P : Object;
   begin
      C.Type_name             := B (Name);
      C.Control_byte.Name_len := Name'Length - 1;
      C.Payload := U (Payload);

      Safe_child.Bind (P, C);

      return P;
   end Create;

   -- Serialization of that control byte:
   procedure Write (
      Stream       : access Streams.Root_stream_type'Class;
      this         : Control_byte_type) is

      function To_byte is new
         Unchecked_conversion (Control_byte_type, Byte);

   begin
      Byte'Write (Stream, To_byte (this));
   end Write;

   -- Fully reads a packet from a stream. Allocates it.
   procedure Read(
      Stream       : access Streams.Root_stream_type'Class;
      this         : out Control_byte_type) is

      function To_control is new
         Unchecked_conversion (Byte, Control_byte_type);
      B : Byte;

   begin
      Byte'Read (Stream, B);
      this := To_control (B);
   end Read;

   -- Serialization of packets:
   procedure Serialize(
      Stream       : access Streams.Root_stream_type'Class;
      this         : Object) is
   begin
      raise Unimplemented;
   end Serialize;

     -- Fully reads a packet from a stream. Allocates it.
   procedure Read(
      Stream       : access Streams.Root_stream_type'Class;
      this         : out Packet.Object) is
   begin
      raise Unimplemented;
   end Read;

   -- Debug only:
   procedure Initialize (This : in out Child) is
   begin
      null;
--      Statistics.Object.Update (
--         Stat_num_children,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (1));
   end Initialize;

   -- Recursively frees any children
   procedure Finalize (this : in out Child) is
   begin
      for N in 1 .. Length (this.Children) loop
         Free (this.Children.Vector (N));
      end loop;
--      Statistics.Object.Update (
--         Stat_num_children,
--         Statistics.Integers.Increment'Access,
--         Statistics.Integers.Create (-1));
   end Finalize;

   -- Delete a packet:
   procedure Free (this : in out Child_access) is
      procedure Delete is new Unchecked_deallocation(Child, Child_access);
   begin
      Delete (this);
   end Free;

   ------------------------------------------------------------------------
   -- Clone                                                              --
   ------------------------------------------------------------------------
   -- Deep copy: Clone a child and all its children
   function Clone (this : in Child_access) return Child_access is
      Result : Child_access := new Child;
   begin
      Result.Control_byte := This.Control_byte;
      Result.Len          := This.Len;
      Result.Type_name    := This.Type_name;
      Result.Payload      := This.Payload;
      Result.Arrival_time := This.Arrival_time;
      Result.Children     := This.Children; -- To get the same length
      -- Now clone children
      for N in 1 .. Length (This.Children) loop
         Result.Children.Vector (N) := Clone (This.Children.Vector (N));
      end loop;

      return Result;
   end Clone;

   -- Adds a child to a packet:
   -- May raise exception if too many childs
   procedure Add_child (
      Parent    : in Child_access;
      New_child : in out Child_access) is
   begin
      -- Check null:
      if New_child = null then
         return;
      end if;

      -- Add it:
      begin
         Parent.Control_byte.Compound_flag := true;
         if Length (Parent.Children) = MAX_CHILDREN then
            raise Constraint_error;
         end if;
         if S (New_child.Type_name) = "TO" then
            Children_vector.Insert (
               Parent.Children, New_child, Parent.Children.Vector'First);
         else
            Children_vector.Append (Parent.Children, New_child);
         end if;
         New_child := null;
      exception
         when Constraint_error =>
            Trace.Log("G2.Packet.Add_child: Dropping child " &
               "(max reached). Max is" & Integer'Image (MAX_CHILDREN), Trace.Warning);
            Trace.Log("Packet: " & S (Parent.Type_Name), Trace.Warning);
            Free (New_child);
      end;
   end Add_child;

   -- Full size of a packet, including:
   --    control byte, len, name, children, payload.
   function Full_size (this : in Child_access) return Natural is
   begin
      return
         1 +                              -- Control_byte
         this.Control_byte.Len_len +      -- Len
         this.Control_byte.Name_len + 1 + -- Name
         this.Len;                        -- Children + Payload
   end Full_size;

   -- Full size of children packets of a packet:
   function Children_size (this : in Child_access) return Natural is
      Size : Natural := 0;
   begin
      for n in 1 .. Length (this.Children) loop
         Size := Size + Full_size (this.Children.Vector (n));
      end loop;

      return Size;
   end Children_size;

   -- Makes an object into child of another one.
   -- May raise exception if too many childs
   -- Doesn't check for duplicates.
   -- Check null additions (no effect).
   procedure Add_child (Parent: in Object; New_child : in out Object) is
      C : Child_access := V (New_child);
   begin
      -- Prevent releasing:
      Safe_child.Unbind (New_child);
      New_child := Null_packet;
      -- Add normally:
      Add_child (V (Parent), C);
   end Add_child;

   -- Root name of a packet:
   function Name (this : in Object) return String is
   begin
      return BStrings.To_string (V (this).Type_name);
   end Name;

   -- Root payload as a string:
   function Payload (this : in Object) return String is
   begin
      return S (V (this).Payload);
   end Payload;

   -- Arrival time:
   function Arrival_time (this : in Object) return Calendar.Time is
   begin
      return V (this).Arrival_time;
   end Arrival_time;

   -- Big endian?
   function Big_endian (this: in Object) return Boolean is
   begin
      return V (this).Control_byte.Big_endian;
   end Big_endian;

   -- Hex representation of a packet:
   function To_hex (this : in Object; Interleaving : String := " ")
      return String is

      function To_char is new Unchecked_conversion
        (Control_byte_type, Character);

      Result : UString;

      C : Child_access := V (this);

   begin

      -- Control byte:
      Result := U (Misc.To_hex (To_char (C.Control_byte))) & Interleaving;

      -- Length:
      Result := Result & Misc.To_string (C.Len) & Interleaving;

      -- Name:
      Result := Result & S (C.Type_name) & Interleaving;

      -- Payload:
      declare
         Payload : String := S (C.Payload);
      begin
         for n in Payload'Range loop
            Result := Result & Misc.To_hex(Payload (n));
         end loop;
      end;

      return S (Result);

   end To_hex;

   -- Enumeration of children in a packet:
   function To_Text (
      This : in Object; Show_Payloads : in Boolean := false) return String
   is

      function To_Text (This : in Child_Access; Show_Payloads : in Boolean) return String
      is
         Line : Ustring := U (S (This.Type_Name));
      begin
         if Show_Payloads then
            Asu.Append (Line, ":");
            Asu.Append (Line, This.Payload);
         end if;
         for n in 1 .. Children_vector.Length (this.Children) loop
            Asu.Append (Line, "->");
            Asu.Append (Line, To_Text (this.Children.Vector (n), Show_Payloads));
         end loop;
         Asu.Append (Line, ";");

         return S (Line);
      end To_Text;

   begin
      return To_Text (Safe_Child.Value (This), Show_Payloads);
   end To_Text;

   -- Is_a: says if a packet qualifies for some kind.
   -- Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Object; Kind : in String) return Boolean is
   begin
      return Is_a (V (this), Kind);
   end Is_a;

   -- Is_a: says if a packet qualifies for some kind.
   -- Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Child_access; Kind : in String) return Boolean is
   begin
      if Kind = "" then
         return true;
      end if;
      if Head (Kind) /= S (this.Type_name) then
         return false;
      end if;
      -- Check all children
      declare
         T : String := Tail (Kind);
      begin
         if T = "" then
            return true;
         else
            for N in 1 .. Length (this.Children) loop
               if Is_a (this.Children.Vector (N), T) then
                  return true;
               end if;
            end loop;
            -- Not found, hence:
            return false;
         end if;
      end;
   end Is_a;

   -- Get a given child from an object
   -- Name is in the form "xx/yy/zz"
   -- Must be unique
   function Get_child (this : in Child_access; Name : in String)
      return Child_access is
      H     : String := Head ("/" & Name);
      T     : String := Tail (Name);
      Found : Boolean := false;
      C     : Child_access;
   begin
      -- Search current one:
      for N in 1 .. Length (this.Children) loop
         if S (this.Children.Vector (N).Type_name) = H then
            C := this.Children.Vector (N);
            if Found then
               raise Constraint_error;
            else
               Found := true;
            end if;
         end if;
      end loop;

      -- Null if not found
      if C = null then
         return null;
      end if;

      if T = "" then
         return C;
      else
         return Get_child (C, T (T'First + 1 .. T'Last));
      end if;
   end Get_child;

   -- Returns a child as an object
   -- Will raise Constraint_error if that child appears multiple times
   -- Name is in the form "xx/yy/zz"
   function Get_child (this : in Object; Name : in String) return Object is
      C      : Child_access;
      Result : Object;
   begin
      C := Get_child (V (this), Name);
      if C = null then
         Result := Null_packet;
      else
         -- Duplicate it to not have safe and unsafe refs to the same child:
         Safe_child.Bind (Result, Clone (C));
      end if;

      return Result;
   end;

   ------------------------------------------------------------------------
   -- Get_children                                                       --
   ------------------------------------------------------------------------
   -- Get children of a given type. Inmediate depth only.
   function Get_children (this : in Object; Name : in String)
   return Object_array is
      -- R : Child_vector.Object (First => 1);
      R : Object_Array (1 .. MAX_CHILDREN);
      N : Natural := 0;
      C : Object;
   begin
      for I in 1 .. Length (V (this).Children) loop
         if S (V (this).Children.Vector (I).Type_name) = Name then
            Safe_child.Bind (C, Clone (V (this).Children.Vector (I)));
            N     := N + 1;
            R (N) := C; -- May raise Constraint_Error
            -- Child_vector.Append (R, C);
            -- if Child_Vector.Length (R) > MAX_CHILDREN then
            --   raise Constraint_Error;
            -- end if;
         end if;
      end loop;
      return R (R'First .. N);
      -- return Object_array (R.Vector (1 .. Child_vector.Length (R)));
   end Get_children;

   -- Returns the expected length of child + payload
   -- That's the length of CHILDREN + \0 SEPARATOR IF NEEDED + PAYLOAD
   function Computed_length (this : in Child) return Natural is
      Result : Natural := 0;
      Len    : Natural := Length (this.Children); -- The number of children,
                                                  -- not its size.
   begin
      -- Simple packet
      if Len = 0 and then this.Payload = Null_payload then
         return 0;
      elsif Len > 0 and then this.Payload = Null_payload then
      -- Only children, we can adjust the size without trailing \0
         for N in 1 .. Len loop
            Result := Result + Full_length (this.Children.Vector (N).all);
         end loop;
      elsif Len = 0 and then this.Payload /= Null_payload then
         return ASU.Length (this.Payload);
      else
      -- Children plus payload:
         for N in 1 .. Len loop
            Result := Result + Full_length (this.Children.Vector (N).all);
         end loop;
         Result := Result + 1;            -- The \0 marker for end-of-childs
         Result := Result + ASU.Length (this.Payload);   -- Payload
      end if;

      return Result;
   end Computed_length;

   -- Returns the expected full length (control + len_len + name_len + etc)
   -- That's the FULL LENGTH OF THIS CHILD, HEADERS PLUS ITS CHILDREN
   function Full_length (this : in Child) return Natural is
   begin
      return
         1 +
         Len_len (Computed_length (this)) +
         Computed_length (this) +
         this.Control_byte.Name_len + 1;
   end Full_length;

   -- Returns the expected full length (control + len_len + name_len + etc)
   function Full_length (this : in Object) return Natural is
   begin
      return Full_length (V (this).all);
   end Full_length;

   -- Return the number of bytes neccesaries to carry this number:
   function Len_len (N : Natural) return Natural is
   begin
      if N = 0 then
         return 0;
      elsif N < 2 ** 8 then
         return 1;
      elsif N < 2 ** 16 then
         return 2;
      else
         return 3;
      end if;
   end;

   -- Writing to stream.
   procedure Write (
      Stream : access Streams.Root_stream_type'Class; this : in Object) is
   begin
      Write (Stream, V (this).all);
   end Write;

   -- Writing to stream.
   procedure Write (
      Stream : access Streams.Root_stream_type'Class; this : in Child) is

      Control_byte : Control_byte_type := this.Control_byte;
      Length       : Natural := Computed_length (this);
      Length_array : Network.Endian.Byte_array :=
         Network.Endian.Convert (Length, Control_byte.Big_endian);
         -- Preserve endianness, in case payload needs it.

   begin
      Control_byte.Len_len := Length_array'Length;
      -- Watch to not send a \0:
      if Control_byte.Len_len = 0 then
         Control_byte.Compound_flag := true;
      end if;

      -- Send control_byte
      Control_byte_type'Write (Stream, Control_byte);
      -- Send length (maybe an empty array, so nothing is sent).
      Network.Endian.Byte_array'Write (Stream, Length_array);
      -- Send name
      String'Write (Stream, S (this.Type_name));
      -- Send childs
      for N in 1 .. Children_vector.Length (this.Children) loop
         Write (Stream, this.Children.Vector (N).all);
      end loop;
      -- Send \0 if needed
      if Control_byte.Compound_flag and then this.Payload /= Null_payload then
         Network.Endian.Byte'Write (Stream, 0);
      end if;
      -- Send payload;
      if this.Payload /= Null_payload then
         String'Write (Stream, S (this.Payload));
      end if;
   end Write;

   -- Atomic writing to a socket stream. It guarantees that the entire
   --    packet is written (or not a byte) in a non-blocking socket stream.
   procedure Atomic_Write (
      Stream  : access Streams.Root_stream_type'Class;
      This    : in     Object;
      Success : out    Boolean)
   is
      use Streams;
      Buffer  : aliased Stream_element_array (
         1 .. Stream_element_offset (Full_length (This)));
      BStream : aliased Memory_stream_constrained.Stream_type;
   begin
      Memory_stream_constrained.Create (
         BStream, Buffer'Address, Buffer'Length);
      Write (Streams.Root_stream_type'Class (Bstream)'access, This);
      Write (Stream.all, Buffer);
      Success := true;
   exception
      when E : Socket.Socket_error =>
         case Socket.Get_error (E) is
            when Socket.Operation_would_block =>
               Success := false;
            when others =>
               raise;
         end case;
   end Atomic_write;

   -- Deep copy: Clone an object and all its children
   function Clone (this : in Object) return Object is
      O : Object;
   begin
      Safe_child.Bind (O, Clone (V (this)));

      return O;
   end Clone;

   ---------------
   -- Utilities --
   ---------------
   -- Returns the head kind of a packet : Head ("/PI/PO") returns "PI"
   function Head (S : String) return String is
   begin
      for N in S'First + 1 .. S'Last loop
         if S (N) = '/' then
            return S (S'First + 1 .. N - 1);
         end if;
      end loop;

      return S (S'First + 1 .. S'Last);
   end Head;

   -- Returns the tail kind of a packet : Head ("/PI/PO") returns "/PO"
   function Tail (S : String) return String is
   begin
      for N in S'First + 1 .. S'Last loop
         if S (N) = '/' then
            return S (N .. S'Last);
         end if;
      end loop;

      return "";
   end Tail;

   -- Create a /UPROD/XML packet with gprofile.xsd conformat payload.
   function Create_UPROD return Packet.Object is
      P : Packet.Object := Packet.Create ("UPROD");
      C : Packet.Object;
   begin
      -- Add XML payload
      C := Packet.Create ("XML",
            Unicode.G2_to_string (
            Xml.Compress (Xml.To_string (
            Xml.Get ("gProfile", Globals.Config))),
               Packet.Big_endian (P)));

      Packet.Add_child (P, C);

      return P;
   end Create_UPROD;

begin
--   Statistics.Object.Set (
--      Stat_num_children, Statistics.Integers.Create (0));
   null;
end Adagio.G2.Packet;
