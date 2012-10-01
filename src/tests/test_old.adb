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

with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver;
with Adagio.Http.Header;
with Adagio.Http.Header.Parser;
with Adagio.Ip_address;
with Adagio.Memory_stream_unconstrained;
with Adagio.OS;
with Adagio.Socket;
with Adagio.Zutil;
use  Adagio;

with HashTree;
with TigerTree;

with Gnat.Sockets;
use  Gnat;

with Ada.Calendar; use Ada.Calendar;
with Text_io; use Text_io;
with Ada.Streams;
with Ada.Streams.Stream_io;

procedure Test is

      ------------------------------------------------------------------------
   -- Test_chat                                                          --
   ------------------------------------------------------------------------
   procedure Test_chat is
      use type G2.Packet.Object;
      S : Socket.Object;
      L : Socket.Stream_access;
      Pi, Po, Paux: G2.Packet.Object;
      procedure Rcv (P : out G2.Packet.object) is
         Pr : G2.Packet.Parsing.Object;
      begin
         G2.Packet.Parsing.Create (Pr, S);
         loop
            G2.Packet.Parsing.Check (Pr, Result => P);
            exit when P /= G2.Packet.Null_packet;
         end loop;
      end Rcv;
      H : Http.Header.Set;
      use Http.Header;
      HP : Parser.Object (1024);
   begin
      Socket.Create_stream (S);
      Socket.Connect (S, "127.0.0.1", 36765);
      L := Socket.Stream (S);

      Set_response (H, "CHAT CONNECT/0.2");
      Add (H, "accept", "application/x-gnutella2");
      Add (H, "User-agent", "Shareaza");
      Write (H, L.all, true, true);
      while not Parser.Completed (HP) loop
         Parser.Check (HP, S);
      end loop;
      Set_response (H, "CHAT/0.2 200 OK");
      Add (H, "accept", "application/x-gnutella2");
      Add (H, "content-encoding", "application/x-gnutella2");
      Add (H, "User-agent", "Shareaza");
      Write (H, L.all, true, true);
      
      Po := G2.Packet.Create ("UPROC");
      G2.Packet.Write (L, Po);
      Rcv (Pi);
      Put_line ("Rcv: " & G2.Packet.Name (Pi));
      Po := G2.Packet.Create ("CHATREQ");
      Paux := G2.Packet.Create ("USERGUID", "0123456789012345");
      G2.Packet.Add_child (Po, Paux);
      G2.Packet.Write (L, Po);
      Rcv (Pi);
      Put_line ("Rcv: " & G2.Packet.Name (Pi));
   end Test_chat;

   ------------------------------------------------------------------------
   -- Test_browse                                                        --
   ------------------------------------------------------------------------
   procedure Test_browse is
      use type G2.Packet.Object;
      Start : Time := Clock;
      S : Socket.Object;
      L : Socket.Stream_access;
      Pi, Po, Paux: G2.Packet.Object;
      procedure Rcv (P : out G2.Packet.object) is
         Pr : G2.Packet.Parsing.Object;
      begin
         G2.Packet.Parsing.Create (Pr, S);
         loop
            G2.Packet.Parsing.Check (Pr, Result => P);
            exit when P /= G2.Packet.Null_packet;
         end loop;
      end Rcv;
      H : Http.Header.Set;
      use Http.Header;
      HP : Parser.Object (1024);
      use Ada.Streams;
   begin
      Socket.Create_stream (S);
      Socket.Connect (S, "127.0.0.1", 36765);
      L := Socket.Stream (S);

      String'Write (L, 
      "GET / HTTP/1.1" & CRLF &
      "Accept: application/x-gnutella2" & CRLF &
      "User-Agent: Adagio" & CRLF &
      "Connection: close" & CRLF &
      "Host: 127.0.0.1:40410" & CRLF & 
      "Accept-Encoding: deflate" & CRLF &
      CRLF);
      while not Parser.Completed (HP) loop
         Parser.Check (HP, S);
      end loop;
      Parser.Get_headers (HP, H);
      Put_line ("Received: " & Write (H));
      declare
         Len : Natural := Natural'value (Get (H, "content-length"));
         data : Stream_element_array (1 .. Stream_element_offset (Len));
         use G2;
      begin
         Stream_element_array'read (L, data);
         declare
            Result : aliased Stream_element_array := 
               Zutil.Inflate (Data);
            M : aliased Memory_stream_unconstrained.Stream_type (
               Result'access);
            use Memory_stream_unconstrained;
            P : Packet.Object;
         begin
            -- Read packets:
            while Index (M) < Result'Length loop
               P := Packet.Parsing.From_stream (M'access);
               Put_line ("PACKET");
               Packet.Parsing.Trace_tree (P);
            end loop;
         end;
      end;
      return;
      Rcv (Pi);
      Put_line (G2.Packet.Name (Pi));
      Rcv (Pi);
      Put_line (G2.Packet.Name (Pi));
   end Test_browse;

   ------------------------------------------------------------------------
   -- Thrower                                                            --
   ------------------------------------------------------------------------
   procedure Thrower is
      use G2;
      Tr : Transceiver.Object;
      S  : Socket.Object;
      Q  : Packet.Queue.Object;
      I  : Packet.Queue.Item_type;
      P  : Packet.Object;
      A  : Socket.Sock_addr_type;
      B  : String := (1 .. 2048 => 'a');
   begin
      Socket.Create_datagram (S);
      Socket.Bind (S, "127.0.0.1", 5555);
      Transceiver.Start (Tr, S'unrestricted_access, Q'unrestricted_access);
      -- Send packets like crazy:
      A := Ip_address.Sock_addr ("127.0.0.1", 40410);
      P := Packet.Create ("AGIOtest", B);
      I.Udp_destination := A;
      I.Udp_safe := true;
      I.Packet := P;
      for K in 1 .. 100 loop
      for J in 1 .. 100 loop 
         Transceiver.Send (Tr, I);
         delay 0.1;
         Q.Clear;
      end loop;
      delay 3.0;
      end loop;
   end Thrower;

   procedure Test_g2packet is
   begin
      for n in 1 .. 1000 loop
      declare
         use G2.Packet;
         P1 : Object := Create ("JAR", "JAR");
         P2 : Object := Create ("JAR", "JAR");
         P3 : Object := Create ("JAR", "JAR");
      begin
         put_line ("1");
         Add_child (P1, P2);
         put_line ("2");
         Add_child (P1, P3);
         put_line ("3");
      end;
      Put_line (Natural'Image (os.memory.heap_usage));
      end loop;
   end Test_g2packet;

   procedure Test_zstream is
      C : aliased Circular_stream.Stream_type (10000);
      Z : Zlib.Streams.Stream_type;
      X : Character := 'A';
   begin
      for N in 1 .. 10000 loop
         Zlib.Streams.Create (Z, Zlib.Streams.Out_stream,
            C'Unchecked_access, false);
         Zlib.Streams.Extra.Close_abort (Z);
         Zlib.Streams.Create (Z, Zlib.Streams.Out_stream,
            C'Unchecked_access, true);
         Character'Write (Z'Unrestricted_access, X);
         Circular_stream.Reset (C);
         Zlib.Streams.Extra.Close_abort (Z);
      end loop;
   end Test_zstream;

    procedure Test_z2 is
   begin
      for N in 1 .. Rounds loop
         declare
            Cout, Cin : Circular_stream.Stream_type (4096);
            Zout, Zin : Zlib.Streams.Stream_type;
            C : Character;
         begin
            Zlib.Streams.Create (
               Zout, 
               Zlib.Streams.Out_stream,
               Cout'Unrestricted_access,
               true);
            Zlib.Streams.Create (
               Zin, 
               Zlib.Streams.Out_stream,
               Cin'Unrestricted_access,
               false);
            -- Write compressed
            for N in 1 .. 127 loop
               Character'Write (Zout'Unrestricted_access, Character'Val (N));
               Zlib.Streams.Flush (Zout);
               null;
            end loop;
            -- Uncompress some:
            for N in 1 .. 100 loop
               Character'Read (Cout'Unrestricted_access, C);
               Character'Write (Zin'Unrestricted_access, C);
            end loop;
            Zlib.Streams.Extra.Close_abort (Zin);
            Zlib.Streams.Extra.Close_abort (Zout);
            Circular_stream.Reset (Cin);
            Circular_stream.Reset (Cout);
         exception
            when E : others =>
               Put_line (Trace.Report (E));
         end;
      end loop;
   end Test_z2;

   procedure Make_deflated is
      C : Circular_stream.Stream_type (1024);
      Z : Zlib.Streams.Stream_type;
      P : G2.Packet.Object := G2.Packet.Create ("AGIOtest", "Hello world!");
      X : Character;
      F : Streams.Stream_io.File_type;
   begin
      Zlib.Streams.Create (
         Z, 
         Zlib.Streams.Out_stream,
         C'Unrestricted_access,
         true);
      G2.Packet.Write (Z'Unrestricted_access, P);
      Zlib.Streams.Close (Z);
      Streams.Stream_io.Create (F, Mode => Streams.Stream_io.Out_File,
         Name => "deflated.bin");
      while Circular_stream.Available_read (C) > Natural'(0) loop
         Character'Read (C'Unrestricted_access, X);
         Character'Write (Streams.Stream_io.Stream (F), X);
      end loop;
      Streams.Stream_io.Close (F);
      G2.Packet.Write (C'Unrestricted_access, P);
      Streams.Stream_io.Create (F, Mode => Streams.Stream_io.Out_File,
         Name => "undeflated.bin");
      while Circular_stream.Available_read (C) > Natural'(0) loop
         Character'Read (C'Unrestricted_access, X);
         Character'Write (Streams.Stream_io.Stream (F), X);
      end loop;
      Streams.Stream_io.Close (F);
   end Make_deflated;

   
   procedure Test_deflate is
      use Memory_stream_unconstrained;
      use Ada.Streams.Stream_io;
      use G2;
      F : Ada.Streams.Stream_io.File_type;
      P : Packet.Object;
      B : aliased Ada.Streams.Stream_element_array := (1 .. 1000 => 0);
      L : Ada.Streams.Stream_element_offset;
   begin
      Open (F, mode => in_file, name => "undeflated.bin");
      Put_line ("Reading undeflated");
      P := Packet.Parsing.From_stream (Stream (F));
      Close (F);
      Packet.Parsing.Trace_tree (P);
      Put_line ("Reading deflated");
      Open (F, mode => in_file, name => "deflated.bin");
      Ada.Streams.Read (Stream (F).all, B, L);
      Put_line ("Read" & L'Img & " bytes");
      Close (F);
      declare
         Inflated : aliased Ada.Streams.Stream_element_array :=
            Zutil.Inflate (B (1 .. L));
         S : aliased Memory_stream_unconstrained.Stream_type (
            Inflated'Access);
      begin
         Put_line ("Inflated to" & Inflated'Length'Img & " bytes");
         P := Packet.Parsing.From_stream (S'Access);
      end;
      Packet.Parsing.Trace_tree (P);
   end Test_deflate;

   Start : Time := Clock;
begin
   Put_line ("Starting test...");

--   Test_chat;
--   Test_browse;
--   Thrower;

   Put_line ("Test done in" & Duration'Image (Clock - Start) & " seconds");
   OS.Kill_me;
end Test;
