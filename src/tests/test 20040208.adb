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

with Adagio.Buffered_stream;
with Adagio.Decoupled_file_stream;
with Adagio.File;
with Adagio.Globals;
with Adagio.Hash;
with Adagio.Http;
with Adagio.Http.Header;
with Adagio.Os;
with Adagio.Os.Memory;
with Adagio.G2.Packet;
with Adagio.G2.Packet.Parsing;
with Adagio.G2.Packet.Queue;
with Adagio.G2.Transceiver;
with Adagio.Memory_stream_unconstrained;
with Adagio.Safe_access;
with Adagio.Socket;
with Adagio.Trace;
with Adagio.Ip_address;
with Adagio.Xml;
--with Zlib.Streams;
--with Zlib.Streams.Extra;
--with Adagio.Zutil;
use  Adagio;
with Circular_stream;
with Event_queue;
with Expressions_evaluator;
with Test_aux;

with Ada.Calendar; 
use  Ada.Calendar;
with Ada.Real_time;
with Ada.Streams;
with Ada.Numerics.Float_random;
use  Ada;
with Text_io; 
use  Text_io;

with Ada.Streams.Stream_io;

procedure Test is

   Rounds : Natural;

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
      B  : String := (1 .. 548 => 'a');
   begin
      Socket.Create_datagram (S);
      Socket.Bind (S, "127.0.0.1", 5555);
      Transceiver.Start (Tr, S'unrestricted_access, Q'unrestricted_access);
      -- Send packets like crazy:
      A := Ip_address.Sock_addr ("127.0.0.1", 40410);
      P := Packet.Create ("PI", B);
      I.Udp_destination := A;
      I.Udp_safe := true;
      I.Packet := P;
      for K in 1 .. 100 loop
      for J in 1 .. 100 loop 
         Transceiver.Send (Tr, I);
         Q.Clear;
         delay 0.025;
      end loop;
      delay 3.0;
      end loop;
   end Thrower;

   procedure Test_xml is 
      use Xml;
      D : Document;
      Size : Natural;
      Doc  : String := "<heap><pile><junk>asfdsdfasdf</junk></pile></heap>";
   begin
      delay 5.0;
      Size := os.memory.heap_usage;
      for N in 1 .. 10000 loop
         D := From_string (Doc);
         Delete (D);
--         Put_line (
--            "Pass" & N'Img & 
--            " used" & Natural'Image (os.Memory.Heap_usage));
      end loop;
      Put_line ("Delta memory is" & 
         Natural'Image (os.memory.heap_usage - Size));
   end Test_xml;

   procedure Test_clone is
      Size : Natural;
      use G2.Packet;
   begin
      delay 5.0;
      Size := os.memory.heap_usage;
      declare
         P : Object := Create ("blah");
         C : Object := Create ("Blhi");
         D : Object;
      begin
         for N in 1 .. 1000 loop
            D := Clone (P);
         end loop;
         Put_line ("Delta memory is" & 
            Natural'Image (os.memory.heap_usage - Size));
         Add_child (P, C);
         for N in 1 .. 1000 loop
            D := Clone (P);
         end loop;
      end;
      Put_line ("Delta memory is" & 
         Natural'Image (os.memory.heap_usage - Size));
   end Test_clone;

   procedure Test_event is
      Q : Event_queue.Object (1024 * 64);
      use Event_queue;
      use Ada.Real_time;
      use Test_aux;
      use Ada.Numerics.Float_random;
      E : Event_type;
      G : Generator;
   begin
      Reset (G);
      for N in 1 .. Rounds loop
         Create (Q, E, Real_time.Clock + 
            Real_time.To_time_span (Duration (Random (G))),
                 action_ctx'access, ctx'(Context_type with I => N));
      end loop;
      while not Is_empty (Q) loop
         delay 0.1;
      end loop;
      delay 2.0;
      Shutdown (Q);
   end Test_event;

   procedure Test_xml2 is
   begin
      Put_line ("Defaul xml is " &
         Xml.Get_attribute ("globals/config/foobar", "snafu", 
            Globals.Config, "tralara"));
   end;

   procedure Test_buffered is
      B : Buffered_stream.Buffered_stream (65536);
      D : Decoupled_file_stream.Decoupled_file_stream;
      package DFS renames Decoupled_file_stream;
      package BS renames Buffered_stream;
      Buf : Streams.Stream_element_array (1 .. 1024);
      Lst : Streams.Stream_element_offset;
   begin
      DFS.Get_decoupled_file_stream (D, "C:/san_test.tmp");
      BS.Get_buffered_stream (B, D'unrestricted_access);
      BS.Read (B, Buf, Lst);
   end Test_buffered;
   
   procedure Test_controlled is      
      use Test_aux;
      use Test_aux.Safe_thing;
      use Test_aux.Safe_safe_thing;
      ta : Thing_access := new Thing;
   begin
      declare
         T : Safe_thing_access := new Safe_thing.Object;
         TT : Safe_safe_thing.Object;
      begin
         Bind (T.all, ta);
         Bind (TT, T);
      end;
      delay 1.0;
   end Test_Controlled;

   procedure Test_circular is
      use Circular_stream;
      C : Stream_type (Size => 80);
      R : Streams.Stream_element_array (1 .. 3);
      W : Streams.Stream_element_array (1 .. 5);
      L : Streams.Stream_element_offset;
   begin
      for N in W'range loop
         W (N) := Streams.Stream_element'Val (N);
      end loop;
      for N in 1 .. 100 loop
         Write (C, W);
         Read (C, R, L);
         for N in 1 .. L loop
            Put_line ("Read:" & Streams.Stream_element'Image (R (N)));
         end loop;
         Put_line ("***");
      end loop;
   end Test_circular;

   procedure Test_expr is
      package Eval is new Expressions_evaluator (float);
      F : Float;
   begin
      for N in 1 .. 100000 loop
         declare
            E : Eval.Expressions;
         begin
            E := Eval.Create ("f = -100");
            F := Eval.Evaluate (E, Eval.F);
            Eval.Destroy (E);
         end;
      end loop;
   end Test_expr;

   procedure Test_linux is
      S : Socket.Object;
   begin
      Put_line ("Testing non-blocking...");
      Socket.Create_stream (S);
      declare
	 L : Test_aux.Listener;
      begin
      	 delay 1.0;
         Socket.Set_blocking_io (S, false);
         Socket.Connect (S, "127.0.0.1", 11111);
      exception
         when E : Socket.Socket_error =>
            Put_line ("Connect [nonblocking]: " & Trace.Report (E));
            loop
               delay 0.1;
               if Socket.Is_writable (S) then
	          String'Write (Socket.Stream (S), "GET / HTTP/1.1");
                  Put_line ("Connection successful [nonblocking]");
                  exit;
               elsif Socket.Connection_failed (S) or not Socket.Is_alive (S)
               then
                  Put_line ("Connection failed [nonblocking]");
                  exit;
               end if;
	       Put_line ("Waiting...");
            end loop;
      end;
      Socket.Close (S);
   end Test_linux;

   procedure Test_tth is
   begin
      for N in 1 .. Rounds loop
         declare
            F : File.Object;
         begin
            F := File.Create ("c:\test\e9728k.txt");
            File.Compute_tth (F, Hash.Fast);
            File.Free_tth_bytes (F);
         end;
      end loop;
   end Test_tth;

   procedure Test_download is
      Head : Http.Header.Set;
      So   : Socket.Object;
      X    : Character;
      T, F, R : Natural := 0;
   begin
      Http.Header.Set_response (Head, 
      "GET /uri-res/N2R?urn:sha1:RWBWADPLKXXNQGL6XNGFF7D3AT2PC6OE HTTP/1.1");
      Http.Header.Add (Head, "User-agent", "agiotest");
      Http.Header.Add (Head, "X-Nick", "agiotest");
      Http.Header.Add (Head, "Range", "bytes=1-");
      Socket.Create_stream (So);
      Socket.Connect (So, "127.0.0.1", 24610);
      Http.Header.Write (Head, Socket.Stream (So).all, true, true);
      loop
         if Socket.Is_alive (So) then
            T := T + 1;
         else
            F := F + 1;
         end if;
         if Socket.Available (So) > 0 then
            for N in 1 .. Socket.Available (So) loop
               Character'Read (Socket.Stream (So), X);
               R := R + 1;
            end loop;
            Put_line (
               "Read:" & R'Img & "; Alive:" & T'Img & "; Dead:" & F'Img);
         end if;
      end loop;
   end Test_download;

   ------------------------------------------------------------------------
   -- Crawler                                                            --
   ------------------------------------------------------------------------
   procedure Test_crawler is
      use G2;
      Tr : Transceiver.Object;
      S  : Socket.Object;
      Q  : Packet.Queue.Object;
      I, J, K : Packet.Queue.Item_type;
      P  : Packet.Object;
      P2 : Packet.Object;
      A  : Socket.Sock_addr_type;
      B  : String := (1 .. 548 => 'a');
   begin
      Socket.Create_datagram (S);
      Socket.Bind (S, "127.0.0.1", 5555);
      Transceiver.Start (Tr, S'unrestricted_access, Q'unrestricted_access);
      -- Send packets like crazy:
      A := Ip_address.Sock_addr ("69.47.171.104", 10879);
      P := Packet.Create ("CRAWLR");
      P2 := Packet.Create ("PI");
      I.Udp_destination := A;
      I.Udp_safe := true;
      I.Packet := P;
      K := I;
      K.Packet := P2;
      Transceiver.Send (Tr, I);
      Transceiver.Send (Tr, K);
      delay 1.0;
      for N in 1 .. Rounds loop
         if not Q.Is_empty then
            Q.Get (J);
            Packet.Parsing.Trace_tree (J.Packet);
         end if;
         delay 1.0;
      end loop;
      Put_line ("No more packets.");
      Transceiver.Shutdown (Tr);
      Put_line ("Transceiver shut down.");
   end Test_crawler;

   Start : Time := Clock;
   Size  : Natural;
   Iter  : String (1 .. 80);
   Last  : Natural;
begin
   Size := os.memory.heap_usage;
   Put ("Iterations: ");
   Get_line (Iter, Last);
   Rounds := Natural'Value (Iter (1 .. Last));
   Put_line ("Starting test with" & Rounds'Img & " iterations...");

--   Test_chat;
--   Test_browse;
--   Thrower;
--   Test_g2packet;
--   Test_xml;
--   Test_clone;
--   Test_event;
--   Test_deflate;
--   Test_xml2;
--   Test_buffered;
--   Test_controlled;
--   Test_circular;
--   Test_zstream;
--   Test_expr;
--   Test_linux;
--   Test_z2;
--   Make_deflated;
--   Test_tth;
--   Test_download;
   Test_crawler;

   Put_line ("Test done in" & Duration'Image (Clock - Start) & " seconds");
   Put_line ("Delta memory is" & 
      Natural'Image (os.memory.heap_usage - Size));
   OS.Kill_me;
exception
   when e : others =>
      Put_line (Trace.Report (E));
end Test;
