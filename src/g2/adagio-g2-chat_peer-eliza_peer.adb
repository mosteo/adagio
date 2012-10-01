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
--  $Id: adagio-g2-chat_peer-eliza_peer.adb,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Adagio.Globals.Options;
with Adagio.Misc;

package body Adagio.G2.Chat_peer.Eliza_peer is

   use  Ada.Calendar;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   function Create (From : in Socket.Object; Request : in Http.Header.Set) 
      return Object_access is
      Peer : Object_access;
   begin
      if not Misc.Contains (Misc.To_lower (
         Http.Header.Get (Request, "Accept")), Chat_peer.Chat_content_type)
      then
         raise Chat_peer.Unknown_protocol;
      end if;

      -- Creation
      Peer := new Object;
      Peer.Socket := From;
      Peer.Link   := Socket.Stream (From);
      if not Globals.Options.Chat_enabled then
         Peer.Status := Rejecting;
      end if;
      return Peer;
   end Create;

   ------------------------------------------------------------------------
   -- Do_chat                                                            --
   ------------------------------------------------------------------------
   -- Normal chat workings
   procedure Do_chat (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type) 
   is
      Phrase  : String (1 .. 1024);
      Last    : Natural;
      Success : Boolean;
   begin
      case This.Mood is
         when Waiting =>
            Read_phrase (This, Phrase, Last);
            if Last > 0 then
               This.Its_answer := U (Phrase (1 .. Last));
               This.Mood := Answering;
               if not This.Saluted then
                  This.Our_answer := 
                     U (Misc.To_lower (Eliza.Bot.Get_greeting (This.She)));
                  This.Saluted    := true;
               else
                  This.Our_answer := 
                     U (Misc.To_lower (
                        Eliza.Bot.Get_response (
                           This.She'Access, S (This.Its_answer))));
               end if;
               This.Our_answer_time := 
                  Clock + Delay_for_phrase (S (This.Our_answer));
            end if;
         when Answering =>
            -- Ignore phrases in between:
            Read_phrase (This, Phrase, Last);
            -- Wait while "we" type ;)
            if Clock >= This.Our_answer_time then
               Send_phrase (This, S (This.Our_answer), Success);
               if Success then
                  This.Mood := Waiting;
                  if Eliza.Bot.Is_done (This.She) then
                     Context.Is_done := true;
                  end if;
               end if;
            end if;
      end case;
   end Do_chat;

   ------------------------------------------------------------------------
   -- Do_timeout                                                         --
   ------------------------------------------------------------------------
   -- Send a timeout message.
   procedure Do_timeout (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type) 
   is
      Success : Boolean := false;
   begin
      Send_phrase (
         This, "You're a quiet person. I'm leaving. Good bye...", Success);
      Context.Is_done := Success;
   end Do_timeout;

   ------------------------------------------------------------------------
   -- Delay for phrase                                                   --
   ------------------------------------------------------------------------
   -- Returns how many time a person would employ in typing a phrase.
   -- KPM: Keystrokes per minute.
   function Delay_for_phrase (
      Phrase : in String; 
      KPM    : in Positive := 350) return Duration 
   is
      P : constant Duration := Duration (KPM) / 60.0;
   begin
      return Duration (Phrase'Length) / P;
   end Delay_for_phrase;

end Adagio.G2.Chat_peer.Eliza_peer;
