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
--  $Id: adagio-g2-chat_peer-eliza_peer.ads,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Adagio.Connect.Peer;
with Adagio.Http.Header;
with Eliza.Bot;

with Ada.Calendar;
use  Ada;

package Adagio.G2.Chat_peer.Eliza_peer is

   type Object is new Chat_Peer.Object with private;
   type Object_access is access all Object;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   -- May raise exception if some problems arise.
   function Create (From : in Socket.Object; Request : in Http.Header.Set) 
      return Object_access;

   ------------------------------------------------------------------------
   -- Do_chat                                                            --
   ------------------------------------------------------------------------
   -- Normal chat workings
   procedure Do_chat (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Do_timeout                                                         --
   ------------------------------------------------------------------------
   -- Send a timeout message.
   procedure Do_timeout (
      This    : in out Object; 
      Context : in out Connect.Peer.Context_type);

   ------------------------------------------------------------------------
   -- Delay for phrase                                                   --
   ------------------------------------------------------------------------
   -- Returns how many time a person would employ in typing a phrase.
   -- KPM: Keystrokes per minute.
   function Delay_for_phrase (
      Phrase : in String; 
      KPM    : in Positive := 350) return Duration;

private

   type Moods is (Answering, Waiting);

   type Object is new Chat_Peer.Object with record
      Our_Answer_time : Calendar.Time := Calendar.Clock; -- When to respond
      Saluted         : Boolean := false;

      She             : aliased Eliza.Bot.Object; -- It's a she ;)
      Mood            : Moods := Waiting;
      Its_answer      : Ustring;
      Our_answer      : Ustring;
   end record;

end Adagio.G2.Chat_peer.Eliza_peer;
