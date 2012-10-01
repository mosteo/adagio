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
--  $Id: adagio-g2-chat_factory.adb,v 1.3 2004/01/21 21:05:25 Jano Exp $

with Adagio.G2.Chat_peer;
with Adagio.G2.Chat_peer.Eliza_peer;
with Adagio.Misc;
with Adagio.Trace;

with Ada.Numerics.Float_random;

package body Adagio.G2.Chat_factory is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   -- Creation takes a connected socket and a header object with the
   -- request already read, so our response is due.
   -- Creates a chatter of specified Kind, if unknown an Away replier.
   function Create (
      Kind    : in String;
      From    : in Socket.Object; 
      Request : in Http.Header.Set) 
      return Connect.Peer.Object_access 
   is
      K  : constant String := Misc.To_lower (Kind);
      UK : Ustring;
   begin
      if K = "random" then
         declare
            use Ada.Numerics.Float_random;
            G : Generator;
         begin
            Reset (G);
            if Random (G) > 0.5 then
               UK := U ("eliza");
            else
               UK := U ("away");
            end if;
         end;
      else
         UK := U (K);
      end if;
      if S (UK) = "eliza" then
         Trace.Log ("Creating Eliza chatterbot");
         return 
            Connect.Peer.Object_access (
               Chat_peer.Eliza_peer.Create (From, Request));
      else
         Trace.Log ("Creating Away chatterbot");
         return 
            Connect.Peer.Object_access (Chat_peer.Create (From, Request));
      end if;
   end Create;

end Adagio.G2.Chat_factory;

