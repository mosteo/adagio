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
--  $Id: aenea-hub.ads,v 1.11 2004/03/22 07:14:55 Jano Exp $

with Aenea.Types;

package Aenea.Signals is

   -- This type encapsulates info about the signals that can affect the 
   -- status of a hub. They're used in the main transition function.

   type Kinds is (
      Ask,
      Timeout,
      Answer,
      Found,
      Expire,
      Maintenance);
   -- Explanation of signal kinds:
   -- ASK: A hub is to be asked now. Will change status to CHECKING
   -- TIMEOUT: Received when it's timeout deadline. The hub must have answered
   --    prior to this signal if alive.
   -- ANSWER: Message received from some host.
   -- FOUND: Host reported by a third party (neighbor, gwebcache)
   -- EXPIRE: Signal for GONE hubs to pass to DEAD status
   -- MAINTENANCE: Periodic check for hosts in ERRONEOUS status (should not happen)

   type Ask_Kinds    is (PI, CRAWLR);
   type Answer_Kinds is (PO, CRAWLA);

   type Object (Kind : Kinds) is record
      case Kind is 
         when Ask     =>
            null;
         when Timeout =>
            null;
         when Answer  =>
            Answer_Kind : Answer_Kinds;
            Is_Hub      : Boolean;
            Leaves      : Natural; -- A hub may have 0 leaves
            Max_Leaves  : Natural;
            Nick        : Ustring; -- Hub's owner nick
            Vendor      : Ustring;
            Version     : Ustring;
         when Found   =>
            null;
         when Expire  => 
            null;
         when Maintenance =>
            null;
      end case;
   end record;

end Aenea.Signals;
