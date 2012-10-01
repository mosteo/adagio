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
--  $Id: adagio-upload.ads,v 1.4 2004/01/21 21:05:51 Jano Exp $

--  Root package for all search packages

with
Sha1,
Agpl.Types.Ustrings;

use Agpl.Types.Ustrings;

package Adagio.Searches is

   use type Agpl.Types.Ustrings.Ustring;

   Secure_Key : constant String := "freenet";

   type Search_Id is private;

   type Kinds is (Keywords, SHA1_digest);

   type Payload (Kind : Kinds) is record
      case Kind is
         when Keywords =>
            Words  : Ustring;
         when SHA1_digest =>
            Digest : Sha1.Digest;
      end case;
   end record;
   type Payload_Access is access all Payload;

   type Priorities is (
      Auto,
      Idle,
      Low,
      Medium,
      High,
      Exclusive5m,
      Exclusive15m,
      Exclusive30m,
      Exclusive60m,
      Exclusive_Forever);

   Priority_Delays : array (Priorities) of Duration := (
      Auto              => 0.0,
      Idle              => 0.0,
      Low               => 0.0,
      Medium            => 0.0,
      High              => 0.0,
      Exclusive5m       => 60.0 * 5.0,
      Exclusive15m      => 60.0 * 15.0,
      Exclusive30m      => 60.0 * 30.0,
      Exclusive60m      => 60.0 * 60.0,
      Exclusive_Forever => Duration'Last);

   ------------------------------------------------------------------------
   -- From_String                                                        --
   ------------------------------------------------------------------------
   function From_String (Id : in String) return Search_Id;

   ------------------------------------------------------------------------
   -- To_String                                                          --
   ------------------------------------------------------------------------
   function To_String (Id : in Search_Id) return String;

private

   type Search_Id is record
      Id : Ustring;
   end record;

   pragma Inline (From_String);
   pragma Inline (To_String);

end Adagio.Searches;
