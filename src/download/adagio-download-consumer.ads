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

-- Generic consumer of data. Consumers are added to slots, and they interact
-- with the data received by a source.
-- They have a priority which allows to prioritize them about some data.
-- (For example, the writer must pass to disk the data before a Sha1 verifier
-- can read it from disk)
-- Obvious descendents are Writer, Sha1_Verifier, TTH_Verifier

with Adagio.Download.Data;
with Adagio.Download.Slot;

package Adagio.Download.Consumer is

   type Object is abstract tagged null record;
   type Object_Access is access all Object'Class;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Unique Id. 
   -- Must be the same for objects that shouldn't be repeated.
   function Get_Id (This : in Object) return Consumer_Id is abstract;

   ------------------------------------------------------------------------
   -- Get_Priority                                                       --
   ------------------------------------------------------------------------
   -- Lower ones are called first
   function Get_Priority (This : in Object) return Positive 
   is abstract;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Do whatever has to be done with this data
   procedure Process (
      This  : in out Object; 
      Datum : in Data.Object'Class;
      On    : in out Slot.Object)
   is abstract;
   
   ------------------------------------------------------------------------
   -- Non abstract subprograms

   ------------------------------------------------------------------------
   -- Equal                                                              --
   ------------------------------------------------------------------------
   function Equal (L, R : in Object_Access) return Boolean;

   ------------------------------------------------------------------------
   -- More_Prioritary                                                    --
   ------------------------------------------------------------------------
   function More_Prioritary (L, R : in Object_Access) return Boolean;

end Adagio.Download.Consumer;
