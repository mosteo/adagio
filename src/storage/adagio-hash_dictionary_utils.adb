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

with Adagio.Trace;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Adagio.Hash_Dictionary_Utils is

   ------------------------------------------------------------------------
   -- From_Magnet                                                        --
   ------------------------------------------------------------------------
   -- Creates a hash dictionary containing the hashes in a magnet link
   function From_Magnet (Link : in Agpl.Magnet.Object) return 
      Hash_Dictionary.Object 
   is
      use Agpl;
      This : Hash_Dictionary.Object;
   begin
      -- Trace.Log ("Magnet: " & Magnet.To_String (Link), Trace.Always);
      for I in 1 .. Magnet.Get_Num_Attributes (Link, Magnet.Uri_Attr) loop
         Hash_Dictionary.Add_Word (
            This,
            Magnet.Get_Hash_Type  (Link, I),
            U (Magnet.Get_Hash_Value (Link, Magnet.Get_Hash_Type (Link, I))));
--            Trace.Log ("Constructing hash: " & 
--               Magnet.Get_hash_Type (Link, I) & ":" &
--               Magnet.Get_Hash_Value (Link, Magnet.Get_Hash_Type (Link, I)), Trace.Always);
      end loop;
      return This;
   end From_Magnet;
   
end Adagio.hash_Dictionary_Utils;
