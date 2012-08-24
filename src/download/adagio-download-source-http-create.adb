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

package body Adagio.Download.Source.Http.Create is

   Creation_Failed : exception;

   ------------------------------------------------------------------------
   -- From_G2_Hit                                                        --
   ------------------------------------------------------------------------
   function From_G2_Hit (Hit : in G2.Hit.Object) return Object_Access is
      Src : Object_Access := new Object;
   begin
      Src.Firewalled := G2.Hit.Is_Firewalled (Hit);
      Src.Status     := Ready;
      Src.Urn        := U (G2.Hit.Get_Urn (Hit));

      if Src.Firewalled then 
         declare
            Addr : Ustring_Array := G2.Hit.Get_Proxies (Hit);
         begin
            for I in Addr'Range loop
               Ustring_Vector.Append (Src.Proxies, Addr (I));
            end loop;
         end;
      else
         Src.Address := U (G2.Hit.Get_Address (Hit));
      end if;

      return Src;
   exception
      when Creation_Failed =>
         Free (Source.Object_Access (Src));
         return null;
   end From_G2_Hit;

end Adagio.Download.Source.Http.Create;
