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
--  $Id: aenea-gwc2.adb,v 1.5 2004/01/21 21:05:24 Jano Exp $

with Aenea.Globals;
with Aenea.Net;
with Aenea.Trace;
with Aenea.Walker;

with Adagio.Gwcache2;
with Adagio.Misc;

package body Aenea.Gwc2 is

   package agwc2 renames Adagio.Gwcache2;

   task Inquirer is
      entry Start (Target : in Natural);
   end Inquirer;

   task body Inquirer is
      Target   : Natural;
   begin
      accept Start (Target : in Natural) do
         Inquirer.Target := Target;
      end Start;

      loop
         exit when Globals.Requested_exit;
         delay 1.0;

         -- Escapegoat for all dead hubs:
         if Net.Counter.Hubs_Count < 20 then
            Target   := Integer'Max (0, 20 - Net.Counter.Hubs_Count);
         else
            Target := 0;
         end if;

         if Target > 0 then
            Trace.Log ("Querying webcaches, Targetted:" & Target'Img, Trace.Never);
            declare
               Hubs : agwc2.Network_node_array := agwc2.Query_any ("Gnutella2", 20);
               Acquired : Natural := 0;
            begin
               for N in Hubs'Range loop
                  Acquired := Acquired + 1;
                  Walker.Adder.Add_Found (
                     S (Hubs (N).Address & ":" &
                        Adagio.Misc.To_string (Hubs (N).Port)));
               end loop;
               if Acquired > 0 then
                  Trace.Log (Acquired'Img & " new hubs acquired from gwc2", Trace.Informative);
               end if;
            exception
               when E : others =>
                  Trace.Log ("GWC2.Inquierer: " & Trace.Report (E), Trace.Error);
            end;
         end if;
      end loop;
   end Inquirer;

   ------------------------------------------------------------------------
   -- Query                                                              --
   ------------------------------------------------------------------------
   -- Will query until the desired number of hosts is obtained.
   procedure Query (Target : in Natural := 10) is
   begin
      agwc2.Set_client_id (Vendor, Version_id);
      Inquirer.Start (Target);
   end Query;

end Aenea.Gwc2;
