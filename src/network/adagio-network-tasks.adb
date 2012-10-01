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
--  $Id: adagio-network-tasks.adb,v 1.3 2004/01/21 21:05:37 Jano Exp $

With
Adagio.Globals,
Adagio.Trace,
Adagio.Xml;

package body Adagio.Network.Tasks is

   -- Launchs automatic connecting to every configured network,
   -- polling every Period miliseconds to reconnect if dropped
   procedure Start(Period: Duration := 1.0) is
      Nodes: Xml.Node_array:=
         Xml.Get_all(Xml.Get("network", Globals.Config));
	Pragma Unreferenced( Period );
    begin
      Trace.Log("Network.Tasks.Start: Initializing networks...");
      -- Try to connect to every found network:
      for i in Nodes'range loop
         declare
            Node: Xml.Node renames Nodes(i);
            Net:  Network.Object_access;
            Name: String renames Xml.Get_name(Node);
         begin
            -- If connected, let's try:
            if Xml.Get_attribute(Node, "connect", "yes") = "yes" and then
               Name /= "GWebCache2"
            then
               Net:= Network.List.Get(Name);
               if Net /= null then
                  Trace.Log("Network.Tasks.Start: Starting connection to " &
                     Name);
                  Network.Connect (Net.all);
                  Network.List.Prepare (Net);
               else
                  Trace.Log("Network.Tasks.Start: " & Name & " missing.",
                     Trace.Warning);
               end if;
            else
               Trace.Log("Network.Tasks.Start: Skipping network " & Name);
            end if;
         exception
            when E: others =>
               Trace.Log("Netowrk.Tasks.Start: Error while connecting to " &
                  Name & ": " & Trace.Report(E), Trace.Error);
         end;
      end loop;
   end Start;

end Adagio.Network.Tasks;
