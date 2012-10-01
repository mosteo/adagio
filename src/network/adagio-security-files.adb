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
--  $Id: adagio-security-files.adb,v 1.1 2004/03/22 07:14:59 Jano Exp $

with Adagio.Trace;
with Adagio.Xml;

package body Adagio.Security.Files is

   -----------------------
   -- Add_security_file --
   -----------------------
   procedure Add_security_file (Path : in String) is
      use type Xml.Document;
      Doc : Xml.Document;
   begin
      Doc := Xml.Parse (Path);

      declare
         Rules : Xml.Node_array := Xml.Get_all ("rule", Doc);
      begin
         for N in Rules'Range loop
            if Xml.Get_attribute (Rules (N), "action", "") = "deny" and then
               xml.Get_attribute (Rules (N), "type", "") = "address"
            then
               Security.Add_ban_rule (
                  Xml.Get_attribute (Rules (N), "address", ""),
                  Xml.Get_attribute (Rules (N), "mask", "255.255.255.255"));
            end if;
         end loop;
      end;

      Xml.Delete (Doc);
      Trace.Log ("Added security rules in " & Path);
   exception
      when E : others =>
         if Doc /= null then
            Xml.Delete (Doc);
         end if;
         Trace.Log ("Startup.Add_security_file: " & Path & ": " &
            Trace.Report (E), Trace.Error);
   end Add_security_file;

end Adagio.Security.Files;
