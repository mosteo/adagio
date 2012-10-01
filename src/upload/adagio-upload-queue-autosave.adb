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
--  $Id: adagio-upload-queue-autosave.adb,v 1.3 2004/01/21 21:05:49 Jano Exp $

with Adagio.Globals.Options;
with Adagio.Trace;
with Adagio.Upload.Queue.Manager;
with Adagio.Xml.Utils;

package body Adagio.Upload.Queue.Autosave is

   task body Object is
      Period : Natural := Natural (Globals.Options.Uploads_AutosavePeriod);
   begin
      loop
         for N in 1 .. Period loop
            exit when Globals.Requested_exit;
            delay 1.0;
         end loop;

         begin
            Manager.Object.Save_queues;
            Trace.Log ("Upload.Queue.Autosave completed.");
         exception
            when E : others =>
               Trace.Log ("Upload.Queue.Autosave: " & Trace.Report (E), 
                  Trace.Error);
         end;
      end loop;
      Trace.Log ("Upload.Queue.Autosave exited.");
   end Object;
   
end Adagio.Upload.Queue.Autosave;














