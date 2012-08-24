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
--  $Id: adagio-os-shutdown.adb,v 1.3 2004/01/21 21:05:40 Jano Exp $

-- Package with OS dependent functions.

with Adagio.Globals;
with Adagio.Trace;

with Win32;
with Win32.Wincon;
use  Win32;

package body Adagio.Os.Shutdown is

   function Handler (Ctrl : Win32.DWORD) return Win32.BOOL is
      use Wincon;
   begin
      case Ctrl is
         when 
            CTRL_C_EVENT | 
            CTRL_BREAK_EVENT |
            CTRL_CLOSE_EVENT |
            CTRL_LOGOFF_EVENT |
            CTRL_SHUTDOWN_EVENT =>
            Trace.Log ("Shutdown request received, exiting...", 
               Trace.Informative);
            Globals.Requested_exit := true;
         when others =>
            null;
      end case;
      return Win32.TRUE;
   end Handler;

begin
   if Wincon.SetConsoleCtrlHandler (Handler'Access, Win32.TRUE) /= Win32.TRUE
   then
      Trace.Log ("Os.Shutdown: Console handler installation failed.", 
         Trace.Error);
   else
      Trace.Log ("Os.Shutdown: Console handler installation successful.");
   end if;
end Adagio.Os.Shutdown;
