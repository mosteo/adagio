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
--  $Id: adagio-file-criteria.ads,v 1.3 2004/01/21 21:05:27 Jano Exp $

--  Criteria to qualify files

package Adagio.File.Criteria is

   -- Defined criteria:
   -- Greater_than            Size greater OR EQUAL than 
   -- Smaller_than            Size smaller than 
   -- Is_in                   File is in a certain subfolder.
   --                           Spaces must be replaced wiht '+' so no paths
   --                           with + can be used here.
   -- Extension_is            Extension comparison (with dot)
   --    Additionally we have: and or, evaluated always left to righ

   -- Examples:
   -- Smaller_than 1024kB or Is_in c:/test or Extension_is .mp3
   -- Greater_than 1024kB and Smaller_than 10240kB

   Syntax_error : Exception;

   function Qualify (
      this      : in File.Object; 
      Criterion : in String;
      Initially : in Boolean := false) 
      return Boolean;
   
end Adagio.File.Criteria;
