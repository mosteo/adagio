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
--  $Id: adalid-globals-options.adb,v 1.5 2004/03/22 07:14:52 Jano Exp $

--  Centralized facilities for all configuration options.
--  These variables are all initialized during elaboration from the xml file.

with Adalid.Misc;

with Adagio.Xml;
with Adagio.Xml.Utils;

package body Adalid.Globals.Options is

   -- Shortcuts
   function L (S : in String) return String renames Misc.To_lower;
   function Attr (
      Path, Name : in String; 
      Node       : in Xml.Node; 
      Def        : in String;
      Pos        : in Natural := 1;
      Unique     : in Boolean := true) return String 
      renames Xml.Get_attribute;
   function Attr (Path, Name : in String; Node : in Xml.Node; Def : in String)
      return UString is
      R : String := Attr (Path, Name, Node, Def);
   begin
      return U (R);
   end Attr;

   procedure Set_options is
      package XUtils renames Xml.Utils;
   begin
      null;
   end Set_options;

begin

   Set_options;

end Adalid.Globals.Options;
