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
--  $Id: adagio-xml-utils.ads,v 1.5 2004/03/10 23:50:01 Jano Exp $
-- Helper functions for the XML/Ada DOM Component

with Adagio.Types; use Adagio.Types;

package Adagio.Xml.Utils is

   pragma Elaborate_body;

   function Get_num is new
      Get_numeric_attribute_from_path (Integer'Base);

   function Get_num is new
      Get_numeric_attribute_from_node (Integer'Base);

   -- Interpret an attribute as a duration with units:
   function Get_duration (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in Duration) return Duration;
   function Get_duration (
      From    : in Node;
      Attr    : in String;
      Default : in Duration) return Duration;

   -- Interpret an attribute as a float:
   function Get_float (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in Float) return Float;
   function Get_Float (
      From    : in Node;
      Attr    : in String;
      Default : in Float) return Float;

   -- Interpret an attribute as a boolean (yes/no; true/false)
   function Get_boolean (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in Boolean) return Boolean;
   function Get_boolean (
      From    : in Node;
      Attr    : in String;
      Default : in Boolean) return Boolean;

   -- Interpret an attribute as a size in bytes:
   function Get_size (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in File_size) return File_size;
   function Get_size (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in Natural) return Natural;
   function Get_size (
      From    : in Node;
      Attr    : in String;
      Default : in File_size) return File_size;

   -- Interpret an attribute as a speed in bytes/second:
   function Get_speed (
      Path    : in String; 
      Attr    : in String;
      Parent  : in Document;
      Default : in Speed) return Speed;
   function Get_speed (
      From    : in Node;
      Attr    : in String;
      Default : in Speed) return Speed;

   -- return a normalized path (forward slashes, ending slash)
   function Get_path (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in String) return String;
   function Get_path (
      From    : in Node;
      Attr    : in String;
      Default : in String) return String;

end Adagio.Xml.Utils;
