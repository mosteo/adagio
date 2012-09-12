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
--  $Id: adagio-xml-utils.adb,v 1.5 2004/03/10 23:50:01 Jano Exp $

with Adagio.Misc;
with Adagio.Os;

with Ada.Exceptions; use Ada.Exceptions;

with Gnat.Directory_operations;

package body Adagio.Xml.Utils is

   ------------------------------------------------------------------------
   -- Get_duration                                                       --
   ------------------------------------------------------------------------
   -- Interpret an attribute as a duration with units:
   function Get_duration (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in Duration) return Duration is
      S : String := Get_attribute (Path, Attr, Parent,
         Duration'Image (Default));
   begin
      return Misc.Parse_duration (S);
   end Get_duration;

   function Get_duration (
      From    : in Node;
      Attr    : in String;
      Default : in Duration) return Duration is
      S : String := Get_attribute (From, Attr, Duration'Image (Default));
   begin
      return Misc.Parse_duration (S);
   end Get_duration;

   ------------------------------------------------------------------------
   -- Get_Float                                                       --
   ------------------------------------------------------------------------
   -- Interpret an attribute as a Float with units:
   function Get_Float (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in Float) return Float is
      S : String := Get_attribute (Path, Attr, Parent,
         Float'Image (Default));
   begin
      return Float'Value (S);
   end Get_Float;

   function Get_Float (
      From    : in Node;
      Attr    : in String;
      Default : in Float) return Float is
      S : String := Get_attribute (From, Attr, Float'Image (Default));
   begin
      return Float'Value (S);
   end Get_float;

   -- Interpret an attribute as a boolean (yes/no; true/false)
   function Get_boolean (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in Boolean) return Boolean
   is
      S : String := Misc.To_lower (Get_attribute (Path, Attr, Parent,
         Boolean'Image (Default)));
   begin
      return S = "yes" or else S = "true";
   end Get_boolean;

   function Get_boolean (
      From    : in Node;
      Attr    : in String;
      Default : in Boolean) return Boolean
   is
      S : String :=
         Misc.To_lower (Get_attribute (From, Attr, Boolean'Image (Default)));
   begin
      return S = "yes" or else S = "true";
   end Get_boolean;

   ------------------------------------------------------------------------
   -- Get_size                                                           --
   ------------------------------------------------------------------------
   -- Interpret an attribute as a size in bytes:
   function Get_size (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in File_size) return File_size is
      S : String := Get_attribute (Path, Attr, Parent,
         File_size'Image (Default));
   begin
      return Misc.Parse_size( S );
   end Get_size;

   function Get_size (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in Natural) return Natural is
   begin
      return Natural (Get_Size (Path, Attr, Parent, File_Size (Default)));
   end Get_Size;

   function Get_size (
      From    : in Node;
      Attr    : in String;
      Default : in File_size) return File_size is
      S : String :=
         Get_attribute (From, Attr, File_size'Image (Default));
   begin
      return Misc.Parse_size( S );
   end Get_size;

   ------------------------------------------------------------------------
   -- Get_speed                                                          --
   ------------------------------------------------------------------------
   -- Interpret an attribute as a speed in bytes/second:
   function Get_speed (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in Speed) return Speed is
      S : String := Get_attribute (Path, Attr, Parent,
         Speed'Image (Default) & "B/s");
   begin
      if Misc.To_lower (S) = "unlimited" then
         return Speed (Integer'Last);
      elsif S (S'Last - 1 .. S'Last) /= "/s" then
         Raise_exception (Constraint_error'Identity,
            "Xml.Utils.Get_speed: Incorrect format : " & S);
      else
         return Speed (
            Misc.Parse_size (S (S'First .. S'Last - 2)));
      end if;
   end Get_speed;

   function Get_speed (
      From    : in Node;
      Attr    : in String;
      Default : in Speed) return Speed is
      S : String := Get_attribute (
         From, Attr, Speed'Image (Default) & "B/s");
   begin
      if Misc.To_lower (S) = "unlimited" then
         return Speed (Integer'Last);
      elsif S (S'Last - 1 .. S'Last) /= "/s" then
         Raise_exception (Constraint_error'Identity,
            "Xml.Utils.Get_speed: Incorrect format : " & S);
      else
         return Speed (
            Misc.Parse_size (S (S'First .. S'Last - 2)));
      end if;
   end Get_speed;

   -- return a normalized path (forward slashes, ending slash)
   function Get_path (
      Path    : in String;
      Attr    : in String;
      Parent  : in Document;
      Default : in String) return String is
      use Gnat.Directory_operations;
      Result : String :=
         Format_pathname (Get_attribute (Path, Attr, Parent, Default), UNIX);
   begin
      if Result (Result'Last) = Os.Folder_separator then
         return Result;
      else
         return Result & Os.Folder_separator;
      end if;
   end;

   function Get_path (
      From    : in Node;
      Attr    : in String;
      Default : in String) return String is
      use Gnat.Directory_operations;
      Result : String :=
         Format_pathname (Get_attribute (From, Attr, Default), UNIX);
   begin
      if Result (Result'Last) = Os.Folder_separator then
         return Result;
      else
         return Result & Os.Folder_separator;
      end if;
   end;

end Adagio.Xml.Utils;
