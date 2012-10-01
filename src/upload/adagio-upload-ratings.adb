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
--  $Id: adagio-upload-ratings.adb,v 1.3 2004/01/21 21:05:50 Jano Exp $

with Adagio.Os;
with Adagio.Trace;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Expressions_evaluator;
with Strings.Utils;  use Strings.Utils;

package body Adagio.Upload.Ratings is

   package Eval is new Expressions_evaluator (Float);

   ------------------------------------------------------------------------
   -- Check_syntax                                                       --
   ------------------------------------------------------------------------
   -- Ensure validity of a expression to be evaluated
   function Check_syntax (Expression : in String) return Boolean is
      Cooked_expr : Ustring := U ("f = " & Expression);
      E           : Eval.Expressions;
      Result      : Float;
   begin
      -- Do replacements:
      Cooked_expr := 
         U (Replace (S (Cooked_expr), "uploads", "(0)"));

      Cooked_expr := 
         U (Replace (S (Cooked_expr), "bytes_sent", "(0)"));

      Cooked_expr := 
         U (Replace (S (Cooked_expr), "file_size", "(0)")); 

      Cooked_expr := 
         U (Replace (S (Cooked_expr), "waited", "(1.0)"));

      begin
         E := Eval.Create (S (Cooked_expr));
         Result := Eval.Evaluate (E, Eval.f);
         Eval.Destroy (E);
         return true;
      exception
         when Ex : others =>
            Eval.Destroy (E);
            Trace.Log ("Upload.Ratings.Check_syntax: " & Trace.Report (Ex));
            Os.Message_box ("Syntax error", "Queue expression not valid: " &
               Expression);
            return false;
      end;
   end Check_syntax;

end Adagio.Upload.Ratings;
