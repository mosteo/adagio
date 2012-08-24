--  $Source: /nile.c/cvs/Dev/NT/Win32Ada/src/win32-windef.adb,v $
--  $Revision: 1.3 $ $Date: 2002/10/23 15:46:21 $ $Author: fofanov $
-------------------------------------------------------------------------------
--
--  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS"
--  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
--  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY
--  AND/OR FITNESS FOR A PARTICULAR PURPOSE.  The user assumes the
--  entire risk as to the accuracy and the use of this file.
--
--  Copyright (c) Intermetrics, Inc. 1995
--  Royalty-free, unlimited, worldwide, non-exclusive use, modification,
--  reproduction and further distribution of this file is permitted.
--
-------------------------------------------------------------------------------


package body Win32.Windef is

   function Max (A, B : T) return T is
   begin
      if A > B then return A; else return B; end if;
   end Max;

   function Min (A, B : T) return T is
   begin
      if A < B then return A; else return B; end if;
   end Min;

end Win32.Windef;


