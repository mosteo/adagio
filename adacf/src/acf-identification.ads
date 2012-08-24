------------------------------------------------------------------------
--         (c) 2001, Antonio Duran. All rights reserved               --
--                       aduran@inicia.es                             --
------------------------------------------------------------------------
-- The Ada Cryptographic Framework (ACF) is free software; you can    --
-- redistribute it and/or modify it under terms of the GNU General    --
-- Public License as published by the Free Software Foundation;       --
-- either version 2, or (at your option) any later version.           --
--                                                                    --
-- The ACF is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of        --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   --
-- General Public License for  more details. You should have received --
-- a copy of the GNU General Public License distributed with the ACF; --
-- see file COPYING. If not, write to the Free Software Foundation,   --
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------
-- Identification
--    File name         : acf-identification.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : December 6th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    This package provides constant definitions that identify the
--    framework and its current version.
------------------------------------------------------------------------
-- Portability issues:
-- TBD.
------------------------------------------------------------------------
-- Performance issues:
-- TBD.
------------------------------------------------------------------------
-- Revision history:
--
-- Ver   Who   When     Why
-- 1.0   ADD   12062001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Calendar;               use Ada.Calendar;

package ACF.Identification is

   ---------------------------------------------------------------------
   -- Constants
   ---------------------------------------------------------------------

   ACF_Name                      : constant String    := "Ada Cryptographic Framework";
   ACF_Copyright                 : constant String    := "Copyright (c) 2001 Antonio Duran";
   ACF_Version_Major             : constant Natural   := 0;
   ACF_Version_Minor             : constant Natural   := 1;
   ACF_Release                   : constant Natural   := 0;
   ACF_Version_String            : constant String    := "0.1.0";
   ACF_Version_Comments          : constant String    := "First alpha release";

end ACF.Identification;
