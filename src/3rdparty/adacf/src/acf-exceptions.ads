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
--    File name         : acf-exceptions.ads
--    File kind         : Ada package specification
--    Author            : Antonio Duran
--    Creation date     : Novembre 20th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
-- Provides declarations of the exceptions propagated outside the
-- packages of the library.
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
-- 1.0   ADD   11202001 Initial implementation
--
------------------------------------------------------------------------

package ACF.Exceptions is

   pragma Pure;

   ---------------------------------------------------------------------
   -- Exception declarations
   ---------------------------------------------------------------------

   ACF_Overflow_Error         : exception;
   ACF_Underflow_Error        : exception;
   ACF_Index_Error            : exception;
   ACF_Storage_Error          : exception;
   ACF_Bad_Argument_Error     : exception;
   ACF_Null_Argument_Error    : exception;
   ACF_Syntax_Error           : exception;
   ACF_IO_Error               : exception;
   ACF_Unexpected_Error       : exception;
   ACF_Not_Implemented_Error  : exception;

end ACF.Exceptions;
