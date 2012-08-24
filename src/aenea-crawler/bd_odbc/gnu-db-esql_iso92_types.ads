-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/esql/gnu-db-esql_iso92_types.ads,v $
--  Description     : Types shared between support and code generator        --
--  Author          : Michael Erdmann                                        --
--  Created         : 10.6.2002                                              --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2002/06/11 20:18:40 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2002 Michael Erdmann                                       --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains types which are shared between the code generator  --
--  of the esql translator. It has been placed in a seperate file in order   --
--  to avoid the linking of the odbc support with the esql translator.       --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
package GNU.DB.ESQL_ISO92_Types is

   type ISO92_Host_Var_Type is (
       ISO92_CHAR_TYPE,
       ISO92_BIT_TYPE,
       ISO92_SMALLINT_TYPE,
       ISO92_INT_TYPE,
       ISO92_REAL_TYPE,
       ISO92_DOUBLE_PRECISION_TYPE,
       ISO92_SQLCODE_TYPE,
       ISO92_SQLSTATE_TYPE,
       ISO92_INDICATOR_TYPE,
       ISO92_Unknown_Type,
       -- GNADE specific data types
       GNADE_VARCHAR_TYPE,
       GNADE_BINARY_TYPE,
       GNADE_VARBINARY_TYPE
   );

end GNU.DB.ESQL_ISO92_Types;
