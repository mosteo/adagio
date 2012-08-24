-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--
--  Copyright (C) 2000-2001 Juergen Pfeifer
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
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
-------------------------------------------------------------------------------
--
--  A large amount of functions in the ODBC API deal with getting/setting
--  of attribute values from a ODBC resource (i.e. statement attributes,
--  connection attributes etc.).
--
--  For that purpose ODBC has a quite generic mechanism: most of the
--  ODBC resources are represented by so called "Handles" to the
--  application and there are Set/Get routines that take such a handle
--  and an enumeration value indicating the attribute type as well as
--  a description of the location where the attribute is or should be
--  stored. We reflect this genericity of the ODBC API design in the
--  Ada95 binding by using generic packages and tagged types to represent
--  the Attribute/Value concept and the various ODBC resources.
--
generic
   type Context is private;    --  The "type of handle" to be used
   type T is (<>);             --  The enum of the attributes
   type Base is range <>;      --  The integer type used for length info
   type Aux  is range <>;      --  The integer type used for Aux data

   with procedure Get (Ctx       : in Context;
                       AttrType  : in T;
                       Value     : in SQLPOINTER;
                       Length    : in out Base;
                       Data      : in Aux;
                       ErrorCode : access SQLRETURN);

   with procedure Set (Ctx       : in Context;
                       AttrType  : in T;
                       Value     : in SQLPOINTER;
                       Length    : in Base;
                       Data      : in Aux;
                       ErrorCode : out SQLRETURN);

   with function Default_Context return Context;

package GNU.DB.SQLCLI.Generic_Attr is

   --  This is the abstract base type for Attribute/Value pairs.
   --  As different attributes have different types for their values,
   --  we use child packages that extent this type with record components
   --  according to the type of the attribute defined by ODBC.
   type Attribute_Value_Pair is abstract tagged
      record
         Attribute : T;
      end record;

   --  Any attribute value should be representable as string, mostly for
   --  debugging purposes.
   function To_String (Object : Attribute_Value_Pair) return String
      is abstract;

   function GetAttr (Handle    : Context;
                     Attribute : T;
                     Data      : Aux;
                     MaxLength : SQLSMALLINT := 0;
                     ErrorCode : access SQLRETURN)
                    return Attribute_Value_Pair
      is abstract;

   procedure SetAttr (Handle    : in Context;
                      AV_Pair   : in Attribute_Value_Pair;
                      Data      : in Aux;
                      ErrorCode : out SQLRETURN) is abstract;

end GNU.DB.SQLCLI.Generic_Attr;
