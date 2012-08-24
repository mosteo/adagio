-------------------------------------------------------------------------------
--                                                                           --
--                     GNADE  : GNat Ada Database Environment                --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/support/gnu-db-support-string_builder.adb,v $
--  Description     : Simple string builder package                          --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 16-Mar-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2004/03/21 20:52:02 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
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
--  <a>                                                                      --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --

--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - The handling of the tree nodes is currently not task save          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Unchecked_Deallocation ;

package body GNU.DB.Support.String_Builder is

   Version : constant String := "$Id: gnu-db-support-string_builder.adb,v 1.1 2004/03/21 20:52:02 merdmann Exp $";

   type String_Access is access String;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type  is record
      Length : Natural := 0;
      Buffer : String_Access := null;
   end record;

   procedure Free is new Unchecked_Deallocation(
	String, String_Access );

   procedure Free is new Unchecked_Deallocation(
	Object_Data_Type, Object_Data_Access );

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is
      Result : Object_Data_Access := new Object_Data_Type ;
   begin
      return Result;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      This : in out Object ) is
   begin
      if This.Data.Buffer /= null then
         Free( This.Data.Buffer );
      end if;
      Free( This.Data );
   end Finalize;

   ---------------
   -- To_String --
   ----------------
   function To_String(
      This : in Object ) return String is
      -- convert the contents of the string buffer into a string
      Data : Object_Data_Access renames This.Data ;
   begin
      return Data.Buffer( 1..Data.Length );
   end To_String;

   -----------
   -- Clear --
   -----------
   procedure Clear(
      This : in out Object) is
      Data : Object_Data_Access renames This.Data ;
   begin
      Data.Length := 0;
   end Clear ;

   ------------
   -- Insert --
   ------------
   procedure Insert(
      This : in out Object;
      Char : in Character ) is
      -- Insert a character into the string buffer.
      -- R.1 - If the buffer does not exist create a buffer with 1000 chars
      -- R.2 - If the buffer get to low to store the character extend the
      --       buffer.
      Data : Object_Data_Access renames This.Data ;
   begin
      if Data.Buffer = null then
         Data.Buffer := new String( 1..2000 );
         Data.Length := 0;
      end if;


      if not ( Data.Length+1 > Data.Buffer'Length ) then
         declare
            Tmp : String_Access := new String( 1..(Data.Length + 1000) );
         begin
            Tmp( 1..Data.Length ) := Data.Buffer( 1..Data.Length );
            Free( Data.Buffer );
            Data.Buffer := Tmp ;
         end ;

         Data.Length := Data.Length + 1;
         Data.Buffer( Data.Length ) := Char ;

      end if;
   end Insert;

   ------------
   -- Insert --
   ------------
   procedure Insert(
      This : in out Object;
      Str  : in String ) is
   begin
      for i in Str'Range loop
         Insert( This, Str(i) );
      end loop;
   end Insert;

end GNU.DB.Support.String_Builder;
