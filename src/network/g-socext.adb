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

with Ada.Exceptions; use Ada;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with GNAT.Sockets.Constants;
with GNAT.Sockets.Thin;          use GNAT.Sockets.Thin;

with GNAT.Sockets.Linker_Options;
pragma Warnings (Off, GNAT.Sockets.Linker_Options);
--  Need to include pragma Linker_Options which is platform dependent.

package body GNAT.Sockets.Extra is

   use type C.Int;

   subtype Fd_Set is C.Int;
   Null_Fd_Set : constant := 0;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval (Val : Duration) return Timeval is
      S  : constant Timeval_Unit := Timeval_Unit (Val);
      MS : constant Timeval_Unit := Timeval_Unit (1_000_000 * (Val - Duration (S)));
   begin
      return (S, MS);
   end To_Timeval;


   -----------------------
   -- Check_Read_socket --
   -----------------------
   -- True if socket is alive (readable even without data available)
   function Check_Read_Socket (Socket : in Socket_Type) return Boolean is
      Res  : C.int;
      Len  : aliased C.int;
      RSet : aliased Fd_Set := Null_FD_set;
      TVal : aliased Timeval;
      TPtr : Timeval_Access;
      Err  : Integer;
   begin

      -- Zero wait seconds in select call
      TVal := To_Timeval (0.0);
      TPtr := TVal'Unchecked_Access;

      --  Set R_Socket_Set in RSet.
      Insert_Socket_In_Set (RSet'Address, C.Int (Socket));

      Last_Socket_In_Set (Rset'Address, Len'Unchecked_Access);
      Len := Len + 1;

      Res :=
        C_Select
         (Len,
          RSet'Address,
          No_Fd_Set,
          No_Fd_Set,
          TPtr);

      if Res < 0 then
         Err := Socket_errno;
         if Err /= Constants.EINTR then
            Exceptions.Raise_Exception
              (C_Select_Error'Identity,
               Value (Socket_error_message (Err)));
         else
            return false;
         end if;
      else
         return Res > 0;
      end if;
   end Check_Read_Socket;

   ------------------------
   -- Check_Write_socket --
   ------------------------
   -- True if the socket can be written without blocking on it
   function Check_Write_Socket (Socket : in Socket_Type) return Boolean is
      Res  : C.int;
      Len  : aliased C.int;
      WSet : aliased Fd_Set := Null_FD_set;
      TVal : aliased Timeval;
      TPtr : Timeval_Access;
      Err  : Integer;
   begin

      -- Zero wait seconds in select call
      TVal := To_Timeval (0.0);
      TPtr := TVal'Unchecked_Access;

      --  Set W_Socket_Set in WSet.
      Insert_Socket_In_Set (WSet'Address, C.Int (Socket));

      Last_Socket_In_Set (Wset'Address, Len'Unchecked_Access);
      Len := Len + 1;

      Res :=
        C_Select
         (Len,
          No_Fd_Set,
          WSet'Address,
          No_Fd_Set,
          TPtr);

      if Res < 0 then
         Err := Socket_errno;
         if Err /= Constants.EINTR then
            Exceptions.Raise_Exception
              (C_Select_Error'Identity,
               Value (Socket_error_message (Err)));
         else
            return false;
         end if;
      else
         return Res > 0;
      end if;
   end Check_Write_Socket;

end Gnat.Sockets.Extra;
