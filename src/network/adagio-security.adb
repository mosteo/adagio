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
--  $Id: adagio-security.adb,v 1.4 2004/02/29 20:36:45 Jano Exp $

With
Adagio.Exceptions,
Adagio.Globals.Options,
Adagio.Misc,
Adagio.Socket.Ip,
Adagio.Trace,
Binary_tree,
Bit_arrays,
Bit_arrays.Modular,
Agpl.Dynamic_vector,
Agpl.Types.Ustrings,
Charles.Hash_string,
Charles.Maps.Hashed.Strings.Unbounded,
Strings.Fields,
Gnat.Regexp,
Gnat.Sockets;

Use
GNAT,
Adagio.Exceptions,
Agpl,
Agpl.Types.Ustrings;

package body Adagio.Security is

   use type Agpl.Types.Ustrings.Ustring;
   use type Bit_arrays.Bit_array;

   ------------------------------------------------------------------------
   -- Binary addresses types                                             --
   ------------------------------------------------------------------------
   use type Ip_address.Family_type;

   type Byte is mod 2 ** 8;
   type Byte_array is array (Positive range <>) of Byte;

   package Byte_to_bit is new Bit_arrays.Modular (Byte);

   type Binary_address (Family : Ip_address.Family_type) is record
      Data : Byte_array (1 .. 16) := (others => 0);
   end record;

   type UABan is record
      Kind    : User_agent_ban_type;
      Pattern : Ustring;
      Regexp  : Gnat.Regexp.Regexp;
   end record;

   package UABan_list is new Dynamic_vector (UABan);
   use UABan_list;

   UABans : UABan_list.Object (First => 1);

   package Maps is new Charles.Maps.Hashed.Strings.Unbounded (
      Boolean, Charles.Hash_string, "=", "=");

   Country_bans : Maps.Container_type;
   use Maps;

   ------------------------------------------------------------------------
   -- Tree things                                                        --
   ------------------------------------------------------------------------
   type Node_data is record
      Mask_end : Boolean;
   end record;

   package Mask_trees is new Binary_tree (Node_data);

   Mask_tree : Mask_trees.Tree :=
      Mask_trees.Add_child (null, Mask_trees.Left, (Mask_end => false));

   Side_chooser : array (Boolean) of Mask_trees.Sides :=
      (false => Mask_trees.Left, true => Mask_trees.Right);

   ------------------
   -- To_bit_array --
   ------------------
   Dummy : Bit_arrays.Bit_array (1 .. 0);
   function To_bit_array (This : Binary_address) return Bit_arrays.Bit_array
   is
   begin
      case This.Family is
         when Sockets.Family_inet =>
            return
               Byte_to_bit.To_bit_array_BE (This.Data (1)) &
               Byte_to_bit.To_bit_array_BE (This.Data (2)) &
               Byte_to_bit.To_bit_array_BE (This.Data (3)) &
               Byte_to_bit.To_bit_array_BE (This.Data (4));
         when others =>
            raise Unimplemented;
            return Dummy;
      end case;
   end To_bit_array;

   ---------------------
   -- Add_ban_to_tree --
   ---------------------
   procedure Add_ban_to_tree (
      This : in out Mask_trees.Tree; Addr, Mask : in Binary_address)
   is
      use Bit_arrays;
      BA   : Bit_array := To_bit_array (Addr);
      MA   : Bit_array := To_bit_array (Mask);
      Pos  : Mask_trees.Node_access := This;
      Next : Mask_trees.Node_access;
      Side : Mask_trees.Sides;
      use Mask_trees;
   begin
      if BA'First /= MA'First then
         raise Constraint_error;
      end if;
      for N in MA'Range loop
         exit when not MA (N);
         -- Earlier mask:
         if Get_data (Pos).Mask_end then
            return;
         end if;
         Side := Side_chooser (BA (N));
         Next := Get_child (Pos, Side);
         if Next = null then
            Next := Add_child (Pos, Side, (Mask_end => false));
         end if;
         Pos := Next;
      end loop;
      Set_data (Pos, (Mask_end => true));
   end Add_ban_to_tree;

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (This : in Mask_trees.Tree; Addr : in Binary_address)
      return Boolean
   is
      use Bit_arrays;
      use Mask_trees;
      BA   : Bit_array := To_bit_array (Addr);
      Pos  : Mask_trees.Node_access := This;
      Next : Mask_trees.Node_access;
   begin
      for N in BA'Range loop
         -- End of some mask, ban:
         if Get_data (Pos).Mask_end then
            return true;
         end if;
         Next := Get_child (Pos, Side_chooser (BA (N)));
         -- No way, no ban:
         if Next = null then
            return not Globals.Options.Security_policy_allow;
         end if;
         Pos := Next;
      end loop;
      -- All the way, ban:
      return Get_data (Pos).Mask_end;
   end Is_banned;

   ----------
   -- Mask --
   ----------
   function Mask (L, R : in Binary_address) return Binary_address is
      Result : Binary_address (L.Family);
      Last : array (Ip_address.Family_type) of Natural :=
         (Sockets.Family_inet => 4, Sockets.Family_inet6 => 16);
   begin
      if L.Family /= R.Family then
         raise Constraint_error;
      else
         for N in 1 .. Last (L.Family) loop
            Result.Data (N) := L.Data (N) and R.Data (N);
         end loop;
         return Result;
      end if;
   end Mask;

   -----------------------
   -- To_binary_address --
   -----------------------
   function To_binary_address (This : in Ip_address.Inet_addr_type)
      return Binary_address is
      Addr : Binary_address (This.Family);
      Img  : String := Sockets.Image  (This);
      use Strings.Fields;
      Sep  : Character;
      Last : Natural;
   begin
      case This.Family is
         when Sockets.Family_inet =>
            Sep  := '.';
            Last := 4;
         when Sockets.Family_inet6 =>
            Sep  := ':';
            Last := 16;
      end case;

      for N in 1 .. Last loop
         Addr.Data (N) := Byte'Value (Select_field (Img, N, Sep));
      end loop;

      return Addr;
   end To_binary_address;

   -----------------------
   -- To_binary_address --
   -----------------------
   -- Dotted address/mask
   function To_binary_address (This : in String) return Binary_address is
      Last : Natural;
      Addr : Binary_address (Sockets.Family_inet);
      Sep  : Character := '.';
      use Strings.Fields;
   begin
      if Misc.Contains (This, ".") then
         Last := 4;
         Sep  := '.';
      else
         raise Unimplemented;
      end if;

      for N in 1 .. Last loop
         Addr.Data (N) := Byte'Value (Select_field (This, N, Sep));
      end loop;

      return Addr;
   end To_binary_address;


   -----------
   -- Image --
   -----------
   function Image (This : in Binary_address) return String is
      Result : Ustring;
   begin
      case This.Family is
         when Sockets.Family_inet =>
            for N in 1 .. 4 loop
               Result := Result &
                  U (Misc.To_string (Natural (This.Data (N))));
               if N /= 4 then
                  Result := Result & '.';
               end if;
            end loop;
         when others =>
            raise Unimplemented;
      end case;
      return S (Result);
   end Image;

   ------------------
   -- Add_ban_rule --
   ------------------
   -- Dotted format
   procedure Add_ban_rule (Address : in String; Mask : in String) is
      A, M : Binary_address (Sockets.Family_inet);
   begin
      A := To_binary_address (Address);
      M := To_binary_address (Mask);
      if A /= Security.Mask (A, M) then
         Trace.Log ("Security.Add_ban_rule: Correcting incorrect range for" &
            " rule " & Mask & "/" & Address, Trace.Warning);
         A := Security.Mask (A, M);
      end if;
      Add_ban_to_tree (Mask_tree, A, M);
   end Add_ban_rule;

   ----------------
   -- Is_allowed --
   ----------------
   function Is_allowed (Address : in Ip_address.Inet_addr_type)
      return Boolean is
   begin
      return not Is_banned (Address);
   end Is_allowed;

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (Address : in Ip_address.Inet_addr_type)
      return Boolean
   is
      Addr   : Binary_address := To_binary_address (Address);
      Img    : String         := Sockets.Image     (Address);
      Banned : Boolean;
   begin
      if Socket.Ip.Is_public (Img) and then
         Is_country_banned (Agpl.Geoip.Country_code_from_addr (Img))
      then
         return true;
      else
         Banned := Is_banned (Mask_tree, Addr);
         if Banned then
            Trace.Log ("Adagio.Security.Is_banned: IP ban detected",
               Trace.Debug);
         end if;
         return Banned;
      end if;
   end;

   -------------------
   -- Add_ban_agent --
   -------------------
   procedure Add_ban_agent (
      Agent : in String; Kind : in User_agent_ban_type) is
      Rule : UABan;
   begin
      Rule.Kind    := Kind;
      Rule.Pattern := U (Misc.To_lower (Agent));
      Rule.Regexp  := Gnat.Regexp.Compile ("1*");
      begin
         if Kind = Regexp then
            Rule.Regexp :=
               Gnat.Regexp.Compile (
                  Agent,
                  Glob => true,
                  Case_sensitive => false);
         end if;
      exception
         when Gnat.Regexp.Error_in_regexp =>
            raise Syntax_error;
      end;

      Append (UABans, Rule);
      Trace.Log ("Added User-Agent ban: " & Rule.Kind'Img & "; " &
         S (Rule.Pattern));

   end Add_ban_agent;

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (Agent : in String) return Boolean is
   begin
      for N in 1 .. Last (UABans) loop
         declare
            Rule : UABan renames UABans.Vector (N);
         begin
            case Rule.Kind is
               when Regexp =>
                  if Gnat.Regexp.Match (Agent, Rule.Regexp) then
                     Trace.Log (
                        "Adagio.Security.Is_banned: Regexp Agent ban detected" &
                        " for " & Agent, Trace.Debug);
                     return true;
                  end if;
               when Substring =>
                  if Misc.Contains (Misc.To_lower (Agent), S (Rule.Pattern))
                  then
                     Trace.Log (
                        "Adagio.Security.Is_banned: Substr Agent ban detected" &
                        " for " & Agent, Trace.Debug);
                     return true;
                  end if;
            end case;
         end;
      end loop;

      return false;
   end Is_banned;

   ---------------------
   -- Add_country_ban --
   ---------------------
   procedure Add_country_ban (
      Country : in Agpl.Geoip.Country_code; Allow : in Boolean := false)
   is
      Pass : constant array (Boolean) of String (1 .. 7) :=
         (true => "allowed", false => "denied ");
   begin
      Insert (Country_bans, Misc.To_lower (Country), Allow);
      Trace.Log ("Adding country rule for " &
         Misc.To_upper (Country) & ": pass is " & Pass (Allow),
         Trace.Informative);
   end Add_country_ban;

   -----------------------
   -- Is_country_banned --
   -----------------------
   function Is_country_banned (
      Country : in Agpl.Geoip.Country_code) return Boolean
   is
      I : Iterator_type := Find (Country_bans, Misc.To_lower (Country));
   begin
      if I /= Back (Country_bans) then
         if not Element (I) then
            Trace.Log (
               "Adagio.Security.Is_banned: Country ban detected" &
               " for " & Country, Trace.Debug);
         else
            Trace.Log (
               "Adagio.Security.Is_banned: Country pass detected" &
               " for " & Country, Trace.Debug);
         end if;
         return not Element (I);
      else
         if not Globals.Options.Security_policy_allow then
            Trace.Log (
               "Adagio.Security.Is_banned: General policy applied (deny)" &
               " for " & Country, Trace.Debug);
         end if;
         return not Globals.Options.Security_policy_allow;
      end if;
   end Is_country_banned;


    --	UNREFERENCED ITEMS
	Pragma Unreferenced( Image );
end Adagio.Security;
