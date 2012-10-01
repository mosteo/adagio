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
--  $Id: adagio-security-old.adb,v 1.3 2004/01/21 21:05:38 Jano Exp $

with Adagio.Misc;
with Adagio.Trace;
with Dynamic_vector;

with Charles.Sets.Sorted.Unbounded;

with Strings.Fields;

with Gnat.Regexp;
with Gnat.Sockets;
use  Gnat;

package body Adagio.Security is

   use type Ip_address.Family_type;

   type Byte is mod 2 ** 8;
   type Byte_array is array (Positive range <>) of Byte;

   type Binary_address (Family : Ip_address.Family_type) is record
      Data : Byte_array (1 .. 16);
   end record;

   type Rules (Family : Ip_address.Family_type) is record
      Addr : Binary_address (Family);
      Mask : Binary_address (Family);
   end record;

   subtype Rules_ip is Rules (Sockets.Family_inet);

   function Less (L, R : in Rules_ip) return Boolean;

   package Ban_list is new Charles.Sets.Sorted.Unbounded (
      Rules_ip, Less, "=");
   use Ban_list;

   Bans : Ban_list.Container_type;

   type UABan is record
      Kind    : User_agent_ban_type;
      Pattern : Ustring;
      Regexp  : Gnat.Regexp.Regexp;
   end record;

   package UABan_list is new Dynamic_vector (UABan);
   use UABan_list;

   UABans : UABan_list.Object (First => 1);

   ----------
   -- Less --
   ----------
   function Less (L, R : in Rules_ip) return Boolean is
   begin
      for N in 1 .. 4 loop
         if L.Addr.Data (N) /= R.Addr.Data (N) then
            return L.Addr.Data (N) < R.Addr.Data (N);
         end if;
      end loop;
      for N in 1 .. 4 loop
         if L.Mask.Data (N) /= R.Mask.Data (N) then
            return L.Mask.Data (N) < R.Mask.Data (N);
         end if;
      end loop;
      return false;
   end Less;

   ---------
   -- "=" --
   ---------
   function "=" (L, R : in Binary_address) return Boolean is
   begin
      if L.Family /= R.Family then
         return false;
      else
         case L.Family is
            when Sockets.Family_inet =>
               return L.Data (1 .. 4) = R.Data (1 .. 4);
            when Sockets.Family_inet6 =>
               return L.Data (1 .. 16) = R.Data (1 .. 16);
         end case;
      end if;
   end "=";

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

   -----------
   -- Image --
   -----------
   function Image (This : in Rules) return String is
   begin
      return Image (This.Mask) & "/" & Image (This.Addr);
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
      Insert (Bans, (Family => A.Family, Addr => A, Mask => M));
   end Add_ban_rule;

   ----------------
   -- Is_allowed --
   ----------------
   function Is_allowed (Address : in Ip_address.Inet_addr_type)
      return Boolean is

      Addr : Binary_address := To_binary_address (Address);

      function Allowed (Rule : in Rules) return Boolean is
      begin
         return not
            (Mask (Addr, Rule.Mask) = Rule.Addr);
      end Allowed;

      Pos : Iterator_type := First (Bans);
   begin
      while Pos /= Back (Bans) loop
         if not Allowed (Element (Pos)) then
            Trace.Log ("Security.Is_allowed: " & Image (Addr) & " disallowed"
               & " because of " & Image (Element (Pos)));
            return false;
         end if;
         Pos := Succ (Pos);
      end loop;

      return true;
   end Is_allowed;

   ---------------
   -- Is_banned --
   ---------------
   function Is_banned (Address : in Ip_address.Inet_addr_type)
      return Boolean is
   begin
      return not Is_allowed (Address);
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
                     return true;
                  end if;
               when Substring =>
                  if Misc.Contains (Misc.To_lower (Agent), S (Rule.Pattern))
                  then
                     return true;
                  end if;
            end case;
         end;
      end loop;

      return false;
   end Is_banned;

end Adagio.Security;
