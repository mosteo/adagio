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
--  $Id: aenea-hub.ads,v 1.11 2004/03/22 07:14:55 Jano Exp $

with Aenea.Globals.Options;
with Aenea.Hub.Actions;
with Aenea.Net;
with Aenea.Trace;

with Agpl.Chronos;
use Agpl;

package body Aenea.Hub.Automaton is

   type Transition is access
      procedure (This : in out Hub.Object; Signal : in Signals.Object);

   procedure Null_Transition (This : in out Hub.Object; Signal : in Signals.Object);
   pragma Inline (Null_Transition);

   ------------------------------------------------------------------------
   -- Null_Transition                                                    --
   ------------------------------------------------------------------------
   procedure Null_Transition (This : in out Hub.Object; Signal : in Signals.Object)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Signal);
   begin
      null;
   end Null_Transition;

   ------------------------------------------------------------------------
   -- Dead_Found                                                         --
   ------------------------------------------------------------------------
   procedure Dead_Found (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Cancel  (This);
      Actions.Ask_Now (This);
      Actions.Timeout (This);
      This.Status := Unknown;
   end Dead_Found;

   ------------------------------------------------------------------------
   -- Dead_Maintenance                                                   --
   ------------------------------------------------------------------------
   procedure Dead_Maintenance (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Maintenance (This);
   end Dead_Maintenance;

   ------------------------------------------------------------------------
   -- Gone_Expire                                                        --
   ------------------------------------------------------------------------
   procedure Gone_Expire (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Maintenance (This);
      This.Status := Dead;
   end Gone_Expire;

   ------------------------------------------------------------------------
   -- Hub_Check_Answer                                                   --
   ------------------------------------------------------------------------
   procedure Hub_Check_Answer (This : in out Hub.Object; Signal : in Signals.Object) is
   begin
      Actions.Cancel (This);
      case Signal.Answer_Kind is
         when Signals.CRAWLA =>
            if Signal.Is_Hub then
               Actions.Update    (This, Signal);
               Actions.Ask_Later (This, Globals.Options.Walk_RefreshUnit);
               This.Status := Hub_Waiting;
            else
               Actions.Cancel   (This);
               Actions.Discount (This);
               Actions.Expire   (This);
               This.Status := Leaf_Waiting;
            end if;
         when Signals.PO =>
            Actions.Update    (This, Signal);
            Actions.Ask_Later (This, Globals.Options.Walk_RefreshUnit);
            This.Status := Hub_Waiting;
      end case;
   end Hub_Check_Answer;

   ------------------------------------------------------------------------
   -- Hub_Check_Timeout                                                  --
   ------------------------------------------------------------------------
   procedure Hub_Check_Timeout (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Fail (This);

      if This.Failures >= Globals.Options.Walk_Failures then
         Actions.Discount (This);
         Actions.Expire
           (This,
            Globals.Options.Walk_RefreshUnit - Globals.Options.Walk_Timeout);
         This.Status := Gone;
      else
         Actions.Ask_Later
           (This,
            Globals.Options.Walk_RefreshUnit - Globals.Options.Walk_Timeout);
         This.Status := Hub_Waiting;
      end if;
   end Hub_Check_Timeout;

   ------------------------------------------------------------------------
   -- Hub_Waiting_Answer                                                 --
   ------------------------------------------------------------------------
   procedure Hub_Waiting_Answer (This : in out Hub.Object; Signal : in Signals.Object) is
   begin
      Actions.Update   (This, Signal);
   end Hub_Waiting_Answer;

   ------------------------------------------------------------------------
   -- Hub_Waiting_Ask                                                    --
   ------------------------------------------------------------------------
   procedure Hub_Waiting_Ask (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Ask_Now  (This);
      Actions.Timeout  (This);
      This.Status := Hub_Checking;
   end Hub_Waiting_Ask;

   ------------------------------------------------------------------------
   -- Leaf_Waiting_Expire                                                --
   ------------------------------------------------------------------------
   procedure Leaf_Waiting_Expire (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Maintenance (This);
      This.Status := Dead;
   end Leaf_Waiting_Expire;

   ------------------------------------------------------------------------
   -- Ready_Found                                                         --
   ------------------------------------------------------------------------
   procedure Ready_Found (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Actions.Cancel  (This);
      Actions.Ask_Now (This);
      Actions.Timeout (This);
      This.Status := Unknown;
   end Ready_Found;

   ------------------------------------------------------------------------
   -- Ready_Expire                                                        --
   ------------------------------------------------------------------------
   procedure Ready_Expire (This : in out Hub.Object; Signal : in Signals.Object) is
      pragma Unreferenced (Signal);
   begin
      Trace.Log ("Ready_Expire: Shouldn't happen! for & " & S (This.Address), Trace.Error);
      Actions.Maintenance (This);
      This.Status := Dead;
   end Ready_Expire;

   ------------------------------------------------------------------------
   -- Unknown_Timeout                                                    --
   ------------------------------------------------------------------------
   procedure Unknown_Timeout (This : in out Hub.Object; Signal : in Signals.Object)  is
      pragma Unreferenced (Signal);
   begin
      Actions.Expire      (
         This,
         Globals.Options.Walk_RefreshUnit - Globals.Options.Walk_Timeout);
      This.Status := Gone;
   end Unknown_Timeout;

   ------------------------------------------------------------------------
   -- XXX_Answer                                                         --
   ------------------------------------------------------------------------
   -- When an answer is received, in any state but Hub_Checking.
   procedure XXX_Answer (This : in out Hub.Object; Signal : in Signals.Object) is
      Cron        : Chronos.Object;
      Orig_Status : constant Status_Type := This.Status;
   begin
      case Signal.Answer_Kind is
         when Signals.CRAWLA =>
            if Signal.Is_Hub then
               case This.Status is
                  when Ready | Gone | Dead | Unknown | Leaf_Waiting =>
                     Actions.Cancel    (This);
                     Actions.Count     (This, Signal);
                     Actions.Ask_Later (This, Globals.Options.Walk_RefreshUnit);
                     This.Status := Hub_Waiting;
                  when others =>
                     null;
               end case;
            else
               case This.Status is
                  when Ready | Unknown =>
                     Actions.Cancel (This);
                     Actions.Expire (This);
                     This.Status := Leaf_Waiting;
                  when others =>
                     null;
               end case;
            end if;
         when Signals.PO =>
            case This.Status is
               when Ready => -- Can be if just created in Process_Answer
                  Actions.Cancel  (This);
                  Actions.Ask_Now (This);
                  Actions.Timeout (This);
                  This.Status := Unknown;
               when others =>
                  null;
            end case;
      end case;
      if Cron.Elapsed > 2.0 then
         Trace.Log ("XXX_Answer abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
         Trace.Log ("(Kind, Status) = (" &
                    Signal.Answer_Kind'Img & ", " &
                    Orig_Status'Img & ")", Trace.Warning);
      end if;
   end XXX_Answer;

   Transitions : constant array (Status_Type, Signals.Kinds) of Transition := (
      Ready  => (
         Signals.Answer       => XXX_Answer                 'Access,
         Signals.Expire       => Ready_Expire               'Access,
         Signals.Found        => Ready_Found                'Access,
         others               => Null_Transition            'Access),
      Dead   => (
         Signals.Answer       => XXX_Answer                 'Access,
         Signals.Found        => Dead_Found                 'Access,
         Signals.Maintenance  => Dead_Maintenance           'Access,
         others               => Null_Transition            'Access),
      Gone   => (
         Signals.Answer       => XXX_Answer                 'Access,
         Signals.Expire       => Gone_Expire                'Access,
         others               => Null_Transition            'Access),
      Hub_Checking => (
         Signals.Answer       => Hub_Check_Answer           'Access,
         Signals.Timeout      => Hub_Check_Timeout          'Access,
         others               => Null_Transition            'Access),
      Hub_Waiting => (
         Signals.Answer       => Hub_Waiting_Answer         'Access,
         Signals.Ask          => Hub_Waiting_Ask            'Access,
         others               => Null_Transition            'Access),
      Leaf_Waiting => (
         Signals.Answer       => XXX_Answer                 'Access,
         Signals.Expire       => Leaf_Waiting_Expire        'Access,
         others               => Null_Transition            'Access),
      Unknown => (
         Signals.Answer       => XXX_Answer                 'Access,
         Signals.Timeout      => Unknown_Timeout            'Access,
         others               => Null_Transition            'Access),
      others =>
         (others              => Null_Transition            'Access));

   procedure Process_Signal (This : in out Object; Signal : in Signals.Object)
   is
      Evolution : Ustring;
      Null_Tr   : Boolean;
      Former_St : constant Hub.Status_Type := This.Status;
   begin
      -- Dispatch according to status/signal:
      if Transitions (This.Status, Signal.Kind) = Null_Transition'Access then
         Evolution := U ("Null Transition for " &
            S (This.Address) & " => " & This.Status'Img & " * " & Signal.Kind'Img);
         Null_Tr   := True;
      else
         Evolution := U ("     Transition for " &
            S (This.Address) & " => " & This.Status'Img & " * " & Signal.Kind'Img);
         Null_Tr   := False;
      end if;

      pragma Assert (
         Net.Counter.Hubs_Count =
            Hub.By_Status.Count (Hub.HUB_WAITING'Img) + Hub.By_Status.Count (Hub.HUB_CHECKING'Img));

      Hub.By_Status.Sum_Key (This.Status'Img, -1);

      declare
         Cron : Chronos.Object;
         St   : constant Status_Type := This.Status;
      begin
         Transitions (This.Status, Signal.Kind) (This, Signal);

         if Cron.Elapsed > 2.0 then
            Trace.Log ("Transition abnormal duration:" & Cron.Elapsed'Img, Trace.Warning);
            Trace.Log ("(Status, Signal) = (" & St'Img & ", " & Signal.Kind'Img & ")", Trace.Warning);
         end if;
      end;

      Hub.By_Status.Sum_Key (This.Status'Img, +1);

      pragma Assert (
         Net.Counter.Hubs_Count =
            Hub.By_Status.Count (Hub.HUB_WAITING'Img) + Hub.By_Status.Count (Hub.HUB_CHECKING'Img));

      ASU.Append (Evolution, " = " & This.Status'Img);

      ASU.Append (Evolution, " (Failures:" & This.Failures'Img & ")");
      if Null_Tr then
         Trace.Log (S (Evolution), Trace.Debug);
      else
         Trace.Log (S (Evolution), Trace.Debug);
      end if;
   exception
      when E : others =>
         Trace.Log ("Automaton.Process_Signal: " & Trace.Report (E), Trace.Error);
         Trace.Log ("Hub status : " & This.Status'Img & " --> " & Former_St'Img,
            Trace.Error);
         Trace.Log ("Signal     : " & Signal.Kind'Img, Trace.Error);
   end Process_Signal;

end Aenea.Hub.Automaton;
