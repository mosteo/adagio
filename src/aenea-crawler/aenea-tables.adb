with Agpl.Http.Server.Sort_Handler;

package body Aenea.Tables is

   ------------------
   -- Interval_Key --
   ------------------

   function Interval_Key (Num, Step : in Natural;
                          Pad       : in Natural := 4) return String is
      use Agpl.Http.Server.Sort_Handler;
      Idx : constant Natural := Num / Step;
   begin
      return S (Rpad (Idx * Step, Pad)) & "-" &
             S (Rpad ((Idx + 1) * Step - 1, Pad));
   end Interval_Key;

end Aenea.Tables;
