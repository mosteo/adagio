package Helpers is

   function Get_Pid return Integer;
   pragma Import (C, Get_Pid, "__getpid");

end Helpers;
