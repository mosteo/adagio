with System;
with System.Error_reporting;
with Win32;
with Win32.Winuser;

package body Adagio.Os is

   --  Some linker instructions
   pragma Linker_Options ("-lkernel32");
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("-lgdi32");
   pragma Linker_Options ("-lcomdlg32");
   pragma Linker_Options ("-ladvapi32");
   pragma Linker_Options ("-lshell32");

   use type System.Address;

   ------------------------------------------------------------------------
   -- Message_box                                                        --
   ------------------------------------------------------------------------
   procedure Message_box
     (title, text: String; Kind: Message_kind:= Message_error) is
      i: Win32.INT;
      pragma Unreferenced (i);
   begin
      case Kind is
         when Message_error =>
            i:= Win32.Winuser.MessageBox
              (System.Null_address,
               Win32.Addr(text & Nul),
               Win32.Addr(title & Nul),
               Win32.Winuser.MB_ICONHAND);
         when others =>
            i:= Win32.Winuser.MessageBox
              (System.Null_address,
               Win32.Addr(text & Nul),
               Win32.Addr(title & Nul),
               Win32.Winuser.MB_ICONINFORMATION);
      end case;
   end Message_box;

   ------------------------------------------------------------------------
   -- Kill_me                                                            --
   ------------------------------------------------------------------------
   -- Kill entire process inmediately:
   procedure Kill_me is
   begin
      if System.Error_reporting.Shutdown ("Forced shutdown") then
         null;
      end if;
   end Kill_me;

end Adagio.Os;

