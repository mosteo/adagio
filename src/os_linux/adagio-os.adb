with Gnat.Os_Lib;
with Text_io;

package body Adagio.Os is

   ------------------------------------------------------------------------
   -- Message_box                                                        --
   ------------------------------------------------------------------------
   procedure Message_box
     (title, text: String; Kind: Message_kind:= Message_error) is
      pragma Unreferenced (Kind);
     use Text_io;
   begin
      Put_line ("** " & Title & " **");
      Put_line ("** " & Text & " **");
   end Message_box;

   ------------------------------------------------------------------------
   -- Kill_me                                                            --
   ------------------------------------------------------------------------
   --  Kill entire process inmediately:
   procedure Kill_me is
   begin
      Gnat.Os_Lib.Os_Exit (1);
   end Kill_me;

end Adagio.Os;

