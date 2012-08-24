with System.Error_reporting;
with Text_io;

package body Adagio.Os is

   ------------------------------------------------------------------------
   -- Message_box                                                        --
   ------------------------------------------------------------------------
   procedure Message_box
     (title, text: String; Kind: Message_kind:= Message_error) is
     use Text_io;
   begin
      Put_line ("** " & Title & " **");
      Put_line ("** " & Text & " **");
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

