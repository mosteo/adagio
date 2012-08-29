--  Package with OS dependent functions.

package Adagio.Os is

   Nul              : constant Character := Character'Val(0);

   Folder_separator : constant Character := '/';

   Version_Suffix   : constant String    := " linux";

   --  Simple message box:
   type Message_kind is (Message_error, Message_informative);
   procedure Message_box
     (title, text: String; Kind: Message_kind:= Message_error);

   -- Kill entire process inmediately:
   procedure Kill_me;

end Adagio.Os;
