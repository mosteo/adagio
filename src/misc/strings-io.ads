with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package Strings.IO is

   ---------
   -- Put --
   ---------
   -- Pad constructs new strings by truncating/padding them with spaces
   -- It either truncates or pads with spaces the item field.
   -- Alignment can be either left or right within the field
   -- Truncate specifies whether text is chopped off if it
   -- doesn't fit into the appropriate field
   -- The strings are then output to standard output
   --
   -- The Alignment options come from the package Ada.Strings and are...
   --  Left, Right, Center
   --
   -- The Truncate option only truncates on the right of the string
   -- to use this do...
   -- 
   --   with Text_IO;     use Text_IO;
   --   with Ada.Strings; use Ada.Strings;
   --   with Strings.IO;  use Strings.IO;
   --
   --  E.g.
   --       Put ("Hello", Width => 20);
   --
   --       Put ("Hello", Width => 20, Align => Right);
   --
   --       Put ("Hello", Width => 20, Align => Ada.Strings.Center);
   --
   --       Put ("Hello", Width => 2, Align => Center, Truncate => True);

   procedure Put (
      Item     : in   String;
      Width    : in   Positive;
      Truncate : in   Boolean := True;
      Align    : in   Alignment := Left);

   procedure Put (
      File     : in   File_Type;
      Item     : in   String;
      Width    : in   Positive;
      Truncate : in   Boolean := True;
      Align    : in   Alignment := Left);

end Strings.IO;
