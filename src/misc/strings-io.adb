with Ada.Strings; use Ada.Strings;

package body Strings.IO is

   ---------
   -- Put --
   ---------
   -- In effect calls the function Pad, and outputs the return
   -- string

   procedure Put (
      File     : in   File_Type;
      Item     : in   String;
      Width    : in   Positive;
      Truncate : in   Boolean := True;
      Align    : in   Alignment := Left)
   is
      procedure Spaces (Count : Natural) is
      begin
         for i in 1..Count loop
            Put (File, ' ');
         end loop;
      end Spaces;


      Length : constant Natural := Integer'Min (Item'Length,
                                                Width);
      -- natural in case of null string

   begin
      if Width <= Item'Length then
         -- truncation
         if Truncate then
            Put (File, Item (Item'First..Item'First + Length - 1));
            -- prefer Item (1..Length), but can't be sure 'first _is_ 1! :-(
         else
            Put (File, Item);
         end if;

      else
         -- Padding
         declare
            No_Spaces : constant Positive := Width - Item'Length;
         begin
            case Align is
               when Left =>
                  Put (File, Item);
                  Spaces (No_Spaces);

               when Center =>
                  declare
                     Pre  : constant Natural := No_Spaces / 2;
                     Post : constant Natural := No_Spaces - Pre;
                  begin
                     Spaces (Pre);
                     Put (File, Item);
                     Spaces (Post);
                  end;

               when Right =>
                  Spaces (No_Spaces);
                  Put (File, Item);
            end case;
         end;
      end if;
   end Put;

   procedure Put (
      Item     : in   String;
      Width    : in   Positive;
      Truncate : in   Boolean := True;
      Align    : in   Alignment := Left)
   is
   begin
      Put (Standard_Output, Item, Width, Truncate, Align);
   end Put;

end Strings.IO;
