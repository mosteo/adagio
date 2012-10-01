package body Adagio.Storage.HTable is

   function Hash_handler_string is new
      Gnat.HTable.Hash(Hash_range);
   
   function Hash_handler(Key: Ada.Strings.Unbounded.Unbounded_string) 
      return Hash_range is
   begin
      return Hash_handler_string(Ada.Strings.Unbounded.To_string(Key));
   end Hash_handler;

   -- Hash table from gnat, to store handlers.
   function Equal(a, b: Ada.Strings.Unbounded.Unbounded_string)
      return boolean is
      use type Ada.Strings.Unbounded.Unbounded_string;
   begin
      return a = b;
   end Equal;
   pragma Inline(Equal);
end Adagio.Storage.HTable;
