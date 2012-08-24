-- Package for a simple HTable based on Gnat one,
-- Using Strings as Keys and a generic data type as values.

with Ada.Strings.Unbounded;
with Gnat.HTable;

generic 
  type Item is private;
  Empty: Item;
package Adagio.Storage.HTable is

  use Ada.Strings.Unbounded;

  type Hash_range is range 0..127;

  function Hash_handler(Key: Ada.Strings.Unbounded.Unbounded_string)
     return Hash_range;
  
  package Table is new Gnat.Htable.Simple_HTable
      (Hash_range,
       Item,
       Empty,
       Ada.Strings.Unbounded.Unbounded_string,
       Hash_handler,
       "="
       );
end Adagio.Storage.HTable;
