with Agpl.Containers.Naked_Vectors;

generic
   type Item_type is private;
   Initial_size : Natural := 16;
   Grow_factor  : Float   := 1.5;
package Agpl.Dynamic_Vector is

   package Naked is new Agpl.Containers.Naked_Vectors (Item_Type,
                                                       Initial_Size,
                                                       Grow_Factor);

   type Object is new Naked.Object with null record;

end Agpl.Dynamic_Vector;
