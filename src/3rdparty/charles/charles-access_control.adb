with System;  use type System.Address;

package body Charles.Access_Control is

   function Get (Pointer : Pointer_Type) return Name is
   begin
      return Pointer.X;
   end;
   
   function Is_Null (Pointer : Pointer_Type) return Boolean is
   begin
      return Pointer.X = Null_Name;
   end;

   procedure Initialize
     (Pointer : in out Pointer_Type; 
      Value   : in     Name) is
   begin
      if Pointer.X /= Value then
         Free (Pointer.X);
         Pointer.X := Value;
      end if;
   end;
   
   function Release (Pointer : Pointer_Type) return Name is
      X : constant Name := Pointer.X;
   begin
      Pointer.H.P.X := Null_Name;
      return X;
   end;
   
   procedure Release (Pointer : in out Pointer_Type) is
   begin
      Pointer.X := Null_Name;
   end;
   
   procedure Free (Pointer : in out Pointer_Type) is
   begin
      Free (Pointer.X);
   end;
   

   procedure Assign
     (Target : in out Pointer_Type;
      Source : in out Pointer_Type) is
   begin
      if Source'Address = Target'Address then
         return;
      end if;
      
      Free (Target.X);      
      Target.X := Source.X;

      Source.X := Null_Name;
   end;
   

   procedure Assign
     (Target : in out Pointer_Type;
      Source : in out Name) is
   begin
      if Source /= Target.X then      
         Free (Target.X);
         Target.X := Source;
      end if;
      
      Source := Null_Name;
   end Assign;      
      
   
   procedure Finalize (Pointer : in out Pointer_Type) is
   begin
      Free (Pointer.X);
   end;
   
end Charles.Access_Control;
      