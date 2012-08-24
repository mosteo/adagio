with Ada.Finalization;

generic

   --type Object (<>) is limited private;

   type Name is private; --access all Object;
   
   Null_Name : in Name;
   
   with procedure Free (X : in out Name) is <>;
   
package Charles.Access_Control is

   type Pointer_Type is limited private;
   
   function Get (Pointer : Pointer_Type) return Name;
   
   function "+" (Pointer : Pointer_Type) return Name renames Get;
         
   procedure Initialize 
     (Pointer : in out Pointer_Type;
      Value   : in     Name);
                  
   procedure Assign 
     (Target : in out Pointer_Type;
      Source : in out Pointer_Type);
                   
   procedure Assign 
     (Target : in out Pointer_Type;
      Source : in out Name);

   function Release (Pointer : Pointer_Type) return Name;
      
   procedure Release (Pointer : in out Pointer_Type);                  

   procedure Free (Pointer : in out Pointer_Type);
   
   function Is_Null (Pointer : Pointer_Type) return Boolean;
   
private

   type Handle_Type (P : access Pointer_Type) is
      limited null record;
      
   type Pointer_Type is
      new Ada.Finalization.Limited_Controlled with record
         H : Handle_Type (Pointer_Type'Access);
         X : Name;
      end record;
      
   procedure Finalize (Pointer : in out Pointer_Type);
   
end Charles.Access_Control;

