generic

   type Source_Type is private;
   
   type Target_Type (<>) is limited private;
   
   with procedure Assign
     (Target : in Target_Type;
      Source : in Source_Type) is <>;

   with procedure Succ (Source : in out Source_Type) is <>;
   
   with procedure Succ (Target : in out Target_Type) is <>;

   with function "=" (L, R : Source_Type) return Boolean is <>;
   
procedure Charles.Algorithms.Generic_Copy 
  (First, Back : in     Source_Type;
   Target      : in out Target_Type);
   