package Sha1.Files is

   -- Hash a regular file.
   -- We must supply a full/relative path to the file.
   function Hash
     (Name: String; Implementation: Method:= Time_efficient) return Digest;
   
end Sha1.Files;
