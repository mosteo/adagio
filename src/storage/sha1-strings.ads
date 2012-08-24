package Sha1.Strings is

   function Hash
     (s: String; Implementation: Method:= Time_efficient) return Digest;
   
end Sha1.Strings;
