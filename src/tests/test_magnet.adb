with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
use  Ada;

with Text_IO; use Text_IO;

with Agpl; use  Agpl;
with Agpl.Magnet;
with Agpl.Base64;

procedure Test is

   M : Magnet.Object;
         
begin
   M := Magnet.Create (
      Base64.To_String (
         "bWFnbmV0Oj9kbj1TdGV2ZSUyME1pbGxlciUyMEJhbmQlMjAtJTIwQWJyYWNhZGFicmEubXAzJnguc3o9NDkxMzExMCZ4dD11cm46c2hhMTpPN0xTQUFXM1BBT1JGTjQ2SURPMllJNlBRS0xESFlCMw=="));
   Put_Line (Magnet.To_String (M));
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Exceptions.Exception_Information (E));
end test;
