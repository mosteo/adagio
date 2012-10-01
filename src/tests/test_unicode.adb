with Adagio.Trace;
with Adagio.Unicode;
use  Adagio;

with Text_IO;

with Unicode.CCS.Iso_8859_1;  use Unicode.CCS;
with Unicode.CES.Utf8;        use Unicode.CES;

procedure test is
   Test_String : constant String := "xxxáéíóúüï";
begin
   Text_IO.Put_Line ("Before: " & Test_String);
   Text_IO.Put_Line ("Utf8  : " & Adagio.Unicode.To_utf8 (Test_String));
--   Text_IO.Put_Line ("Latin : " & Adagio.Unicode.From_utf8 (Test_String));
   Text_IO.Put_Line ("After : " &
      utf8.To_unicode_LE (
         Adagio.Unicode.To_utf8 (Test_String),
         Cs => Iso_8859_1.Iso_8859_1_character_set));
exception
   when E: others =>
      Text_IO.Put_Line ("Exception: " & Trace.Report (E));
end test;
