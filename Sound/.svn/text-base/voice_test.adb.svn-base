-- Test Program for doubletalk.adb
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt

with DoubleTalk; use DoubleTalk;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);
procedure Voice_Test is

   My_Phrase : Phrase_Strings.Bounded_String;

   Phrase_1  : constant Phrase_Strings.Bounded_String :=
                 Phrase_Strings.To_Bounded_String ("Phrase 1");
   Phrase_2  : constant Phrase_Strings.Bounded_String :=
                 Phrase_Strings.To_Bounded_String ("Phrase 2");
   Phrase_3  : constant Phrase_Strings.Bounded_String :=
                 Phrase_Strings.To_Bounded_String ("Phrase 3");
   Phrase_4  : constant Phrase_Strings.Bounded_String :=
                 Phrase_Strings.To_Bounded_String ("Phrase 4");

begin
--   loop to test all the Voices
   for n in Voice_Range loop
      My_Phrase := Phrase_Strings.To_Bounded_String ("This is "
                                                   & Voice_Range'Image (n));
      DoubleTalk.Speak (Phrase => My_Phrase,
                        Voice  => n);
      delay 3.0;
   end loop;

   DoubleTalk.Speak (Phrase => Phrase_1,
                     Voice  => Vader);
   DoubleTalk.Speak (Phrase => Phrase_2,
                     Voice  => Vader);
   DoubleTalk.Speak (Phrase => Phrase_3,
                     Voice  => Vader);
   DoubleTalk.Speak (Phrase => Phrase_4,
                     Voice  => Vader);

end Voice_Test;
