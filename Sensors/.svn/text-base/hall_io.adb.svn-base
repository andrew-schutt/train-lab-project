-- Package body to compensate for scope
-- Hall_IO
-- Cleveland Express Spring 2011
-- Real-Time Embedded Systems
-- Managed by Elliot Schmitt

with DoubleTalk; use DoubleTalk;
with Ada.Text_IO; use Ada.Text_IO;

package body Hall_IO is

   -----------------
   -- Sensor_Trig --
   -----------------

   procedure Sensor_Trig (Hall : in Halls.Hall_ID) is
      Hall_Phrase : constant Phrase_Strings.Bounded_String :=
                         Phrase_Strings.To_Bounded_String ("Hall Sensor "
                                               & Halls.Hall_ID'Image (Hall)
                                               & " has been triggered.");


      Trigger_Phrase : Phrase_Strings.Bounded_String;

   begin


      Ada.Text_IO.Put_Line ("Hall Sensor " & Halls.Hall_ID'Image (Hall)
                            & " has been triggered.");
      DoubleTalk.Speak (Phrase => Hall_Phrase,
                        Voice  => Robo);

      delay 0.5;

      if Halls.Is_Triggered (Hall) then
         Trigger_Phrase := Phrase_Strings.To_Bounded_String
           ("The magnet is directly over the sensor.");
      else
         Trigger_Phrase := Phrase_Strings.To_Bounded_String
           ("The magnet is not on the sensor.");
      end if;

      DoubleTalk.Speak (Phrase => Trigger_Phrase,
                        Voice  => Robo);

   end Sensor_Trig;

end Hall_IO;
