-- Specification for Analog to Digitial portions of the CIO-DAS08/Jr board
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

package ADC is

   type Channel_Number is range 0 .. 7;
   type Input_Volts is delta 1.0 / 2.0**12 range -5.0 .. 5.0;

   procedure Read (Channel : in Channel_Number;
                   Value   : out Input_Volts);

   -- Reads the voltage on the given channel

end ADC;
