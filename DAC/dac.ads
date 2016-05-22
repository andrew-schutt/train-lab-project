-- Specification for Digital to Analog portions of the CIO-DDA06/Jr board
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Elliot Schmitt

package DAC is

   type Channel_Number is range 0 .. 5;
   type Output_Volts is delta 1.0 / 2.0**12 range -5.0 .. 5.0;

   procedure Write (Channel : in Channel_Number;
                    Value   : in Output_Volts);

   -- Writes the voltage on the given channel

end DAC;
