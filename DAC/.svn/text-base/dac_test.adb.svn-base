-- Test Program which tests the DAC output of the 6 DAC Channels
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Elliot Schmitt

with DAC;
use DAC;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure dac_test is
   Channel : Integer;
   Volts   : Integer;
begin
   loop
      Put_Line ("Enter a Channel:");
      Ada.Integer_Text_IO.Get (Channel);
      Put_Line ("Enter a Voltage:");
      Ada.Integer_Text_IO.Get (Volts);
      New_Line;

      Write (Channel_Number (Channel), Output_Volts (Volts));

   end loop;

end dac_test;
