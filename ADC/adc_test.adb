-- Test Program which diplays the status for all three Hand Controllers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with hand_controller;
use hand_controller;
with ADC;
use ADC;
with Ada.Text_IO;
use Ada.Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure ADC_test is


   package Controller_IO is new Ada.Text_IO.Enumeration_IO (Controller);

   package Volts_IO is new Ada.Text_IO.Fixed_IO (ADC.Input_Volts);


   procedure Print_Controller (Control    : in  Controller;
                               Knob_Value : in ADC.Input_Volts) is
   begin

      New_Line;
      Put ("Controller: ");
      Controller_IO.Put (Control);
      New_Line;
      Volts_IO.Put (Knob_Value);
      New_Line;

   end Print_Controller;

   Knob_Volts : ADC.Input_Volts;

begin

   loop

      Put_Line ("Press Enter");
      Skip_Line;

      Read (0, Knob_Volts);
      Print_Controller (A, Knob_Volts);


      Read (1, Knob_Volts);
      Print_Controller (B, Knob_Volts);


      Read (2, Knob_Volts);
      Print_Controller (C, Knob_Volts);

   end loop;

end ADC_test;
