-- Body for the Hand Controllers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson & Andrew Schutt

with System; use System;
with Ada.Unchecked_Conversion;
with Port_IO;
use Port_IO;

package body hand_controller is


   Base_Address : constant Port_IO.Address_Range := (16#240#);

   type Address_Array is array (Controller) of Port_IO.Address_Range;

   Address : constant Address_Array := (A => Base_Address + 12,
                                        B => Base_Address + 13,
                                        C => Base_Address + 14);

   type Controller_Rec is
      record
         Red_Button            : Button_Position;
         Black_Button          : Button_Position;
         Two_Position_Switch   : Two_Position;
         Three_Position_Switch : Three_Position;
      end record;

   for Controller_Rec use
      record
         Red_Button            at 0 range 0 .. 0;
         Black_Button          at 0 range 1 .. 1;
         Two_Position_Switch   at 0 range 2 .. 2;
         Three_Position_Switch at 0 range 3 .. 4;
      end record;

   for Controller_Rec'Size use 8;
   for Controller_Rec'Bit_Order use System.Low_Order_First;

   function To_Controller is new Ada.Unchecked_Conversion
                                                     (Source => Port_IO.Byte,
                                                      Target => Controller_Rec);


   procedure Read
     (Device                : in     Controller;
      Black_Button          :    out Button_Position;
      Red_Button            :    out Button_Position;
      Two_Position_Switch   :    out Two_Position;
      Three_Position_Switch :    out Three_Position;
      Black_Knob            :    out ADC.Input_Volts) is

      Value : Port_IO.Byte;

      Data  : Controller_Rec;

      Knob_Volts : ADC.Input_Volts;

   begin

      Value := Port_IO.In_Byte (Address (Device));

      Data := To_Controller (Value);

      Black_Button          := Data.Black_Button;
      Red_Button            := Data.Red_Button;
      Two_Position_Switch   := Data.Two_Position_Switch;
      Three_Position_Switch := Data.Three_Position_Switch;

      if Device = A then
         ADC.Read (Channel => 0,
                   Value   => Knob_Volts);
      elsif Device = B then
         ADC.Read (Channel => 1,
                   Value   => Knob_Volts);
      else
         ADC.Read (Channel => 2,
                   Value   => Knob_Volts);
      end if;

      Black_Knob := Knob_Volts;
   end Read;

end hand_controller;
