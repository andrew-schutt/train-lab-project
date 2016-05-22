-- Specification for the Hand Controllers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson & Andrew Schutt

with ADC;

package hand_controller is

   type Controller     is (A, B, C);

   type Three_Position is (Left, Right, Centered);

   type Two_Position   is (Backward, Forward);

   type Button_Position is (Down, Up);

   procedure Read (Device                : in  Controller;
                   Black_Button          : out Button_Position;
                   Red_Button            : out Button_Position;
                   Two_Position_Switch   : out Two_Position;
                   Three_Position_Switch : out Three_Position;
                   Black_Knob            : out ADC.Input_Volts);
   --
   --
   -- This procedure returns the current state of the given controller.
   --
   --

private
   for Button_Position       use (Down => 2#0#, Up => 2#1#);
   for Button_Position 'Size use 1;
   for Two_Position          use (Forward => 2#1#, Backward => 2#0#);
   for Two_Position 'Size    use 1;
   for Three_Position        use (Left     => 2#01#, Right => 2#10#,
                                  Centered => 2#11#);
   for Three_Position 'Size  use  2;
end hand_controller;

