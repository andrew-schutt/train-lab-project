-- Test Program which diplays the status for all three Hand Controllers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with hand_controller;
use hand_controller;
with Ada.Text_IO;
use Ada.Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Controller_Test is

   Black : Button_Position;
   Red   : Button_Position;
   Two   : Two_Position;
   Three : Three_Position;

   package Controller_IO is new Ada.Text_IO.Enumeration_IO (Controller);

   package Three_Position_IO is new Ada.Text_IO.Enumeration_IO (Three_Position);

   package Two_Position_IO is new Ada.Text_IO.Enumeration_IO (Two_Position);

   package Button_Position_IO is new Ada.Text_IO.Enumeration_IO
                                                              (Button_Position);

   procedure Print_Controller (Control            : in  Controller;
                            Black_Button          : in Button_Position;
                            Red_Button            : in Button_Position;
                            Two_Position_Switch   : in Two_Position;
                            Three_Position_Switch : in Three_Position) is
   begin

      Put ("Controller: ");
      Controller_IO.Put (Control);
      New_Line;
      Put ("Black Button: ");
      Button_Position_IO.Put (Black_Button);
      New_Line;
      Put ("Red Button: ");
      Button_Position_IO.Put (Red_Button);
      New_Line;
      Put ("Two Position Switch: ");
      Two_Position_IO.Put (Two_Position_Switch);
      New_Line;
      Put ("Three Position Switch: ");
      Three_Position_IO.Put (Three_Position_Switch);
      New_Line (3);

   end Print_Controller;



begin

   loop

      Put_Line ("Press Enter");
      Skip_Line;

      Read (A, Black, Red, Two, Three);
      Print_Controller (A, Black, Red, Two, Three);

      Read (B, Black, Red, Two, Three);
      Print_Controller (B, Black, Red, Two, Three);

      Read (C, Black, Red, Two, Three);
      Print_Controller (C, Black, Red, Two, Three);

   end loop;

end Controller_Test;
