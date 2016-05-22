-- Button Press Count Test for Hand Controller
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Hand_Controller;
use hand_controller;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Button_Test is

   Black : Button_Position;
   Red   : Button_Position;
   Two   : Two_Position;
   Three : Three_Position;

   Red_Was_Down   : Boolean := False;
   Black_Was_Down : Boolean := False;

   Push_Count     : Natural := 0;

begin

   loop

      Read (B, Black, Red, Two, Three);
      if Black = Down then
         Black_Was_Down := True;
      else
         if Black_Was_Down then
            Push_Count := Push_Count + 1;
            Black_Was_Down := False;
         end if;
      end if;

      if Red = Down then
         Red_Was_Down := True;
      else
         if Red_Was_Down then
            Put ("Button Presses: ");
            Ada.Integer_Text_IO.Put (Push_Count, 0);
            New_Line (2);
            Push_Count := 0;
            Red_Was_Down := False;
         end if;
      end if;
      delay 0.10;
   end loop;

end Button_Test;
