-- Test Program which tests the settings of the cabs
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Cabs;
use Cabs;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Cabs_Test is

   package Cab_IO is new Ada.Text_IO.Integer_IO (Control_Cab_ID);

   package Percent_IO is new Ada.Text_IO.Integer_IO (Percent);

   procedure Out_Setting (Cab : in Control_Cab_ID) is

      Output_Percent : Percent := 0;

   begin
      New_Line;
      Get (Cab, Output_Percent);
      Put ("Current Setting: ");
      Put (Integer (Output_Percent), 0);
   end Out_Setting;

   procedure Out_Limit (Cab : in Control_Cab_ID) is

      Output_Percent : Percent := 0;

   begin
      New_Line;
      Get_Limit (Cab, Output_Percent);
      Put ("Current Limit: ");
      Put (Integer (Output_Percent), 0);
      New_Line;
   end Out_Limit;

   In_Cab     : Control_Cab_ID;
   In_Percent : Percent;

begin

   Put_Line ("Cleveland Express Cab Test");
   New_Line (2);
   loop

      Put_Line ("Which cab to set the Limit?");
      Put ("Cab: ");

      Cab_IO.Get (In_Cab);

      New_Line (2);
      Put_Line ("What Percent?");
      Put ("Percent: ");

      Percent_IO.Get (In_Percent);

      Set_Limit (In_Cab, In_Percent);

      Out_Setting (In_Cab);
      Out_Limit (In_Cab);
      New_Line (2);

      Put_Line ("Which cab to set?");
      Put ("Cab: ");

      Cab_IO.Get (In_Cab);

      New_Line (2);
      Put_Line ("What Percent?");
      Put ("Percent: ");

      Percent_IO.Get (In_Percent);

      Set (In_Cab, In_Percent);

      Out_Setting (In_Cab);
      Out_Limit (In_Cab);
      New_Line (2);

   end loop;

end Cabs_Test;
