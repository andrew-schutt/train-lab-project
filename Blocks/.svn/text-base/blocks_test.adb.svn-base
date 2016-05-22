-- Test Program for Blocks
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Layout;
with Blocks;
with Cabs; use Cabs;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Blocks_test is

   package Cab_IO is new Ada.Text_IO.Integer_IO (Cabs.Control_Cab_ID);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_Number);
   package Pol_IO is new Ada.Text_IO.Enumeration_IO (Layout.Polarity);
   package Percent_IO is new Ada.Text_IO.Integer_IO (Percent);

   Decision : Natural;
   Test_Cab : Cabs.Control_Cab_ID;
   Test_Block : Layout.Block_Number;
   Test_Direction : Layout.Polarity;
   Test_Percent : Cabs.Percent;
   Null_Cab : constant Cabs.Cab_ID := 0;
begin

   Put_Line ("Cleveland Express Blocks Test");
   New_Line (2);
   loop

      Put_Line ("What would you like to do?");
      Put_Line ("1 : Power a block");
      Put_Line ("2 : Reserve a block");
      Put_Line ("3 : Un-Reserve a block");
      Put_Line ("4 : Print Reserved blocks");

      Get (Decision);
      Skip_Line;

      if Decision = 1 then
         Put_Line ("Which block?");
         New_Line;
         Block_IO.Get (Test_Block);
         Skip_Line;
         New_Line;

         Put_Line ("Which cab?");
         New_Line;
         Cab_IO.Get (Test_Cab);
         Skip_Line;
         New_Line;

         Put_Line ("Which direction?");
         New_Line;
         Pol_IO.Get (Test_Direction);
         Skip_Line;
         New_Line;

         Put_Line ("How much power?");
         New_Line;
         Percent_IO.Get (Test_Percent);
         Skip_Line;
         New_Line;

         Blocks.Set_Power (Test_Cab, Test_Block, Test_Direction);

         Cabs.Set_Limit (Test_Cab, Test_Percent);
         Cabs.Set (Test_Cab, Test_Percent);

         Put_Line ("Pressing Enter will cut power from the block");
         Skip_Line;
         Blocks.Set_Power (Null_Cab, Test_Block, Test_Direction);

      elsif Decision = 2 then
         null;
      elsif Decision = 3 then
         null;
      else
         null;
      end if;

   end loop;

end Blocks_test;
