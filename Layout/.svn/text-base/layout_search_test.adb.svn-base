-- test program for layout-search
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Andrew Schutt

with Layout;        use Layout;
with Layout.Search; use Layout.Search;
with Ada.Text_IO;   use Ada.Text_IO;

procedure Layout_Search_Test is

   package Block_Num_IO is new Ada.Text_IO.Integer_IO (Layout.Block_Number);

   Loco        : Layout.Block_Number;
   Caboose     : Layout.Block_Number;
   My_Blocks   : Block_List (4);
   My_Turnouts : Turnout_List (6);
   Success     : Boolean;

begin
   loop
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Please enter block of locomotive.");
      Ada.Text_IO.New_Line;
      Block_Num_IO.Get (Item => Loco);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Please enter block of Caboose.");
      Ada.Text_IO.New_Line;
      Block_Num_IO.Get (Item => Caboose);
      Ada.Text_IO.New_Line;

      My_Blocks.Size := 0;
      My_Turnouts.Size := 0;

      Layout.Search.Blocks_Beneath (Loco,
                                    Caboose,
                                    My_Blocks,
                                    My_Turnouts,
                                    Success);

      Ada.Text_IO.Put_Line (Boolean'Image (Success));

      New_Line;


      Put_Line ("Block_List Size" & Natural'Image (My_Blocks.Size));
      for Block_Rec in 1 .. My_Blocks.Size loop
         Put_Line (Block_Number'Image (My_Blocks.Items (Block_Rec).Block));
      end loop;

      New_Line;

      Put_Line ("Turnout_List Size" & Natural'Image (My_Turnouts.Size));
      for Turnout_Rec in 1 .. My_Turnouts.Size loop
         Put_Line (Turnout_Num'Image (My_Turnouts.Items (Turnout_Rec).Turnout));
      end loop;
      Put_Line ("----------------------------------");
   end loop;

end Layout_Search_Test;
