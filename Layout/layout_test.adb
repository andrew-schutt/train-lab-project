-- Test Program for Layout
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by Andrew Schutt

with Layout; use Layout;
with Ada.Text_IO; use Ada.Text_IO;

procedure Layout_Test is

   Block      : Layout.Block_Number;
   Block_Next : Layout.Block_Number;
   Block_Pol  : Layout.Polarity;
   Turnout    : Layout.Turnout_Num;
   Turn_Limb  : Layout.Limb;
   An_End     : Layout.Block_End;

   procedure Do_Next (Block      : in     Layout.Block_Number;
                      Block_Pol  : in     Layout.Polarity;
                      Block_Next :    out Layout.Block_Number;
                      Turnout    :    out Layout.Turnout_Num;
                      Turn_Limb  : in     Layout.Limb;
                      An_End     : in     Layout.Block_End) is
   begin

      case An_End is
         when A_Block =>
            Layout.Next_Block (Block, Block_Pol, Block_Next);
            Put ("The next Block is number: "
                 & Block_Number'Image (Block_Next));
            New_Line;
         when Choice =>
            Layout.Get_Turnout_Num (Block, Block_Pol, Turnout);
            Put ("The number of the turnout is " & Turnout_Num'Image (Turnout));
            New_Line;
            Layout.Take_Limb (Turnout, Turn_Limb, Block_Next);
            Put ("The end of the left limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;
            if Layout.Is_Joint (Turn => Turnout,
                                Dir  => Turn_Limb,
                                A_Polarity => Block_Pol) then
               Put ("Is a joint turnout: ");
               Put_Line (Turnout_Num'Image (
                 Layout.Get_Joint_Turn (Turn      => Turnout,
                                        Block_Pol => Block_Pol,
                                        Dir       => Turn_Limb)));
            end if;
            New_Line;
            Layout.Take_Limb (Turnout, Layout.Opposite (Turn_Limb), Block_Next);
            Put ("The end of the right limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;
            if Layout.Is_Joint (Turn => Turnout,
                                Dir  => Opposite (Turn_Limb),
                                A_Polarity => Block_Pol) then
               Put ("Is a joint turnout: ");
                 Put_Line (Turnout_Num'Image (
                   Layout.Get_Joint_Turn (Turn      => Turnout,
                                          Block_Pol => Block_Pol,
                                          Dir       =>
                                          Opposite (Turn_Limb))));
            end if;
            New_Line;
            Layout.Take_Limb (Turnout, Layout.Common, Block_Next);
            Put ("The end of the common limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;

         when Forced =>
            Layout.Get_Turnout_Num (Block, Block_Pol, Turnout);
            Put ("The number of the turnout is " & Turnout_Num'Image (Turnout));
            New_Line;
            Layout.Take_Limb (Turnout, Turn_Limb, Block_Next);
            Put ("The end of the left limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;
            Layout.Take_Limb (Turnout, Layout.Opposite (Turn_Limb), Block_Next);
            Put ("The end of the right limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;
            Layout.Take_Limb (Turnout, Layout.Common, Block_Next);
            Put ("The end of the common limb is block "
                 & Block_Number'Image (Block_Next));
            New_Line;
         when A_Deadend =>
            Put ("Don't do anything. We have reached a dead end.");
            New_Line;
      end case;
   end Do_Next;

begin

   for i in Block_Number loop
      for n in Polarity loop
         Block     := i;
         Block_Pol := n;
         Turn_Limb := Left;

         New_Line;
         New_Line;
         An_End := Layout.On_The_End (Block_Pol, Block);

         New_Line;
         Ada.Text_IO.Put ("The end of Block number " &
                          Block_Number'Image (Block) &
                          " in the " & Polarity'Image (Block_Pol) &
                          " is a: " & Block_End'Image (An_End));
         New_Line;

         Do_Next (Block      => Block,
                  Block_Pol  => Block_Pol,
                  Block_Next => Block_Next,
                  Turnout    => Turnout,
                  Turn_Limb  => Turn_Limb,
                  An_End     => An_End);

      end loop;
   end loop;



end Layout_Test;
