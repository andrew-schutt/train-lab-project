-- Body for Engineers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with ADC;
use ADC;
with Trains; use Trains;
with Layout; use Layout;
with Turnouts; use Turnouts;
with Blocks; use Blocks;

package body Engineers is

   type Controller_Status is
      record
         Red_Button       : Button_Position;
         Black_Button     : Button_Position;
         Two_Position_S   : Two_Position;
         Three_Position_S : Three_Position;
         Throttle         : ADC.Input_Volts;
      end record;

   task type Engineer_Task is

      entry Enable (Train   : in Trains.Train_ID;
                    Control : in Controller;
                    Skill   : in Skill_Rating);

   end Engineer_Task;

   task body Engineer_Task is

      My_Train        : Trains.Train_ID;
      My_Controller   : Controller;
      My_Skill        : Skill_Rating := Novice;
      pragma Unreferenced (My_Skill);
      Saved           : Controller_Status;
      New_State       : Controller_Status;
      My_End          : Layout.Block_End;
      Temp_Turn       : Layout.Turnout_Num;
      Turn_Pol        : Layout.Polarity;
      Lead_Block      : Layout.Block_Number;
      Reserve_Block   : Layout.Block_Number;
      Unreserve_Block : Layout.Block_Number;
      Reserve_Train   : Trains.Request_ID;
      My_Success      : Boolean;


      Force_Turn_Num : Layout.Turnout_Num;
      Force_Turn_Dir : Layout.Turn_Dir;
   begin
      Saved.Red_Button := Up;
      Saved.Black_Button := Up;
      Saved.Two_Position_S := Forward;
      Saved.Three_Position_S := Centered;
      Saved.Throttle := 0.0;

      accept Enable (Train   : in Trains.Train_ID;
                     Control : in Controller;
                     Skill   : in Skill_Rating) do
         My_Train := Train;
         My_Controller := Control;
         My_Skill := Skill;
      end Enable;

      loop

         hand_controller.Read (Device                 => My_Controller,
                               Black_Button           => New_State.Black_Button,
                               Red_Button             => New_State.Red_Button,
                            Two_Position_Switch    =>  New_State.Two_Position_S,
                           Three_Position_Switch  => New_State.Three_Position_S,
                               Black_Knob             => New_State.Throttle);

         if Saved.Black_Button /= New_State.Black_Button then
            if New_State.Black_Button = Down then
               Trains.Sound_Horn (Moving => True,
                                  Train  => My_Train);
            end if;
         end if;

         if Saved.Red_Button /= New_State.Red_Button then
            if New_State.Red_Button = Down then
               Trains.Dispatcher_Halt (My_Train);
            end if;
         end if;

         if Saved.Two_Position_S /= New_State.Two_Position_S then
            Trains.Change_Moving_Direction (My_Train, New_State.Two_Position_S);
         end if;

         if Saved.Three_Position_S /= New_State.Three_Position_S then

            Turn_Pol := Trains.Get_Train_Polarity (My_Train);
            Lead_Block := Trains.Get_Leading_Block (My_Train);

            My_End := Layout.On_The_End
              (Block_Pol => Turn_Pol, Current_Block => Lead_Block);


            if My_End = Choice then
               if New_State.Three_Position_S = hand_controller.Left then
                  Layout.Get_Turnout_Num (Current_Block => Lead_Block,
                                          Direction     => Turn_Pol,
                                          Turn_Num      => Temp_Turn);
                  if Turnouts.Direction_Of (Turnout => Temp_Turn) /= Left then
                     Turnouts.Set (Requestor => My_Train,
                                   Turnout   => Temp_Turn,
                                   Direction => Left);

                     -- block to unreserve
                     Unreserve_Block := Layout.Limb_End (Turn_Num  => Temp_Turn,
                                                         Turn_Choice => Right);

                     -- block to reserve
                     Reserve_Block := Layout.Limb_End (Turn_Num    => Temp_Turn,
                                                       Turn_Choice => Left);

                     Reserve_Train := Blocks.Train_Reserving (Unreserve_Block);

                     if Reserve_Train = My_Train then
                        Blocks.Un_Reserve_Block (Block => Unreserve_Block,
                                                 Train => My_Train);
                     end if;
                     Blocks.Reserve_Block (Block   => Reserve_Block,
                                           Train   => My_Train,
                                           Success => My_Success);
                     if not My_Success then
                        Trains.Engineer_Turn_Wait (My_Train, Reserve_Block);
                     end if;

                     -- check for joint turnout
                     if Layout.Is_Joint (Temp_Turn, Left,
                                         Turn_Pol) then

                        Force_Turn_Num := Layout.Get_Joint_Turn
                          (Turn      => Temp_Turn,
                           Block_Pol => Turn_Pol,
                           Dir       => Left);

                        Layout.Force_Direction (Next_Block   => Lead_Block,
                                             Turn_To_Force   => Force_Turn_Num,
                                            F_Dir            => Force_Turn_Dir);

                        Turnouts.Set (Requestor => My_Train,
                                      Turnout   => Force_Turn_Num,
                                      Direction => Force_Turn_Dir);
                     end if;
                  end if;

               elsif New_State.Three_Position_S = hand_controller.Right then
                  Layout.Get_Turnout_Num (Current_Block => Lead_Block,
                                          Direction     => Turn_Pol,
                                          Turn_Num      => Temp_Turn);
                  if Turnouts.Direction_Of (Turnout => Temp_Turn) /= Right then
                     Turnouts.Set (Requestor => My_Train,
                                   Turnout   => Temp_Turn,
                                   Direction => Right);

                     -- block to unreserve
                     Unreserve_Block := Layout.Limb_End (Turn_Num  => Temp_Turn,
                                                         Turn_Choice => Left);

                     -- block to reserve
                     Reserve_Block := Layout.Limb_End (Turn_Num    => Temp_Turn,
                                                       Turn_Choice => Right);

                     Reserve_Train := Blocks.Train_Reserving (Unreserve_Block);

                     if Reserve_Train = My_Train then

                        Blocks.Un_Reserve_Block (Block => Unreserve_Block,
                                                 Train => My_Train);
                     end if;

                     Blocks.Reserve_Block (Block   => Reserve_Block,
                                           Train   => My_Train,
                                           Success => My_Success);
                     if not My_Success then
                        Trains.Engineer_Turn_Wait (My_Train, Reserve_Block);
                     end if;
                     -- check for joint turnout
                     if Layout.Is_Joint (Temp_Turn, Right,
                                         Turn_Pol) then

                        Force_Turn_Num := Layout.Get_Joint_Turn
                          (Turn      => Temp_Turn,
                           Block_Pol => Turn_Pol,
                           Dir       => Right);

                        Layout.Force_Direction (Next_Block    => Lead_Block,
                                                Turn_To_Force => Force_Turn_Num,
                                                F_Dir        => Force_Turn_Dir);

                        Turnouts.Set (Requestor => My_Train,
                                      Turnout   => Force_Turn_Num,
                                      Direction => Force_Turn_Dir);
                     end if;
                  end if;
               end if;
            end if;
         end if;

         if Saved.Throttle /= New_State.Throttle then
            Trains.Set_Throttle (Train    => My_Train,
                                 In_Volts => New_State.Throttle);
         end if;

         Saved.Red_Button := New_State.Red_Button;
         Saved.Black_Button := New_State.Black_Button;
         Saved.Two_Position_S := New_State.Two_Position_S;
         Saved.Three_Position_S := New_State.Three_Position_S;
         Saved.Throttle := New_State.Throttle;

      end loop;
   end Engineer_Task;

   type Engineer_Task_Array is array (Engineer_ID) of Engineer_Task;
   The_Engineers : Engineer_Task_Array;

   ------------
   -- Enable --
   ------------

   procedure Enable (Engineer   : in Engineer_ID;
                     Train      : in Trains.Train_ID;
                     Control    : in Controller;
                     Skill      : in Skill_Rating) is
   begin
      The_Engineers (Engineer).Enable (Train, Control, Skill);
   end Enable;

end Engineers;
