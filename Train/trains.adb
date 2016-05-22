-- This package is the body for trains
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Blocks;
with hand_controller; use hand_controller;
with Turnouts;
with Ada.Text_IO; use Ada.Text_IO;
--  with Display;

package body Trains is

   Directions       : Direction_Set;
   Polarities       : Polarity_Set;
   Train_Cabs_List  : Train_Cabs;
   Number_Powered   : Powered_Blocks;
   Locos            : Loco_Recs;

   Train_Units     : constant Train_Unit_Array := (1 => 1,
                                                   2 => 2,
                                                   3 => 3);
   -- protected type to store Stop Records
   protected type Stop_Protector is
      entry Try_To_Reserve;
      procedure Set_Waiting_Block (Block  : in Layout.Block_Number);
      procedure Set_Reason (Reason : in Stop_Reason;
                            Status : in Boolean);
      procedure Set_Wait_Turnout (Turnout : in Layout.Turnout_Num;
                                  Status  : in Boolean);
      procedure Set_Train (In_Train : in Request_ID);
      function Can_I_Go return Boolean;

   private
      Train        : Request_ID := 0;
      Stop_Reasons : Stop_Rec;
      Waiting      : Boolean := False;
   end Stop_Protector;

   protected body Stop_Protector is

      entry Try_To_Reserve when Waiting is
         Success : Boolean;
      begin
         Blocks.Reserve_Block (Stop_Reasons.Block, Train, Success);
         if Success then
            Waiting := False;
            Stop_Reasons.Reasons (Reservation_Failure) := False;
            if Can_I_Go then
               Cabs.Set_Limit (Train_Cabs_List (Train), 100);
               Sound.Bell_On (Unit => Train_Units (Train));
            end if;
         end if;
      end Try_To_Reserve;

      procedure Set_Waiting_Block (Block : in Layout.Block_Number) is
      begin
         Stop_Reasons.Block := Block;
         Stop_Reasons.Reasons (Reservation_Failure) := True;
         Waiting := True;
      end Set_Waiting_Block;

      procedure Set_Reason (Reason : in Stop_Reason;
                            Status : in Boolean) is
      begin
         Stop_Reasons.Reasons (Reason) := Status;
--           Display.Put (Train  => Train,
--                        Status => Stop_Reasons);
      end Set_Reason;

      procedure Set_Wait_Turnout (Turnout : in Layout.Turnout_Num;
                                  Status  : in Boolean) is
         All_Clear : Boolean;
      begin
         Stop_Reasons.Turnouts (Turnout) := Status;
         if Status = False then
            for Turns in Stop_Reasons.Turnouts'Range loop
               if Stop_Reasons.Turnouts (Turns) then
                  All_Clear := False;
               end if;
            end loop;
            if All_Clear then
               Stop_Reasons.Reasons (Turnout_Failure) := False;
            end if;
         end if;
      end Set_Wait_Turnout;

      procedure Set_Train (In_Train : in Request_ID) is
      begin
         Train := In_Train;
      end Set_Train;

      function Can_I_Go return Boolean is
         Can_Go : Boolean := True;
      begin
--           Display.Put (Train  => Train,
--                        Status => Stop_Reasons);
         for Reason in Stop_Reason'Range loop
            if Stop_Reasons.Reasons (Reason) then
               Can_Go := False;
            end if;
         end loop;
--           Display.Put (Train  => Train,
--                        Status => Stop_Reasons);
         return Can_Go;
      end Can_I_Go;

   end Stop_Protector;

   type Volts is delta 1.0 / 2.0 ** 12 range -45_000.0 .. 45_000.0;

   type Train_Stops_Array is array (Train_ID) of Stop_Protector;
   Stops : Train_Stops_Array;

   -- task for waiting for reserved blocks
   task type Wait_For_Block is
      entry Set_Train (This_Train : in Train_ID);
   end Wait_For_Block;

   task body Wait_For_Block is
      My_Train : Train_ID;
   begin
      accept Set_Train (This_Train : in Train_ID) do
         My_Train := This_Train;
      end Set_Train;

      loop
         Stops (My_Train).Try_To_Reserve;
         delay 2.0;
      end loop;

   end Wait_For_Block;

   -- Array of block waiting tasks, one for each train
   type Block_Task_Array is array (Train_ID) of Wait_For_Block;
   Wait_Blocks : Block_Task_Array;
---------------------------------------------------------------------------
   procedure Get_Reserve_Block (Next_Block    : in Layout.Block_Number;
                                Train         : in Trains.Train_ID;
                                Reserve_Block : out Layout.Block_Number;
                                To_Reserve    : out Boolean) is

      My_End          : Layout.Block_End;
      Choice_Turn_Num : Layout.Turnout_Num;
      Choice_Turn_Dir : Layout.Turn_Dir;

      Force_Turn_Num : Layout.Turnout_Num;
      Force_Turn_Dir : Layout.Turn_Dir;

   begin
      To_Reserve := True;

      My_End := Layout.On_The_End (Polarities (Train), Next_Block);
      if My_End = A_Block then
         Layout.Next_Block (Current_Block => Next_Block,
                            A_Polarity    => Polarities (Train),
                            Next_Block    => Reserve_Block);
      elsif My_End = Choice then
         Layout.Get_Turnout_Num (Next_Block, Polarities (Train),
                                 Choice_Turn_Num);

         Choice_Turn_Dir := Turnouts.Direction_Of  (Choice_Turn_Num);

         Layout.Take_Limb (Turn_Num    => Choice_Turn_Num,
                           Turn_Choice => Choice_Turn_Dir,
                           Next_Limb   => Reserve_Block);
         if Layout.Is_Joint (Choice_Turn_Num, Choice_Turn_Dir,
                             Polarities (Train)) then

            Force_Turn_Num := Layout.Get_Joint_Turn
                                                  (Turn      => Choice_Turn_Num,
                                                Block_Pol => Polarities (Train),
                                                  Dir       => Choice_Turn_Dir);

            Layout.Force_Direction (Next_Block       => Next_Block,
                                    Turn_To_Force    => Force_Turn_Num,
                                    F_Dir            => Force_Turn_Dir);

            Turnouts.Set (Requestor => Train,
                          Turnout   => Force_Turn_Num,
                          Direction => Force_Turn_Dir);
         end if;
      elsif My_End = Forced then
         Layout.Get_Turnout_Num (Next_Block, Polarities (Train),
                                 Choice_Turn_Num);
         Layout.Take_Limb (Turn_Num    => Choice_Turn_Num,
                           Turn_Choice => Common,
                           Next_Limb   => Reserve_Block);

         Layout.Force_Direction (Next_Block    => Next_Block,
                                 Turn_To_Force => Choice_Turn_Num,
                                 F_Dir         => Force_Turn_Dir);

         Turnouts.Set (Requestor => Train,
                       Turnout   => Choice_Turn_Num,
                       Direction => Force_Turn_Dir);
      elsif My_End = A_Deadend then
         To_Reserve := False;
      end if;
   end Get_Reserve_Block;

-------------------------------------------------------------
   procedure Hall_Triggered (Hall : in Layout.Hall_Sensor) is

      To_Reserve      : Boolean;
      IsBack          : Boolean := False;
      Reserve_Success : Boolean;
      Normal_Block    : Layout.Block_Number;
      Reverse_Block   : Layout.Block_Number;

      Next_Block      : Layout.Block_Number;
      Current_Block   : Layout.Block_Number;
      The_Train       : Train_ID;


      Reserve_Block   : Layout.Block_Number;
   begin
      --  Returns the blocks on each end of the hall sensor
      Layout.Hall_Blocks (Sensor         => Hall,
                          Normal_Block   => Normal_Block,
                          Reversed_Block => Reverse_Block);
      -- if block 1 doesn't have power, then it is the next block
      if Blocks.Cab_Powering (Normal_Block) = 0 or
        Blocks.Cab_Powering (Normal_Block) = 7 then
         Next_Block := Normal_Block;
         Current_Block := Reverse_Block;
         The_Train := Blocks.Train_Reserving (Current_Block);
         -- else if block 2 doesn't have power, then it is the next block
      elsif Blocks.Cab_Powering (Reverse_Block) = 0 or
        Blocks.Cab_Powering (Reverse_Block) = 7 then
         Next_Block := Reverse_Block;
         Current_Block := Normal_Block;
         The_Train := Blocks.Train_Reserving (Current_Block);
         -- else the back of the back of the train must have tripped the sensor
      else
         IsBack := True;
         The_Train := Blocks.Train_Reserving (Normal_Block);
      end if;
      -- operations for the front of the train
      if not IsBack then
         --
         if Layout.Is_Reversing_Point (Hall) then
            Polarities (The_Train) := Layout.Opposite (Polarities (The_Train));
         end if;

         Blocks.Set_Power (Cab       => Train_Cabs_List (The_Train),
                           Block     => Next_Block,
                           Direction => Polarities (The_Train));

         -- Add 1 to the number of blocks the train has powered
         Number_Powered (The_Train).Size := Number_Powered (The_Train).Size + 1;
         Number_Powered (The_Train).Blocks (Number_Powered (The_Train).Size) :=
           Next_Block;

         Get_Reserve_Block (Next_Block     => Next_Block,
                            Train          => The_Train,
                            Reserve_Block  => Reserve_Block,
                            To_Reserve     => To_Reserve);
         -- Figure out which block is after Next_Block
         -- Reserve next block
         if To_Reserve then
            Blocks.Reserve_Block (Block   => Reserve_Block,
                                  Train   => The_Train,
                                  Success => Reserve_Success);
            -- Reserve failed operations
            if not Reserve_Success then
               Cabs.Set_Limit (Cab   => Train_Cabs_List (The_Train),
                               Value => 0);
               Stops (The_Train).Set_Waiting_Block (Reserve_Block);
            end if;
         end if;            -- Check for train too long / lost caboose
         if Number_Powered (The_Train).Size > Max_Length_Running then
            Cabs.Set_Limit (Cab   => Train_Cabs_List (The_Train),
                            Value => 0);
            Sound.Sound_Horn (Train_Units (The_Train), Stop);
            Stops (The_Train).Set_Reason (Lost_Caboose, True);
         end if;
         -- operations for the back of the train
      else

         Blocks.Un_Reserve_Block (Number_Powered (The_Train).Blocks (1),
                                  The_Train);


         Blocks.Set_Power (7, Number_Powered (The_Train).Blocks (1), Normal);
         -- Subtract 1 from the number of blocks the train has powered
         Number_Powered (The_Train).Size := Number_Powered (The_Train).Size - 1;

         for Block in 1 .. Number_Powered (The_Train).Size loop
            Number_Powered (The_Train).Blocks (Block) :=
              Number_Powered (The_Train).Blocks (Block + 1);
         end loop;

         -- Adjust for possibility of waiting on lost caboose
         if Number_Powered (The_Train).Size < Max_Length_Running then
            Stops (The_Train).Set_Reason (Lost_Caboose, False);
         end if;
      end if;
   end Hall_Triggered;

   procedure Dispatcher_Halt (Train  : in Train_ID) is
   begin
      Cabs.Set_Limit (Train_Cabs_List (Train), 0);
      Sound.Sound_Horn (Train_Units (Train), Stop);
      Stops (Train).Set_Reason (Dispatcher_Request, True);
   end Dispatcher_Halt;

   procedure Turnout_Fail_Halt (Train   : in Request_ID;
                                Turnout : in Layout.Turnout_Num) is
   begin
      Cabs.Set_Limit (Train_Cabs_List (Train), 0);
      Sound.Sound_Horn (Train_Units (Train), Stop);
      Stops (Train).Set_Wait_Turnout (Turnout, True);
      Stops (Train).Set_Reason (Turnout_Failure, True);
   end Turnout_Fail_Halt;

   procedure Turnout_Fixed (Turnout : in Layout.Turnout_Num) is
   begin
      for Train in Train_ID'Range loop
         Stops (Train).Set_Wait_Turnout (Turnout, False);
         if Stops (Train).Can_I_Go then
            Cabs.Set_Limit (Train_Cabs_List (Train), 100);
            Sound.Bell_On (Unit => Train_Units (Train));
         end if;
      end loop;
   end Turnout_Fixed;

   procedure Dispatcher_Go (Train   : in Train_ID;
                            Success : out Boolean) is
   begin
      Stops (Train).Set_Reason (Dispatcher_Request, False);
      if Stops (Train).Can_I_Go then
         Cabs.Set_Limit (Train_Cabs_List (Train), 100);
         Success := True;
         Sound.Bell_On (Unit => Train_Units (Train));
      else
         Success := False;
      end if;
   end Dispatcher_Go;

   procedure Set_Throttle (Train    : in Train_ID;
                           In_Volts : in ADC.Input_Volts) is
      Amount : Cabs.Percent;
      Can_Go : Boolean;
   begin
      Can_Go := Stops (Train).Can_I_Go;
      if Can_Go then
         Amount := Percent (20 * Volts (In_Volts)); -- change to 0 - 5
         Cabs.Set (Train_Cabs_List (Train), Amount);
         if Amount > 35 then
            Sound.Bell_Off (Unit => Train_Units (Train));
         end if;
--           Display.Put (Train    => Train,
--                        Throttle => Amount);
      end if;
   end Set_Throttle;

   procedure Change_Moving_Direction (Train : in Train_ID;
                                   Setting : in hand_controller.Two_Position) is
      My_Hall : Layout.Hall_Sensor;
      Count   : Natural;
      Temp_List : Power_List;
      Reserve_Block : Layout.Block_Number;
      Un_Reserve : Layout.Block_Number;
      To_Reserve    : Boolean;
      Reserve_Success : Boolean;
   begin
      if (Setting = Backward and Directions (Train) = Forward) or
        (Setting = Forward and Directions (Train) = Backward) then

         Polarities (Train) := Layout.Opposite (Polarities (Train));

         for Block in 1 .. Number_Powered (Train).Size loop
            if Block > 1 and Block < Number_Powered (Train).Size then
               My_Hall := Layout.Get_Sensor_Number
                 (Number_Powered (Train).Blocks (Block - 1),
                  Number_Powered (Train).Blocks (Block));
               if Layout.Is_Reversing_Point (My_Hall) then
                  Polarities (Train) := Layout.Opposite (Polarities (Train));
               end if;
            end if;

            Blocks.Set_Power (Cab => Train_Cabs_List (Train),
                             Block => Number_Powered (Train).Blocks (Block),
                              Direction => Polarities (Train));
         end loop;

--           Polarities (Train) := Layout.Opposite (Polarities (Train));
         Put_Line ("Pol: " & Layout.Polarity'Image (Polarities (Train)));
         Get_Reserve_Block (Next_Block => Number_Powered (Train).Blocks (1),
                            Train         => Train,
                            Reserve_Block => Un_Reserve,
                            To_Reserve    => To_Reserve);
         Put_Line ("Block 1: " & Layout.Block_Number'Image
                   (Number_Powered (Train).Blocks (1)));
         Put_Line ("Block: " & Layout.Block_Number'Image (Un_Reserve));
         if To_Reserve then
            Blocks.Reserve_Block (Un_Reserve, Train, Reserve_Success);
         end if;

         Put_Line ("Pol: " & Layout.Polarity'Image (Polarities (Train)));
         Temp_List := Number_Powered (Train).Blocks;
         Count := Number_Powered (Train).Size;
         for Block in 1 .. Number_Powered (Train).Size loop
            Number_Powered (Train).Blocks (Block) :=
              Temp_List (Count);
            Count := Count - 1;
         end loop;
         Polarities (Train) := Layout.Opposite (Polarities (Train));
         Put_Line ("Pol: " & Layout.Polarity'Image (Polarities (Train)));
         Get_Reserve_Block (Next_Block => Number_Powered (Train).Blocks (1),
                            Train         => Train,
                            Reserve_Block => Reserve_Block,
                            To_Reserve    => To_Reserve);
         Put_Line ("Block: " & Layout.Block_Number'Image (Reserve_Block));
         if To_Reserve then
            Blocks.Un_Reserve_Block (Block   => Reserve_Block,
                                     Train   => Train);
         end if;
         Polarities (Train) := Layout.Opposite (Polarities (Train));
         if not Reserve_Success then
            Engineer_Turn_Wait (Train, Reserve_Block);
         end if;

         if Directions (Train) = Backward then
            Directions (Train) := Forward;
         else
            Directions (Train) := Backward;
            Sound.Bell_On (Unit => Train_Units (Train));
         end if;
--
--           Display.Put (Train     => Train,
--                        Direction => Directions (Train));


      end if;
   end Change_Moving_Direction;

   procedure Set_Direction (Train     : in Train_ID;
                            Direction : in Direction_Type) is
   begin
      Directions (Train) := Direction;
--        Display.Put (Train     => Train,
--                     Direction => Direction);
   end Set_Direction;

   procedure Sound_Horn (Moving : in Boolean;
                         Train  : in Train_ID) is
   begin
      if Moving then
         Sound.Sound_Horn (Train_Units (Train), Approach_Highway);
      else
         Sound.Sound_Horn (Train_Units (Train), Start);
      end if;
   end Sound_Horn;

   function Get_Direction (Train : in Train_ID) return Direction_Type is
   begin
      return Directions (Train);
   end Get_Direction;

   procedure Set_Cab (Train : in Train_ID;
                      Cab   : in Cabs.Cab_ID) is
   begin
      Train_Cabs_List (Train) := Cab;
   end Set_Cab;

   function Get_Cab (Train : in Train_ID) return Cabs.Cab_ID is
   begin
      return Train_Cabs_List (Train);
   end Get_Cab;

   procedure Set_Name (Train : in Train_ID;
                       Name : in Locomotives.Loco_String) is
   begin
       Locos (Train).Name := Name;
   end Set_Name;

   function Get_Name (Train : in Train_ID) return Locomotives.Loco_String is
   begin
       return Locos (Train).Name;
   end Get_Name;

   procedure Set_Min_Throttle (Train : in Train_ID;
                               Min   : in Locomotives.Percent) is
   begin
      Locos (Train).Minimum_Throttle := Min;
   end Set_Min_Throttle;

   function Get_Min_Throttle (Train : in Train_ID)
                               return Locomotives.Percent is
   begin
      return Locos (Train).Minimum_Throttle;
   end Get_Min_Throttle;

   procedure Set_Initial_Polarity (Train     : in Train_ID;
                                   Direction : in Layout.Polarity) is
   begin
      Polarities (Train) := Direction;
   end Set_Initial_Polarity;

   procedure Set_Initial_Blocks (Train  : in Train_ID;
                                 In_Blocks : in Search.Block_List) is
   begin
      Number_Powered (Train).Size := In_Blocks.Size;
      for Block in 1 .. In_Blocks.Size loop
         Number_Powered (Train).Blocks (Block) :=
           In_Blocks.Items (Block).Block;
      end loop;
   end Set_Initial_Blocks;

   function Get_Leading_Block (Train  : in Train_ID) return Layout.Block_Number
   is
   begin
      return Number_Powered (Train).Blocks (Number_Powered (Train).Size);
   end Get_Leading_Block;

   procedure Engineer_Turn_Wait (Train : in Train_ID;
                                 Block : in Layout.Block_Number) is
   begin
      Cabs.Set_Limit (Cab   => Train_Cabs_List (Train),
                      Value => 0);
      Stops (Train).Set_Waiting_Block (Block);
   end Engineer_Turn_Wait;

   function Get_Train_Polarity (Train : in Train_ID) return Layout.Polarity is
   begin
      return Polarities (Train);
   end Get_Train_Polarity;

begin
   for Trains in Train_ID'Range loop
      Stops (Trains).Set_Train (Trains);
   end loop;

   for Trains in Train_ID'Range loop
      Wait_Blocks (Trains).Set_Train (Trains);
   end loop;

   for Trains in Train_ID'Range loop
      Stops (Trains).Set_Reason (Dispatcher_Request, False);
      Stops (Trains).Set_Reason (Turnout_Failure, False);
      Stops (Trains).Set_Reason (Reservation_Failure, False);
      Stops (Trains).Set_Reason (Lost_Caboose, False);
   end loop;

end Trains;
