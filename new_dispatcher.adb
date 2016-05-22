-- Dispatcher Set-Up Information Validation
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Ada.Text_IO;
use Ada.Text_IO;
with Engineers; use Engineers;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Locomotives; use Locomotives;
with Trains; use Trains;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Layout; use Layout;
with Cabs; use Cabs;
with DoubleTalk; use DoubleTalk;
with Display; use Display;
with Layout.Search; use Layout.Search;
with Halls; use Halls;
with Turnouts; use Turnouts;
with Blocks; use Blocks;
with Command; use Command;
with hand_controller; use hand_controller;

with Console_Management; use Console_Management;

with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure New_Dispatcher is
   type Input_Polarity is (N, R);
   type Num_Trains is range 1 .. 3;
   type Menu_Entry is (Y, N, R, Q);

   type State_Type is (Run, Setup, Start, Shutdown);

   package Num_Trains_IO is new Ada.Text_IO.Integer_IO (Num_Trains);

   package Block_ID_IO is new Ada.Text_IO.Integer_IO (Layout.Block_Number);

   package Input_Polarity_IO is new Ada.Text_IO.Enumeration_IO (Input_Polarity);

   package Menu_Entry_IO is new Ada.Text_IO.Enumeration_IO (Menu_Entry);

   type Pol_Array is array (Trains.Train_ID) of Layout.Polarity;

   type Start_Blocks_Array is array (Trains.Train_ID)
                                 of Search.Block_List (Trains.Max_Length_Start);

   type Start_Turns_Array is array (Trains.Train_ID)
                                    of Search.Turnout_List (6);
   Initial_Pols : Pol_Array;

   Start_Blocks : Start_Blocks_Array;

   Start_Turns  : Start_Turns_Array;

--------------------------------------------------------------------------------

   -- This procedure validates the information for the Cleveland Express
   -- Welcome Screen


   procedure Get_Number_Of_Trains (Number : out Num_Trains) is

      Trains : Num_Trains;

   begin
         Put_Line ("                  Cleveland Express Software");
         New_Line;
         Put_Line ("How many trains do you wish to run?  (1, 2, or 3)");
         New_Line;
      Put_Line ("There must be at least one unoccupied block between each" &
                  " Pair of Trains");
         loop
            begin
               Num_Trains_IO.Get (Trains);
               exit;
            exception
               when Constraint_Error =>
                  Put_Line ("Invalid input, please enter a number between 1" &
                            " and 3");
               when Ada.IO_Exceptions.Data_Error =>
                  Put_Line ("Invalid input, please enter a number between 1" &
                            " and 3");
                  Skip_Line;
            end;
         end loop;
         Number := Trains;
   end Get_Number_Of_Trains;

--------------------------------------------------------------------------------
   -- This procedure validates the information for Locomotive Selection
   -- Screen

   function Locomotive_Pulling_Train (Train_Num : in Num_Trains)
                                                 return Locomotives.Loco_Rec is

      Available_Locos : constant Locomotives.Loco_Array
                                                 := Locomotives.Available_Locos;

      type Menu_Num is new Integer range 1 .. 11;

      package Menu_Num_IO is new Ada.Text_IO.Integer_IO (Menu_Num);

      subtype Throttle_Percentage is Locomotives.Percent range 1 .. 100;

      package Throttle_Percentage_IO is new Ada.Text_IO.Integer_IO
                                                          (Throttle_Percentage);

      Loco_Num : Menu_Num;

      Other_Throttle : Throttle_Percentage;

      Other_Name : Locomotives.Loco_String;

      Other_Loco_Rec : Locomotives.Loco_Rec;

      Last : Natural;

   begin
      New_Line;
      Put_Line ("The following locomotives are available");
      New_Line;
      Put_Line ("  Road Name      Model      Number");

      for Index in Available_Locos'Range loop
         Put (Index, 1);
         Put (" " & Available_Locos (Index).Name);
         New_Line;
      end loop;
      Put (Integer (Menu_Num'Last), 1);
      Put (" Other");
      New_Line;

      Put ("Enter the line number from the above table of the locomotive" &
           " pulling Train #");
      Num_Trains_IO.Put (Train_Num, 1);
      New_Line;

      loop
         begin
            Menu_Num_IO.Get (Loco_Num);
            exit;
         exception
            when Constraint_Error =>
               Put ("Invalid input, please enter a number between 1 and");
               Menu_Num_IO.Put (Menu_Num'Last, 1);
               New_Line;
            when Ada.IO_Exceptions.Data_Error =>
               Put_Line ("Invalid input, please enter a number between 1 and");
               Menu_Num_IO.Put (Menu_Num'Last, 1);
               Skip_Line;
               New_Line;
         end;
      end loop;

      if Loco_Num < Menu_Num'Last - 1 then
         return Available_Locos (Integer (Loco_Num));
      else
         -- Obtain User train info when "Other" is selected
         Put_Line ("Please enter the road name, the model and the number for" &
                   " the locomotive all seperated by a space.");
         New_Line;
         Skip_Line;
         Get_Line (Other_Name, Last);
         Move (Other_Name (1 .. Last), Other_Name);
         Put_Line ("What is the throttle percentage?");
         loop
            begin
               Throttle_Percentage_IO.Get (Other_Throttle);
               exit;
            exception
               when Constraint_Error =>
                  Put_Line ("Invalid input, please enter a percentage between" &
                            " 1 and 100");
               when Ada.IO_Exceptions.Data_Error =>
                  Put_Line ("Invalid input, please enter a percentage between" &
                            " 1 and 100");
                  Skip_Line;
            end;
         end loop;

         Other_Loco_Rec.Name := Other_Name;
         Other_Loco_Rec.Minimum_Throttle := Other_Throttle;

         return Other_Loco_Rec;

      end if;
   end Locomotive_Pulling_Train;

--------------------------------------------------------------------------------

   -- This procedure validates the information for a Locomotive on the
   -- Train Location Screen

   function Get_Loco_Block (Train_Num : in Num_Trains)
                                                  return Layout.Block_Number is

      Loco_Block : Layout.Block_Number;

   begin

       loop
         Put ("on which block is the locomotive pulling train ");
         Num_Trains_IO.Put (Train_Num, 1);
         Put (" located?");
         New_Line;
          begin
              Block_ID_IO.Get (Loco_Block);
             exit;
          exception
             when Constraint_Error =>
                Put_Line ("Invalid input, please enter a number between 1" &
                          " and 40");
             when Ada.IO_Exceptions.Data_Error =>
                Put_Line ("Invalid input, please enter a number between 1" &
                          " and 40");
                Skip_Line;
          end;
       end loop;
       return Loco_Block;
   end Get_Loco_Block;

--------------------------------------------------------------------------------
   -- This procedure validates the information for a Caboose on the
   -- Train Location Screen

   function Get_Caboose_Block (Train_Num : in Num_Trains)
                                                  return Layout.Block_Number is

      Caboose_Block : Layout.Block_Number;

   begin

       loop
         Put ("on which block is the caboose (or observation car) for train ");
         Num_Trains_IO.Put (Train_Num, 1);
         Put (" located?");
         New_Line;
          begin
              Block_ID_IO.Get (Caboose_Block);
             exit;
          exception
             when Constraint_Error =>
                Put_Line ("Invalid input, please enter a number between 1" &
                          " and 40");
             when Ada.IO_Exceptions.Data_Error =>
                Put_Line ("Invalid input, please enter a number between 1" &
                          " and 40");
                Skip_Line;
          end;
       end loop;
       return Caboose_Block;
   end Get_Caboose_Block;

--------------------------------------------------------------------------------
   -- This procedure validates the information for a direction of the Locomotive
   -- on The Train Location Screen

   function Get_Direction return Layout.Polarity is

      Loco_Polarity : Input_Polarity;

   begin

       loop
         Put_Line ("What is the direction of the locomotive on the block?");
         Put_Line ("N = Normal");
         Put_Line ("R = Reversed");

         New_Line;
          begin
              Input_Polarity_IO.Get (Loco_Polarity);
             exit;
          exception
             when Constraint_Error =>
                Put_Line ("Invalid input, please enter either N or R");
             when Ada.IO_Exceptions.Data_Error =>
                Put_Line ("Invalid input, please enter either N or R" &
                          " and 40");
                Skip_Line;
          end;
       end loop;

       if Loco_Polarity = N then
         return Layout.Normal;
       else
         return Layout.Reversed;
       end if;

   end Get_Direction;

--------------------------------------------------------------------------------
   -- This procedure validates the information for the Confirmation Screen

   procedure Get_Confirmation (Entered : out Menu_Entry) is

      Selection : Menu_Entry;

   begin

      loop

         begin
          New_Line;
          Put_Line ("Is this information correct? Enter one of the following:");
          Put_Line ("Y = yes, the information for this train is correct.");
          Put_Line ("N = no,  I wish to enter different information" &
                    "for This Train. ");
          Put_Line ("R = no, I wish to restart setting up from the beginning.");
          Put_Line ("Q = no, I wish to terminate this operating session.");

          New_Line;

          Menu_Entry_IO.Get (Selection);
          exit;
          exception
             when Constraint_Error =>
                Put_Line ("Please enter on of the specified values.");
             when Ada.IO_Exceptions.Data_Error =>
                Put_Line ("Please enter on of the specified values.");
                Skip_Line;
         end;
      end loop;

      Entered := Selection;

   end Get_Confirmation;
--------------------------------------------------------------------------------
   -- This procedure validates the information for the Confirmation Screen

   procedure Get_3_Screen (Entered : out Menu_Entry) is

      Selection : Menu_Entry;

   begin
      loop
         begin
            New_Line;
            Put_Line ("Would you like to reenter the information?");
            Put_Line ("Y = yes, I wish to reenter this train's" &
                      " location.");
            Put_Line ("N = no, I wish restart set up from the" &
                      " beginning.");
            Put_Line ("Q = no, I wish to terminate this" &
                      " operating session.");
            New_Line;
            Menu_Entry_IO.Get (Selection);
            if Selection /= R then
               exit;
            end if;
         exception
            when Constraint_Error =>
               Put_Line ("Please enter on of the specified values.");
            when Ada.IO_Exceptions.Data_Error =>
               Put_Line ("Please enter on of the specified values.");
               Skip_Line;
         end;
      end loop;

      Entered := Selection;

   end Get_3_Screen;
--------------------------------------------------------------------------------

   procedure Check_Distance (Train     : in Trains.Train_ID;
                             Loco      : in Layout.Block_Number;
                             Caboose   : in Layout.Block_Number;
                             Success   : out Boolean;
                             Confliction : out Boolean) is

      Check_Success : Boolean;
      Train_Index   : Natural := 1;
   begin
      Layout.Search.Blocks_Beneath (Loco     => Loco,
                                    Caboose  => Caboose,
                                    Blocks   => Start_Blocks (Train),
                                    Turnouts => Start_Turns (Train),
                                    Success  => Check_Success);
      if not Check_Success then
         Success := False;
         Confliction := False;
      else
         if Train = 1 then
            Success := True;
            Confliction := False;
         else
            Success := True;
            Confliction := False;
            loop
               exit when Train_ID (Train_Index) = Train;
               for Outer_Block in 1 .. Start_Blocks (Train).Size loop
                  for Inner_Block in 1 .. Start_Blocks
                    (Train_ID (Train_Index)).Size loop
                     if Start_Blocks (Train).Items (Outer_Block).Block =
                       Start_Blocks (Train_ID (Train_Index)).Items
                       (Inner_Block).Block then
                        Confliction := True;
                        exit;
                     end if;
                  end loop;
                  if Confliction then
                     exit;
                  end if;
               end loop;
               Train_Index := Train_Index + 1;
            end loop;
         end if;
      end if;

   end Check_Distance;
--------------------------------------------------------------------------------
      procedure Ready_Turnouts is
      begin
         Turnouts.Set_Failure_Callback (Trains.Turnout_Fail_Halt'Access);
         Turnouts.Set_Recovery_Callback (Trains.Turnout_Fixed'Access);
      end Ready_Turnouts;

--------------------------------------------------------------------------------
   procedure Enable_Sensors is
      Sensor_Ptr : constant Halls.Callback_Ptr := Trains.Hall_Triggered'Access;
   begin
         Halls.Initialize;
         Halls.Enable (Sensor_Ptr);
   end Enable_Sensors;
--------------------------------------------------------------------------------
   procedure Enable_Session (Train_Amount : in Num_Trains) is

      My_Hall    : Layout.Hall_Sensor;
      Start_Pols : Pol_Array := Initial_Pols;
      Reserve_Success : Boolean;
      Reserve_Block : Layout.Block_Number;
      To_Reserve    : Boolean;
      Temp_List     : Search.Block_List (3);
      Count : Natural;
   begin
      Display.Enable;
      Set_Text_Background_Color (Brown);
      Set_Text_Color (LightMagenta);
      Ready_Turnouts;
      Enable_Sensors;
      Turnouts.Set (Requestor => 0,
                    Turnout   => 6,
                    Direction => Left);
      for Train in 1 .. Train_Amount loop
         Trains.Set_Cab (Train => Train_ID (Train),
                         Cab   => Cabs.Cab_ID (Train));
         Cabs.Set_Limit (Cab   => Cabs.Cab_ID (Train),
                         Value => 0);
      end loop;

    for Train in 1 .. Train_Amount loop

         for Block in 1 .. Start_Blocks (Train_ID (Train)).Size loop
          if Block > 1 and Block < Start_Blocks (Train_ID (Train)).Size then
               My_Hall := Layout.Get_Sensor_Number
                 (Start_Blocks  (Train_ID (Train)).Items (Block - 1).Block,
                  Start_Blocks  (Train_ID (Train)).Items (Block).Block);
               if Layout.Is_Reversing_Point (My_Hall) then
                  Start_Pols (Train_ID (Train)) :=
                    Layout.Opposite (Start_Pols (Train_ID (Train)));
               end if;
          end if;
            Blocks.Set_Power (Cab       => Trains.Get_Cab (Train_ID (Train)),
                  Block => Start_Blocks  (Train_ID (Train)).Items (Block).Block,
                              Direction => Start_Pols (Train_ID (Train)));

            Blocks.Reserve_Block
              (Block   => Start_Blocks  (Train_ID (Train)).Items (Block).Block,
               Train   => Train_ID (Train),
               Success => Reserve_Success);

            Cabs.Set_Limit (Cab   => Trains.Get_Cab (Train_ID (Train)),
                            Value => 100);
         end loop;
         Trains.Set_Direction (Train     => Train_ID (Train),
                               Direction => Forward);

         Trains.Get_Reserve_Block (Next_Block    => Start_Blocks
                                   (Train_ID (Train)).Items
                                   (1).Block,
                                   Train         => Train_ID (Train),
                                   Reserve_Block => Reserve_Block,
                                   To_Reserve    => To_Reserve);

         if To_Reserve then
            Blocks.Reserve_Block
              (Block   => Reserve_Block,
               Train   => Train_ID (Train),
               Success => Reserve_Success);
         else
            Display.Put_Error ("You're going into the yard");
         end if;


         Temp_List := Start_Blocks (Train_ID (Train));
         Count := Start_Blocks (Train_ID (Train)).Size;
         for Block in 1 .. Start_Blocks (Train_ID (Train)).Size loop
            Start_Blocks (Train_ID (Train)).Items
              (Block).Block :=
              Temp_List.Items (Count).Block;
            Count := Count - 1;
         end loop;

         Trains.Set_Initial_Blocks
           (Train_ID (Train), Start_Blocks (Train_ID (Train)));

         Trains.Dispatcher_Halt (Train_ID (Train));

         if Train = 1 then
            Engineers.Enable (Engineer => Engineer_ID (Train),
                              Train    => Train_ID (Train),
                              Control  => A,
                              Skill    => Novice);
         elsif Train = 2 then
            Engineers.Enable (Engineer => Engineer_ID (Train),
                              Train    => Train_ID (Train),
                              Control  => B,
                              Skill    => Novice);
         else
            Engineers.Enable (Engineer => Engineer_ID (Train),
                              Train    => Train_ID (Train),
                              Control  => C,
                              Skill    => Novice);
         end if;
         for Train in Train_ID'Range loop
            Display.Put (Train => Train,
                         Skill => Novice);
            Display.Put (Train => Train,
                         Name  => Trains.Get_Name (Train));
            Trains.Set_Direction (Train, Forward);
            Display.Put (Train     => Train,
                         Direction => Forward);
         end loop;
    end loop;

   end Enable_Session;
--------------------------------------------------------------------------------
   procedure Process_Commands (Train_Amount : in Num_Trains;
                               State        : in out State_Type) is

      My_Command : Command.Command_Rec;
      Disp_Success : Boolean;
   begin
      loop
         Command.Get (My_Command);
         case My_Command.Which is
            when Stop_All | Restart | Quit | Error =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                            & Command.Command_Type'Image (My_Command.Which));
               if My_Command.Which = Quit then
                  exit;
               elsif My_Command.Which = Stop_All then
                  for Train in 1 .. Train_Amount loop
                     Trains.Dispatcher_Halt (Train_ID (Train));
                  end loop;
               elsif My_Command.Which = Restart then
                  State := Setup;
                  exit;
               elsif My_Command.Which = Error then
                  Display.Put_Error ("You typed an invalid command");
               end if;
            when Stop | Go =>
               if My_Command.Which = Stop then
                  Trains.Dispatcher_Halt (My_Command.Train);
               elsif My_Command.Which = Go then
                  Trains.Dispatcher_Go (My_Command.Train, Disp_Success);
                  if not Disp_Success then
                     Display.Put_Error ("Dispatcher start failed");
                  end if;
               end if;
            when Left | Right =>
               if My_Command.Which = Left then
                  Turnouts.Set (Requestor => 0,
                                Turnout   => My_Command.Turnout,
                                Direction => Left);
               elsif My_Command.Which = Right then
                  Turnouts.Set (Requestor => 0,
                                Turnout   => My_Command.Turnout,
                                Direction => Right);
               end if;
            when Free =>
               Blocks.Un_Reserve_Block (Block => My_Command.Block,
                                        Train => 0);
            when Skill =>
               null;
--                 Engineers.Set_Skill (My_Command.Engineer);
         end case;
      end loop;

   end Process_Commands;
--------------------------------------------------------------------------------
   procedure Disable_Session (Train_Amount : in Num_Trains) is

   begin
      for Train in 1 .. Train_Amount loop
         Trains.Dispatcher_Halt (Train_ID (Train));
      end loop;
      Turnouts.Shut_Down;
      Display.Put_Error  ("It is now safe to turn off the power to the" &
                          "system and remove the floppy disk");
   end Disable_Session;
--------------------------------------------------------------------------------

   procedure Set_Up_Trains (Train_Amount : in  Num_Trains;
                            State        : in out State_Type) is

      To_Terminate   : Boolean := False;
      Confliction    : Boolean := False;
      Check_Complete : Boolean := False;
      Confirmation   : Menu_Entry;
      Selection      : Menu_Entry;
      Temp_Loco      : Loco_Rec;
      Search_Success : Boolean;
      Loco_Block     : Layout.Block_Number;
      Caboose_Block  : Layout.Block_Number;
      Direction      : Layout.Polarity;
      Locos          : Natural := 1;
      The_Train_Amount : Natural;
   begin

      The_Train_Amount := Natural (Train_Amount);

      loop
         exit when Locos > The_Train_Amount;
         Temp_Loco := Locomotive_Pulling_Train (Num_Trains (Locos));
         Trains.Set_Name (Train_ID (Locos), Temp_Loco.Name);
         Trains.Set_Min_Throttle (Train_ID (Locos), Temp_Loco.Minimum_Throttle);
         Search_Success := False;
         loop
            Loco_Block := Get_Loco_Block (Num_Trains (Locos));
            Caboose_Block := Get_Caboose_Block (Num_Trains (Locos));
            Direction := Get_Direction;
            Initial_Pols (Train_ID (Locos)) := Direction;
            Trains.Set_Initial_Polarity (Train     => Train_ID (Locos),
                                         Direction => Direction);

            Check_Distance (Train_ID (Locos), Loco_Block, Caboose_Block,
                            Search_Success, Confliction);

            if not Search_Success and not Confliction then
               Put_Line ("The maximum number of blocks beneath a train" &
                         " is 3");
               Put_Line ("There are more than 3 blocks beneath your" &
                         " train");
               New_Line;

               Get_3_Screen (Selection);
            elsif Search_Success and Confliction then
               Put ("The location information for Train");
               Num_Trains_IO.Put (Num_Trains (Locos), 1);
               Put (" is not valid because it conflicts with a train" &
                                    " you entered earlier");
               New_Line;

               Get_3_Screen (Selection);
            else
               Check_Complete := True;
            end if;
            exit when Check_Complete;

            if Selection = N then
               Set_Up_Trains (Train_Amount, State);
            elsif Selection = Q then
               To_Terminate := True;
               State := Shutdown;
            end if;
            exit when To_Terminate;
         end loop;

         exit when To_Terminate;

         Put_Line (Trains.Get_Name (Train_ID (Locos)));
         Put ("Locomotive on block  ");
         Block_ID_IO.Put (Loco_Block);
         New_Line;
         Put ("Caboose    on block  ");
         Block_ID_IO.Put (Caboose_Block);
         New_Line;
         Put ("Train occupies blocks ");
         for Block in 1 .. Start_Blocks (Train_ID (Locos)).Size loop
            Put (Layout.Block_Number'Image
                 (Start_Blocks (Train_ID (Locos)).Items (Block).Block) & " ");
         end loop;
         New_Line;
         New_Line;

         Get_Confirmation (Confirmation);

         if Confirmation = N then
            null;
         elsif Confirmation = R then
            Set_Up_Trains (Train_Amount, State);
         elsif Confirmation = Q then
            To_Terminate := True;
            State := Shutdown;
         else
            Locos := Locos + 1;
         end if;

         exit when To_Terminate;

      end loop;

      if not To_Terminate then
         State := Run;
      end if;

   end Set_Up_Trains;

   subtype Run_State is State_Type range Run .. Shutdown;

   State : Run_State;
   Number_of_Trains : Num_Trains;

begin
   State := Start;
   Session_Loop :
   loop
      case State is
         when Start =>
            DoubleTalk.Speak
              (Phrase_Strings.To_Bounded_String
                 ("Ready to begin new operating Session"), Vader);
            Get_Number_Of_Trains (Number_of_Trains);
            State := Setup;
         when Setup =>
            Set_Up_Trains (Number_of_Trains, State);
         when Run =>
            Enable_Session (Number_of_Trains);
            Process_Commands (Number_of_Trains, State);
            Disable_Session (Number_of_Trains);
            State := Shutdown;
            null;
         when Shutdown =>
            exit Session_Loop;
      end case;
   end loop Session_Loop;

   Display.Disable;

   DoubleTalk.Speak (Phrase_Strings.To_Bounded_String
                     ("Shutting down the system, please wait"), Vader);
   DoubleTalk.Speak (Phrase_Strings.To_Bounded_String
                     ("The system has been shutdown"), Vader);
end New_Dispatcher;
