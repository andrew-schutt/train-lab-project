-- This package defines the characteristics of the Trains
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Layout; use Layout;
with ADC;
with hand_controller;
with Cabs; use Cabs;
with Sound; use Sound;
with Locomotives;
with Layout.Search; use Layout.Search;

package Trains is

   Max_Length_Running : constant := 5;
   Max_Length_Start : constant := 3;
   Max_Trains : constant := 3;
   type Request_ID is range 0 .. Max_Trains;
   subtype Train_ID is Request_ID range 1 .. Max_Trains;

   Dispatcher : constant Request_ID := 0;
   type Stop_Reason is (Dispatcher_Request, Turnout_Failure,
                        Reservation_Failure, Lost_Caboose);

   type Direction_Type is (Backward, Forward);

   type Stop_Set is array (Stop_Reason) of Boolean;
   type Train_Cabs is array (Train_ID) of Cabs.Control_Cab_ID;
   type Loco_Recs is array (Train_ID) of Locomotives.Loco_Rec;
   type Turnout_Set is array (Layout.Turnout_Num) of Boolean;
   type Direction_Set is array (Train_ID) of Direction_Type;
   type Train_Unit_Array is array (Train_ID) of Sound.Installed_Range;
   type Polarity_Set is array (Train_ID) of Layout.Polarity;

   type Stop_Rec is
      record
         Reasons  : Stop_Set;
         Block    : Layout.Block_Number;
         Turnouts : Turnout_Set;
      end record;

   type Power_List is array (1 .. Max_Length_Running + 1)
   of Layout.Block_Number;

   type Occupy_Rec is
      record
         Size   : Natural := 0;
         Blocks : Power_List;
      end record;

   type Powered_Blocks is array (Train_ID) of Occupy_Rec;


   type Blocks_Occupied_Array is array (Train_ID) of Occupy_Rec;

   -- This package defines the characteristics of the Trains in the lab

   -- Types for Train descriptive information
   procedure Get_Reserve_Block (Next_Block    : in Layout.Block_Number;
                                Train         : in Trains.Train_ID;
                                Reserve_Block : out Layout.Block_Number;
                                To_Reserve    : out Boolean);
   -- This procedure will attempt to reserve the next block for the Train
   --
   -- Preconditions  : none
   --
   -- Postconditions :


   procedure Hall_Triggered (Hall : in Layout.Hall_Sensor);
   -- This procedure will attempt to reserve the next block for the Train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Dispatcher_Halt (Train  : in Train_ID);
   -- This procedure will set the limit of the trains Cab to 0
   --
   -- Preconditions  : none
   --
   -- Postconditions : The train will be stopped

   procedure Turnout_Fail_Halt (Train   : in Request_ID;
                                Turnout : in Layout.Turnout_Num);
   -- This procedure will set the limit of the trains Cab to 0
   --
   -- Preconditions  : none
   --
   -- Postconditions : The train will be stopped

   procedure Turnout_Fixed (Turnout : in Layout.Turnout_Num);
   -- This procedure will set the limit of the trains Cab to 0
   --
   -- Preconditions  : none
   --
   -- Postconditions : The train will be stopped

   procedure Dispatcher_Go (Train   : in Train_ID;
                            Success : out Boolean);
   -- This procedure will set the limit of the trains Cab to 0
   --
   -- Preconditions  : none
   --
   -- Postconditions : The train will be stopped

   procedure Set_Throttle (Train    : in Train_ID;
                           In_Volts : in ADC.Input_Volts);
   -- This procedure will set the setting of the Trains Cab to the supplied
   -- amount.
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Change_Moving_Direction (Train   : in Train_ID;
                                     Setting : in hand_controller.Two_Position);
   -- This procedure will set the direction of the train in the direction
   -- supplied
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Direction (Train     : in Train_ID;
                            Direction : in Direction_Type);
   -- This procedure will set the direction of the train in the direction
   -- supplied
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Sound_Horn (Moving : in Boolean;
                         Train  : in Train_ID);
   -- Based on the position of the throttle this procedure will know whether
   -- the train is moving and will sound the Horn accordingly.
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Direction (Train : in Train_ID) return Direction_Type;
   -- This will return the direction the Train is moving, either forward or
   -- backward
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Cab (Train : in Train_ID;
                      Cab   : in Cabs.Cab_ID);

   -- This procedure will set the Cab associated with the train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Cab (Train : in Train_ID) return Cabs.Cab_ID;
   -- This procedure will set the Cab associated with the train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Name (Train : in Train_ID;
                       Name  : in Locomotives.Loco_String);
   -- This procedure will set the Name of the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Name (Train : in Train_ID) return Locomotives.Loco_String;
   -- This procedure will return the Name of the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Min_Throttle (Train : in Train_ID;
                               Min   : in Locomotives.Percent);
   -- This procedure will set the Minimum Throttle of the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Min_Throttle (Train : in Train_ID)
                               return Locomotives.Percent;
   -- This procedure will return the Minimum Throttle of the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Initial_Polarity (Train     : in Train_ID;
                                   Direction : in Layout.Polarity);
   -- This procedure will be used by dispatcher to initialize the trains
   -- Polarity
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Set_Initial_Blocks (Train  : in Train_ID;
                                 In_Blocks : in Search.Block_List);
   -- This procedure will be used by dispatcher to initialize the trains
   -- powered blocks
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Leading_Block (Train  : in Train_ID) return Layout.Block_Number;
   -- Reutrns the leading block from the list of powered blocks
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   procedure Engineer_Turn_Wait (Train : in Train_ID;
                                 Block : in Layout.Block_Number);
   -- Engineer calls this procedure to set the waiting block for the train
   --
   -- Preconditions  : none
   --
   -- Postconditions :

   function Get_Train_Polarity (Train : in Train_ID) return Layout.Polarity;
   -- Returns the last polarity used by the train for setting a block
   --
   -- Preconditions  : none
   --
   -- Postconditions :

end Trains;
