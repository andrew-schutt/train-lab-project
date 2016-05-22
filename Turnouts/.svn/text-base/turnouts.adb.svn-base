-- Package body for turnouts
-- Turnouts
-- Cleveland Express Spring 2011
-- Real-Time Embedded Systems
-- Managed by Andrew Schutt

with Motors; use Motors;
with Layout; use Layout;
with Trains; use Trains;

package body Turnouts is

   -- possible states of turnouts
   type State_Type is (Left, Right, Moving_Left, Moving_Right);

   -- array type to store turnout state
   type Turnout_States is array (Layout.Turnout_Num) of Turnouts.State_Type;

   -- pointers for events
   Fail_Ptr  : Failure_Ptr;
   Recov_Ptr : Recover_Ptr;
   Chan_Ptr  : Change_Ptr;
   pragma Unreferenced (Chan_Ptr);

   -- protected object for states of turnouts
   protected Turnout_State is

      procedure Set_State (To      : in State_Type;
                           Turnout : in Turnout_Num);
      function Current_State (Index : in Turnout_Num) return State_Type;

   private
      -- array of turnout states
      State      : Turnout_States;
   end Turnout_State;

   protected body Turnout_State is

      -- sets the state of turnout at "Turnout" as an index
      procedure Set_State (To      : in State_Type;
                           Turnout : in Turnout_Num) is
      begin
         State (Turnout) := To;
      end Set_State;

      -- returns current state of turnout at "Index"
      function Current_State (Index : in Turnout_Num) return State_Type is
      begin
         return State (Index);
      end Current_State;
   end Turnout_State;

   -- declare task type that represents each turnout
   task type Turnout_Task is
      entry Assign_ID        (Input_ID  : in Turnout_Num);
      entry Return_Desired   (Wanted    : out Layout.Turn_Dir);
      entry Set_Left         (Requestor : Request_ID);
      entry Set_Right        (Requestor : Request_ID);
   end Turnout_Task;

   -- declare and initalize array of turnout tasks
   type Turnout_Tasks_Array is array (Turnout_Num) of Turnout_Task;
   Turnout_Tasks : Turnout_Tasks_Array;

   task body Turnout_Task is
      ID             : Turnout_Num;
      Last_Requested : Request_ID;
      Desired        : Layout.Turn_Dir := Left;
      Failure        : Boolean;
      Delay_Clock    : Ada.Real_Time.Time;
   begin
      Failure := False;

      -- assign ID associated with turnout number
      accept Assign_ID (Input_ID : in Turnout_Num) do
         ID := Input_ID;
      end Assign_ID;

      loop
         case Turnout_State.Current_State (ID) is
            when Left =>

               select
                  accept Return_Desired (Wanted    : out Layout.Turn_Dir) do
                     Wanted := Desired;
                  end Return_Desired;
               or
                  accept Set_Right (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Right;
                  Desired        := Right;

                  -- update protected object
                  Turnout_State.Set_State (To      => Moving_Right,
                                           Turnout => ID);

                  -- attempt to move turnout
                  Motors.Set (Motor     => ID,
                              Direction => Layout.Right);
                  Delay_Clock := Ada.Real_Time.Clock;

               or
                  accept Set_Left (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Left;
               end select;

            when Right =>
               select
                  accept Return_Desired (Wanted    : out Layout.Turn_Dir) do
                     Wanted := Desired;
                  end Return_Desired;
               or
                  accept Set_Right (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Right;
               or
                  accept Set_Left (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Left;
                  Desired        := Left;

                  -- update protected object
                  Turnout_State.Set_State (To      => Moving_Left,
                                           Turnout => ID);

                  -- attempt to set turnout
                  Motors.Set (Motor     => ID,
                              Direction => Left);
                  Delay_Clock := Ada.Real_Time.Clock;
               end select;

            when Moving_Left =>

               select
                  accept Return_Desired (Wanted    : out Layout.Turn_Dir) do
                     Wanted := Desired;
                  end Return_Desired;
               or
                  accept Set_Right (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Right;

                  Desired        := Right;

                  -- attempt to set turnout
                  Motors.Set (Motor     => ID,
                              Direction => Layout.Right);
                  Delay_Clock := Ada.Real_Time.Clock;
               or
                  accept Set_Left (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Left;
               or
                  delay until Delay_Clock + Time_Limit;
                  if Motors.In_Position (ID) then
                     Turnout_State.Set_State (To => Left,
                                              Turnout => ID);
                     if Failure then
                        if Recov_Ptr /= null then
                           Recov_Ptr.all (Turnout => ID);
                        end if;
                        Failure := False;
                     end if;
                  else
                     Turnout_State.Set_State (To => Left,
                                               Turnout => ID);
                     if not Failure then
                        Failure := True;
                        if Fail_Ptr /= null then
                           Fail_Ptr.all (Requestor => Last_Requested,
                                         Turnout   => ID);
                        end if;
                     end if;
                  end if;
               end select;

            when Moving_Right =>

               select
                  accept Return_Desired (Wanted    : out Layout.Turn_Dir) do
                     Wanted := Desired;
                  end Return_Desired;
               or
                  accept Set_Right (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Right;
               or
                  accept Set_Left (Requestor : Request_ID) do
                     Last_Requested := Requestor;
                  end Set_Left;
                  Desired        := Left;

                  -- attempt to set turnout
                  Motors.Set (Motor     => ID,
                              Direction => Layout.Left);
                  Delay_Clock := Ada.Real_Time.Clock;
               or
                  delay until Delay_Clock + Time_Limit;
                  if Motors.In_Position (ID) then
                     Turnout_State.Set_State (To => Right,
                                              Turnout => ID);
                     if Failure then
                        if Recov_Ptr /= null then
                           Recov_Ptr.all (Turnout => ID);
                        end if;
                        Failure := False;
                     end if;
                  else
                     Turnout_State.Set_State (To => Right,
                                              Turnout => ID);
                     if not Failure then
                        Failure := True;
                        if Fail_Ptr /= null then
                           Fail_Ptr.all (Requestor => Last_Requested,
                                         Turnout   => ID);
                        end if;
                     end if;
                  end if;
               end select;
         end case;
      end loop;
   end Turnout_Task;

   --------------------------
   -- Set_Failure_Callback --
   --------------------------

   procedure Set_Failure_Callback (To : in Failure_Ptr) is
   begin
      Fail_Ptr := To;
   end Set_Failure_Callback;

   ---------------------------
   -- Set_Recovery_Callback --
   ---------------------------

   procedure Set_Recovery_Callback (To : in Recover_Ptr) is
   begin
      Recov_Ptr := To;
   end Set_Recovery_Callback;

   -------------------------
   -- Set_Change_Callback --
   -------------------------

   procedure Set_Change_Callback (To : in Change_Ptr) is
   begin
      Chan_Ptr := To;
   end Set_Change_Callback;

   ---------
   -- Set --
   ---------

   procedure Set
     (Requestor : in Trains.Request_ID;
      Turnout   : in Layout.Turnout_Num;
      Direction : in Layout.Turn_Dir)
   is
   begin
      if Direction = Layout.Left then
         Turnout_Tasks (Turnout).Set_Left (Requestor);
      else
         Turnout_Tasks (Turnout).Set_Right (Requestor);
      end if;
      Motors.Set (Turnout, Direction);
   end Set;

   ------------
   -- Status --
   ------------

   function Status (Turnout : in  Layout.Turnout_Num) return Status_Rec is
      Desired : Layout.Turn_Dir;
      Current : Layout.Turn_Dir;  -- Currently in or moving toward
      Moving  : Boolean;
      A_Rec   : Status_Rec;
   begin
      if Turnout_State.Current_State (Turnout) = Left then
         Current := Left;
         Moving  := False;
      elsif Turnout_State.Current_State (Turnout) = Right then
         Current := Right;
         Moving  := False;
      elsif Turnout_State.Current_State (Turnout) = Moving_Left then
         Current := Left;
         Moving  := True;
      elsif Turnout_State.Current_State (Turnout) = Moving_Right then
         Current := Right;
         Moving  := True;
      end if;
      Turnout_Tasks (Turnout).Return_Desired (Desired);
      A_Rec := (Desired, Current, Moving);
      return A_Rec;
   end Status;

   ------------------
   -- Direction_Of --
   ------------------

   function Direction_Of
     (Turnout : in Layout.Turnout_Num)
      return Layout.Turn_Dir
   is
      Dir : Layout.Turn_Dir;
   begin
      if Turnout_State.Current_State (Turnout) = Left then
         Dir := Left;
      elsif Turnout_State.Current_State (Turnout) = Right then
         Dir := Right;
      elsif Turnout_State.Current_State (Turnout) = Moving_Left then
         Dir := Left;
      elsif Turnout_State.Current_State (Turnout) = Moving_Right then
         Dir := Right;
      end if;
      return Dir;
   end Direction_Of;

   ---------------
   -- Shut_Down --
   ---------------

   procedure Shut_Down is
   begin
      for A_Task_Num in Turnout_Tasks'Range loop
         Set (Requestor => 0,
              Turnout   => A_Task_Num,
              Direction => Layout.Left);
      end loop;
   end Shut_Down;

begin

   for A_Task in Turnout_Num'Range loop
      Turnout_Tasks (A_Task).Assign_ID (A_Task);
   end loop;

end Turnouts;
