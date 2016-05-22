-- spec for Blocks
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Layout; use Layout;
with Trains; use Trains;
with Cabs;
package Blocks is

   -- This package provides operations for Blocks

   -----------------------------------------------------------------------------
   procedure Reserve_Block (Block   : in Layout.Block_Number;
                            Train   : in Trains.Train_ID;
                            Success : out Boolean);
   -- Reserve the given block for the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions : The block is reserved

   -----------------------------------------------------------------------------
   procedure Un_Reserve_Block (Block   : in Layout.Block_Number;
                               Train   : in Trains.Request_ID);
   -- Unreserve the given block for the given train
   --
   -- Preconditions  : none
   --
   -- Postconditions : The block is changed to unreserved

   -----------------------------------------------------------------------------
   procedure Set_Power (Cab       : in Cabs.Cab_ID;
                        Block     : in Layout.Block_Number;
                        Direction : in Layout.Polarity);
   -- Set the polarity of the given Block to the given direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : The polarity of the block is changed

   function Cab_Powering (Block : in Layout.Block_Number) return Cabs.Cab_ID;
   -- Set the polarity of the given Block to the given direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : The polarity of the block is changed

   function Train_Reserving (Block : in Layout.Block_Number)
                             return Trains.Request_ID;
   -- Set the polarity of the given Block to the given direction
   --
   -- Preconditions  : none
   --
   -- Postconditions : The polarity of the block is changed
end Blocks;
