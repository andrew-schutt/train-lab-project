-- Specification for the Map Class
-- Written for Dr. McCormick's Train Lab
-- Group: Cleveland Express
-- Managed by: Andrew Schutt

package Layout is
   pragma Pure (Layout);

   type Block_Number is range 1 .. 40;
   type Turnout_Num  is range 1 .. 26;
   type Hall_Sensor  is range 1 .. 51;
   type Block_End    is (A_Block, Choice, Forced, A_Deadend);
   type Limb         is (Left, Right, Common);
   subtype Turn_Dir  is Limb range Left .. Right;
   type Polarity     is (Normal, Reversed);
   type Cross_Array  is array (Integer range <>) of Block_Number;

   -- This makes display happy
   type Display_Block_Array is array (Positive range <>) of Block_Number;

   function On_The_End (Block_Pol     : in Polarity;
                        Current_Block : in Block_Number) return Block_End;

      -- Determines what is on the given end of a block
      --
      -- Preconditions:
      --
      -- Postconditions: Returns what's on the end of Current_Block
      --                 in the Block_Pol direction.


   procedure Get_Turnout_Num (Current_Block : in     Block_Number;
                              Direction     : in     Polarity;
                              Turn_Num      :    out Turnout_Num);

      -- Gets the turnout number of a block end
      --
      -- Preconditions: Current_Block has a turnout on the end in the direction
      --
      -- Postconditions: Turn_Num is the id of the turnout
      --                 end of Current_Block


   function Get_Sensor_Number (Block_A : in Block_Number;
                               Block_B : in Block_Number) return Hall_Sensor;

      -- returns the hall sensor number on the given end of a block
      --
      -- Preconditions: Current_Block does not have a deadend or
      --                turnout on the Block_Pol end.
      --
      -- Postconditions: the hall sensor number for Current_Block
      --                 on the Block_Pol end is returned


   procedure Take_Limb (Turn_Num    : in     Turnout_Num;
                        Turn_Choice : in     Limb;
                        Next_Limb   :    out Block_Number);

      -- Gets the block number of the limb for the user
      --
      -- Preconditions:
      --
      -- Postconditions: For the Turnout Turn_Num, Next_Limb is
      --                 the limb in the direction of Turn_Choice

   function Limb_End (Turn_Num    : in     Turnout_Num;
                      Turn_Choice : in     Limb) return Block_Number;

      -- Returns the block number of the limb for the user
      --
      -- Preconditions:
      --
      -- Postconditions: returns the block number on the end of

   procedure Next_Block (Current_Block : in     Block_Number;
                         A_Polarity    : in     Polarity;
                         Next_Block    :    out Block_Number);

      -- Changes the current block to that of the next block
      -- when there isn't a choice turnout.
      --
      -- Preconditions: Current_Block is a valid Block
      --
      -- Postconditions: Next_Block is the block
      --                 number of the next block


   function Is_Reversing_Point (Sensor : in Hall_Sensor) return Boolean;
      -- Returns true if the specified hall sensor is a reversing point.
      --
      -- Preconditions: none
      --
      -- Postconditions: Returns true if the specified hall sensor is
      --                 a reversing point


   function Is_Force_Turnout (Current_Block : in Block_Number;
                              Block_Pol     : in Polarity) return Boolean;
      -- Given a block number and a polarity, return true if
      -- there is a force turnout on that end of the block.
      --
      -- Preconditions:
      --
      -- Postconditoins: Returns true if there is a force turnout
      --                 on the Block_Pol end of Current_Block.

   function Opposite (Block_Pol     : in Polarity) return Polarity;
      -- Returns the opposite of a given block direction
      --
      -- Preconditions:
      --
      -- Postconditions: returns the Polarity that is the opposite
      --                 of Block_Pol.

   function Opposite (Dir  : in Limb) return Limb;
      -- Returns the opposite limb of a turnout
      --
      -- Preconditions: Dir is either "Right" or "Left"
      --
      -- Postconditions: the direction returned is either
      --                 "Right" or "Left" and is the opposite
      --                 of the direction Dir

   procedure Hall_Blocks (Sensor         : in     Hall_Sensor;
                          Normal_Block   :    out Block_Number;
                          Reversed_Block :    out Block_Number);
      -- Gets the block numbers of the blocks on either
      -- side of the given hall sensor
      --
      -- Preconditions:
      --
      -- Postconditions: Block_1 is the Block_Number on one
      --                 side of Sensor and Block_2 is the
      --                 Block_number of the block on the
      --                 side.
      --                 Block_1 and Block_2 can be the same
      --                 Block_Number.

   procedure Force_Direction (Next_Block    : in     Block_Number;
                              Turn_To_Force : in     Turnout_Num;
                              F_Dir         :    out Limb);
      -- Given a block number and polarity, returns the force turnout
      -- number and which limb must be taken.
      --
      -- Preconditions: There is a force turnout on the Block_Pol
      --                end of Curretn_Block.
      --
      -- Postconditions: F_Turn is the Turnout_Num on the Block_Pol
      --                 end of Current_Block and F_Dir is the Limb
      --                 that the turnout must be set to.

   function Is_Joint (Turn         : in Turnout_Num;
                      Dir          : in Limb;
                      A_Polarity   : in Polarity) return Boolean;
      -- Returns true if the given turnout and limb are half of a
      -- joint turnout.
      --
      -- Preconditions:
      --
      -- Postconditions: Returns true if the turnout Turn's Dir limb
      --                 leads to another turnout.

   function Get_Joint_Turn (Turn      : in Turnout_Num;
                            Block_Pol : in Polarity;
                            Dir       : in Limb) return Turnout_Num;
      -- Given a turnout and limb direction, returns the tunout
      -- number of its joint turnout.
      --
      -- Preconditions: Turn's Dir limb leads to another turnout.
      --
      -- Postconditions: The Turnout_Num of the next turnout is returned.

   procedure Crossed_By (Current_Block : in     Block_Number;
                         Cross_Blocks  :    out Cross_Array);
      -- Given a block number, return a list of block numbers that cross
      -- that block.
      --
      -- Preconditions:
      --
      -- Postconditions: Cross_Blocks is an array of the block
      --                 numbers that cross Current_Block
      --                 Could return an empty array.

end Layout;
