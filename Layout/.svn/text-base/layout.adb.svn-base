-- The body of Layout for use in
-- Dr. McCormick's Train Lab
-- (Real-Time Embedded Systems)
-- Group name: Cleveland Express
-- Managed by: Andrew Schutt

package body Layout is

   -------------------------------------------------------------------------
   -- Declarations of Arrays for storage of Blocks, Turnouts, Sensors and --
   -- Reversing Points.                                                   --
   -------------------------------------------------------------------------

   type Block_End_Array is array (Polarity) of Block_End;
   type Block_Array     is array (Block_Number) of Block_End_Array;

   type Block_Block_End is array (Polarity) of Block_Number;
   type Block_End_Block_Array is array (Block_Number) of Block_Block_End;

   type Block_Turn_End_Array is array (Polarity) of Turnout_Num;
   type Block_Turnout_End is array (Block_Number) of Block_Turn_End_Array;

   type Limb_Array  is array (Limb) of Block_Number;
   type Turnout_Rec is
      record
         Limbs      : Limb_Array;
         Common_Pol : Polarity;
      end record;
   type Turnout_Array is array (Turnout_Num) of Turnout_Rec;

   type Sensor_Block_Array is array (Polarity) of Block_Number;
   type Hall_Sensor_Array  is array (Hall_Sensor) of Sensor_Block_Array;

   type Reversing_Point_Array is array (Hall_Sensor) of Boolean;
   -- each sensor number will have a boolean value of true if the
   -- sensor is a reversing point.


   ---------------------------------------------------------------------------
   -- Declaration of Block array, Turnout array, Sensor array and Reversing --
   -- Point array for use in layout procedures.                             --
   ---------------------------------------------------------------------------

   My_Blocks   : constant Block_Array := (1  => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          2  => (Reversed => Choice,
                                                 Normal   => Choice),
                                          3  => (Reversed => Forced,
                                                 Normal   => Forced),
                                          4  => (Reversed => Choice,
                                                 Normal   => A_Block),
                                          5  => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          6  => (Reversed => Choice,
                                                 Normal   => Forced),
                                          7  => (Reversed => Choice,
                                                 Normal   => Forced),
                                          8  => (Reversed => Choice,
                                                 Normal   => Choice),
                                          9  => (Reversed => Forced,
                                                 Normal   => Forced),
                                          10 => (Reversed => Choice,
                                                 Normal   => Choice),
                                          11 => (Reversed => Forced,
                                                 Normal   => A_Block),
                                          12 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          13 => (Reversed => Forced,
                                                 Normal   => Choice),
                                          14 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          15 => (Reversed => Choice,
                                                 Normal   => Choice),
                                          16 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          17 => (Reversed => Choice,
                                                 Normal   => Forced),
                                          18 => (Reversed => Choice,
                                                 Normal   => Choice),
                                          19 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          20 => (Reversed => Choice,
                                                 Normal   => Forced),
                                          21 => (Reversed => Choice,
                                                 Normal   => Choice),
                                          22 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          23 => (Reversed => Forced,
                                                 Normal   => A_Block),
                                          24 => (Reversed => A_Block,
                                                 Normal   => Choice),
                                          25 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          26 => (Reversed => Choice,
                                                 Normal   => Forced),
                                          27 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          28 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          29 => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          30 => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          31 => (Reversed => Choice,
                                                 Normal   => Choice),
                                          32 => (Reversed => Forced,
                                                 Normal   => A_Block),
                                          33 => (Reversed => A_Block,
                                                 Normal   => A_Block),
                                          34 => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          35 => (Reversed => Choice,
                                                 Normal   => Forced),
                                          36 => (Reversed => Forced,
                                                 Normal   => A_Block),
                                          37 => (Reversed => A_Block,
                                                 Normal   => A_Block),
                                          38 => (Reversed => A_Block,
                                                 Normal   => Forced),
                                          39 => (Reversed => Forced,
                                                 Normal   => Forced),
                                          40 => (Reversed => A_Deadend,
                                                 Normal   => A_Block));

   My_Turnouts : constant Turnout_Array := (1  => (Limbs => (Left   => 13,
                                                             Right  => 12,
                                                             Common => 31),
                                                   Common_Pol => Normal),
                                            2  => (Limbs      => (Left   => 2,
                                                                  Right  => 14,
                                                                  Common => 13),
                                                   Common_Pol => Normal),
                                            3  => (Limbs      => (Left   => 13,
                                                                  Right  => 1,
                                                                  Common => 2),
                                                   Common_Pol => Reversed),
                                            4  => (Limbs      => (Left   => 26,
                                                                  Right  => 14,
                                                                  Common => 15),
                                                   Common_Pol => Reversed),
                                            5  => (Limbs      => (Left   => 16,
                                                                  Right  => 28,
                                                                  Common => 15),
                                                   Common_Pol => Normal),
                                            6  =>  (Limbs      => (Left   => 3,
                                                                   Right  => 17,
                                                                   Common => 2),
                                                    Common_Pol => Normal),
                                            7  => (Limbs      => (Left   => 16,
                                                                  Right  => 2,
                                                                  Common => 17),
                                                   Common_Pol => Reversed),
                                            8  => (Limbs      => (Left   => 30,
                                                                  Right  => 17,
                                                                  Common => 18),
                                                   Common_Pol => Reversed),
                                            9  => (Limbs      => (Left   => 34,
                                                                  Right  => 38,
                                                                  Common => 35),
                                                   Common_Pol => Reversed),
                                            10 => (Limbs      => (Left   => 39,
                                                                  Right  => 7,
                                                                  Common => 8),
                                                   Common_Pol => Reversed),
                                            11 => (Limbs      => (Left   => 25,
                                                                  Right  => 28,
                                                                  Common => 24),
                                                   Common_Pol => Normal),
                                            12 => (Limbs      => (Left   => 9,
                                                                  Right  => 27,
                                                                  Common => 8),
                                                   Common_Pol => Normal),
                                            13 => (Limbs      => (Left   => 9,
                                                                  Right  => 29,
                                                                  Common => 10),
                                                   Common_Pol => Reversed),
                                            14 => (Limbs      => (Left   => 25,
                                                                  Right  => 27,
                                                                  Common => 26),
                                                   Common_Pol => Reversed),
                                            15 => (Limbs      => (Left   => 11,
                                                                  Right  => 12,
                                                                  Common => 10),
                                                   Common_Pol => Normal),
                                            16 => (Limbs      => (Left   => 36,
                                                                  Right  => 32,
                                                                  Common => 31),
                                                   Common_Pol => Reversed),
                                            17 => (Limbs      => (Left   => 4,
                                                                  Right  => 19,
                                                                  Common => 18),
                                                   Common_Pol => Normal),
                                            18 => (Limbs      => (Left   => 18,
                                                                  Right  => 3,
                                                                  Common => 4),
                                                   Common_Pol => Reversed),
                                            19 => (Limbs      => (Left   => 35,
                                                                  Right  => 19,
                                                                  Common => 20),
                                                   Common_Pol => Reversed),
                                            20 => (Limbs      => (Left   => 39,
                                                                  Right  => 20,
                                                                  Common => 21),
                                                   Common_Pol => Reversed),
                                            21 => (Limbs      => (Left   => 22,
                                                                  Right  => 23,
                                                                  Common => 21),
                                                   Common_Pol => Normal),
                                            22 => (Limbs      => (Left   => 22,
                                                                  Right  => 5,
                                                                  Common => 6),
                                                   Common_Pol => Reversed),
                                            23 => (Limbs      => (Left   => 6,
                                                                  Right  => 40,
                                                                  Common => 7),
                                                   Common_Pol => Reversed),
                                            24 => (Limbs      => (Left   => 40,
                                                                  Right  => 40,
                                                                  Common => 40),
                                                   Common_Pol => Reversed),
                                            25 => (Limbs      => (Left   => 40,
                                                                  Right  => 40,
                                                                  Common => 40),
                                                   Common_Pol => Reversed),
                                            26 => (Limbs      => (Left   => 40,
                                                                  Right  => 40,
                                                                  Common => 40),
                                                   Common_Pol => Reversed));

   Block_Block_Ends   : constant Block_End_Block_Array :=
                          (1 => (Normal    => 2,
                                 Reversed  => 11),
                           2 => (Normal    => 3,
                                 Reversed  => 1),
                           3 => (Normal    => 4,
                                 Reversed  => 2),
                           4 => (Normal    => 5,
                                 Reversed  => 3),
                           5 => (Normal    => 6,
                                 Reversed  => 4),
                           6 => (Normal    => 7,
                                 Reversed  => 40),
                           7 => (Normal    => 8,
                                 Reversed  => 40),
                           8 => (Normal    => 40,
                                 Reversed  => 40),
                           9 => (Normal    => 10,
                                 Reversed  => 8),
                           10 => (Normal   => 40,
                                 Reversed  => 40),
                           11 => (Normal   => 1,
                                  Reversed => 10),
                           12 => (Normal   => 31,
                                  Reversed => 10),
                           13 => (Normal   => 40,
                                  Reversed => 31),
                           14 => (Normal   => 15,
                                  Reversed => 13),
                           15 => (Normal   => 40,
                                  Reversed => 40),
                           16 => (Normal    => 17,
                                  Reversed  => 15),
                           17 => (Normal   => 18,
                                  Reversed => 40),
                           18 => (Normal   => 40,
                                  Reversed => 40),
                           19 => (Normal   => 20,
                                  Reversed => 18),
                           20 => (Normal    => 21,
                                  Reversed  => 40),
                           21 => (Normal   => 40,
                                  Reversed => 40),
                           22 => (Normal   => 6,
                                  Reversed => 21),
                           23 => (Normal   => 24,
                                  Reversed => 21),
                           24 => (Normal    => 40,
                                  Reversed  => 23),
                           25 => (Normal   => 26,
                                  Reversed => 24),
                           26 => (Normal   => 15,
                                  Reversed => 40),
                           27 => (Normal   => 26,
                                  Reversed => 8),
                           28 => (Normal    => 24,
                                  Reversed  => 15),
                           29 => (Normal   => 10,
                                  Reversed => 30),
                           30 => (Normal   => 18,
                                  Reversed => 29),
                           31 => (Normal   => 40,
                                  Reversed => 40),
                           32 => (Normal    => 33,
                                  Reversed  => 31),
                           33 => (Normal   => 34,
                                  Reversed => 32),
                           34 => (Normal   => 35,
                                  Reversed => 33),
                           35 => (Normal   => 20,
                                  Reversed => 40),
                           36 => (Normal    => 37,
                                  Reversed  => 31),
                           37 => (Normal   => 38,
                                  Reversed => 36),
                           38 => (Normal   => 35,
                                  Reversed => 37),
                           39 => (Normal   => 21,
                                  Reversed => 8),
                           40 => (Normal    => 7,
                                  Reversed  => 40));


   Block_Turnout_Ends : constant Block_Turnout_End :=
                          (1  => (Normal    => 3,
                                  Reversed  => 26),
                           2  => (Normal    => 6,
                                  Reversed  => 3),
                           3  => (Normal    => 18,
                                  Reversed  => 6),
                           4  => (Normal    => 26,
                                  Reversed  => 18),
                           5  => (Normal    => 22,
                                  Reversed  => 26),
                           6  => (Normal    => 23,
                                  Reversed  => 22),
                           7  => (Normal    => 10,
                                  Reversed  => 23),
                           8  => (Normal    => 12,
                                  Reversed  => 10),
                           9  => (Normal    => 13,
                                  Reversed  => 12),
                           10 => (Normal    => 15,
                                  Reversed  => 13),
                           11 => (Normal    => 26,
                                  Reversed  => 15),
                           12 => (Normal   => 1,
                                  Reversed => 15),
                           13 => (Normal    => 2,
                                  Reversed  => 1),
                           14 => (Normal    => 4,
                                  Reversed  => 2),
                           15 => (Normal    => 5,
                                  Reversed  => 4),
                           16 => (Normal   => 7,
                                  Reversed => 5),
                           17 => (Normal    => 8,
                                  Reversed  => 7),
                           18 => (Normal    => 17,
                                  Reversed  => 8),
                           19 => (Normal    => 19,
                                  Reversed  => 17),
                           20 => (Normal   => 20,
                                  Reversed => 19),
                           21 => (Normal    => 21,
                                  Reversed  => 20),
                           22 => (Normal    => 22,
                                  Reversed  => 21),
                           23 => (Normal    => 26,
                                  Reversed  => 21),
                           24 => (Normal   => 11,
                                  Reversed => 26),
                           25 => (Normal    => 14,
                                  Reversed  => 11),
                           26 => (Normal    => 4,
                                  Reversed  => 14),
                           27 => (Normal    => 14,
                                  Reversed  => 12),
                           28 => (Normal   => 11,
                                  Reversed => 5),
                           29 => (Normal    => 13,
                                  Reversed  => 26),
                           30 => (Normal    => 8,
                                  Reversed  => 26),
                           31 => (Normal    => 1,
                                  Reversed  => 16),
                           32 => (Normal   => 26,
                                  Reversed => 16),
                           33 => (Normal    => 26,
                                  Reversed  => 26),
                           34 => (Normal    => 9,
                                  Reversed  => 26),
                           35 => (Normal    => 19,
                                  Reversed  => 9),
                           36 => (Normal   => 26,
                                  Reversed => 16),
                           37 => (Normal    => 26,
                                  Reversed  => 26),
                           38 => (Normal    => 9,
                                  Reversed  => 26),
                           39 => (Normal    => 20,
                                  Reversed  => 10),
                           40 => (Normal   => 24,
                                  Reversed => 26));

   My_Sensors  : constant Hall_Sensor_Array := (1  => (Normal   => 11,
                                                       Reversed => 1),
                                                2  => (Normal   => 31,
                                                       Reversed => 13),
                                                3  => (Normal   => 31,
                                                       Reversed => 12),
                                                4  => (Normal   => 36,
                                                       Reversed => 31),
                                                5  => (Normal   => 32,
                                                       Reversed => 31),
                                                6  => (Normal   => 10,
                                                       Reversed => 11),
                                                7  => (Normal   => 10,
                                                       Reversed => 12),
                                                8  => (Normal   => 13,
                                                       Reversed => 14),
                                                9  => (Normal   => 13,
                                                       Reversed => 2),
                                                10 => (Normal   => 1,
                                                       Reversed => 2),
                                                11 => (Normal   => 14,
                                                       Reversed => 15),
                                                12 => (Normal   => 26,
                                                       Reversed => 15),
                                                13 => (Normal   => 36,
                                                       Reversed => 37),
                                                14 => (Normal   => 32,
                                                       Reversed => 33),
                                                15 => (Normal   => 15,
                                                       Reversed => 16),
                                                16 => (Normal   => 15,
                                                       Reversed => 28),
                                                17 => (Normal   => 25,
                                                       Reversed => 26),
                                                18 => (Normal   => 27,
                                                       Reversed => 26),
                                                19 => (Normal   => 9,
                                                       Reversed => 10),
                                                20 => (Normal   => 29,
                                                       Reversed => 10),
                                                21 => (Normal   => 16,
                                                       Reversed => 17),
                                                22 => (Normal   => 2,
                                                       Reversed => 17),
                                                23 => (Normal   => 2,
                                                       Reversed => 3),
                                                24 => (Normal   => 33,
                                                       Reversed => 34),
                                                25 => (Normal   => 37,
                                                       Reversed => 38),
                                                26 => (Normal   => 28,
                                                       Reversed => 24),
                                                27 => (Normal   => 24,
                                                       Reversed => 25),
                                                28 => (Normal   => 8,
                                                       Reversed => 27),
                                                29 => (Normal   => 8,
                                                       Reversed => 9),
                                                30 => (Normal   => 30,
                                                       Reversed => 29),
                                                31 => (Normal   => 39,
                                                       Reversed => 8),
                                                32 => (Normal   => 7,
                                                       Reversed => 8),
                                                33 => (Normal   => 17,
                                                       Reversed => 18),
                                                34 => (Normal   => 30,
                                                       Reversed => 18),
                                                35 => (Normal   => 38,
                                                       Reversed => 35),
                                                36 => (Normal   => 34,
                                                       Reversed => 35),
                                                37 => (Normal   => 23,
                                                       Reversed => 24),
                                                38 => (Normal   => 3,
                                                       Reversed => 4),
                                                39 => (Normal   => 18,
                                                       Reversed => 4),
                                                40 => (Normal   => 18,
                                                       Reversed => 19),
                                                41 => (Normal   => 19,
                                                       Reversed => 20),
                                                42 => (Normal   => 35,
                                                       Reversed => 20),
                                                43 => (Normal   => 20,
                                                       Reversed => 21),
                                                44 => (Normal   => 39,
                                                       Reversed => 21),
                                                45 => (Normal   => 4,
                                                       Reversed => 5),
                                                46 => (Normal   => 21,
                                                       Reversed => 23),
                                                47 => (Normal   => 21,
                                                       Reversed => 22),
                                                48 => (Normal   => 22,
                                                       Reversed => 6),
                                                49 => (Normal   => 5,
                                                       Reversed => 6),
                                                50 => (Normal   => 6,
                                                       Reversed => 7),
                                                51 => (Normal   => 40,
                                                       Reversed => 7));

   My_R_Points : constant Reversing_Point_Array := (1  => False, 2  => False,
                                                    3  => True,  4  => True,
                                                    5  => True,  6  => False,
                                                    7  => False, 8  => False,
                                                    9  => False, 10 => False,
                                                    11 => False, 12 => False,
                                                    13 => False, 14 => False,
                                                    15 => False, 16 => False,
                                                    17 => False, 18 => False,
                                                    19 => False, 20 => False,
                                                    21 => False, 22 => False,
                                                    23 => False, 24 => False,
                                                    25 => False, 26 => True,
                                                    27 => False, 28 => False,
                                                    29 => False,  30 => True,
                                                    31 => True,  32 => False,
                                                    33 => False, 34 => False,
                                                    35 => False, 36 => False,
                                                    37 => False, 38 => False,
                                                    39 => False, 40 => False,
                                                    41 => False, 42 => False,
                                                    43 => False, 44 => False,
                                                    45 => False, 46 => False,
                                                    47 => False, 48 => False,
                                                    49 => False, 50 => False,
                                                    51 => False);


   ----------------
   -- On_the_End --
   ----------------

   function On_The_End (Block_Pol     : in Polarity;
                        Current_Block : in Block_Number)
                        return Block_End
   is
   begin
      return My_Blocks (Current_Block) (Block_Pol);
   end On_The_End;

   -----------------------
   -- Get_Sensor_Number --
   -----------------------

   function Get_Sensor_Number
     (Block_A   : in Block_Number;
      Block_B   : in Block_Number)
        return Hall_Sensor
   is
      Temp_Sensor : Hall_Sensor;
   begin
      for Sensor in Hall_Sensor loop

         -- check if block is on either end of sensor index value
         if (My_Sensors (Sensor) (Normal) = Block_A and
               My_Sensors (Sensor) (Reversed) = Block_B) or

           (My_Sensors (Sensor) (Reversed) = Block_A and
              My_Sensors (Sensor) (Normal) = Block_B) then
            -- if both blocks passed in are on either side of current
            -- indexed sensor return that sensor number
               -- set value to be returned
               Temp_Sensor := Sensor;

         end if;
      end loop;
      return Temp_Sensor;
   end Get_Sensor_Number;

   ---------------
   -- Take_Limb --
   ---------------

   procedure Take_Limb
     (Turn_Num    : in     Turnout_Num;
      Turn_Choice : in     Limb;
      Next_Limb   :    out Block_Number)
   is
   begin
      -- returns block that is on the end of limb choice
      Next_Limb := My_Turnouts (Turn_Num).Limbs (Turn_Choice);
   end Take_Limb;

   --------------
   -- Limb_End --
   --------------

   function Limb_End (Turn_Num : in Turnout_Num;
                      Turn_Choice : in Limb)
                      return Block_Number
   is
   begin
      return My_Turnouts (Turn_Num).Limbs (Turn_Choice);
   end Limb_End;

   ----------------
   -- Next_Block --
   ----------------

   procedure Next_Block
     (Current_Block : in     Block_Number;
      A_Polarity    : in     Polarity;
      Next_Block    :    out Block_Number)
   is
   begin

      -- access array with block number stored on the end of block
      -- returns 40 if a block is on the end of Current_Block

      -- assumes a block is on the end of Current_Block!
      -- if a forced turnout is on the end of the block returns block you must
      -- get after passing forced turnout

      Next_Block := Block_Block_Ends (Current_Block) (A_Polarity);

   end Next_Block;

   ---------------------
   -- Get_Turnout_Num --
   ---------------------

   procedure Get_Turnout_Num (Current_Block : in     Block_Number;
                              Direction     : in     Polarity;
                              Turn_Num      :    out Turnout_Num)
   is
   begin

      -- access array with turnout number stored on the end of block
      -- returns 26 if a block is on the end of Current_Block

      -- assumes a turnout is on the end of Current_Block!
      Turn_Num := Block_Turnout_Ends (Current_Block) (Direction);

   end Get_Turnout_Num;

   ------------------------
   -- Is_Reversing_Point --
   ------------------------

   function Is_Reversing_Point (Sensor : in Hall_Sensor) return Boolean is
   begin
      return My_R_Points (Sensor);
   end Is_Reversing_Point;

   ---------------------
   -- Is_Force_Turnout --
   ---------------------

   function Is_Force_Turnout
     (Current_Block : in Block_Number;
      Block_Pol     : in Polarity)
      return Boolean
   is
      TEnd : Block_End;
   begin
      TEnd := On_The_End (Block_Pol, Current_Block);
      return TEnd = Forced;

   end Is_Force_Turnout;

   --------------
   -- Opposite --
   --------------

   function Opposite
     (Block_Pol     : in Polarity)
      return Polarity
   is
   begin
      if Block_Pol = Layout.Reversed then
         return Layout.Normal;
      else
         return Layout.Reversed;
      end if;
   end Opposite;

   --------------
   -- Opposite --
   --------------

   function Opposite
     (Dir  : in Limb)
      return Limb
   is
   begin
      if Dir = Right then
         return Left;
      else
         return Right;
      end if;
   end Opposite;

   -----------------
   -- Hall_Blocks --
   -----------------

   procedure Hall_Blocks
     (Sensor         : in     Hall_Sensor;
      Normal_Block   :    out Block_Number;
      Reversed_Block :    out Block_Number)
   is
   begin
      Normal_Block := My_Sensors (Sensor) (Normal);
      Reversed_Block := My_Sensors (Sensor) (Reversed);
   end Hall_Blocks;

   ---------------------
   -- Force_Direction --
   ---------------------

   procedure Force_Direction
     (Next_Block    : in     Block_Number;
      Turn_To_Force : in     Turnout_Num;
      F_Dir         :    out Limb)
   is
   begin

      if My_Turnouts (Turn_To_Force).Limbs (Right) = Next_Block then
         F_Dir := Right;
      elsif My_Turnouts (Turn_To_Force).Limbs (Left) = Next_Block then
         F_Dir := Left;
      end if;

   end Force_Direction;

   --------------
   -- Is_Joint --
   --------------

   function Is_Joint
     (Turn         : in Turnout_Num;
      Dir          : in Limb;
      A_Polarity   : in Polarity)
      return Boolean
   is
      Opposite_End : Block_Number;
      TEnd         : Block_End;
   begin

      Opposite_End := Layout.Limb_End (Turn_Num    => Turn,
                                       Turn_Choice => Dir);
      TEnd := On_The_End (Block_Pol     => Opposite (A_Polarity),
                          Current_Block => Opposite_End);

      if TEnd = Choice then
         return True;
      end if;
      return False;

   end Is_Joint;

   --------------------
   -- Get_Joint_Turn --
   --------------------

   function Get_Joint_Turn
     (Turn      : in Turnout_Num;
      Block_Pol : in Polarity;
      Dir       : in Limb)
      return Turnout_Num
   is
      Opposite_End : Block_Number;
      Next_Turn    : Turnout_Num;
   begin
      Opposite_End := Layout.Limb_End (Turn_Num    => Turn,
                                       Turn_Choice => Dir);
      Layout.Get_Turnout_Num (Current_Block => Opposite_End,
                              Direction     => Opposite (Block_Pol),
                              Turn_Num      => Next_Turn);
      return Next_Turn;
   end Get_Joint_Turn;

   ----------------
   -- Crossed_By --
   ----------------

   procedure Crossed_By
     (Current_Block : in     Block_Number;
      Cross_Blocks  :    out Cross_Array)
   is
      Block_8  : constant Cross_Array := (1 => 29);
      Block_12 : constant Cross_Array := (1 => 32, 2 => 36);
      Block_23 : constant Cross_Array := (1 => 39);
      Block_24 : constant Cross_Array := (1 => 30);
      Block_26 : constant Cross_Array := (1 => 32, 2 => 36);
      Block_28 : constant Cross_Array := (1 => 33, 2 => 37);
      Block_29 : constant Cross_Array := (1 => 8);
      Block_30 : constant Cross_Array := (1 => 34, 2 => 38);
      Block_32 : constant Cross_Array := (1 => 12, 2 => 26);
      Block_33 : constant Cross_Array := (1 => 28);
      Block_34 : constant Cross_Array := (1 => 30);
      Block_36 : constant Cross_Array := (1 => 12, 2 => 26);
      Block_37 : constant Cross_Array := (1 => 28);
      Block_38 : constant Cross_Array := (1 => 30);
      Block_39 : constant Cross_Array := (1 => 23);
      Array_Nu : Cross_Array (1 .. 0);
   begin
      case Current_Block is
         when 8 =>
            Cross_Blocks := Block_8;
         when 12 =>
            Cross_Blocks := Block_12;
         when 23 =>
            Cross_Blocks := Block_23;
         when 24 =>
            Cross_Blocks := Block_24;
         when 26 =>
            Cross_Blocks := Block_26;
         when 28 =>
            Cross_Blocks := Block_28;
         when 29 =>
            Cross_Blocks := Block_29;
         when 30 =>
            Cross_Blocks := Block_30;
         when 32 =>
            Cross_Blocks := Block_32;
         when 33 =>
            Cross_Blocks := Block_33;
         when 34 =>
            Cross_Blocks := Block_34;
         when 36 =>
            Cross_Blocks := Block_36;
         when 37 =>
            Cross_Blocks := Block_37;
         when 38 =>
            Cross_Blocks := Block_38;
         when 39 =>
            Cross_Blocks := Block_39;
         when others =>
            Cross_Blocks := Array_Nu;
      end case;
   end Crossed_By;


end Layout;
