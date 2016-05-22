-- Performs layout search
-- Cleveland Express Spring 2011
-- Real-Time Embedded Systems
-- Managed by Andrew Schutt

with Ada.Text_IO;   use Ada.Text_IO;

package body Layout.Search is

   procedure Find_Loco (Direction          : in  Polarity;
                        Current            : in  Block_Number;
                        Looking_For        : in  Block_Number;
                        Recur_Depth        : in  Positive; -- recursion depth
                        All_Blocks         : out Block_List;
                        All_Turnouts       : out Turnout_List;
                        Success            : out Boolean)
   is
      -- holder variables to hold future search information
      Next_Block      : Layout.Block_Number; -- next block...
      Next_Turnout    : Layout.Turnout_Num; -- next turnout...
      Compare         : Layout.Block_End; -- used to compare next block end
      A_Direction     : Layout.Polarity; -- needed for opposite ()

      A_Limb          : Layout.Limb := Left; -- used for turnouts
      A_Sensor        : Layout.Hall_Sensor; -- needed for opposite ()
   begin
      Put_Line (Positive'Image (Recur_Depth));
      -- check base case of search too far
      if Recur_Depth = All_Blocks.Max_Size then
         Success := False;
         return;
      end if;

      -- check second base case, found correct block
      if Current = Looking_For then
         All_Blocks.Size := All_Blocks.Size + 1; -- add looking for block
         All_Blocks.Items (All_Blocks.Size).Block := Current;
         All_Blocks.Items (All_Blocks.Size).Direction := Direction;
         Success := True;
         return; -- begin returns
      end if;

      Compare := Layout.On_The_End (Direction, Current); -- grab next block end

      case Compare is
         when A_Block =>

            Layout.Next_Block (Current_Block => Current,
                               A_Polarity    => Direction,
                               Next_Block    => Next_Block);

            A_Sensor := Layout.Get_Sensor_Number (Block_A => Current,
                                                  Block_B => Next_Block);

            -- check reversing point
            if Is_Reversing_Point (A_Sensor) then
               A_Direction := Layout.Opposite (Direction);
            else
               A_Direction := Direction;
            end if;

            Find_Loco (Direction          => A_Direction,
                       Current            => Next_Block,
                       Looking_For        => Looking_For,
                       Recur_Depth        => Recur_Depth + 1,
                       All_Blocks         => All_Blocks,
                       All_Turnouts       => All_Turnouts,
                       Success            => Success);

            if Success then
               All_Blocks.Size := All_Blocks.Size + 1;
               All_Blocks.Items (All_Blocks.Size).Block := Current;
               All_Blocks.Items (All_Blocks.Size).Direction := Direction;
               return;
            end if;

         when Choice =>

            Layout.Get_Turnout_Num (Current_Block => Current,
                                    Direction     => Direction,
                                    Turn_Num      => Next_Turnout);

            Layout.Take_Limb (Turn_Num      => Next_Turnout,
                              Turn_Choice   => A_Limb,
                              Next_Limb     => Next_Block);

            A_Sensor := Layout.Get_Sensor_Number (Block_A => Current,
                                                  Block_B => Next_Block);

            -- check reversing point
            if Is_Reversing_Point (A_Sensor) then
               A_Direction := Layout.Opposite (Direction);
            else
               A_Direction := Direction;
            end if;

            Find_Loco (Direction          => A_Direction,
                       Current            => Next_Block,
                       Looking_For        => Looking_For,
                       Recur_Depth        => Recur_Depth + 1,
                       All_Blocks         => All_Blocks,
                       All_Turnouts       => All_Turnouts,
                       Success            => Success);
            -- not found down left limb
            if not Success then
               -- reassign "A_Limb"
               A_Limb := Layout.Opposite (A_Limb);

               Layout.Take_Limb (Turn_Num      => Next_Turnout,
                                 Turn_Choice   => A_Limb,
                                 Next_Limb     => Next_Block);

               A_Sensor := Layout.Get_Sensor_Number (Block_A => Current,
                                                     Block_B => Next_Block);

               -- check reversing point
               if Is_Reversing_Point (A_Sensor) then
                  A_Direction := Layout.Opposite (Direction);
               else
                  A_Direction := Direction;
               end if;

               Find_Loco (Direction          => A_Direction,
                          Current            => Next_Block,
                          Looking_For        => Looking_For,
                          Recur_Depth        => Recur_Depth + 1,
                          All_Blocks         => All_Blocks,
                          All_Turnouts       => All_Turnouts,
                          Success            => Success);
            end if;

            if Success then
               All_Turnouts.Size := All_Turnouts.Size + 1;
               All_Turnouts.Items (All_Turnouts.Size).Turnout := Next_Turnout;
               All_Turnouts.Items (All_Turnouts.Size).Direction := A_Limb;
               if Layout.Is_Joint (Turn => Next_Turnout, -- check if joint
                                   Dir  => A_Limb,
                                   A_Polarity => Direction) then
                  -- get joint turnout
                  Next_Turnout := Get_Joint_Turn (Turn      => Next_Turnout,
                                                  Block_Pol => Direction,
                                                  Dir       => A_Limb);
                  -- add to turnout list
                  All_Turnouts.Size := All_Turnouts.Size + 1;
                 All_Turnouts.Items (All_Turnouts.Size).Turnout := Next_Turnout;
                  All_Turnouts.Items (All_Turnouts.Size).Direction := A_Limb;
               end if;
            end if;

         when Forced =>

            Layout.Next_Block (Current_Block => Current,
                               A_Polarity    => Direction,
                               Next_Block    => Next_Block);

            Layout.Get_Turnout_Num (Current_Block => Current,
                                    Direction     => Direction,
                                    Turn_Num      => Next_Turnout);
            A_Sensor := Layout.Get_Sensor_Number (Block_A => Next_Block,
                                                  Block_B => Current);

            if Current = Limb_End (Turn_Num    => Next_Turnout,
                                   Turn_Choice => Opposite (A_Limb)) then
               A_Limb := Opposite (A_Limb);
            end if;

            -- check reversing point
            if Is_Reversing_Point (A_Sensor) then
               A_Direction := Layout.Opposite (Direction);
            else
               A_Direction := Direction;
            end if;

            Find_Loco (Direction          => A_Direction,
                       Current            => Next_Block,
                       Looking_For        => Looking_For,
                       Recur_Depth        => Recur_Depth + 1,
                       All_Blocks         => All_Blocks,
                       All_Turnouts       => All_Turnouts,
                       Success            => Success);

            if Success then
               All_Turnouts.Size := All_Turnouts.Size + 1;
               All_Turnouts.Items (All_Turnouts.Size).Turnout := Next_Turnout;
               All_Turnouts.Items (All_Turnouts.Size).Direction := A_Limb;
            end if;

         when A_Deadend =>
            Success := False;
            return;
      end case;

      -- add final block
      if Success then
         All_Blocks.Size := All_Blocks.Size + 1;
         All_Blocks.Items (All_Blocks.Size).Block := Current;
         All_Blocks.Items (All_Blocks.Size).Direction := Direction;
      end if;
   end Find_Loco;
   procedure Blocks_Beneath (Loco     : in Block_Number;
                             Caboose  : in Block_Number;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean)
   is
   begin
      Find_Loco (Direction          => Normal,
                 Current            => Caboose,
                 Looking_For        => Loco,
                 Recur_Depth        => 1,
                 All_Blocks         => Blocks,
                 All_Turnouts       => Turnouts,
                 Success            => Success);
      -- if not found in Normal direction, search reversed
      if not Success then
         Find_Loco (Direction          => Reversed,
                    Current            => Caboose,
                    Looking_For        => Loco,
                    Recur_Depth        => 1,
                    All_Blocks         => Blocks,
                    All_Turnouts       => Turnouts,
                    Success            => Success);
      end if;
   end Blocks_Beneath;
end Layout.Search;
