-- Body for Turnout Motors
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt

with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;
package body Motors is

   Base_1 : constant Port_IO.Address_Range := (16#220#);
   Base_2 : constant Port_IO.Address_Range := (16#228#);

   type Turnout_Range is range 0 .. 7;
   type Limb_Array    is array (Turnout_Range) of Layout.Turn_Dir;
   for Limb_Array'Component_Size use 1;
   for Limb_Array'Size use 8;

   type Pos_Array is array (Turnout_Range) of Boolean;
   for Pos_Array'Component_Size use 1;
   for Pos_Array'Size use 8;

   function To_Array is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                      Target => Pos_Array);

   function To_Array is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                      Target => Limb_Array);

   function To_Byte is new Ada.Unchecked_Conversion (Source => Limb_Array,
                                                     Target => Port_IO.Byte);

   --------------------------------
   -- Protected object Set_Turns --
   --------------------------------

   -- Used to make setting a turnout mutually exclusive --
   protected Set_Turns is
      procedure Set_Out (Motor     : in Layout.Turnout_Num;
                         Direction : in Layout.Turn_Dir);
   end Set_Turns;

   protected body Set_Turns is

      procedure Set_Out (Motor     : in Layout.Turnout_Num;
                         Direction : in Layout.Turn_Dir) is
         Change : Limb_Array;
         Motor_Num : constant Integer := Integer (Motor);
      begin

         if Motor_Num > 24 then
            Change := To_Array (Port_IO.In_Byte (Base_2 + 4));
            Change (Turnout_Range (Motor_Num - 25)) := Direction;
            Port_IO.Out_Byte (Address => Base_2 + 4,
                              Data    => To_Byte (Change));
         elsif Motor_Num < 9 then
            Change := To_Array (Port_IO.In_Byte (Base_1 + 4));
            Change (Turnout_Range (Motor_Num - 1)) := Direction;
            Port_IO.Out_Byte (Address => Base_1 + 4,
                              Data    => To_Byte (Change));
         elsif Motor_Num < 17 then
            Change := To_Array (Port_IO.In_Byte (Base_1 + 6));
            Change (Turnout_Range (Motor_Num - 9)) := Direction;
            Port_IO.Out_Byte (Address => Base_1 + 6,
                              Data    => To_Byte (Change));
         else
            Change := To_Array (Port_IO.In_Byte (Base_1 + 1));
            Change (Turnout_Range (Motor_Num - 17)) := Direction;
            Port_IO.Out_Byte (Address => Base_1 + 1,
                              Data    => To_Byte (Change));
         end if;
      end Set_Out;

   end Set_Turns;

   ---------
   -- Set --
   ---------

   procedure Set (Motor     : in Layout.Turnout_Num;
                  Direction : in Layout.Turn_Dir) is
   begin

      Set_Turns.Set_Out (Motor, Direction);

   end Set;

   -----------------
   -- In_Position --
   -----------------

   function In_Position (Motor : in Layout.Turnout_Num) return Boolean is
      Data       : Port_IO.Byte;
      Directions : Pos_Array;
      Motor_Num  : constant Integer := Integer (Motor);
   begin

      if Motor_Num > 24 then
         Data := Port_IO.In_Byte (Base_2 + 5);
         Directions := To_Array (Data);
         return Directions (Turnout_Range (Motor_Num - 25));
      elsif Motor_Num < 9 then
         Data := Port_IO.In_Byte (Base_1 + 5);
         Directions := To_Array (Data);
         return Directions (Turnout_Range (Motor_Num - 1));
      elsif Motor_Num < 17 then
         Data := Port_IO.In_Byte (Base_1);
         Directions := To_Array (Data);
         return Directions (Turnout_Range (Motor_Num - 9));
      else
         Data := Port_IO.In_Byte (Base_1 + 2);
         Directions := To_Array (Data);
         return Directions (Turnout_Range (Motor_Num - 17));
      end if;

   end In_Position;

begin
   -- Initialize the port addresses --
   Port_IO.Out_Byte (Address => Base_1 + 7,
                     Data    => (16#82#));
   Port_IO.Out_Byte (Address => Base_2 + 7,
                     Data    => (16#82#));
   Port_IO.Out_Byte (Address  => Base_1 + 3,
                     Data    => (16#99#));

   -- Initialize all turnouts to left --
   Port_IO.Out_Byte (Address => Base_1 + 4,
                     Data    => 0);
   Port_IO.Out_Byte (Address => Base_1 + 6,
                     Data    => 0);
   Port_IO.Out_Byte (Address => Base_1 + 1,
                     Data    => 0);
   Port_IO.Out_Byte (Address => Base_2 + 4,
                     Data    => 0);

end Motors;
