-- body for Blocks
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with System; use System;
with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;

package body Blocks is

   Blocks_1_12  : constant Port_IO.Address_Range := (16#200#);
   Blocks_13_24 : constant Port_IO.Address_Range := (16#208#);
   Blocks_25_36 : constant Port_IO.Address_Range := (16#210#);
   Blocks_37_48 : constant Port_IO.Address_Range := (16#218#);

   type Block_Status_Rec is
      record
         Cab      : Cabs.Cab_ID := 0;
         Polarity : Boolean;
         Next_Cab : Cabs.Cab_ID;
         Next_Pol : Boolean;
      end record;

   for Block_Status_Rec use
      record
         Cab      at 0 range 0 .. 2;
         Polarity at 0 range 3 .. 3;
         Next_Cab at 0 range 4 .. 6;
         Next_Pol at 0 range 7 .. 7;
      end record;

   for Block_Status_Rec 'Size use 8;
   for Block_Status_Rec 'Bit_Order use System.Low_Order_First;

   type Reserve_Rec is
      record
         Train : Trains.Request_ID := 0;
         Count : Integer := 0;
      end record;

   type Block_Status_Array is array (Layout.Block_Number)
   of Reserve_Rec;

   type Block_Cabs_Array is array (Layout.Block_Number)
   of Cabs.Cab_ID;

   type Add_Range is range 1 .. 12;
   type Add_Array is array (Add_Range) of Natural;
   Add : constant Add_Array := (1  => 0, 2  => 0, 3  => 1, 4  => 1, 5  => 2,
                                6  => 2, 7  => 4, 8  => 4, 9  => 5, 10 => 5,
                                11 => 6, 12 => 6);

   function To_Block_Status is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Block_Status_Rec);

   function To_Byte is new Ada.Unchecked_Conversion (Source => Block_Status_Rec,
                                                     Target => Port_IO.Byte);

   protected Access_Blocks is
      procedure Reserve (Block   : in Layout.Block_Number;
                         Train   : in Trains.Train_ID;
                         Success : out Boolean);

      procedure UnReserve (Block   : in Layout.Block_Number;
                           Train   : in Trains.Request_ID);

      procedure Set_Block_Power (Cab       : in Cabs.Cab_ID;
                                 Block     : in Layout.Block_Number;
                                 Direction : in Layout.Polarity);

      function Train_Reserving_Block (Block : in Layout.Block_Number)
                                      return Trains.Train_ID;

      function Cab_Powering_Block (Block : in Layout.Block_Number)
                                   return Cabs.Cab_ID;

   private
      Train_Blocks : Block_Status_Array;
      Block_Cabs   : Block_Cabs_Array;
   end Access_Blocks;

   protected body Access_Blocks is

      procedure Reserve (Block   : in Layout.Block_Number;
                         Train   : in Trains.Train_ID;
                         Success : out Boolean) is
      begin

         if Train_Blocks (Block).Train = 0 then
            Train_Blocks (Block).Train := Train;
            Train_Blocks (Block).Count := Train_Blocks (Block).Count + 1;
            Success := True;
         elsif Train_Blocks (Block).Train = Train then
            Train_Blocks (Block).Count := Train_Blocks (Block).Count + 1;
            Success := True;
         else
            Success := False;
         end if;

      end Reserve;

      procedure UnReserve (Block   : in Layout.Block_Number;
                           Train   : in Trains.Request_ID) is
      begin
         if Train = 0 then
            -- its the dispatcher
            Train_Blocks (Block).Train := 0;
            Train_Blocks (Block).Count := 0;
         elsif Train_Blocks (Block).Train = Train then
            Train_Blocks (Block).Count := Train_Blocks (Block).Count - 1;
            if Train_Blocks (Block).Count = 0 then
               Train_Blocks (Block).Train := 0;
            end if;
         end if;
      end UnReserve;

      procedure Set_Block_Power (Cab       : in Cabs.Cab_ID;
                                 Block     : in Layout.Block_Number;
                                 Direction : in Layout.Polarity) is

         Address  : Port_IO.Address_Range;

         Data : Block_Status_Rec;


      begin
         if Block > 36 then
            Address := Blocks_37_48 +
              Address_Range (Add (Add_Range (Block - 36)));
         elsif Block > 24 then
            Address := Blocks_25_36 +
              Address_Range (Add (Add_Range (Block - 24)));
         elsif Block > 12 then
            Address := Blocks_13_24 +
              Address_Range (Add (Add_Range (Block - 12)));
         else
            Address := Blocks_1_12 +
              Address_Range (Add (Add_Range (Block)));
         end if;

         Data := To_Block_Status (Port_IO.In_Byte (Address));

         if Block mod 2 = 0 then
            Data.Cab := Cab;
            if Direction = Layout.Normal then
               Data.Polarity := False; -- sets plarity to normal
            else
               Data.Polarity := True; -- sets polarity to reversed
            end if;
         else
            Data.Next_Cab := Cab;
            if Direction = Layout.Normal then
               Data.Next_Pol := False; -- sets plarity to normal
            else
               Data.Next_Pol := True; -- sets polarity to reversed
            end if;
         end if;

         Port_IO.Out_Byte (Address, To_Byte (Data));

         Block_Cabs (Block) := Cab;
      end Set_Block_Power;

      function Train_Reserving_Block (Block : in Layout.Block_Number)
                                      return Trains.Train_ID is
      begin
         return Train_Blocks (Block).Train;
      end Train_Reserving_Block;

      function Cab_Powering_Block (Block : in Layout.Block_Number)
                                   return Cabs.Cab_ID is
      begin
         return Block_Cabs (Block);
      end Cab_Powering_Block;

   end Access_Blocks;

   ----------------------------------------------------------------------------
   procedure Reserve_Block (Block   : in Layout.Block_Number;
                            Train   : in Trains.Train_ID;
                            Success : out Boolean) is
   begin
      Access_Blocks.Reserve (Block, Train, Success);
   end Reserve_Block;


   procedure Un_Reserve_Block (Block   : in Layout.Block_Number;
                               Train   : in Trains.Request_ID) is
   begin
      Access_Blocks.UnReserve (Block, Train);
   end Un_Reserve_Block;


   procedure Set_Power (Cab       : in Cabs.Cab_ID;
                        Block     : in Layout.Block_Number;
                        Direction : in Layout.Polarity) is
   begin
      -- 0 = Normal Polarity, 1 = Reversed
      Access_Blocks.Set_Block_Power (Cab, Block, Direction);
   end Set_Power;

   Initialize : constant Port_IO.Byte := (2#10000000#);
   for Initialize 'Size use 8;

   function Cab_Powering (Block : in Layout.Block_Number) return Cabs.Cab_ID is
      Cab : Cabs.Cab_ID;
   begin
      Cab := Access_Blocks.Cab_Powering_Block (Block);
      return Cab;
   end Cab_Powering;

   function Train_Reserving (Block : in Layout.Block_Number)
                             return Trains.Request_ID is
      Train : Trains.Request_ID;
   begin
      Train := Access_Blocks.Train_Reserving_Block (Block);
      return Train;
   end Train_Reserving;

begin
   Port_IO.Out_Byte (Blocks_1_12 + 3, Initialize);
   Port_IO.Out_Byte (Blocks_1_12 + 7, Initialize);

   Port_IO.Out_Byte (Blocks_13_24 + 3, Initialize);
   Port_IO.Out_Byte (Blocks_13_24 + 7, Initialize);

   Port_IO.Out_Byte (Blocks_25_36 + 3, Initialize);
   Port_IO.Out_Byte (Blocks_25_36 + 7, Initialize);

   Port_IO.Out_Byte (Blocks_37_48 + 3, Initialize);
   Port_IO.Out_Byte (Blocks_37_48 + 7, Initialize);
end Blocks;
