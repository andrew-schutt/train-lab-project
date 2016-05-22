-- body for our analog to digital converter
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with System; use System;
with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;

package body ADC is

   Base            : constant Port_IO.Address_Range := (16#260#);
   Convert_Address : constant Port_IO.Address_Range := (Base + 1);
   CSR_Address     : constant Port_IO.Address_Range := (Base + 2);

   protected type Semaphore (Initial_Value : Natural) is
      procedure Signal;

      entry Wait;
   private
      Count : Natural := Initial_Value;
   end Semaphore;

   protected body Semaphore is
      procedure Signal is
      begin
         Count := Count + 1;
      end Signal;

      entry Wait when Count > 0 is
      begin
         Count := Count - 1;
      end Wait;
   end Semaphore;

   My_Semaphore : Semaphore (Initial_Value => 1);

   type ADC_Rec is
      record
         Channel : Channel_Number;
         Read    : Boolean;
      end record;

   for ADC_Rec use
      record
         Channel at 0 range 0 .. 2;
         Read    at 0 range 7 .. 7;
      end record;

   for ADC_Rec 'Size use 8;
   for ADC_Rec 'Bit_Order use System.Low_Order_First;

   type Data_Type is range 0 .. 4095;
   for  Data_Type 'Size use 16;

   type Volts is delta 1.0 /2.0**12 range -45_000.0 .. 45_000.0;

   function To_ADC is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                    Target => ADC_Rec);

   function To_Byte is new Ada.Unchecked_Conversion (Source => ADC_Rec,
                                                     Target => Port_IO.Byte);

   ----------
   -- Read --
   ----------

   procedure Read (Channel : in Channel_Number;
                   Value   : out Input_Volts)
   is

      Value_Byte : constant Port_IO.Byte := 1;

      CSR_Rec    : ADC_Rec;
      for CSR_Rec'Size use 8;

      Data_Volts : Data_Type;
      for Data_Volts'Size use 16;

   begin
      My_Semaphore.Wait;

      for Index in 1 .. 2 loop

         CSR_Rec.Channel := Channel;

         Port_IO.Out_Byte (CSR_Address, To_Byte (CSR_Rec));

         Port_IO.Out_Byte (Convert_Address, Value_Byte);

         loop
            delay 0.025;
            CSR_Rec := To_ADC (Port_IO.In_Byte (CSR_Address));
            exit when not CSR_Rec.Read;
         end loop;

         Data_Volts := Data_Type (Port_IO.In_Word (Base) / 16);

         -- Add 1 to count to let next task through
         Value := Input_Volts ((10 * Volts (Data_Volts) / 4095) - 5.0);

      end loop;
      My_Semaphore.Signal;

   end Read;

end ADC;
