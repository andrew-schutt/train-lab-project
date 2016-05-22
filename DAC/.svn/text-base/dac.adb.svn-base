-- Body for Digital to Analog Converter
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by: Andrew Schutt

with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;

package body DAC is

   Base_Address : constant Port_IO.Address_Range := (16#240#);

   type Address_Array is array (Channel_Number) of Port_IO.Address_Range;

   Addresses : constant Address_Array := (0 => Base_Address,
                                          1 => Base_Address + 2,
                                          2 => Base_Address + 4,
                                          3 => Base_Address + 6,
                                          4 => Base_Address + 8,
                                          5 => Base_Address + 10);

   type Data_Type is range 0 .. 4095;
   for  Data_Type 'Size use 16;

   function To_Word is new Ada.Unchecked_Conversion (Source => Data_Type,
                                                     Target => Port_IO.Word);
   -----------
   -- Write --
   -----------

   procedure Write
     (Channel : in Channel_Number;
      Value   : in Output_Volts)
   is

      Data : Data_Type;

   begin

      Data := Data_Type (409.5 * Float (Value) + 2047.5);

      Port_IO.Out_Word (Addresses (Channel), To_Word (Data));

   end Write;

end DAC;
