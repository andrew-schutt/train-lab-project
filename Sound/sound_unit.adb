-- Specification for Dallee Sound Unit Device Driver
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt & Drew Persson

with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;

package body Sound_Unit is

   IO_Control : constant Port_IO.Address_Range := (16#260# + 3);

   type Sound_Array is array (Unit_Number, Sound_Type) of Boolean;
   for Sound_Array'Component_Size use 1;
   for Sound_Array'Size use 8;

   Sounds : Sound_Array;

   function To_Byte is new Ada.Unchecked_Conversion (Source => Sound_Array,
                                                     Target => Port_IO.Byte);
   ------------------
   -- Turn_On_Bell --
   ------------------

   procedure Turn_On_Bell (Dallee : in Unit_Number) is
   begin

      Sounds (Dallee, Bell) := True;
      Port_IO.Out_Byte (IO_Control, To_Byte (Sounds));

   end Turn_On_Bell;

   ------------------
   -- Turn_On_Horn --
   ------------------

   procedure Turn_On_Horn (Dallee : in Unit_Number) is
   begin

      Sounds (Dallee, Horn) := True;
      Port_IO.Out_Byte (IO_Control, To_Byte (Sounds));

   end Turn_On_Horn;

   -------------------
   -- Turn_Off_Bell --
   -------------------

   procedure Turn_Off_Bell (Dallee : in Unit_Number) is
   begin

      Sounds (Dallee, Bell) := False;
      Port_IO.Out_Byte (IO_Control, To_Byte (Sounds));

   end Turn_Off_Bell;

   -------------------
   -- Turn_Off_Horn --
   -------------------

   procedure Turn_Off_Horn (Dallee : in Unit_Number) is
   begin

      Sounds (Dallee, Horn) := False;
      Port_IO.Out_Byte (IO_Control, To_Byte (Sounds));

   end Turn_Off_Horn;

begin

   Sounds := (others => (others => False));
   Port_IO.Out_Byte (Address => IO_Control,
                     Data    => 0);

end Sound_Unit;
