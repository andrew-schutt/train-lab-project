-- Test Program for hall.adb
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by Elliot Schmitt

with Halls; use Halls;
with Hall_IO;
with Ada.Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Hall_Test is

   Sensor_Ptr : constant Halls.Callback_Ptr := Hall_IO.Sensor_Trig'Access;

begin

   Halls.Initialize;

   for Sensor in Halls.Hall_ID loop

      Halls.Enable (Sensor_Ptr);

   end loop;

   Ada.Text_IO.Skip_Line;

   Halls.Disable;

end Hall_Test;
