-- Test Program for Turnout Motors
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by Andrew Schutt

with Motors; use Motors;
with Layout; use Layout;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO; use Ada.Text_IO;

procedure Motors_Test is

  -- type Turnout_Array is array (Turnout_Num) of Layout.Turnout_Num;
   type Turnout_Range is range 1 .. 24;

begin

   -- Attempt to set all turnouts to right
   for Index in Turnout_Range loop
      Motors.Set (Turnout_Num (Index), Right);
      -- Poll to check if turnout is set in position
      while not Motors.In_Position (Turnout_Num (Index)) loop
         Put_Line ("Testing turnout position of "
                   & Turnout_Num'Image (Turnout_Num (Index)));
      end loop;
      Put_Line ("Press Enter");
      Skip_Line;
   end loop;

   -- Attempt to set all turnouts to left
   for Index in Turnout_Range loop
      Motors.Set (Turnout_Num (Index), Left);
      -- Poll to check if turnout is set in position
      while not Motors.In_Position (Turnout_Num (Index)) loop
         Put_Line ("Testing turnout position of "
                   & Turnout_Num'Image (Turnout_Num (Index)));
      end loop;
      Put_Line ("Press Enter");
      Skip_Line;
   end loop;

   -- Attempt to set all turnouts to right
   -- then set all turnouts to left immediately
   for Index in Turnout_Range loop
      Motors.Set (Turnout_Num (Index), Right);
      Motors.Set (Turnout_Num (Index), Left);
      -- Poll to check if turnout is set in position
      while not Motors.In_Position (Turnout_Num (Index)) loop
         Put_Line ("Testing turnout position of "
                   & Turnout_Num'Image (Turnout_Num (Index)));
      end loop;
      Put_Line ("Press Enter");
      Skip_Line;
   end loop;
end Motors_Test;
