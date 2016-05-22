-- Test Program for Sound_Unit.adb
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt & Drew Persson

with Sound_Unit; use Sound_Unit;
with Ada.Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Sound_Check is

   package Sound_IO is new Ada.Text_IO.Enumeration_IO (Sound_Unit.Sound_Type);
   package Unit_IO is new Ada.Text_IO.Integer_IO (Sound_Unit.Unit_Number);

   Sound : Sound_Unit.Sound_Type;
   Unit  : Sound_Unit.Unit_Number;

begin

   loop
      Ada.Text_IO.Put_Line ("Enter a Unit Number.");
      Unit_IO.Get (Unit);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Enter a Sound type (Horn or Bell)");
      Sound_IO.Get (Sound);

      case Sound is
         when Horn =>
            Sound_Unit.Turn_On_Horn (Unit);
            delay 5.0;
            Sound_Unit.Turn_Off_Horn (Unit);
         when Bell =>
            Sound_Unit.Turn_On_Bell (Unit);
            delay 5.0;
            Sound_Unit.Turn_Off_Bell (Unit);
      end case;

      Ada.Text_IO.New_Line;

   end loop;

end Sound_Check;
