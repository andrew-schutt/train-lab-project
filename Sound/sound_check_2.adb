-- Test Program for Sound_Unit.adb
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt & Drew Persson

with Sound_Unit; use Sound_Unit;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Sound_Check_2 is

begin

   for Unit in Unit_Number loop


      Sound_Unit.Turn_On_Horn (Unit);
      delay 1.0;
      Sound_Unit.Turn_On_Bell (Unit);
      delay 3.0;

      if Unit < 4 then
         Sound_Unit.Turn_On_Bell (Unit + 1);
         delay 2.0;
         Sound_Unit.Turn_Off_Bell (Unit + 1);
      end if;


      Sound_Unit.Turn_Off_Horn (Unit);
      delay 1.0;
      Sound_Unit.Turn_Off_Bell (Unit);
      delay 1.0;

   end loop;

end Sound_Check_2;
