-- Test Procedure for Sound
-- Real-Time Embedded Systems
-- Spring 2011 - Cleveland Express
-- managed by Elliot Schmitt

with Sound;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);
procedure Test_Sound is

begin
   -- test bells on all units;
   for Unit in Sound.Installed_Range loop

      Sound.Bell_On (Unit);
      delay 5.0;
      Sound.Bell_Off (Unit);

   end loop;
   -- test all horn signals on all units
   for Unit in Sound.Installed_Range loop
      for Signal in Sound.Horn_Signal loop
         Sound.Sound_Horn (Unit   => Unit,
                           Signal => Signal);
         delay 20.0;
      end loop;
   end loop;
   -- same as previous loop, but with out the delay
   -- to test if the not buffering works
   for Unit in Sound.Installed_Range loop
      for Signal in Sound.Horn_Signal loop
         Sound.Sound_Horn (Unit   => Unit,
                           Signal => Signal);
      end loop;
   end loop;

end Test_Sound;
