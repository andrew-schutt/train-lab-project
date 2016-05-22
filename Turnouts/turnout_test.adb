-- Package body for testing turnouts
-- Turnouts
-- Cleveland Express Spring 2011
-- Real-Time Embedded Systems
-- Managed by Andrew Schutt

with Layout; use Layout;
with Turnouts;
with Ada.Text_IO; use Ada.Text_IO;
with MaRTE_OS;
pragma Warnings (off, MaRTE_OS);

procedure Turnout_Test is
begin
   Skip_Line;
   for I in Layout.Turnout_Num'Range loop
      Put_Line ("Turnout " & I'Img & ":");
      Turnouts.Set (Requestor => 0,
                    Turnout   => I,
                    Direction => Right);
      Skip_Line;
   end loop;
   New_Line;
   Put_Line ("Press Enter to continue");
   Skip_Line;
   for I in Layout.Turnout_Num'Range loop
      Put_Line ("Turnout " & I'Img & ":");
      Turnouts.Set (Requestor => 0,
                    Turnout   => I,
                    Direction => Left);
      Skip_Line;
   end loop;
   Put_Line ("Shutting Down turnouts");
   Turnouts.Shut_Down;
end Turnout_Test;
