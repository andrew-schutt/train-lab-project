-- Package body for Sound
-- Real-Time Embedded Systems
-- Spring 2011 - Cleveland Express
-- managed by Elliot Schmitt

package body Sound is

   ------------------------------------
   -- Protected Objected Sound_Calls --
   ------------------------------------

   protected Sound_Calls is

      procedure On_Bell (Unit : in Installed_Range);

      procedure Off_Bell (Unit : in Installed_Range);

      procedure Horn_On (Unit : in Installed_Range);

      procedure Horn_Off (Unit : in Installed_Range);

   end Sound_Calls;

   protected body Sound_Calls is

      procedure On_Bell (Unit : in Installed_Range) is
      begin
         Sound_Unit.Turn_On_Bell (Unit);
      end On_Bell;

      procedure Off_Bell (Unit : in Installed_Range) is
      begin
         Sound_Unit.Turn_Off_Bell (Unit);
      end Off_Bell;

      procedure Horn_On (Unit : in Installed_Range) is
      begin
         Sound_Unit.Turn_On_Horn (Unit);
      end Horn_On;

      procedure Horn_Off (Unit : in Installed_Range) is
      begin
         Sound_Unit.Turn_Off_Horn (Unit);
      end Horn_Off;

   end Sound_Calls;

   --------------------------------
   -- Task for sounding the horn --
   --------------------------------
   task type Horn_Call is
      entry Set_Unit (Unit : in Installed_Range);
      entry Blow_Horn (Signal : in Horn_Signal);
   end Horn_Call;

   task body Horn_Call is
      My_Unit : Installed_Range;
      My_Signal : Horn_Signal;
   begin

      accept Set_Unit (Unit : in Installed_Range) do
         My_Unit := Unit;
      end Set_Unit;

      loop
         accept Blow_Horn (Signal : in Horn_Signal) do
            My_Signal := Signal;
         end Blow_Horn;
         case My_Signal is
            when Stop =>
               -- one short
               Sound_Calls.Horn_On (My_Unit);
               delay 1.0;
               Sound_Calls.Horn_Off (My_Unit);
            when Start =>
               -- two long
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);

               delay 0.3;

               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);

            when Approach_Highway =>
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- short horn
               Sound_Calls.Horn_On (My_Unit);
               delay 1.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);

            when Approach_Crossing =>
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- short horn
               Sound_Calls.Horn_On (My_Unit);
               delay 1.0;
               Sound_Calls.Horn_Off (My_Unit);

            when Warning =>
               -- long horn
               Sound_Calls.Horn_On (My_Unit);
               delay 3.0;
               Sound_Calls.Horn_Off (My_Unit);
               delay 0.3;
               -- short horn
               Sound_Calls.Horn_On (My_Unit);
               delay 1.0;
               Sound_Calls.Horn_Off (My_Unit);
         end case;
      end loop;
   end Horn_Call;

   -- An array of tasks to simplify which Dallee unit is used
   type Horn_Task_Array is array (Installed_Range) of Horn_Call;
   My_Horns : Horn_Task_Array;

   ----------------
   -- Sound_Horn --
   ----------------

   procedure Sound_Horn
     (Unit   : in Installed_Range;
      Signal : in Horn_Signal)
   is
   begin
      My_Horns (Unit).Blow_Horn (Signal);
   end Sound_Horn;

   -------------
   -- Bell_On --
   -------------

   procedure Bell_On (Unit : in Installed_Range) is
   begin
      Sound_Calls.On_Bell (Unit);
   end Bell_On;

   --------------
   -- Bell_Off --
   --------------

   procedure Bell_Off (Unit : in Installed_Range) is
   begin
      Sound_Calls.Off_Bell (Unit);
   end Bell_Off;

begin

   for Unit in Installed_Range loop
      My_Horns (Unit).Set_Unit (Unit);
   end loop;
end Sound;
