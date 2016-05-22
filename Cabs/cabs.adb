-- body for cabs
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with DAC;
package body Cabs is

  type Limit_Array is array (Control_Cab_ID) of Percent;

  Limits : Limit_Array := (1 => 0,
                           2 => 0,
                           3 => 0,
                           4 => 0,
                           5 => 0,
                           6 => 0);

  Settings : Limit_Array := (1 => 0,
                             2 => 0,
                             3 => 0,
                             4 => 0,
                             5 => 0,
                             6 => 0);

   type Volts is delta 1.0 / 2.0 ** 12 range -45_000.0 .. 45_000.0;

   procedure Set
     (Cab   : in Control_Cab_ID;
      Value : in Percent)
   is
   begin
      if Value > Limits (Cab) then
         Settings (Cab) := Limits (Cab);
         DAC.Write (DAC.Channel_Number (Cab - 1),
                    DAC.Output_Volts (5 * Volts (Settings (Cab)) / 100));
      else
         Settings (Cab) := Value;
         DAC.Write (DAC.Channel_Number (Cab - 1),
                    DAC.Output_Volts (5 * Volts (Value) / 100));
      end if;

   end Set;

   ---------
   -- Get --
   ---------

   procedure Get
     (Cab   : in  Cab_ID;
      Value : out Percent)
   is
   begin
      Value := Settings (Cab);
   end Get;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Cab   : in Control_Cab_ID;
      Value : in Percent)
   is
   begin
      if Settings (Cab) > Value then
         Set (Cab, Value);
      end if;

      Limits (Cab) := Value;

   end Set_Limit;

   ---------------
   -- Get_Limit --
   ---------------

   procedure Get_Limit
     (Cab   : in  Cab_ID;
      Value : out Percent)
   is
   begin
      Value := Limits (Cab);
   end Get_Limit;

begin

   -- Initializes all cabs to zero volts
   for Cab in Control_Cab_ID'Range loop
      DAC.Write (DAC.Channel_Number (Cab - 1), DAC.Output_Volts (0));
   end loop;

end Cabs;
