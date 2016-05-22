-- Specification for Dallee Sound Unit Device Driver
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Elliot Schmitt & Drew Persson

package Sound_Unit is

   type Unit_Number is range 1 .. 4;

   type Sound_Type is (Horn, Bell);

   procedure Turn_On_Bell (Dallee : in Unit_Number);

   procedure Turn_On_Horn (Dallee : in Unit_Number);

   procedure Turn_Off_Bell (Dallee : in Unit_Number);

   procedure Turn_Off_Horn (Dallee : in Unit_Number);


end Sound_Unit;
