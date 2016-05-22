-- Specification for Engineers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with hand_controller; use hand_controller;
with Trains;

package Engineers is

   -- This package provides operations for Engineers

   type Skill_Rating is (Novice, Expert);
   type Engineer_ID  is range 1 .. 3;

   ----------------------------------------------------------------------------
   -- These two procedures control whether or not Engineers is currently
   -- monitoring its hand controller. If Disabled this package simply discards
   -- the Information. Initiallially Engineers is disabled.

   procedure Enable (Engineer   : in Engineer_ID;
                     Train      : in Trains.Train_ID;
                     Control    : in Controller;
                     Skill      : in Skill_Rating);
   -- Enables Engineers to monitor the Hand Controller

   ----------------------------------------------------------------------------
end Engineers;
