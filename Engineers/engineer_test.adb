-- Test Program for Engineers
-- 810:174:01 Spring '11
-- group name: Cleveland Express
-- managed by: Drew Persson

with Engineers; use Engineers;
with hand_controller; use hand_controller;
with Trains; use Trains;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);

procedure Engineer_test is

   Control_A : constant Controller := A;
   Control_B : constant Controller := B;
   Control_C : constant Controller := C;

begin

   loop

      Engineers.Enable (Engineer => Engineer_ID (1),
                        Train    => Train_ID (1),
                        Control  => Control_A);

      Engineers.Enable (Engineer => Engineer_ID (2),
                        Train    => Train_ID (2),
                        Control  => Control_B);

      Engineers.Enable (Engineer => Engineer_ID (3),
                        Train    => Train_ID (3),
                        Control  => Control_C);
   end loop;

end Engineer_test;
