-- Test program for Command
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by Elliot Schmitt

with Command; use Command;
with Layout;
with Engineers;
with Trains;
with Ada.Text_IO;
with MaRTE_OS; pragma Warnings (Off, MaRTE_OS);
procedure Command_Test is

   My_Command : Command.Command_Rec;

begin
   loop
      Ada.Text_IO.Put_Line ("Enter A Command.");
      Command.Get (My_Command);
      case My_Command.Which is
         when Stop_All | Restart | Quit | Error =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                            & Command.Command_Type'Image (My_Command.Which));
         when Stop | Go =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                                 & Command.Command_Type'Image (My_Command.Which)
                                 & " for train number "
                                 & Trains.Train_ID'Image (My_Command.Train));
         when Left | Right =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                               & Command.Command_Type'Image (My_Command.Which)
                               & " for turnout number "
                               & Layout.Turnout_Num'Image (My_Command.Turnout));
         when Free =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                                & Command.Command_Type'Image (My_Command.Which)
                                & " for block number "
                                & Layout.Block_Number'Image (My_Command.Block));
         when Skill =>
            Ada.Text_IO.Put_Line ("The Command entered was "
                           & Command.Command_Type'Image (My_Command.Which)
                           & " for engineer number "
                           & Engineers.Engineer_ID'Image (My_Command.Engineer));
      end case;
   end loop;
end Command_Test;
