-- Package body for Command
-- 810:174:01 Spring '11
-- Cleveland Express
-- Managed by Elliot Schmitt

with Console_Management;
with Ada.Text_IO;
package body Command is

     First_Input   : Character;
     Second_Input  : Character;
     Third_Input   : Character;
     Number_String : String (1 .. 2);

   ---------
   -- Get --
   ---------

   procedure Get (Command : out Command_Rec) is
   begin
      Console_Management.Disable_Echo;
      Console_Management.Set_Raw_Mode;
      Ada.Text_IO.Get (First_Input);
      if First_Input >= '1' and First_Input <= '9' then
         Ada.Text_IO.Get (Second_Input);
         if Second_Input >= '0' and Second_Input <= '9' then
            Number_String := First_Input & Second_Input;
            if Integer'Value (Number_String) > 40 then
               Command := (Which => Error);
            else
               Ada.Text_IO.Get (Third_Input);
               if (Third_Input = 'R' or Third_Input = 'r')
                 and Integer'Value (Number_String) <= 26 then
                  Command := (Which   => Right,
                              Turnout =>
                            Layout.Turnout_Num (Integer'Value (Number_String)));
               elsif (Third_Input = 'L' or Third_Input = 'l')
                 and Integer'Value (Number_String) <= 26 then
                  Command := (Which   => Left,
                              Turnout =>
                    Layout.Turnout_Num (Integer'Value (Number_String)));
               elsif (Third_Input = 'F' or Third_Input = 'f')
                 and Integer'Value (Number_String) <= 40 then
                  Command := (Which => Free,
                              Block =>
                    Layout.Block_Number (Integer'Value (Number_String)));
               else
                  Command := (Which => Error);
               end if;
            end if;
         elsif (Second_Input = 'S' or Second_Input = 's')
           and First_Input <= '3' then
            Command := (Which => Stop,
                        Train =>
              Trains.Train_ID (Integer'Value (String'(1 .. 1 => First_Input))));
         elsif (Second_Input = 'G' or Second_Input = 'g')
           and First_Input <= '3' then
            Command := (Which => Go,
                        Train =>
              Trains.Train_ID (Integer'Value (String'(1 .. 1 => First_Input))));
         elsif Second_Input = 'R' or Second_Input = 'r' then
            Command := (Which   => Right,
                        Turnout =>
              Layout.Turnout_Num (Integer'Value
                                  (String'(1 .. 1 => First_Input))));
         elsif Second_Input = 'L' or Second_Input = 'l' then
            Command := (Which   => Left,
                        Turnout =>
              Layout.Turnout_Num (Integer'Value
                                  (String'(1 .. 1 => First_Input))));
         elsif (Second_Input = 'E' or Second_Input = 'e')
           and First_Input <= '3' then
            Command := (Which    => Skill,
                        Engineer =>
              Engineers.Engineer_ID (Integer'Value
                                     (String'(1 .. 1 => First_Input))));
         else
            Command := (Which => Error);
         end if;
      elsif First_Input = 'R' or First_Input = 'r' then
         Command := (Which => Restart);
      elsif First_Input = 'Q' or First_Input = 'q' then
         Command := (which => Quit);
      elsif First_Input = ' ' then
         Command := (Which => Stop_All);
      else
         Command := (Which => Error);
      end if;

   end Get;

end Command;
