-- body for doubletalk
-- 810:174:01 Spring '11
-- Group: Cleveland Express
-- Managed by Andrew Schutt & Drew Persson

with Bounded_Queue;
with Ada.Unchecked_Conversion;
with Port_IO; use Port_IO;

package body DoubleTalk is


   Null_Char : constant Character := ASCII.NUL;
   Command   : constant Character := ASCII.SOH;

   TTS_Port : constant Port_IO.Address_Range := (16#31F#);

   type Speak_Rec is
      record
         Phrase : Phrase_Strings.Bounded_String;
         Voice  : Voice_Range;
      end record;

   type CR_Range is range 0 .. 7;

   type CR_Array is array (CR_Range) of Boolean;
   for  CR_Array'Component_Size use 1;
   for  CR_Array'Size use 8;

   function Byte_To_Array is new Ada.Unchecked_Conversion
                                                       (Source => Port_IO.Byte,
                                                        Target => CR_Array);
   function Char_To_Byte is new Ada.Unchecked_Conversion
                                                     (Source => Character,
                                                      Target => Port_IO.Byte);

   package Speak_Queue is new
     Bounded_Queue (Element_Type => Speak_Rec);

   protected type Speak_Buffer (Max_Size : Positive) is
      procedure Put (Item : in Speak_Rec);
      -- enqueue item to Speak_Buffer
      entry Take (Item : out Speak_Rec);
      -- dequeue item from Speak_Buffer
   private
      Buffer : Speak_Queue.Queue_Type (Max_Size);
   end Speak_Buffer;

   protected body Speak_Buffer is
      procedure Put (Item : in Speak_Rec) is
         Removed_Item : Speak_Rec;
      begin
         if Speak_Queue.Full (Buffer) then
            Speak_Queue.Dequeue (Queue => Buffer,
                                 Item  => Removed_Item);
         end if;
         Speak_Queue.Enqueue (Queue => Buffer,
                              Item  => Item);
      end Put;

      entry Take (Item : out Speak_Rec)
        when not Speak_Queue.Empty (Buffer) is
      begin
         Speak_Queue.Dequeue (Queue => Buffer,
                              Item  => Item);
      end Take;
   end Speak_Buffer;

   The_Buffer   : Speak_Buffer (Max_Size => Buffer_Size);

   procedure Send_Char (Char : in Character) is

      Control_Register : CR_Array;

   begin
      loop
         delay 0.05;
         Control_Register := Byte_To_Array (Port_IO.In_Byte (TTS_Port));
         exit when Control_Register (4);
      end loop;
      Port_IO.Out_Byte (TTS_Port, Char_To_Byte (Char));
   end Send_Char;

   procedure Make_Sound (Phrase_To_Speak : in Speak_Rec) is

   begin

      Send_Char (Command);
      Send_Char (Character'Val (Voice_Range'Pos (Phrase_To_Speak.Voice)
                                                      + Character 'Pos ('0')));
      Send_Char ('O');

      for Char in 1 .. Phrase_Strings.Length (Phrase_To_Speak.Phrase) loop
         Send_Char (Phrase_Strings.Element (Phrase_To_Speak.Phrase, Char));
      end loop;

      Send_Char (Null_Char);

   end Make_Sound;

   -------------
   -- Speaker --
   -------------

   task Speaker;

   task body Speaker is
      Rec_to_Speak : Speak_Rec;
   begin
      loop
         -- grab the first item in The_Buffer
         The_Buffer.Take (Rec_to_Speak);
         Make_Sound (Rec_to_Speak);
      end loop;
   end Speaker;

   -----------
   -- Speak --
   -----------

   procedure Speak
     (Phrase : in Phrase_Strings.Bounded_String;
      Voice  : in Voice_Range)
   is
      -- create new Speak_Rec off of passed in parameters
      Temp_Rec : Speak_Rec;
   begin
      Temp_Rec.Phrase := Phrase;
      Temp_Rec.Voice := Voice;

      The_Buffer.Put (Temp_Rec);
   end Speak;

end DoubleTalk;
