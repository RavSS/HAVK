-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System PS/2 Driver                      --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_PS2,
   HAVK_PS2.Keyboard,
   HAVK_PS2.User_Input;
USE
   HAVK_Operating_System,
   HAVK_PS2,
   HAVK_PS2.Keyboard;

PROCEDURE Main
WITH
   No_Return => true
IS
   Main_Tag          : CONSTANT string := "MAIN";
   Condition         : controller_condition;

   Error_Check       : error;
   Old_Keyboard_IRQs : general_register := 0;
   Keyboard_IRQs     : arguments :=
     (Operation_Call => irq_statistics_operation, Argument_1 => IRQ_Base + 1,
      OTHERS => 0);

   Terminal_Name     : CONSTANT string := "Terminal";
   Terminal_Status   : task_status;
   Terminal_Timeout  : number := 10;

   -- TODO: All PS/2 logic needs to be reworked or I need to support user-space
   -- interrupt handlers. I've copied over my kernel-level PS/2 driver, but now
   -- that interrupts are much harder to handle, it needs to be modified to
   -- better deal with polling. Note that polling does not work so well if both
   -- a PS/2 keyboard and mouse are being used at the same time.
   Initial_Messages  : CONSTANT ARRAY(1 .. 2) OF string(1 .. 41) :=
      ("TODO: The input handling is not accurate.",
       "You can now begin typing to test input...");

   -- Check the port in the terminal's "main.adb".
   Text_Data         : arguments :=
     (send_message_operation, Argument_3 => 404, OTHERS => 0);
   Text_Data_String  : XMM_string := (OTHERS => NUL);
BEGIN
   Log("Attempting to initialise PS/2 controller.", Tag => Main_Tag);
   Setup;
   Condition := Check_Condition;

   IF
      Condition /= functional
   THEN
      RAISE Program_Error
      WITH
         Source_Location & " - Non-working PS/2 controller detected.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "HAVK needs a PS/2 controller for any and all input as of now.");
   ELSE
      Log("PS/2 controller is initialised.", Tag => Main_Tag);
   END IF;

   LOOP
      Task_Finder(Terminal_Name, Terminal_Status);
      EXIT WHEN Terminal_Status.Index /= 0 AND THEN Terminal_Status.Alive;
   END LOOP;

   Text_Data.Argument_1 := general_register(Terminal_Status.Index);
   Log("Found terminal task. Identity index is 0x" &
      Image(Terminal_Status.Index, Base => 16) & '.', Tag => Main_Tag);

   FOR
      Initial_Message OF Initial_Messages
   LOOP
      Text_Data.Argument_2 := Initial_Message'length + 1;

      FOR
         ASCII_Index IN Initial_Message'range
      LOOP
         Text_Data_String(ASCII_Index) := Initial_Message(ASCII_Index);
      END LOOP;

      Text_Data_String(Initial_Message'last + 1) := LF;

      System_Call(Text_Data, Text_Data_String);
      Text_Data_String := (OTHERS => NUL);
   END LOOP;

   -- I'll be sending a message per key press. Right now, I am just sending the
   -- ASCII data.
   Text_Data.Argument_2 := 1; -- Character size (byte).

   LOOP -- A very crude yet simple loop to showcase user-space PS/2 input.
      Wait_For_IRQ : LOOP
         System_Call(Keyboard_IRQs);
         EXIT Wait_For_IRQ WHEN Keyboard_IRQs.Argument_2 /= Old_Keyboard_IRQs;
      END LOOP Wait_For_IRQ;

      Old_Keyboard_IRQs := Keyboard_IRQs.Argument_2;
      Interrupt_Manager;

      IF
         Break_State
      THEN
         Text_Data_String(1) := User_Input.Get_Key;
         Error_Check := System_Call(Text_Data, Text_Data_String);

         IF
            Error_Check /= no_error
         THEN
            Log("Failed to send text data to terminal.", Tag => Main_Tag,
               Warn => true);
            Terminal_Timeout := Terminal_Timeout - 1;

            IF
               Terminal_Timeout = 0
            THEN
               Log("Terminal server has timed out. " &
                  "Now seeking new terminal server.", Tag => Main_Tag,
                  Warn => true);
            END IF;
         END IF;
      END IF;

      WHILE
         Terminal_Timeout = 0
      LOOP
         Task_Finder(Terminal_Name, Terminal_Status);
         IF
            Terminal_Status.Index /= 0 AND THEN
            Terminal_Status.Alive
         THEN
            Terminal_Timeout := 10;
         END IF;
      END LOOP;
   END LOOP;

END Main;
