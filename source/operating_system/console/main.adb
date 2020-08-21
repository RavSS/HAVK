-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Console                          --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_Console,
   HAVK_Console.PS2,
   HAVK_Console.PS2.Keyboard,
   HAVK_Console.User_Input;
USE
   HAVK_Operating_System,
   HAVK_Console,
   HAVK_Console.PS2,
   HAVK_Console.PS2.Keyboard,
   HAVK_Console.User_Input;

PROCEDURE Main
WITH
   No_Return => true
IS
   Main_Tag          : CONSTANT string := "MAIN";
   Condition         : PS2.controller_condition;

   Output_Buffer     : string(1 .. 32);
   Character_Buffer  : character;
   Index             : positive RANGE Output_Buffer'range;
   Old_Keyboard_IRQs : general_register := 0;
   Keyboard_IRQs     : arguments :=
     (Operation_Call => irq_statistics_operation, Argument_1 => IRQ_Base + 1,
      OTHERS => 0);
BEGIN
   Log("Attempting to initialise PS/2 controller.", Tag => Main_Tag);
   PS2.Setup;
   Condition := PS2.Check_Condition;

   IF
      Condition /= PS2.functional
   THEN
      RAISE Program_Error
      WITH
         Source_Location & " - Non-working PS/2 controller detected.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "HAVK needs a PS/2 controller for any and all input as of now.");
   ELSE
      Log("PS/2 controller is initialised.", Tag => Main_Tag);
   END IF;

   -- TODO: This entire package needs to be reworked. I've copied over my
   -- kernel-level PS/2 driver, but now that interrupts are much harder to
   -- handle, it needs to be modified to better deal with polling. Note that
   -- polling does not work so well if both a PS/2 keyboard and mouse are being
   -- used at the same time.
   Log("TODO: The input handling is not accurate. Input is outputted as logs.",
      Tag => Main_Tag, Warn => true);

   LOOP
      Output_Buffer := (OTHERS => NUL);
      Index := Output_Buffer'first;

      LOOP -- A very crude yet simple loop to showcase user space PS/2 input.
         Wait_For_IRQ : LOOP
            System_Call(Keyboard_IRQs);
            EXIT Wait_For_IRQ WHEN
               Keyboard_IRQs.Argument_2 /= Old_Keyboard_IRQs;
         END LOOP Wait_For_IRQ;

         Old_Keyboard_IRQs := Keyboard_IRQs.Argument_2;
         PS2.Keyboard.Interrupt_Manager;

         IF
            PS2.Keyboard.Break_State
         THEN
            Character_Buffer := User_Input.Get_Key;
            EXIT WHEN Character_Buffer = LF;

            Output_Buffer(Index) := Character_Buffer;
            EXIT WHEN Index >= Output_Buffer'last;

            Index := Index + 1;
         END IF;
      END LOOP;

      Log(Output_Buffer, Tag => Main_Tag);
   END LOOP;

END Main;
