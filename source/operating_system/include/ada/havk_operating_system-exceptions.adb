-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-exceptions.adb                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Tasking;
USE
   HAVK_Operating_System.Call;

PACKAGE BODY HAVK_Operating_System.Exceptions
IS
   PROCEDURE Last_Chance_Handler
     (String_Address : IN address;
      Line           : IN natural)
   IS
      Message        : CONSTANT XMM_string
      WITH
         Import     => true,
         Convention => C,
         Address    => String_Address;

      Message_Length : natural RANGE Message'range := 1;

      Call_Arguments : arguments :=
        (Operation_Call => log_operation, OTHERS => <>);
      Call_String : XMM_string := (OTHERS => NUL);

      Imaged_Line : CONSTANT string := Line'image;
   BEGIN
      FOR
         ASCII OF Message
      LOOP
         EXIT WHEN ASCII = NUL;
         Message_Length := Message_Length + 1;
      END LOOP;

      Call_String(1 .. Message_Length) := Message(1 .. Message_Length);

      IF
         Line /= 0 AND THEN
         Message_Length + Imaged_Line'first IN Call_String'range AND THEN
         Message_Length + Imaged_Line'last IN Call_String'range
      THEN
         FOR
            Index IN Imaged_Line'range
         LOOP
            Call_String(Message_Length + Index) := Imaged_Line(Index);
         END LOOP;
         -- Replace the empty space produced by the image function in the RTS.
         Call_String(Message_Length + Imaged_Line'first) := ':';
      END IF;

      System_Call(Call_Arguments, Call_String); -- Send the log.

      pragma warnings(off);
      loop null; end loop;

      Call.Tasking.Exit_Task(1); -- Now exit the task in a failure state.
   END Last_Chance_Handler;

   PROCEDURE Stack_Check_Failure
   IS
   BEGIN
      RAISE Program_Error
      WITH
         "Stack smashing detected.";
   END Stack_Check_Failure;

END HAVK_Operating_System.Exceptions;