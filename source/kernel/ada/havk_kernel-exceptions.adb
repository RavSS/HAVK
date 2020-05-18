-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-exceptions.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.Exceptions
WITH
   -- If anything in this package is called, then it's already game over.
   -- Utilising SPARK outside of here should mean nothing here matters, or in
   -- the case of a kernel panic, a program failure is intended on purpose.
   SPARK_Mode => off
IS
   PROCEDURE Last_Chance_Handler
     (String_Pointer : IN address;
      Line           : IN natural)
   IS
      PROCEDURE Crash_Message
      WITH
         Inline => true;

      PROCEDURE Crash_Message
      IS
         FUNCTION Length
           (Pointer : IN address;
            Limit   : IN natural)
            RETURN natural
         WITH
            Global        => NULL,
            Import        => true,
            Inline        => true,
            Convention    => Assembler,
            External_Name => "assembly__string_length",
            Pre           => Pointer /= 0,
            Post          => Length'result <= Limit;

         -- The limit for the exception message is technically the stack size,
         -- but a string length maximum of 1000 characters is enough.
         C_Length : CONSTANT natural := Length(String_Pointer, 1000);

         C_String : CONSTANT string
           (1 .. (IF C_Length /= 0 THEN C_Length ELSE 1))
         WITH
            Import     => true,
            Convention => C,
            Address    => String_Pointer;
      BEGIN
         Tasking.Switch_To_Kernel_CR3;

         IF
            NOT Elaborated
         THEN
            Log("Elaboration failed.", Critical => true);
         END IF;

         IF
            Line /= 0
         THEN
            Log(C_String & ":" & integer'image(Line), Critical => true);
         ELSE
            Log(C_String, Critical => true);
         END IF;
      END Crash_Message;
   BEGIN
      Intrinsics.Disable_Interrupts;

      Crash_Message;

      LOOP -- Infinite loop.
         Intrinsics.Spinlock_Pause;
      END LOOP;
   END Last_Chance_Handler;

   PROCEDURE Stack_Smash_Handler
   IS
   BEGIN
      RAISE Panic
      WITH
         "The stack has been smashed.";
   END Stack_Smash_Handler;
END HAVK_Kernel.Exceptions;
