-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-exceptions.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Exceptions
WITH
   -- If anything in this package is called, then it's already game over.
   -- Utilising SPARK outside of here should mean nothing here matters, or in
   -- the case of a kernel panic, a program failure is intended on purpose.
   SPARK_Mode => off
IS
   PROCEDURE Last_Chance_Handler
     (Source_Location : IN address;
      Line            : IN integer)
   IS
      PROCEDURE Crash_Message
      WITH
         Inline    => true;

      PROCEDURE Crash_Message
      IS
         FUNCTION Length
           (Pointer   : IN address)
            RETURN integer
         WITH
            Import        => true,
            Inline        => true,
            Convention    => Assembler,
            External_Name => "assembly__string_length";

         C_String     : CONSTANT string(1 .. Length(Source_Location) + 1)
         WITH
            Import        => true,
            Convention    => C,
            Address       => Source_Location;
      BEGIN
         IF
            NOT Elaborated
         THEN
            Log("Elaboration failed.", fatal);
         END IF;

         IF
            Line /= 0
         THEN
            Log(C_String & ":" & integer'image(Line), fatal);
         ELSE
            Log(C_String, fatal);
         END IF;
      END Crash_Message;
   BEGIN
      Disable_Interrupts;

      PRAGMA Debug(Crash_Message);

      LOOP -- Infinite loop.
         Spinlock_Pause;
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
