-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.APIC,
   HAVK_Kernel.APIC.Timer,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.Interrupts
IS
   PRAGMA Warnings(off,
      "formal parameter ""Interrupt_Frame"" is not referenced",
      Reason => "The ISRs should take the parameter in regardless of usage.");

   PROCEDURE ISR_048_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      IF -- Overflow check in case "number" turns into a signed type later.
         APIC.Timer.Ticks /= number'last
      THEN
         APIC.Timer.Ticks := APIC.Timer.Ticks + 1;
      ELSE
         APIC.Timer.Ticks := 0;
      END IF;

      Tasking.Schedule;
   END ISR_048_Handler;

END HAVK_Kernel.Interrupts;
