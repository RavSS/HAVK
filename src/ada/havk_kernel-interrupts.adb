-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.APIC,
   HAVK_Kernel.APIC.Timer,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.Interrupts
IS
   PRAGMA Warnings(off, "formal parameter ""Stack_Frame"" is not referenced",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   PROCEDURE Spurious_Interrupt_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      -- I don't believe we have to signal EOI for spurious interrupts, at
      -- least for when we're using the APIC. Only need to `REX.W IRET`.
      NULL;
   END Spurious_Interrupt_Handler;

   PROCEDURE ISR_048_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Timer.Ticks := APIC.Timer.Ticks + 1;
      Tasking.Schedule;
      APIC.Reset;
   END ISR_048_Handler;
END HAVK_Kernel.Interrupts;
