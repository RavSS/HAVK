-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-isa_irqs.adb                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.APIC,
   HAVK_Kernel.PS2.Keyboard,
   HAVK_Kernel.PS2.Mouse,
   HAVK_Kernel.PIT;

PACKAGE BODY HAVK_Kernel.Interrupts.ISA_IRQs
IS
   PRAGMA Warnings(off, "formal parameter ""Stack_Frame"" is not referenced",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   PROCEDURE ISR_032_Handler -- Legacy timer.
     (Stack_Frame : IN access_interrupted_state)
   IS
      USE
         HAVK_Kernel.PIT;
   BEGIN
      Interrupt_Manager;
      APIC.Reset;
   END ISR_032_Handler;

   PROCEDURE ISR_033_Handler -- Keyboard.
     (Stack_Frame : IN access_interrupted_state)
   IS
      USE
         HAVK_Kernel.PS2.Keyboard;
   BEGIN
      Interrupt_Manager;
      APIC.Reset;
   END ISR_033_Handler;

   PROCEDURE ISR_034_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_034_Handler;

   PROCEDURE ISR_035_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_035_Handler;

   PROCEDURE ISR_036_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_036_Handler;

   PROCEDURE ISR_037_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_037_Handler;

   PROCEDURE ISR_038_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_038_Handler;

   PROCEDURE ISR_039_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_039_Handler;

   -- Slave PIC is used for interrupts below this line (if not using the APIC).

   PROCEDURE ISR_040_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_040_Handler;

   PROCEDURE ISR_041_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_041_Handler;

   PROCEDURE ISR_042_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_042_Handler;

   PROCEDURE ISR_043_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_043_Handler;

   PROCEDURE ISR_044_Handler -- Mouse.
     (Stack_Frame : IN access_interrupted_state)
   IS
      USE
         HAVK_Kernel.PS2.Mouse;
   BEGIN
      Interrupt_Manager;
      APIC.Reset;
   END ISR_044_Handler;

   PROCEDURE ISR_045_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_045_Handler;

   PROCEDURE ISR_046_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_046_Handler;

   PROCEDURE ISR_047_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      APIC.Reset;
   END ISR_047_Handler;
END HAVK_Kernel.Interrupts.ISA_IRQs;
