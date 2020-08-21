-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts-isa_irqs.adb                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.PIT;

PACKAGE BODY HAVK_Kernel.Interrupts.ISA_IRQs
IS
   PRAGMA Warnings(off,
      "formal parameter ""Interrupt_Frame"" is not referenced",
      Reason => "The ISRs should take the parameter in regardless of usage.");

   PROCEDURE ISR_032_Handler -- Legacy timer.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      PIT.Interrupt_Manager;
   END ISR_032_Handler;

   PROCEDURE ISR_033_Handler -- Keyboard.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_033_Handler;

   PROCEDURE ISR_034_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_034_Handler;

   PROCEDURE ISR_035_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_035_Handler;

   PROCEDURE ISR_036_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_036_Handler;

   PROCEDURE ISR_037_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_037_Handler;

   PROCEDURE ISR_038_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_038_Handler;

   PROCEDURE ISR_039_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_039_Handler;

   -- Slave PIC is used for interrupts below this line (if not using the APIC).

   PROCEDURE ISR_040_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_040_Handler;

   PROCEDURE ISR_041_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_041_Handler;

   PROCEDURE ISR_042_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_042_Handler;

   PROCEDURE ISR_043_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_043_Handler;

   PROCEDURE ISR_044_Handler -- Mouse.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_044_Handler;

   PROCEDURE ISR_045_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_045_Handler;

   PROCEDURE ISR_046_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_046_Handler;

   PROCEDURE ISR_047_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      NULL;
   END ISR_047_Handler;
END HAVK_Kernel.Interrupts.ISA_IRQs;
