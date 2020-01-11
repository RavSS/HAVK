-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-irqs.adb                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Interrupts.PIC,
   HAVK_Kernel.PS2.Keyboard,
   HAVK_Kernel.PS2.Mouse,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.Interrupts.IRQs
IS
   PRAGMA Warnings(off, "formal parameter ""Stack_Frame"" is not referenced",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   PROCEDURE ISR_32_Handler -- Timer.
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Ticker := Ticker + 1;
      Tasking.Scheduler;
      PIC.Master_Reset;
   END ISR_32_Handler;

   PROCEDURE ISR_33_Handler -- Keyboard.
     (Stack_Frame : IN access_interrupt)
   IS
      USE
         HAVK_Kernel.PS2.Keyboard;
   BEGIN
      Interrupt_Manager;
      PIC.Master_Reset;
   END ISR_33_Handler;

   PROCEDURE ISR_34_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_34_Handler;

   PROCEDURE ISR_35_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_35_Handler;

   PROCEDURE ISR_36_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_36_Handler;

   PROCEDURE ISR_37_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_37_Handler;

   PROCEDURE ISR_38_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_38_Handler;

   PROCEDURE ISR_39_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Master_Reset;
   END ISR_39_Handler;

   -- Slave PIC is used for interrupts below this line.

   PROCEDURE ISR_40_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_40_Handler;

   PROCEDURE ISR_41_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_41_Handler;

   PROCEDURE ISR_42_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_42_Handler;

   PROCEDURE ISR_43_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_43_Handler;

   PROCEDURE ISR_44_Handler -- Mouse.
     (Stack_Frame : IN access_interrupt)
   IS
      USE
         HAVK_Kernel.PS2.Mouse;
   BEGIN
      Interrupt_Manager;
      PIC.Dual_Reset;
   END ISR_44_Handler;

   PROCEDURE ISR_45_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_45_Handler;

   PROCEDURE ISR_46_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_46_Handler;

   PROCEDURE ISR_47_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC.Dual_Reset;
   END ISR_47_Handler;
END HAVK_Kernel.Interrupts.IRQs;
