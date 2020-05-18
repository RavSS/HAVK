-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts-isa_irqs.ads                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package handles the legacy system IRQs. It presumes that the (LA)PIC
-- interrupt vector has been remapped to the appropriate values.
-- READ: https://en.wikipedia.org/wiki/Interrupt_request_(PC_architecture)
PACKAGE HAVK_Kernel.Interrupts.ISA_IRQs
WITH
   Preelaborate => true
IS
   PRAGMA Warnings(GNATprove, off, "pragma ""Machine_Attribute"" ignored",
      Reason => "The pragma must be used to create IRQ ISRs.");

   PRAGMA Warnings(GNATprove, off, "unused variable ""Stack_Frame""",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   -- IRQ 0 - PIT.
   PROCEDURE ISR_032_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_032_Handler, "interrupt");

   -- IRQ 1 - PS/2 keyboard controller.
   PROCEDURE ISR_033_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_033_Handler, "interrupt");

   -- IRQ 2 (behaves like IRQ 9) - Cascades interrupts down to slave PIC.
   PROCEDURE ISR_034_Handler -- Not actually used when using the APICs.
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_034_Handler, "interrupt");

   -- IRQ 3 - RS232 serial port COM2 and/or COM4.
   PROCEDURE ISR_035_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_035_Handler, "interrupt");

   -- IRQ 4 - RS232 serial port COM1 and/or COM3.
   PROCEDURE ISR_036_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_036_Handler, "interrupt");

   -- IRQ 5 - Parallel port LPT2 and/or LPT3, or a sound card.
   PROCEDURE ISR_037_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_037_Handler, "interrupt");

   -- IRQ 6 - Floppy disk controller.
   PROCEDURE ISR_038_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_038_Handler, "interrupt");

   -- IRQ 7 - Parallel port LPT1 or a sound card with careful configuration.
   PROCEDURE ISR_039_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_039_Handler, "interrupt");

   -- IRQ 8 - Real-time clock (RTC).
   PROCEDURE ISR_040_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_040_Handler, "interrupt");

   -- IRQ 9 - ACPI control interrupt handler. IRQ 2 reroutes here.
   PROCEDURE ISR_041_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_041_Handler, "interrupt");

   -- IRQ 10 - Free IRQ.
   PROCEDURE ISR_042_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_042_Handler, "interrupt");

   -- IRQ 11 - Spare IRQ.
   PROCEDURE ISR_043_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_043_Handler, "interrupt");

   -- IRQ 12 - PS/2 mouse.
   PROCEDURE ISR_044_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_044_Handler, "interrupt");

   -- IRQ 13 - Inter-processor interrupt.
   PROCEDURE ISR_045_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_045_Handler, "interrupt");

   -- IRQ 14 - Primary ATA channel.
   PROCEDURE ISR_046_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_046_Handler, "interrupt");

   -- IRQ 15 - Secondary ATA channel.
   PROCEDURE ISR_047_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_047_Handler, "interrupt");
END HAVK_Kernel.Interrupts.ISA_IRQs;
