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
IS
   PRAGMA Warnings(GNATprove, off,
      "subprogram ""ISR_*_Handler"" has no effect",
      Reason => "Not all of them currently have a purpose.");
   PRAGMA Warnings(GNATprove, off,
      "unused variable ""Interrupt_Frame""",
      Reason => "The ISRs should take the parameter in regardless of usage.");
   PRAGMA Warnings(off,
      "formal parameter ""Interrupt_Frame"" is not referenced",
      Reason => "The ISRs should take the parameter in regardless of usage.");

   -- IRQ 0 - PIT.
   PROCEDURE ISR_032_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_032";
   PROCEDURE ISR_032_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_032";

   -- IRQ 1 - PS/2 keyboard controller.
   PROCEDURE ISR_033_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_033";
   PROCEDURE ISR_033_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_033";

   -- IRQ 2 (behaves like IRQ 9) - Cascades interrupts down to slave PIC.
   PROCEDURE ISR_034_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_034";
   PROCEDURE ISR_034_Handler -- Not actually used when using the APICs.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_034";

   -- IRQ 3 - RS232 serial port COM2 and/or COM4.
   PROCEDURE ISR_035_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_035";
   PROCEDURE ISR_035_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_035";

   -- IRQ 4 - RS232 serial port COM1 and/or COM3.
   PROCEDURE ISR_036_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_036";
   PROCEDURE ISR_036_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_036";

   -- IRQ 5 - Parallel port LPT2 and/or LPT3, or a sound card.
   PROCEDURE ISR_037_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_037";
   PROCEDURE ISR_037_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_037";

   -- IRQ 6 - Floppy disk controller.
   PROCEDURE ISR_038_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_038";
   PROCEDURE ISR_038_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_038";

   -- IRQ 7 - Parallel port LPT1 or a sound card with careful configuration.
   PROCEDURE ISR_039_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_039";
   PROCEDURE ISR_039_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_039";

   -- IRQ 8 - Real-time clock (RTC).
   PROCEDURE ISR_040_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_040";
   PROCEDURE ISR_040_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_040";

   -- IRQ 9 - ACPI control interrupt handler. IRQ 2 reroutes here.
   PROCEDURE ISR_041_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_041";
   PROCEDURE ISR_041_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_041";

   -- IRQ 10 - Free IRQ.
   PROCEDURE ISR_042_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_042";
   PROCEDURE ISR_042_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_042";

   -- IRQ 11 - Spare IRQ.
   PROCEDURE ISR_043_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_043";
   PROCEDURE ISR_043_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_043";

   -- IRQ 12 - PS/2 mouse.
   PROCEDURE ISR_044_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_044";
   PROCEDURE ISR_044_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_044";

   -- IRQ 13 - Inter-processor interrupt.
   PROCEDURE ISR_045_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_045";
   PROCEDURE ISR_045_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_045";

   -- IRQ 14 - Primary ATA channel.
   PROCEDURE ISR_046_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_046";
   PROCEDURE ISR_046_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_046";

   -- IRQ 15 - Secondary ATA channel.
   PROCEDURE ISR_047_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_047";
   PROCEDURE ISR_047_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_047";

END HAVK_Kernel.Interrupts.ISA_IRQs;
