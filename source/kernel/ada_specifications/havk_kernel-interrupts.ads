-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package and its sub-packages contain the interrupt handler stubs.
PACKAGE HAVK_Kernel.Interrupts
WITH
   Preelaborate => true
IS
   -- This is the data structure put on the handler's stack after an interrupt,
   -- in which `REX.W IRET` uses to return to the interrupted location. The
   -- error code (if relevant) will be passed as a separate parameter.
   -- Essentially, the values change from doublewords (4 bytes) to quadwords (8
   -- bytes) for x86-64. The interrupt frame is placed on a 16-byte alignment
   -- by the CPU on the stack.
   TYPE interrupted_state IS RECORD
      RIP    : address;
      CS     : number;
      RFLAGS : number;
      RSP    : address;
      SS     : number;
   END RECORD
   WITH
      Object_Size => (32 * 8) + 63 + 1;
   FOR interrupted_state USE RECORD
      RIP    AT 00 RANGE 0 .. 63;
      CS     AT 08 RANGE 0 .. 63;
      RFLAGS AT 16 RANGE 0 .. 63;
      RSP    AT 24 RANGE 0 .. 63;
      SS     AT 32 RANGE 0 .. 63;
   END RECORD;

   PRAGMA Warnings(off, "unused variable ""Interrupt_Frame""",
      Reason => "The ISRs should take the parameter in regardless of usage.");

   -- The spurious interrupt handler. All interrupt vectors should use this as
   -- their entry point if nothing else is assigned to them.
   PROCEDURE ISR_Default
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_spurious";

   -- Local APIC timer.
   PROCEDURE ISR_048_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_048";
   PROCEDURE ISR_048_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_048";

   -- Raising this causes a context switch and shifts the task. This does not
   -- use an assembly stub; instead, the routine specially deals with the
   -- interrupt.
   PROCEDURE ISR_049
   WITH
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__switch_task",
      Linker_Section => ".isolated_text";

END HAVK_Kernel.Interrupts;
