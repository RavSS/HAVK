-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

-- This package and its sub-packages contain the interrupt handler stubs.
PACKAGE HAVK_Kernel.Interrupts
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

   -- This type will be used by the interrupt handler stubs to increment the
   -- respective counters. Note that the value will eventually wrap around, but
   -- that is acceptable. The stub expects packed 8-byte elements.
   TYPE interrupt_counters IS ARRAY(number RANGE 0 .. 255) OF ALIASED quadword
   WITH
      Default_Component_Value => 00,
      Component_Size          => 64;

   Counters : ALIASED interrupt_counters := (OTHERS => 0)
   WITH
      Export         => true,
      Convention     => Assembler,
      External_Name  => "ada__interrupt_counters",
      Linker_Section => ".isolated_bss";

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

END HAVK_Kernel.Interrupts;
