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
   -- Hopefully I have defined the 64-bit interrupt stack frame properly.
   -- Check Intel's ASD Manual (6-12 Vol. 1) for a description
   -- of the IA-32 interrupt stack frame or the 32-bit version of HAVK.
   -- So far, there does not seem to be any changes to its definition.
   -- Also check Figure 8-13. in AMD's AMD64 manual. The error code
   -- (if relevant) will be passed as a separate parameter. Essentially,
   -- the values change from double-words (4 bytes) to quad-words (8 bytes).
   TYPE interrupted_state IS RECORD
      RIP    : address;
      CS     : number;
      RFLAGS : number;
      RSP    : address;
      SS     : number;
   END RECORD
   WITH
      Alignment => 128; -- The stack frame is on a 16-byte alignment.
   FOR interrupted_state USE RECORD
      RIP    AT 00 RANGE 0 .. 63;
      CS     AT 08 RANGE 0 .. 63;
      RFLAGS AT 16 RANGE 0 .. 63;
      RSP    AT 24 RANGE 0 .. 63;
      SS     AT 32 RANGE 0 .. 63;
   END RECORD;

   -- The processor passes a pointer to the interrupt stack frame and GCC
   -- demands that we account for it regardless of interaction. GCC also does
   -- not allow for this to be replaced with an anonymous access parameter.
   TYPE access_interrupted_state IS NOT NULL ACCESS interrupted_state;

   PRAGMA Warnings(GNATprove, off, "pragma ""Machine_Attribute"" ignored",
      Reason => "The pragma must be used to create ISRs.");

   PRAGMA Warnings(GNATprove, off, "unused variable ""Stack_Frame""",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   -- Used for handling all spurious interrupt vectors.
   -- TODO: Need a confirmation that this should or shouldn't signal EOI to
   -- avoid confusion, as I can't find any information on whether it should.
   PRAGMA Warnings(GNATprove, off,
      "subprogram ""Spurious_Interrupt_Handler"" has no effect",
      Reason => "The handler is empty on purpose and has a hidden effect.");
   PROCEDURE Spurious_Interrupt_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(Spurious_Interrupt_Handler, "interrupt");

   -- Local APIC timer.
   PROCEDURE ISR_048_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_048_Handler, "interrupt");

   -- Raising this causes a context switch and shifts the task.
   PROCEDURE ISR_100_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__switch_task",
      Linker_Section => ".isolated_text";

END HAVK_Kernel.Interrupts;
