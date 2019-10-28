-- This package controls the 8259 compatible PIC. I hope to replace it
-- with the newer APIC, but that requires ACPI tables to be parsed, which
-- is a big task. While primitive, this will do for now, assuming that
-- the UEFI system still emulates the ancient chip and is not buggy...
-- https://wiki.osdev.org/8259_PIC

PACKAGE HAVK_Kernel.Interrupts.PIC
IS
   Master_Command   : CONSTANT num := 16#20#;
   Master_Data      : CONSTANT num := 16#21#;
   Slave_Command    : CONSTANT num := 16#A0#;
   Slave_Data       : CONSTANT num := 16#A1#;
   Interrupt_End    : CONSTANT num := Master_Command;
   Request_Register : CONSTANT num := 16#0A#;
   Service_Register : CONSTANT num := 16#0B#;

   -- Remaps the interrupt vector so the IRQs do not overlap with the CPU
   -- exceptions, but instead come directly after them.
   PROCEDURE Remap;

   -- Resets the master PIC only.
   PROCEDURE Master_Reset
   WITH
      Inline_Always => true;

   -- TODO: Very odd GCC glitch where if the optimisation is set to O2, then
   -- inlining a function that has two `OUTB` instructions causes an error
   -- where the ISR is trying to call itself (or at least GCC says that).
   -- It does not occur with the above procedure that only resets the master
   -- PIC and only that... somehow. Also does not occur on O3, O1, O0, etc.
   ----------------------------------------------------------------------------
   -- Resets both the master PIC and the slave PIC.
   PROCEDURE Dual_Reset
   WITH
      Inline        => false;
END HAVK_Kernel.Interrupts.PIC;
