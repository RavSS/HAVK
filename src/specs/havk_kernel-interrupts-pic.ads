-- This package controls the 8259 compatible PIC. I hope to replace it
-- with the newer APIC, but that requires ACPI tables to be parsed, which
-- is a big task. While primitive, this will do for now, assuming that
-- the UEFI system still emulates the ancient chip and is not buggy...
-- https://wiki.osdev.org/8259_PIC

PACKAGE HAVK_Kernel.Interrupts.PIC IS
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

   -- Sends an End Of Interrupt to both PICs regardless if the slave PIC was
   -- utilized. This is put into a NASM routine for higher performance,
   -- as I doubt GCC will inline a similiar native Ada procedure.
   PROCEDURE End_Of_Interrupt
   WITH
      Import        => true,
      Inline        => true,
      Convention    => NASM,
      External_Name => "pic_eoi";
END HAVK_Kernel.Interrupts.PIC;
