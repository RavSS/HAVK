-- This package controls the 8259 compatible PIC. I hope to replace it
-- with the newer APIC, but that requires ACPI tables to be parsed, which
-- is a big task. While primitive, this will do for now, assuming that
-- the UEFI system still emulates the ancient chip and is not buggy...
-- https://wiki.osdev.org/8259_PIC

PACKAGE HAVK_Kernel.Interrupts.PIC IS
   PIC_Master_Command   : CONSTANT num := 16#20#;
   PIC_Master_Data      : CONSTANT num := 16#21#;
   PIC_Slave_Command    : CONSTANT num := 16#A0#;
   PIC_Slave_Data       : CONSTANT num := 16#A1#;
   PIC_Interrupt_End    : CONSTANT num := PIC_Master_Command;
   PIC_Request_Register : CONSTANT num := 16#0A#;
   PIC_Service_Register : CONSTANT num := 16#0B#;

   PROCEDURE PIC_Remap;

   -- Sends an End Of Interrupt to both PICs regardless if the slave PIC was
   -- utilized. This is put into a NASM routine for higher performance,
   -- as I doubt GCC will inline a similiar native Ada procedure.
   PROCEDURE PIC_EOI
   WITH
      Import        => True,
      Convention    => C,
      External_Name => "pic_eoi";
END HAVK_Kernel.Interrupts.PIC;
