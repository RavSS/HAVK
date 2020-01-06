-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-pic.ads                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package controls the 8259 compatible PIC. I hope to replace it
-- with the newer APIC, but that requires ACPI tables to be parsed, which
-- is a big task. While primitive, this will do for now, assuming that
-- the UEFI system still emulates the ancient chip and is not buggy...
-- https://wiki.osdev.org/8259_PIC

PACKAGE HAVK_Kernel.Interrupts.PIC
IS
   Master_Command   : CONSTANT number := 16#20#;
   Master_Data      : CONSTANT number := 16#21#;
   Slave_Command    : CONSTANT number := 16#A0#;
   Slave_Data       : CONSTANT number := 16#A1#;
   Interrupt_End    : CONSTANT number := 16#20#;
   Request_Register : CONSTANT number := 16#0A#;
   Service_Register : CONSTANT number := 16#0B#;

   -- Remaps the interrupt vector so the IRQs do not overlap with the CPU
   -- exceptions, but instead come directly after them.
   PROCEDURE Remap;

   -- Resets the master PIC only.
   PROCEDURE Master_Reset
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__pic_master_end_interrupt";

   -- Resets both the master PIC and the slave PIC.
   PROCEDURE Dual_Reset
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__pic_dual_end_interrupt";
END HAVK_Kernel.Interrupts.PIC;
