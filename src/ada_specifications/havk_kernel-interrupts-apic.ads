-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-apic.ads                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package handles everything to do with the modern APIC architecture.
-- That includes the I/O APIC and the LAPIC (only plan x2APIC support for now).
PACKAGE HAVK_Kernel.Interrupts.APIC
IS
   -- Each LAPIC (not I/O APIC) belongs to a processor core. Initialised to
   -- zero so counting it is easier during enumeration without subtraction.
   CPU_Cores : number := 0;

   -- Enumerates the ACPI HPET table.
   PROCEDURE Enumerate_MADT
   WITH
      Post => CPU_Cores /= 0;
END HAVK_Kernel.Interrupts.APIC;
