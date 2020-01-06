-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-apic.adb                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.ACPI;
USE
   HAVK_Kernel.ACPI;

PACKAGE BODY HAVK_Kernel.Interrupts.APIC
IS
   PROCEDURE Enumerate_MADT
   IS
      MADT_Address : CONSTANT address :=
         (IF ACPI.Valid_Implementation THEN ACPI.Table_Address("APIC") ELSE 0);
      APICs        : interrupt_controller_descriptors;
   BEGIN
      IF
         MADT_Address /= 0
      THEN
         APICs := ACPI.Get_APICs(MADT_Address);
      ELSE -- Occurs if the ACPI implementation is damaged.
         CPU_Cores := 1; -- The PICs still exist.
         RETURN;
      END IF;

      FOR
         APIC_Entry OF APICs
      LOOP
         EXIT WHEN APIC_Entry.Record_Address = 0;

         -- TODO: Replace the PICs with APICs if possible. Call a new procedure
         -- or something from here to do that.
         IF
            APIC_Entry.Enumeration_Value = local_APIC OR ELSE
            APIC_Entry.Enumeration_Value = local_x2APIC
         THEN
            CPU_Cores := CPU_Cores + 1;
         END IF;
      END LOOP;

      IF -- Occurs if the MADT is completely empty.
         CPU_Cores  = 0
      THEN
         CPU_Cores := 1;
      END IF;
   END Enumerate_MADT;
END HAVK_Kernel.Interrupts.APIC;
