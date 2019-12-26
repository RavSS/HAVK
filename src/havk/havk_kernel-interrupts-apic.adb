-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-apic.adb                        --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   System,
   HAVK_Kernel.ACPI;
USE
   System,
   HAVK_Kernel.ACPI;

PACKAGE BODY HAVK_Kernel.Interrupts.APIC
IS
   PROCEDURE Enumerate_MADT
   IS
      MADT_Address : CONSTANT System.Address :=
         (IF ACPI.Valid_Implementation THEN ACPI.Table_Address("APIC") ELSE 0);
      APICs        : interrupt_controller_descriptors;
   BEGIN
      IF MADT_Address /= 0 THEN
         APICs := ACPI.Get_APICs(MADT_Address);
      ELSE -- Occurs if the ACPI implementation is damaged.
         CPU_Cores := 1; -- The PICs still exist.
         RETURN;
      END IF;

      FOR APIC_Entry OF APICs LOOP
         EXIT WHEN APIC_Entry.Record_Address = System'To_Address(0);

         -- TODO: Replace the PICs with APICs if possible. Call a new procedure
         -- or something from here to do that.
         CASE APIC_Entry.Enumeration_Value IS
            WHEN local_APIC | local_x2APIC =>
               CPU_Cores := CPU_Cores + 1;
            WHEN OTHERS =>
               NULL;
         END CASE;
      END LOOP;

      IF CPU_Cores = 0 THEN -- Occurs if the MADT is completely empty.
         CPU_Cores := 1;
      END IF;
   END Enumerate_MADT;
END HAVK_Kernel.Interrupts.APIC;
