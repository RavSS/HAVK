-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-acpi.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE BODY HAVK_Kernel.ACPI
WITH
   Refined_State => (ACPI_State => NULL)
IS
   FUNCTION Get_RSDP
      RETURN root_system_description_pointer
   IS
      USE TYPE
         UEFI.access_arguments; -- So `gnatprove` knows it cannot be null.
      PRAGMA Assume(UEFI.Bootloader_Arguments /= NULL, "Bug workaround.");

      RSDP       : ALIASED CONSTANT root_system_description_pointer
      WITH
         Import   => true,
         Address  => UEFI.Bootloader_Arguments.RSDP_Address;

      RSDP_Bytes : ALIASED CONSTANT bytes(1 .. RSDP.Length)
      WITH
         Import   => true,
         Address  => UEFI.Bootloader_Arguments.RSDP_Address,
         Annotate => (GNATprove, False_Positive,
                      "object with constraints on bit representation *",
                      "The RSDP must be valid, even if other tables aren't.");

      RSDP_Standard_Length : CONSTANT number :=
         root_system_description_pointer'size / 8
      WITH
         Annotate => (GNATprove, False_Positive, "range check might fail",
                      "It's just the expected byte size of the RSDP record.");
      Total_Size           : number := 0;
   BEGIN
      IF
         RSDP_Bytes'last /= RSDP_Standard_Length
      THEN
         RAISE Panic
         WITH
            Source_Location & " - The system's ACPI RSDP length is too large.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "The record type may be outdated if this happens.");
      END IF;

      FOR
         RSDP_Byte OF RSDP_Bytes
      LOOP
         EXIT WHEN Total_Size > number'last - (2**8 - 1); -- Overflow check.
         Total_Size := Total_Size + RSDP_Byte;
      END LOOP;

      IF
        (Total_Size AND 16#FF#) /= 0
      THEN
         RAISE Panic
         WITH
            Source_Location & " - The system's ACPI RSDP is corrupt.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "I'll panic if it's corrupt, as it's a sign of a bigger issue.");
      END IF;

      RETURN RSDP;
   END Get_RSDP;

   FUNCTION Get_XSDT
      RETURN access_extended_system_description_table
   IS
      RSDP       : CONSTANT root_system_description_pointer := Get_RSDP;
      XSDT       : ALIASED CONSTANT access_extended_system_description_table
      WITH
         Import  => true,
         Address => RSDP.XSDT_Address'address; -- Pointer to a pointer.

      Total_Size : number := 0;
   BEGIN
      IF -- The XSDT address shouldn't be zero, but check it anyway.
         XSDT /= NULL
      THEN
         DECLARE
            XSDT_Bytes : ALIASED CONSTANT bytes(1 .. XSDT.SDT.Length)
            WITH
               Import   => true,
               Address  => RSDP.XSDT_Address,
               Annotate => (GNATprove, False_Positive,
                           "object with constraints on bit representation *",
                           "The check itself doesn't require a valid table.");
         BEGIN
            FOR
               XSDT_Byte OF XSDT_Bytes
            LOOP -- Overflow check below.
               EXIT WHEN Total_Size > number'last - (2**8 - 1);
               Total_Size := Total_Size + XSDT_Byte;
            END LOOP;
         END;
      ELSE
         Total_Size := 16#FF#; -- Make it fail the checksum verification.
      END IF;

      IF
        (Total_Size AND 16#FF#) /= 0
      THEN
         RAISE Panic
         WITH
            Source_Location & " - The system's ACPI XSDT is corrupt.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "I'll panic if it's corrupt, as it's a sign of a bigger issue.");
      END IF;

      RETURN XSDT;
   END Get_XSDT;

   FUNCTION Get_XSDT_Tables
      RETURN system_description_tables
   WITH
      SPARK_Mode => off -- Annotations can't cover all the unchecked errors.
   IS
      FUNCTION To_Pointer
        (Table_Pointer : IN address)
         RETURN access_system_description_table
      WITH
         Import     => true,
         Convention => Intrinsic;

      XSDT            : CONSTANT access_extended_system_description_table :=
         Get_XSDT;

      -- I've hardcoded in the byte size of the SDT or else `gnatprove`
      -- complains.
      Tables_Offset   : CONSTANT := 36;

      -- The XSDT must be valid before this is ever called. Each QWORD here is
      -- a pointer to a table.
      Table_Addresses : addresses(1 .. (XSDT.SDT.Length - Tables_Offset) / 8)
      WITH
         Import   => true,
         Address  => XSDT.Table_Pointers'address;

      -- This is what we must return. It's of a fixed size that is hopefully
      -- enough to contain the present tables.
      Tables          : system_description_tables := (OTHERS => NULL);
   BEGIN
      FOR
         Address_Index IN Table_Addresses'range
      LOOP
         EXIT WHEN Address_Index NOT IN Tables'range;
         Tables(Address_Index) := To_Pointer(Table_Addresses(Address_Index));
      END LOOP;

      RETURN Tables;
   END Get_XSDT_Tables;

   FUNCTION Table_Address
     (Signature : IN string)
      RETURN address
   IS
      FUNCTION To_Address
        (Table  : ALIASED NOT NULL ACCESS CONSTANT system_description_table)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      Tables : CONSTANT system_description_tables := Get_XSDT_Tables
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "Nothing is allocated.");
   BEGIN
      FOR
         Table OF Tables
      LOOP
         IF
            Table /= NULL AND THEN
            Table.Signature = Signature
         THEN
            DECLARE
               Table_Bytes : ALIASED CONSTANT bytes(1 .. Table.Length)
               WITH
                  Import  => true,
                  Address => To_Address(Table),
                  Annotate => (GNATprove, False_Positive,
                               "object with constraints on bit *",
                               "We manually check it for validity.");

               Total_Size : number := 0;
            BEGIN
               FOR
                  Table_Byte OF Table_Bytes
               LOOP -- Overflow check below.
                  EXIT WHEN Total_Size > number'last - (2**8 - 1);
                  Total_Size := Total_Size + Table_Byte;
               END LOOP;

               IF
                 (Total_Size AND 16#FF#) = 0
               THEN
                  RETURN To_Address(Table);
               ELSE
                  RETURN 0; -- Table is corrupt. I won't panic here.
               END IF;
            END;
         END IF;
      END LOOP;

      RETURN 0; -- Table does not exist.
   END Table_Address;

   FUNCTION Get_APICs
     (MADT_Address : IN address)
      RETURN interrupt_controller_descriptors
   WITH
      -- Address attributes are needed for this to work, as I store the APIC
      -- structure's raw address in another record which is then extended via
      -- an import on that very address.
      SPARK_Mode => off
   IS
      FUNCTION Enum_Val IS NEW Ada.Unchecked_Conversion
        (source => number, target => interrupt_controller);

      MADT         : CONSTANT multiple_APIC_description_table
      WITH
         Import     => true,
         Convention => C,
         Address    => MADT_Address;

      APICs_Bytes  : CONSTANT bytes(1 .. MADT.SDT.Length - 44) -- See below.
      WITH
         Import     => true,
         Convention => C,
         Address    => MADT.APIC_Records'address; -- Only get the APICs.

      APICs        : interrupt_controller_descriptors;
      Byte_Index   : number := APICs_Bytes'first;
      APIC_Index   : number := APICs'first;
   BEGIN
      -- Keep going until the indices go out of the array ranges.
      WHILE
         Byte_Index IN APICs_Bytes'range AND THEN
         APIC_Index IN       APICs'range -- Ignore any APICs above 255.
      LOOP
         IF -- Check if it's valid first. The default value is otherwise kept.
            Enum_Val(APICs_Bytes(Byte_Index))'valid
         THEN
            APICs(APIC_Index).Enumeration_Value :=
               Enum_Val(APICs_Bytes(Byte_Index));
         END IF;

         APICs(APIC_Index).Record_Address := APICs_Bytes(Byte_Index)'address;
         Byte_Index := Byte_Index + APICs_Bytes(Byte_Index + 1);
         APIC_Index := APIC_Index + 1;
      END LOOP;

      RETURN APICs;
   END Get_APICs;

END HAVK_Kernel.ACPI;
