-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-acpi.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE BODY HAVK_Kernel.ACPI
WITH
   Refined_State => (ACPI_State => System_Description_Table_Addresses)
IS
   FUNCTION Valid_Table
     (Check_Table_Address : IN address;
      Table_Length        : IN number)
      RETURN boolean
   IS
      Total_Size  : number := 0;
      Table_Bytes : CONSTANT bytes(1 .. Table_Length)
      WITH
         Import  => true,
         Address => Check_Table_Address,
         Annotate => (GNATprove, Intentional,
                      "object is unsuitable for aliasing via address clause",
                      "TODO: Check the length's validity as well.");
   BEGIN
      FOR
         Table_Byte OF Table_Bytes
      LOOP
         EXIT WHEN Total_Size > number'last - (2**8 - 1); -- Overflow check.
         Total_Size := Total_Size + number(Table_Byte);
      END LOOP;

      RETURN (Total_Size AND 16#FF#) = 0;
   END Valid_Table;

   PROCEDURE Parse_ACPI_Tables
   IS
      -- I've hardcoded in the byte size of the SDT or else `gnatprove`
      -- complains.
      Tables_Offset : CONSTANT := 36;

      RSDP_Length   : number RANGE 0 .. 2**32 - 1;
      XSDT_Length   : number RANGE 0 .. 2**32 - 1;
      XSDT_Address  : address;
   BEGIN
      DECLARE
         RSDP : CONSTANT root_system_description_pointer
         WITH
            Import  => true,
            Address => UEFI.Bootloader_Arguments.RSDP_Address;
      BEGIN
         IF
            RSDP.Signature /= "RSD PTR "
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's RSDP is corrupt.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is damaged.");
         ELSIF
            RSDP.Revision /= 2
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's RSDP has an unknown revision.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is incompatible.");
         ELSIF
            RSDP.Length /= ((33 * 8) + 23 + 1) / 8
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's RSDP has an unexpected length.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is incompatible.");
         END IF;

         RSDP_Length := RSDP.Length;
      END;

      IF
         NOT Valid_Table(UEFI.Bootloader_Arguments.RSDP_Address, RSDP_Length)
      THEN
         RAISE Panic
         WITH
            "The ACPI implementation's RSDP checksum is invalid.";
         PRAGMA Annotate(GNATprove, Intentional,
            "exception might be raised",
            "Don't continue if the underlying system is damaged.");
      END IF;

      DECLARE
         RSDP : CONSTANT root_system_description_pointer
         WITH
            Import  => true,
            Address => UEFI.Bootloader_Arguments.RSDP_Address;
      BEGIN
         IF
            RSDP.XSDT_Address = 0
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's RSDP doesn't have an XSDT pointer.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is incompatible.");
         ELSIF
            RSDP.XSDT_Address MOD byte_length'enum_rep /= 0
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's RSDP is not alignable to 8 bits.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is damaged.");
         END IF;

         XSDT_Address := RSDP.XSDT_Address;
      END;

      DECLARE
         XSDT : CONSTANT extended_system_description_table
         WITH
            Import  => true,
            Address => XSDT_Address;
      BEGIN
         IF
            XSDT.SDT.Signature /= "XSDT"
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's XSDT is corrupt.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is damaged.");
         END IF;

         XSDT_Length := XSDT.SDT.Length;

         IF -- Minimum of one pointer after the SDT.
            XSDT_Length <= Tables_Offset + 8
         THEN
            RAISE Panic
            WITH
               "The ACPI implementation's XSDT is too small.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Don't continue if the underlying system is damaged.");
         END IF;
      END;

      IF
         NOT Valid_Table(XSDT_Address, XSDT_Length)
      THEN
         RAISE Panic
         WITH
            "The ACPI implementation's XSDT checksum is invalid.";
         PRAGMA Annotate(GNATprove, Intentional,
            "exception might be raised",
            "Don't continue if the underlying system is damaged.");
      END IF;

      FOR
         Pointer_Index IN 0 .. (XSDT_Length - Tables_Offset) - 1
      LOOP
         DECLARE
            Table_Pointer : CONSTANT address
            WITH
               Import  => true,
               Address => (XSDT_Address + Tables_Offset) +
                           address(Pointer_Index * 8);
         BEGIN
            IF
               Pointer_Index + 1 NOT IN
                  System_Description_Table_Addresses'range
            THEN
               Log("ACPI implementation contains more tables " &
                  "than we can handle.", Tag => ACPI_Tag, Warn => true);
               EXIT;
            END IF;

            System_Description_Table_Addresses(Pointer_Index + 1) :=
               Table_Pointer;
         END;
      END LOOP;

      Log("ACPI RSDP and XSDT parsed successfully.", Tag => ACPI_Tag);
   END Parse_ACPI_Tables;

   FUNCTION Get_Table_Address
     (Signature : IN string)
      RETURN address
   IS
      FUNCTION To_Pointer
        (Table_Address : IN address)
         RETURN NOT NULL ACCESS CONSTANT system_description_table
      WITH
         Import     => true,
         Convention => Intrinsic;
   BEGIN
      FOR
         Table_Address OF System_Description_Table_Addresses
      LOOP
         IF
            Table_Address /= 0                              AND THEN
            To_Pointer(Table_Address).Signature = Signature AND THEN
            To_Pointer(Table_Address).Length > 0            AND THEN
            Valid_Table(Table_Address, To_Pointer(Table_Address).Length)
         THEN
            RETURN Table_Address;
         END IF;
      END LOOP;

      RETURN 0; -- Table does not exist.
   END Get_Table_Address;

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
        (source => byte, target => interrupt_controller);

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
         Byte_Index := Byte_Index + number(APICs_Bytes(Byte_Index + 1));
         APIC_Index := APIC_Index + 1;
      END LOOP;

      RETURN APICs;
   END Get_APICs;

END HAVK_Kernel.ACPI;
