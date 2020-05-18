-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-acpi.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;

PACKAGE BODY HAVK_Kernel.ACPI
IS
   FUNCTION Valid_Implementation
      RETURN boolean
   IS
      Bootloader : CONSTANT UEFI.arguments := UEFI.Get_Arguments;

      RSDP_Bytes : CONSTANT bytes(1 .. Bootloader.RSDP.Length)
      WITH
         Import     => true,
         Convention => C,
         Address    => Bootloader.RSDP.ALL'address;

      XSDT_Bytes : CONSTANT bytes(1 .. Bootloader.RSDP.XSDT.SDT.Length)
      WITH
         Import     => true,
         Convention => C,
         Address    => Bootloader.RSDP.XSDT.ALL'address;

      Total_Bytes : number := 0;
   BEGIN
      -- Took me a year to find out that this rather obscure Ada 2012 loop
      -- syntax existed. It should be shown more prominently.
      FOR
         RSDP_Byte OF RSDP_Bytes
      LOOP
         Total_Bytes := Total_Bytes + RSDP_Byte;
      END LOOP;

      IF
         (Total_Bytes AND 16#FF#) /= 0
      THEN
         RETURN false;
      ELSE
         Total_Bytes := 0; -- Reset it to check the XSDT's byte sum.
      END IF;

      FOR
         XSDT_Byte OF XSDT_Bytes
      LOOP
         Total_Bytes := Total_Bytes + XSDT_Byte;
      END LOOP;

      IF
         (Total_Bytes AND 16#FF#) /= 0
      THEN
         RETURN false;
      END IF;

      RETURN true;
   END Valid_Implementation;

   FUNCTION Table_Address
     (Signature : IN string)
      RETURN address
   IS
      -- Return what the access points to.
      FUNCTION To_Pointer
        (Table  : ALIASED NOT NULL ACCESS CONSTANT system_description_table)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      XSDT      : CONSTANT extended_system_description_table :=
         UEFI.Get_Arguments.RSDP.XSDT.ALL;

      -- I've hardcoded in the size of the SDT or else `gnatprove` complains.
      -- The XSDT must be valid before this is ever called.
      Tables    : CONSTANT system_description_tables
        (1 .. (XSDT.SDT.Length - 36) / 8)
      WITH
         Import     => true,
         Convention => C, -- Use the address without new accesses or copies.
         Address    => UEFI.Get_Arguments.RSDP.XSDT.Table_Pointers'address;
   BEGIN
      FOR
         Table OF Tables
      LOOP
         IF
            Table.Signature = Signature
         THEN
            DECLARE
               Table_Bytes : CONSTANT bytes(1 .. Table.Length)
               WITH
                  Import     => true,
                  Convention => C,
                  Address    => Table.ALL'address;

               Total_Bytes : number := 0;
            BEGIN
               FOR
                  Table_Byte OF Table_Bytes
               LOOP
                  Total_Bytes := Total_Bytes + Table_Byte;
               END LOOP;

               IF
                  (Total_Bytes AND 16#FF#) = 0
               THEN
                  RETURN To_Pointer(Table);
               ELSE
                  RETURN 0; -- Table is corrupt.
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
         DECLARE -- Avoid an unchecked conversion.
            APIC_Name : CONSTANT interrupt_controller
            WITH
               Import  => true,
               Size    => 8,
               Address => APICs_Bytes(Byte_Index)'address;
         BEGIN
            APICs(APIC_Index).Enumeration_Value := APIC_Name;
            APICs(APIC_Index).Record_Address    := APIC_Name'address;
            Byte_Index := Byte_Index + APICs_Bytes(Byte_Index + 1);
            APIC_Index := APIC_Index + 1;
         END;
      END LOOP;

      RETURN APICs;
   END Get_APICs;

END HAVK_Kernel.ACPI;
