-- This package contains ACPI-related records for ACPI 2.0 tables.
-- As of now, I've decided against integrating ACPICA, as it's not written
-- by me, it's all in C, and that's no fun. I don't think I will need any
-- of its advanced features immediately, and if I do need e.g. the AML parser,
-- I can try port it over without needing the entire codebase.
PACKAGE HAVK_Kernel.ACPI
IS
   -- This is the record passed to us by the bootloader.
   -- READ: ACPI Specification Version 6.3, Page 118 - 5.2.5.3.
   TYPE root_system_description_pointer IS RECORD
      Signature      :    string(1 ..           8);
      Checksum       : num RANGE 0 ..       16#FF#;
      OEM_Identity   :    string(1 ..           6);
      Revision       : num RANGE 0 ..       16#FF#;
      RSDT_Address   : num RANGE 0 .. 16#FFFFFFFF#; -- Not used anymore.
      Length         : num RANGE 0 .. 16#FFFFFFFF#;
      XSDT           : num;
      Extra_Checksum : num RANGE 0 ..       16#FF#;
      Reserved       : num RANGE 0 ..   16#FFFFFF#;
   END RECORD
   WITH
      Convention => C;
   FOR root_system_description_pointer USE RECORD
      Signature      AT  0 RANGE 0 .. 63;
      Checksum       AT  8 RANGE 0 ..  7;
      OEM_Identity   AT  9 RANGE 0 .. 47;
      Revision       AT 15 RANGE 0 ..  7;
      RSDT_Address   AT 16 RANGE 0 .. 31;
      Length         AT 20 RANGE 0 .. 31;
      XSDT           AT 24 RANGE 0 .. 63;
      Extra_Checksum AT 32 RANGE 0 ..  7;
      Reserved       AT 33 RANGE 0 .. 23;
   END RECORD;
END HAVK_Kernel.ACPI;
