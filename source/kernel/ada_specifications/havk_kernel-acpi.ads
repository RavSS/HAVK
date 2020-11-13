-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-acpi.ads                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;

-- This package contains ACPI-related records for ACPI 2.0+ tables.
-- As of now, I've decided against integrating ACPICA, as it's not written
-- by me, it's all in C, and that's no fun. I don't think I will need any
-- of its advanced features immediately, and if I do need e.g. the AML parser,
-- I can try port it over without needing the entire codebase. Avoid ACPI
-- firmware features as hard as possible unless they're vital.
PACKAGE HAVK_Kernel.ACPI
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      ACPI_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes)
   )
IS
   -- The type of interrupt controller in the HPET table. Note that a lot of
   -- these are irrelevant to x86-64, particularly the GIC (General Interrupt
   -- Controller) which is found on ARM systems and the SAPIC which is for
   -- IA-64. I've ignored as much of them as I can. The only interrupt
   -- controllers relevant to us are the (local) x2APICs and the I/O APICs.
   -- x2APICs should be supported on systems that are new enough to use UEFI.
   TYPE interrupt_controller IS
     (local_APIC_entry,
      IO_APIC_entry,
      interrupt_source_override_entry,
      NMI_source_entry,
      local_APIC_NMI_entry,
      local_APIC_address_override_entry,
      IO_SAPIC_entry,
      local_SAPIC_entry,
      platform_interrupt_source_entry,
      local_x2APIC_entry,
      local_x2APIC_NMI_entry,
      ARM_interrupt_controller_entry) -- Ignore all below this enumeration.
   WITH
      Size => 8;
   FOR interrupt_controller USE
     (local_APIC_entry                  => 00,
      IO_APIC_entry                     => 01,
      interrupt_source_override_entry   => 02,
      NMI_source_entry                  => 03,
      local_APIC_NMI_entry              => 04,
      local_APIC_address_override_entry => 05,
      IO_SAPIC_entry                    => 06,
      local_SAPIC_entry                 => 07,
      platform_interrupt_source_entry   => 08,
      local_x2APIC_entry                => 09,
      local_x2APIC_NMI_entry            => 10,
      ARM_interrupt_controller_entry    => 11);

   -- Indicates how an e.g. overrided interrupt source's input signal differs
   -- from a standard ISA/EISA input signal. Reused the polarity and trigger
   -- mode representations and made them into one enumeration for convenience.
   TYPE interrupt_signal IS
     (default_ISA_or_EISA_signal,
      active_high_or_edge_triggered_signal,
      reserved_signal,
      active_low_or_level_triggered_signal)
   WITH
      Size => 2;
   FOR interrupt_signal USE
     (default_ISA_or_EISA_signal           => 2#00#,
      active_high_or_edge_triggered_signal => 2#01#,
      reserved_signal                      => 2#10#,
      active_low_or_level_triggered_signal => 2#11#);

   -- Due to how the MADT's list of APICs is handled, a subprogram needs to
   -- scan the list and fill up an array containing this general describer of
   -- the interrupt controller, so we can later retrieve the full descriptor of
   -- the APIC via an import. Storing the size is not important for the
   -- interrupt controllers we wish to seek, as they have static field lengths.
   TYPE interrupt_controller_descriptor IS RECORD
      -- The type of interrupt controller found in the MADT. By default,
      -- this is an IA-64 centric structure so no "Present" field is needed.
      -- That should never ever come up on IA-32e/AMD64, which isn't Itanium.
      Enumeration_Value : interrupt_controller :=
         platform_interrupt_source_entry;
      -- The address location of its structure/record which can be imported.
      -- If is better if you check this for zero instead of the controller name
      -- for an x86 controller's presence.
      Record_Address    : address := 0;
   END RECORD;

   -- An array of the general interrupt controller records found in the MADT.
   -- For now, HAVK only supports 255 APICs, which in turn means it only
   -- supports 255 processor cores and I/O APICs. I don't foresee that being an
   -- issue on the type of hardware HAVK is intended for i.e. not huge servers.
   TYPE interrupt_controller_descriptors IS ARRAY(number RANGE 1 .. 255) OF
      interrupt_controller_descriptor;

   -- This MADT entry describes how the old/legacy ISA IRQs work in conjuction
   -- with the I/O APIC. It destroys any notion of identity-mapped IRQs and
   -- overrides them with a new relation. Should be parsed with the I/O APIC.
   -- READ: ACPI Specification Version 6.3, Page 155 - 5.2.12.5.
   TYPE interrupt_source_override_descriptor IS RECORD
      -- As usual, this is a static value. It should reflect the correct
      -- value for the record's purpose.
      Enumeration_Value : interrupt_controller;
      -- The length of the entire MADT entry. For interrupt source overrides,
      -- this will be ten every time unless corrupt.
      Length            : number RANGE 0 .. 2**08 - 1;
      -- Indicates what bus type this is for. As of ACPI Specification Version
      -- 6.3, this can only be zero, which indicates it's for ISA.
      Bus_Variant       : number RANGE 0 .. 2**08 - 1;
      -- The legacy IRQ itself. If the override was for e.g. IRQ 1 (keyboard),
      -- then this would be one as well.
      IRQ_Value         : number RANGE 0 .. 2**08 - 1;
      -- The global system interrupt (GSI) that corresponds to an I/O APIC.
      -- Check this value against all of the I/O APIC entries.
      GSI_Value         : number RANGE 0 .. 2**32 - 1;
      -- The default or new polarity of the interrupt signal.
      Polarity          : interrupt_signal;
      -- The default or new trigger mode for the interrupt signal.
      Trigger_Mode      : interrupt_signal;
      -- A zeroed/reserved field.
      Reserved          : number RANGE 0 .. 2**12 - 1;
   END RECORD
   WITH
      Dynamic_Predicate => Enumeration_Value = interrupt_source_override_entry
                              AND THEN
                           Length = 10 AND THEN
                           Bus_Variant = 0,
      Object_Size       => (8 * 8) + 15 + 1,
      Convention        => C;
   FOR interrupt_source_override_descriptor USE RECORD
      Enumeration_Value AT 0 RANGE 0 .. 07;
      Length            AT 1 RANGE 0 .. 07;
      Bus_Variant       AT 2 RANGE 0 .. 07;
      IRQ_Value         AT 3 RANGE 0 .. 07;
      GSI_Value         AT 4 RANGE 0 .. 31;
      Polarity          AT 8 RANGE 0 .. 01;
      Trigger_Mode      AT 8 RANGE 2 .. 03;
      Reserved          AT 8 RANGE 4 .. 15;
   END RECORD;

   -- This record describes the I/O APIC layout found in the MADT's end.
   -- READ: ACPI Specification Version 6.3, Page 154 - 5.2.12.3.
   TYPE IO_APIC_descriptor IS RECORD
      -- The enumeration value of the interrupt controller. If the structure
      -- is valid, then this should reflect this record type's purpose.
      Enumeration_Value : interrupt_controller;
      -- The length of the entire structure which describes the I/O APIC.
      -- This will always be twelve as of writing this.
      Length            : number  RANGE 0 .. 2**08 - 1;
      -- The I/O APIC's identity. There can be multiple I/O APICs for a system.
      IO_APIC_Identity  : number  RANGE 0 .. 2**08 - 1;
      -- Reserved field not used for anything.
      Reserved          : number  RANGE 0 .. 2**08 - 1;
      -- The 32-bit MMIO physical address. Each I/O APIC has its own address.
      IO_APIC_Address   : address RANGE 0 .. 2**32 - 1;
      -- A global system interrupt number that indicates where the I/O APIC's
      -- interrupt inputs begin from. To figure out the range, you will have
      -- to consult the I/O APIC itself and check in the IOAPICVER register.
      -- Here's Linus's explanation: https://yarchive.net/comp/linux/tla.html
      GSI_Base          : number  RANGE 0 .. 2**32 - 1;
   END RECORD
   WITH
      Dynamic_Predicate => Enumeration_Value = IO_APIC_entry AND THEN
                           Length = 12,
      Object_Size       => (8 * 8) + 31 + 1,
      Convention        => C;
   FOR IO_APIC_descriptor USE RECORD
      Enumeration_Value      AT 0 RANGE 0 .. 07;
      Length                 AT 1 RANGE 0 .. 07;
      IO_APIC_Identity       AT 2 RANGE 0 .. 07;
      Reserved               AT 3 RANGE 0 .. 07;
      IO_APIC_Address        AT 4 RANGE 0 .. 31;
      GSI_Base               AT 8 RANGE 0 .. 31;
   END RECORD;

   -- Parses the list of APIC records in the MADT and returns an array
   -- of their respective address locations for importation.
   FUNCTION Get_APICs
     (MADT_Address : IN address)
      RETURN interrupt_controller_descriptors
   WITH
      Volatile_Function => true,
      Global            => (Input => ACPI_State),
      Pre               => MADT_Address /= 0;

   -- Checks if a system table exists in the XSDT's pointer section and then
   -- returns an address to the start of a table if it exists and is valid.
   -- Returns zero if it's not found or if it's corrupt.
   FUNCTION Table_Address
     (Signature : IN string)
      RETURN address
   WITH
      Volatile_Function => true,
      Global            => (Input => ACPI_State),
      Pre               => Signature'length = 4;

PRIVATE
   -- This type (the SDT) is effectively a header that is included in other
   -- tables e.g. the XSDT. It is then "extended" after its inclusion.
   -- READ: ACPI Specification Version 6.3, Page 119 - 5.2.6.
   TYPE system_description_table IS RECORD
      -- The table's signature. This can be anything as long as it is listed
      -- in the ACPI specification, which can be found below:
      -- READ: ACPI Specification Version 6.3, Page 120 - Table 5-29.
      Signature      :       string(1 .. 4);
      -- The length of the entire table (and any extension of it) in bytes.
      -- This will always be (at least) greater than 36, which is just the
      -- size of the header itself.
      Length         : number RANGE 0 .. 2**32 - 1;
      -- The revision number of the signature.
      Revision       : number RANGE 0 .. 2**08 - 1;
      -- The checksum of the entire table (including the checksum).
      -- The calculation must equal zero if the table is valid.
      Checksum       : number RANGE 0 .. 2**08 - 1;
      -- Identifies the OEM that supplies the ACPI's table implementation.
      -- For example, for Bochs, this can be "BOCHS ", and for QEMU, it is
      -- potentially the same as it borrows the BIOS from Bochs etc.
      OEM_Identity   :       string(1 .. 6);
      -- The OEM's own string that identifies the table as opposed to
      -- the signature itself.
      OEM_Table_Name :       string(1 .. 8);
      -- The revision number of the table based on the OEM's preference.
      OEM_Revision   : number RANGE 0 .. 2**32 - 1;
      -- A numerical identity belonging to the utility that made the table.
      Maker          : number RANGE 0 .. 2**32 - 1;
      -- The revision number of the maker tool.
      Maker_Revision : number RANGE 0 .. 2**32 - 1;
   END RECORD
   WITH
      Dynamic_Predicate => Length > 36, -- The SDT won't be alone in a table.
      Convention        => C,
      Object_Size       => (32 * 8) + 31 + 1;
   FOR system_description_table USE RECORD
      Signature         AT 00 RANGE 0 .. 31;
      Length            AT 04 RANGE 0 .. 31;
      Revision          AT 08 RANGE 0 .. 07;
      Checksum          AT 09 RANGE 0 .. 07;
      OEM_Identity      AT 10 RANGE 0 .. 47;
      OEM_Table_Name    AT 16 RANGE 0 .. 63;
      OEM_Revision      AT 24 RANGE 0 .. 31;
      Maker             AT 28 RANGE 0 .. 31;
      Maker_Revision    AT 32 RANGE 0 .. 31;
   END RECORD;

   -- These make up the XSDT after its SDT.
   TYPE access_system_description_table IS
      ACCESS system_description_table;

   -- The XSDT contains an array of pointers to other tables. This is used
   -- to retrieve the SDT header of the tables. 128 should be enough, even if
   -- all possible ACPI tables are present. Anymore tables will be ignored.
   TYPE system_description_tables IS ARRAY(number RANGE 1 .. 128) OF
      ALIASED access_system_description_table
   WITH
      Pack => true;

   -- The XSDT. It contains pointers to all of the other SDTs and it must
   -- be valid before attempting to parse any of the other SDTs.
   -- READ: ACPI Specification Version 6.3, Page 125 - 5.2.8.
   TYPE extended_system_description_table IS RECORD
      -- The header describing the system description table.
      SDT            : system_description_table;
      -- A pointer array of other system description tables. You can enumerate
      -- the entire array until you find a specific signature. It is the base.
      -- The elements in the array can be obtained by getting the length from
      -- the SDT and then subtracting the length of the SDT itself from it,
      -- then the value must be subtracted by eight due to 64-bit pointers.
      -- That will compute the value of the total tables in the XSDT from one.
      -- Be sure not to use this in an address clause when the XSDT is copied.
      Table_Pointers : ALIASED void;
      -- The table does not truly end here. Due to a limitation of Ada
      -- (or at least to my knowledge), I cannot make the record use an
      -- internal field to alter the record itself during type declaration.
   END RECORD
   WITH
      Dynamic_Predicate => SDT.Length >= 36 + 8, -- Minimum of one pointer.
      Convention        => C,
      Object_Size       => (36 * 8) + 7 + 1;
   FOR extended_system_description_table USE RECORD
      SDT                   AT 00 RANGE 0 .. 287;
      Table_Pointers        AT 36 RANGE 0 .. 007;
   END RECORD;

   -- We need the actual address of "Table_Pointers" (the first 8 bytes of the
   -- area), so we can't pass around copied versions of the table.
   TYPE access_extended_system_description_table IS
      ACCESS extended_system_description_table;

   -- The RSDP. This is the record passed to us by the bootloader.
   -- It has an SDT-like header, but it varies slightly.
   -- READ: ACPI Specification Version 6.3, Page 118 - 5.2.5.3.
   TYPE root_system_description_pointer IS RECORD
      -- The table's signature. It is "RSD PTR " with the ending blank space.
      Signature          :        string(1 .. 8);
      -- The checksum of the first 20 bytes.
      Checksum           : number  RANGE 0 .. 2**08 - 1;
      -- Identifies the OEM that supplies the ACPI's table implementation.
      -- For example, for Bochs, this can be "Bochs", and for QEMU, it is
      -- potentially the same.
      OEM_Identity       :        string(1 .. 6);
      -- Table revision number. This should be 2.
      Revision           : number  RANGE 0 .. 2**08 - 1;
      -- The address for the RSDT. This is not used anymore and should be
      -- avoided, as the XSDT (for 64-bit systems) has superseded it.
      RSDT_Address       : address RANGE 0 .. 2**32 - 1;
      -- Length of the entire table in bytes.
      Length             : number  RANGE 0 .. 2**32 - 1;
      -- The XSDT which will lead us to other SDTs.
      XSDT_Address       : address;
      -- A new checksum of the entire table, including the first checksum.
      Extra_Checksum     : number  RANGE 0 .. 2**08 - 1;
      -- Reserved bytes.
      Reserved           : number  RANGE 0 .. 2**24 - 1;
   END RECORD
   WITH
      Convention  => C,
      Object_Size => (33 * 8) + 23 + 1;
   FOR root_system_description_pointer USE RECORD
      Signature              AT 00 RANGE 0 .. 63;
      Checksum               AT 08 RANGE 0 .. 07;
      OEM_Identity           AT 09 RANGE 0 .. 47;
      Revision               AT 15 RANGE 0 .. 07;
      RSDT_Address           AT 16 RANGE 0 .. 31;
      Length                 AT 20 RANGE 0 .. 31;
      XSDT_Address           AT 24 RANGE 0 .. 63;
      Extra_Checksum         AT 32 RANGE 0 .. 07;
      Reserved               AT 33 RANGE 0 .. 23;
   END RECORD;

   TYPE address_space_identity IS
     (system_memory_space,
      system_IO_space,
      PCI_configuration_space, -- A special address format is needed for PCI.
      embedded_controller_space,
      SMBus_space,
      system_CMOS_space,
      PCI_BAR_space,
      IPMI_space,
      general_IO_space,
      generic_serial_bus_space,
      PCC_space,
      reserved_space_1,
      fixed_hardware_space,
      reserved_space_2,
      OEM_defined_space)
   WITH
      Size => 8;
   FOR address_space_identity USE
     (system_memory_space       => 16#00#,
      system_IO_space           => 16#01#,
      PCI_configuration_space   => 16#02#,
      embedded_controller_space => 16#03#,
      SMBus_space               => 16#04#,
      system_CMOS_space         => 16#05#,
      PCI_BAR_space             => 16#06#,
      IPMI_space                => 16#07#,
      general_IO_space          => 16#08#,
      generic_serial_bus_space  => 16#09#,
      PCC_space                 => 16#0A#,
      reserved_space_1          => 16#0B#,
      fixed_hardware_space      => 16#7F#,
      reserved_space_2          => 16#80#,
      OEM_defined_space         => 16#C0#);

   TYPE register_access_size IS
     (undefined_access_size,
      byte_access_size,
      word_access_size,
      dword_access_size,
      qword_access_size)
   WITH
      Size => 8;
   FOR register_access_size USE
     (undefined_access_size     => 16#00#,
      byte_access_size          => 16#01#,
      word_access_size          => 16#02#,
      dword_access_size         => 16#03#,
      qword_access_size         => 16#04#);

   -- This is ACPI's universal way of describing registers and structures.
   -- READ: ACPI Specification Version 6.3, Page 115 - 5.2.3.2.
   TYPE generic_address_structure IS RECORD
      -- Details the type of the address space where the register resides.
      Identity       : address_space_identity;
      -- The width of the register itself. It is zero for structures.
      Width          : number RANGE 0 .. 2**8 - 1;
      -- The offset of the register within the address space from the start.
      -- This is zero for structures.
      Offset         : number RANGE 0 .. 2**8 - 1;
      -- Indicates how many bits we can read and write within each access.
      Access_Size    : register_access_size;
      -- The address of the register or date structure.
      Address        : number;
   END RECORD
   WITH
      Convention => C;
   FOR generic_address_structure USE RECORD
      Identity           AT 0 RANGE 0 .. 07;
      Width              AT 1 RANGE 0 .. 07;
      Offset             AT 2 RANGE 0 .. 07;
      Access_Size        AT 3 RANGE 0 .. 07;
      Address            AT 4 RANGE 0 .. 63;
   END RECORD;

   -- A list of the various different power management profiles that can
   -- be potentially specified in the FADT. These are self-explanatory and
   -- must be in this order. The enumeration representation starts from zero.
   TYPE power_management_profile IS
     (unspecified_profile,
      desktop_profile,
      mobile_profile,
      workstation_profile,
      enterprise_server_profile,
      home_server_profile,
      embedded_profile,
      performance_server_profile,
      tablet_profile,
      reserved_profile)
   WITH
      Size => 8;
   FOR power_management_profile USE
     (unspecified_profile        => 0,
      desktop_profile            => 1,
      mobile_profile             => 2,
      workstation_profile        => 3,
      enterprise_server_profile  => 4,
      home_server_profile        => 5,
      embedded_profile           => 6,
      performance_server_profile => 7,
      tablet_profile             => 8,
      reserved_profile           => 9);

   -- The IA-PC flags in the FADT. These are quite useful to us.
   -- READ: ACPI Specification Version 6.3, Page 141 - 5.2.9.3.
   TYPE fixed_ACPI_description_table_IA_PC_flags IS RECORD
      -- If true, then the motherboard supports devices on ancient connectors
      -- and buses, like the LPC bus, LPT port, and the ISA bus.
      Legacy_Devices    : boolean;
      -- If true, then a 8042 PS/2 controller is supported. I'm not sure if
      -- this refers to emulation, actual hardware, or both, but from looking
      -- at the ACPI tables provided by "BOCHS" and "OVMF", it seems to refer
      -- to hardware and not emulation.
      PS2_Controller    : boolean;
      -- If true, then there is no VGA hardware and the classic MMIO addresses
      -- e.g. 0xB8000 must not be touched carelessly. Otherwise, VGA exists.
      No_VGA_Hardware   : boolean;
      -- If true, then message signaled interrupts (MSI) should not be enabled.
      No_MSIs           : boolean;
      -- If true, then OSPM ASPM should not be enabled.
      No_OSPM_ASPM      : boolean;
      -- If true, then there is no CMOS real-time clock as it usually exists
      -- on IA-32. Instead, the clock works via ACPI's timer.
      No_CMOS_RTC       : boolean;
      -- Reserved bits for future IA-32 PC flags.
      Reserved          : number RANGE 0 .. 2**10 - 1;
   END RECORD
   WITH
      Convention => C;
   FOR fixed_ACPI_description_table_IA_PC_flags USE RECORD
      Legacy_Devices        AT 0 RANGE 0 .. 00;
      PS2_Controller        AT 0 RANGE 1 .. 01;
      No_VGA_Hardware       AT 0 RANGE 2 .. 02;
      No_MSIs               AT 0 RANGE 3 .. 03;
      No_OSPM_ASPM          AT 0 RANGE 4 .. 04;
      No_CMOS_RTC           AT 0 RANGE 5 .. 05;
      Reserved              AT 0 RANGE 6 .. 15;
   END RECORD;

   -- A record that indicates all of the feature flags available in the FADT.
   -- READ: ACPI Specification Version 6.3, Page 136 - Table 5-34.
   TYPE fixed_ACPI_description_table_feature_flags IS RECORD
      -- If true, the processor supports the `WBINVD` instruction. This should
      -- be true for ACPI 2.0 systems, but processors might not support it.
      WBINVD_Support    : boolean;
      -- If true, the `WBINVD` instruction clears all caches. If false, then
      -- the flush fields will have to be used to do such a thing.
      WBINVD_Flush      : boolean;
      -- If true, then C-state 1 is supported on all processors.
      C1_Support        : boolean;
      -- If true, then C-state 2 works on multi-processor systems. If false,
      -- then it only works on uni-processor systems.
      C2_Multi_CPU      : boolean;
      -- If true, then the power button is a control method device, of which
      -- may potentially not exist. If false, it is a fixed feature.
      Power_Button      : boolean;
      -- If true, then the sleep button is a control method device, of which
      -- may potentially not exist. If false, it is a fixed feature.
      Sleep_Button      : boolean;
      -- If true, then the real-time clock's wake status is not supported
      -- in fixed register space; otherwise, when false, it is.
      No_Fixed_RTC_Wake : boolean;
      -- If true, then the real-time clock can wake the system from S4.
      -- If false, then it cannot, but it can still wake the system from
      -- lower sleep states.
      RTC_S4_Wake       : boolean;
      -- If true, then the timer's value is a 32-bit value. If false, the
      -- timer's value is only 24 bits.
      Extra_Timer_Value : boolean;
      -- If true, then the system can support docking. This does not indicate
      -- whether or not the system is docked, but only the capability itself.
      Dock_Support      : boolean;
      -- If true, then the system can be reset via the FADT's reset register.
      FADT_Power_Reset  : boolean;
      -- If true, then the system's case is sealed and there are no internal
      -- expansion capabilities. I presume that means e.g. PCIe slots.
      Sealed_System     : boolean;
      -- If true, then the system is headless and has no input. This is not
      -- going to be useful for us anytime soon.
      Headless_System   : boolean;
      -- If true, then a native CPU instruction must be executed after
      -- OSPM writes/changes the sleep state register.
      Sx_Native_Execute : boolean;
      -- If true, then a PCI(e) device can wake the system. It also impacts
      -- PM1 registers.
      PCI_Wake_Support  : boolean;
      -- If true, then HAVK's OSPM will use the platform's timer, which in our
      -- case it is the HPET. Otherwise, it may use ACPI's power management
      -- timer. If false, the CPU's clock may be used instead.
      Platform_Timer    : boolean;
      -- If true, the real-time clock has valid settings upon S4 wake.
      RTC_S4_Wake_Valid : boolean;
      -- If true, the system can be remotely powered on.
      Remote_Power_On   : boolean;
      -- If true, then all LAPICs must be in the cluster model. If false, then
      -- the interrupt delivery method in logical mode is undefined.
      LAPIC_Cluster     : boolean;
      -- If true, then all LAPICs must delivery interrupts in physical mode.
      -- If false, then the interrupt delivery method is undefined.
      -- This is not used for systems with less than eight LAPICs.
      LAPIC_Physical    : boolean;
      -- If true, the ACPI implementation has less hardware to handle ACPI
      -- features, so software must be used to make up for it and any features
      -- that are available in this mode.
      Reduced_Hardware  : boolean;
      -- If true, then sleep state S0 has similiar or better power savings than
      -- higher sleep states, effectively indicating that e.g. S3 is useless.
      S0_Low_Power      : boolean;
      -- Reserved bits for future flags.
      Reserved          : number RANGE 0 .. 2**10 - 1;
   END RECORD
   WITH
      Convention => C;
   FOR fixed_ACPI_description_table_feature_flags USE RECORD
      WBINVD_Support        AT 0 RANGE 00 .. 00;
      WBINVD_Flush          AT 0 RANGE 01 .. 01;
      C1_Support            AT 0 RANGE 02 .. 02;
      C2_Multi_CPU          AT 0 RANGE 03 .. 03;
      Power_Button          AT 0 RANGE 04 .. 04;
      Sleep_Button          AT 0 RANGE 05 .. 05;
      No_Fixed_RTC_Wake     AT 0 RANGE 06 .. 06;
      RTC_S4_Wake           AT 0 RANGE 07 .. 07;
      Extra_Timer_Value     AT 0 RANGE 08 .. 08;
      Dock_Support          AT 0 RANGE 09 .. 09;
      FADT_Power_Reset      AT 0 RANGE 10 .. 10;
      Sealed_System         AT 0 RANGE 11 .. 11;
      Headless_System       AT 0 RANGE 12 .. 12;
      Sx_Native_Execute     AT 0 RANGE 13 .. 13;
      PCI_Wake_Support      AT 0 RANGE 14 .. 14;
      Platform_Timer        AT 0 RANGE 15 .. 15;
      RTC_S4_Wake_Valid     AT 0 RANGE 16 .. 16;
      Remote_Power_On       AT 0 RANGE 17 .. 17;
      LAPIC_Cluster         AT 0 RANGE 18 .. 18;
      LAPIC_Physical        AT 0 RANGE 19 .. 19;
      Reduced_Hardware      AT 0 RANGE 20 .. 20;
      S0_Low_Power          AT 0 RANGE 21 .. 21;
      Reserved              AT 0 RANGE 22 .. 31;
   END RECORD;

   -- This is the FADT. The signature is "FACP". It defines the hardware
   -- information related to ACPI. I find that the length varies depending
   -- on the OEM of the ACPI tables, so be careful. There's ACPI versions
   -- above 2.0 that include the later fields in this record. Any probing for
   -- values should be done based on a judgement of the length in the SDT.
   -- READ: ACPI Specification Version 6.3, Page 126 - 5.2.9.
   TYPE fixed_ACPI_description_table IS RECORD
      -- The header describing the system description table.
      SDT               : system_description_table;
      -- The 32-bit physical address of the FACS, which is the ACPI firmware.
      FACS_Address_Low  : number RANGE 0 .. 2**32 - 1;
      -- The 32-bit physical address of the DSDT.
      DSDT_Address_Low  : number RANGE 0 .. 2**32 - 1;
      -- A reserved field. It was originally used in ACPI 1.0.
      Reserved_1        : number RANGE 0 .. 2**08 - 1;
      -- The power management profile of the current system. HAVK is allowed
      -- to set this field in order to control which profile is used.
      Profile           : power_management_profile;
      -- The 8259 interrupt vector for the system control interrupt (SCI).
      SCI_Vector        : number RANGE 0 .. 2**16 - 1;
      -- The I/O port of the system management interrupt (SMI) command port.
      SMI_Port          : number RANGE 0 .. 2**32 - 1;
      -- This is the value to write to the SMI port if I wish to disable my
      -- ownership of the ACPI hardware registers and pass control over to
      -- OSPM (OS-directed Power Management), which is (supposed to be) a
      -- module of our operating system.
      Enable_Value      : number RANGE 0 .. 2**08 - 1;
      -- Like with "Enable_Value", but instead, this is used for regaining
      -- ownership of the ACPI hardware registers. You should mask all SCIs
      -- before disabling ACPI.
      Disable_Value     : number RANGE 0 .. 2**08 - 1;
      -- The value to pass to the SMI port if we wish to enter the S4 state.
      S4BIOS_Value      : number RANGE 0 .. 2**08 - 1;
      -- If this value is written to the SMI port, then we get control over
      -- the processor's P-state.
      Pstate_Value      : number RANGE 0 .. 2**08 - 1;
      -- This is the power management 1a event register block's I/O port.
      PM1a_Event_Port   : number RANGE 0 .. 2**32 - 1;
      -- This is the power management 1b event register block's I/O port.
      PM1b_Event_Port   : number RANGE 0 .. 2**32 - 1;
      -- This is the power management 1a control register block's I/O port.
      PM1a_Control_Port : number RANGE 0 .. 2**32 - 1;
      -- This is the power management 1b control register block's I/O port.
      PM1b_Control_Port : number RANGE 0 .. 2**32 - 1;
      -- This is the power management 2 control register block's I/O port.
      PM2_Control_Port  : number RANGE 0 .. 2**32 - 1;
      -- This is the power management timer control block's I/O port.
      PMT_Control_Port  : number RANGE 0 .. 2**32 - 1;
      -- This is the general-purpose event 0 register block's I/O port.
      GPE0_Port         : number RANGE 0 .. 2**32 - 1;
      -- This is the general-purpose event 1 register block's I/O port.
      GPE1_Port         : number RANGE 0 .. 2**32 - 1;
      -- Number of bytes decoded by the power management 1a event register
      -- block and the PM1b event register block if it is supported.
      PM1_Event_Size    : number RANGE 4 .. 2**08 - 1;
      -- Number of bytes decoded by the power management 1a event register
      -- block and the PM1b event register block if it is supported.
      PM1_Control_Size  : number RANGE 2 .. 2**08 - 1;
      -- Number of bytes decoded by the power management 2 event register
      -- block. If it is not supported, then this is zero.
      PM2_Control_Size  : number RANGE 0 .. 2**08 - 1;
      -- Number of bytes decoded by the power management timer control block.
      -- If the power management timer is supported, then the value is four.
      -- Otherwise, it is zero.
      PMT_Control_Size  : number RANGE 0 .. 2**08 - 1;
      -- Number of bytes decoded by the general-purpose event 0 register block.
      GPE0_Size         : number RANGE 0 .. 2**08 - 1;
      -- Number of bytes decoded by the general-purpose event 1 register block.
      GPE1_Size         : number RANGE 0 .. 2**08 - 1;
      -- The offset of where GPE1-based events start in the ACPI event model.
      GPE1_Offset       : number RANGE 0 .. 2**08 - 1;
      -- If this is not zero, then this contains the value the OSPM writes
      -- to the "SMI_CMD" register to show that HAVK can support C-state
      -- change notifications.
      Cx_Value          : number RANGE 0 .. 2**08 - 1;
      -- The worst-case latency of how long it takes to shift through
      -- the C2 state. If this is greater than 100, then the system doesn't
      -- support the C2 state.
      C2_Shift_Latency  : number RANGE 0 .. 2**16 - 1;
      -- The worst-case latency of how long it takes to shift through
      -- the C3 state. If this is greater than 1000, then the system doesn't
      -- support the C3 state.
      C3_Shift_Latency  : number RANGE 0 .. 2**16 - 1;
      -- If the `WBINVD` instruction is not available, then this is the value
      -- that indicates how many times a cache-line needs to be read in order
      -- to be flushed. This is not useful for us, as it's intended for
      -- ACPI 1.0 compatibility.
      Flush_Size        : number RANGE 0 .. 2**16 - 1;
      -- If the `WBINVD` instruction is not available, then this is the value
      -- that indicates how wide the cache-lines are. This is not useful for
      -- us, as it's intended for ACPI 1.0 compatibility.
      Flush_Stride      : number RANGE 0 .. 2**16 - 1;
      -- The index of where the duty cycle setting is within the processor's
      -- control register.
      Duty_Cycle_Index  : number RANGE 0 .. 2**08 - 1;
      -- The width of the bits in the processor's duty cycle setting value that
      -- are within the processor's control register. This is used to calculate
      -- a lower frequency than the absolute frequency of a processor.
      Duty_Cycle_Width  : number RANGE 0 .. 2**08 - 1;
      -- This is index for the real-time clock's alarm day in the CMOS RAM.
      -- If this is zero, then it is not supported.
      RTC_Day_Alarm     : number RANGE 0 .. 2**08 - 1;
      -- This is index for the real-time clock's alarm month in the CMOS RAM.
      -- If this is zero, then it is not supported.
      RTC_Month_Alarm   : number RANGE 0 .. 2**08 - 1;
      -- The index for the real-time clock's current century in the CMOS RAM.
      -- If this is zero, then it is not supported for a sensible reason.
      RTC_Century       : number RANGE 0 .. 2**08 - 1;
      -- This is the IA-PC boot flags, which will be important for us.
      PC_Flags          : fixed_ACPI_description_table_IA_PC_flags;
      -- Another reserved field.
      Reserved_2        : number RANGE 0 .. 2**08 - 1;
      -- The FADT's feature flags are detailed in this field.
      Feature_Flags     : fixed_ACPI_description_table_feature_flags;
      -- This is the reset register. In order to use it, it must be
      -- supported in the FADT's feature flags, which is "FADT_Flags".
      Reset_Register    : generic_address_structure;
      -- The value that must be written to the reset register port when a
      -- reset is desired. Again, capability for it is in the feature flags.
      Reset_Value       : number RANGE 0 .. 2**08 - 1;
      -- The boot flags for the ARM architecture. This is irrelevant
      -- for me, so I've disregarded creating a separate record for it.
      ARM_Flags         : number RANGE 0 .. 2**16 - 1;
      -- The FADT's minor revision. The one in the SDT is the major version.
      Revision_Minor    : number RANGE 0 .. 2**08 - 1;
      -- The 64-bit physical address of the FACS, which is the ACPI firmware.
      -- If it's empty, then the address is likely under 4 GiB, which means
      -- the 32-bit physical address must be used.
      FACS_Address      : number;
      -- The 64-bit physical address of the DSDT. If it's empty, then the
      -- address is likely under 4 GiB, which means the 32-bit physical
      -- address must be used.
      DSDT_Address      : number;
      -- This is the power management 1a event register block.
      PM1a_Event        : generic_address_structure;
      -- This is the power management 1b event register block.
      PM1b_Event        : generic_address_structure;
      -- This is the power management 1a control register block.
      PM1a_Control      : generic_address_structure;
      -- This is the power management 1b control register block.
      PM1b_Control      : generic_address_structure;
      -- This is the power management 2 control register block.
      PM2_Control       : generic_address_structure;
      -- This is the power management timer control block.
      PMT_Control       : generic_address_structure;
      -- This is the general-purpose event 0 register block.
      GPE0              : generic_address_structure;
      -- This is the general-purpose event 1 register block.
      GPE1              : generic_address_structure;
      -- The FADT's sleep register.
      Sleep_Register    : generic_address_structure;
      -- The FADT's sleep status register.
      Sleep_Status      : generic_address_structure;
      -- The identity of the hypervisor vendor, if there is a hypervisor.
      -- This appears to purely be a numeric identity, not an ASCII one.
      Hypervisor_Name   : number;
   END RECORD
   WITH
      Convention => C;
   FOR fixed_ACPI_description_table USE RECORD
      SDT                 AT 000 RANGE 0 .. 287;
      FACS_Address_Low    AT 036 RANGE 0 .. 031;
      DSDT_Address_Low    AT 040 RANGE 0 .. 031;
      Reserved_1          AT 044 RANGE 0 .. 007;
      Profile             AT 045 RANGE 0 .. 007;
      SCI_Vector          AT 046 RANGE 0 .. 015;
      SMI_Port            AT 048 RANGE 0 .. 031;
      Enable_Value        AT 052 RANGE 0 .. 007;
      Disable_Value       AT 053 RANGE 0 .. 007;
      S4BIOS_Value        AT 054 RANGE 0 .. 007;
      Pstate_Value        AT 055 RANGE 0 .. 007;
      PM1a_Event_Port     AT 056 RANGE 0 .. 031;
      PM1b_Event_Port     AT 060 RANGE 0 .. 031;
      PM1a_Control_Port   AT 064 RANGE 0 .. 031;
      PM1b_Control_Port   AT 068 RANGE 0 .. 031;
      PM2_Control_Port    AT 072 RANGE 0 .. 031;
      PMT_Control_Port    AT 076 RANGE 0 .. 031;
      GPE0_Port           AT 080 RANGE 0 .. 031;
      GPE1_Port           AT 084 RANGE 0 .. 031;
      PM1_Event_Size      AT 088 RANGE 0 .. 007;
      PM1_Control_Size    AT 089 RANGE 0 .. 007;
      PM2_Control_Size    AT 090 RANGE 0 .. 007;
      PMT_Control_Size    AT 091 RANGE 0 .. 007;
      GPE0_Size           AT 092 RANGE 0 .. 007;
      GPE1_Size           AT 093 RANGE 0 .. 007;
      GPE1_Offset         AT 094 RANGE 0 .. 007;
      Cx_Value            AT 095 RANGE 0 .. 007;
      C2_Shift_Latency    AT 096 RANGE 0 .. 015;
      C3_Shift_Latency    AT 098 RANGE 0 .. 015;
      Flush_Size          AT 100 RANGE 0 .. 015;
      Flush_Stride        AT 102 RANGE 0 .. 015;
      Duty_Cycle_Index    AT 104 RANGE 0 .. 007;
      Duty_Cycle_Width    AT 105 RANGE 0 .. 007;
      RTC_Day_Alarm       AT 106 RANGE 0 .. 007;
      RTC_Month_Alarm     AT 107 RANGE 0 .. 007;
      RTC_Century         AT 108 RANGE 0 .. 007;
      PC_Flags            AT 109 RANGE 0 .. 015;
      Reserved_2          AT 111 RANGE 0 .. 007;
      Feature_Flags       AT 112 RANGE 0 .. 031;
      Reset_Register      AT 116 RANGE 0 .. 095;
      Reset_Value         AT 128 RANGE 0 .. 007;
      ARM_Flags           AT 129 RANGE 0 .. 015;
      Revision_Minor      AT 131 RANGE 0 .. 007;
      FACS_Address        AT 132 RANGE 0 .. 063;
      DSDT_Address        AT 140 RANGE 0 .. 063;
      PM1a_Event          AT 148 RANGE 0 .. 095;
      PM1b_Event          AT 160 RANGE 0 .. 095;
      PM1a_Control        AT 172 RANGE 0 .. 095;
      PM1b_Control        AT 184 RANGE 0 .. 095;
      PM2_Control         AT 196 RANGE 0 .. 095;
      PMT_Control         AT 208 RANGE 0 .. 095;
      GPE0                AT 220 RANGE 0 .. 095;
      GPE1                AT 232 RANGE 0 .. 095;
      Sleep_Register      AT 244 RANGE 0 .. 095;
      Sleep_Status        AT 256 RANGE 0 .. 095;
      Hypervisor_Name     AT 268 RANGE 0 .. 063;
   END RECORD;

   -- The MADT. It contains vital information for utilising the I/O and Local
   -- APICs. This is necessary for going beyond the dual-8259 PIC emulation.
   -- READ: ACPI Specification Version 6.3, Page 151 - 5.2.12.
   TYPE multiple_APIC_description_table IS RECORD
      -- The SDT header. The MADT's signature is "APIC".
      SDT               : system_description_table;
      -- The 32-bit MMIO physical address of the local APIC (LAPIC).
      -- I presume this is only the LAPIC for the bootstrap processor?
      LAPIC_Address     : number RANGE 0 .. 2**32 - 1;
      -- If true, then the APIC is compatible with the dual-8259 PIC
      -- configuration. By default, every PC should theoretically have this.
      PIC_Compatible    : boolean;
      -- Reserved flags that are not used for anything as of now.
      Reserved_Flags    : number RANGE 0 .. 2**31 - 1;
      -- Same conundrum as with the XSDT table. From this address, there is a
      -- range of interrupt controller structures/records that describe the
      -- amount of APICs on the system and if they're LAPICs or IOAPIC etc.
      -- This means a manual parsing must be done. The first byte of each
      -- structure is the interrupt controller type and the second byte is
      -- the length. Once you reach the table length specified in the SDT,
      -- the list has ended.
      APIC_Records      : void;
      -- The table does not end here; again, this is like the XSDT but with
      -- a less simple method of getting the list of interrupt controllers.
   END RECORD
   WITH
      Convention => C;
   FOR multiple_APIC_description_table USE RECORD
      SDT                  AT 00 RANGE 0 .. 287;
      LAPIC_Address        AT 36 RANGE 0 .. 031;
      PIC_Compatible       AT 40 RANGE 0 .. 000;
      Reserved_Flags       AT 40 RANGE 1 .. 031;
      APIC_Records         AT 44 RANGE 0 .. 000;
   END RECORD;

   -- A record that describes the LAPIC in the MADT table.
   -- READ: ACPI Specification Version 6.3, Page 153 - 5.2.12.2.
   TYPE local_APIC_descriptor IS RECORD
      -- The enumeration value of the interrupt controller. If the structure
      -- is valid, then this should reflect this record type's purpose.
      Enumeration_Value : interrupt_controller;
      -- The length of the entire structure which describes the local x2APIC.
      -- This will always be eight as of writing this.
      Length            : number RANGE 0 .. 2**08 - 1;
      -- This is related to ACPI AML. OSPM must match each LAPIC (regardless of
      -- specification) to a processor.
      ACPI_Identity     : number RANGE 0 .. 2**08 - 1;
      -- The (presumably physical) identity of the local APIC.
      APIC_Identity     : number RANGE 0 .. 2**08 - 1;
      -- Whether or not the local APIC is already enabled.
      Enabled           : boolean;
      -- If true, then the local APIC is not enabled and the logical processor
      -- itself has to be turned on. This cannot be true if the local APIC is
      -- already enabled.
      Activatable       : boolean;
      -- Reserved flags that are not used for anything.
      Reserved_Flags    : number RANGE 0 .. 2**30 - 1;
   END RECORD
   WITH
      Dynamic_Predicate => Enumeration_Value = local_APIC_entry AND THEN
                           Length = 8                           AND THEN
                          (IF Enabled     THEN NOT Activatable) AND THEN
                          (IF Activatable THEN NOT Enabled),
      Convention        => C;
   FOR local_APIC_descriptor USE RECORD
      Enumeration_Value    AT 00 RANGE 0 .. 07;
      Length               AT 01 RANGE 0 .. 07;
      ACPI_Identity        AT 02 RANGE 0 .. 07;
      APIC_Identity        AT 03 RANGE 0 .. 07;
      Enabled              AT 04 RANGE 0 .. 00;
      Activatable          AT 04 RANGE 1 .. 01;
      Reserved_Flags       AT 04 RANGE 2 .. 31;
   END RECORD;

   -- A record that describes the x2 version of the LAPIC in the MADT table.
   -- Know that an xAPIC can be put into x2APIC mode, which is often the reason
   -- why you will probably not find any structures of this type in the MADT,
   -- as the local APIC is in xAPIC mode due to backwards compatibility. The
   -- MADT will only have x2APIC structures if the current system has more than
   -- 255 processor cores or I/O APICs as the BIOS is supposed to pre-enable
   -- them for us, which isn't going to happen for the user base of HAVK.
   -- READ: ACPI Specification Version 6.3, Page 161 - 5.2.12.12.
   TYPE local_x2APIC_descriptor IS RECORD
      -- The enumeration value of the interrupt controller. If the structure
      -- is valid, then this should reflect this record type's purpose.
      Enumeration_Value : interrupt_controller;
      -- The length of the entire structure which describes the local x2APIC.
      -- This will always be sixteen as of writing this.
      Length            : number RANGE 0 .. 2**08 - 1;
      -- A reserved field.
      Reserved          : number RANGE 0 .. 2**16 - 1;
      -- The (presumably physical) identity of the local x2APIC.
      x2APIC_Identity   : number RANGE 0 .. 2**32 - 1;
      -- Whether or not the local x2APIC is already enabled.
      Enabled           : boolean;
      -- If true, then the local x2APIC is not enabled and the logical
      -- processor itself has to be turned on. This cannot be true if the
      -- local x2APIC is already enabled.
      Activatable       : boolean;
      -- Reserved flags that are not used for anything.
      Reserved_Flags    : number RANGE 0 .. 2**30 - 1;
      -- This is related to ACPI AML. OSPM must match each LAPIC (regardless of
      -- specification) to a processor.
      ACPI_Identity     : number RANGE 0 .. 2**32 - 1;
   END RECORD
   WITH
      Dynamic_Predicate => Enumeration_Value = local_APIC_entry AND THEN
                           Length = 8                           AND THEN
                          (IF Enabled     THEN NOT Activatable) AND THEN
                          (IF Activatable THEN NOT Enabled),
      Convention        => C;
   FOR local_x2APIC_descriptor USE RECORD
      Enumeration_Value AT 00 RANGE 0 .. 07;
      Length            AT 01 RANGE 0 .. 07;
      Reserved          AT 02 RANGE 0 .. 15;
      x2APIC_Identity   AT 04 RANGE 0 .. 31;
      Enabled           AT 08 RANGE 0 .. 00;
      Activatable       AT 08 RANGE 1 .. 01;
      Reserved_Flags    AT 08 RANGE 2 .. 31;
      ACPI_Identity     AT 12 RANGE 0 .. 31;
   END RECORD;

   -- The HPET table that details the high-precision event timer.
   -- The specification for the ACPI 2.0 table can be found within the HPET
   -- specification from Intel themselves.
   -- READ: IA-PC HPET Specification Revision 1.0a, Page 30 - 3.2.4.
   TYPE high_precision_event_timer_table IS RECORD
      -- This platform-specific table also contains an SDT header.
      SDT               : system_description_table;
      -- The HPET hardware's revision number.
      Hardware_Revision : number RANGE 0 .. 2**08 - 1;
      -- The amount of comparators (current and/or voltage comparison circuit)
      -- that the HPET implementation has. This is usually above three and the
      -- value itself begins from zero, so three comparators would be "2".
      Comparators       : number RANGE 0 .. 2**05 - 1;
      -- If true, then the HPET supports 64-bit operation.
      Long_Mode_Support : boolean;
      -- A reserved field.
      Reserved          : boolean;
      -- If true, then the HPET can replace the old PIT and can have its IRQs
      -- routed to the PIT's PIC IRQs instead.
      PIT_Replacement   : boolean;
      -- The PCI vendor identity number.
      Vendor_Identity   : number RANGE 0 .. 2**16 - 1;
      -- The MMIO address is in ACPI's GAS format. Make sure it's mapped
      -- before you read or write to it as usual.
      Event_Timer_Block : generic_address_structure;
      -- This indicates the current HPET's sequence number. There can be
      -- multiple HPET tables. For example, the first table's HPET sequence
      -- number will be zero, and so on.
      HPET_Sequence     : number RANGE 0 .. 2**08 - 1;
      -- The lowest possible clock period/tick supported without
      -- losing any interrupts in periodic mode. The value written to the
      -- HPET's register must be the minimum period divided by the main
      -- counter period.
      Minimum_Period    : number RANGE 0 .. 2**16 - 1;
      -- The HPET can support page protection if its MMIO address is exposed to
      -- ring 3. I don't think we will be doing that, so this isn't too useful.
      Page_Protection   : number RANGE 0 .. 2**08 - 1;
   END RECORD
   WITH
      Convention => C;
   FOR high_precision_event_timer_table USE RECORD
      SDT                  AT 00 RANGE 0 .. 287;
      Hardware_Revision    AT 36 RANGE 0 .. 007;
      Comparators          AT 37 RANGE 0 .. 004;
      Long_Mode_Support    AT 37 RANGE 5 .. 005;
      Reserved             AT 37 RANGE 6 .. 006;
      PIT_Replacement      AT 37 RANGE 7 .. 007;
      Vendor_Identity      AT 38 RANGE 0 .. 015;
      Event_Timer_Block    AT 40 RANGE 0 .. 095;
      HPET_Sequence        AT 52 RANGE 0 .. 007;
      Minimum_Period       AT 53 RANGE 0 .. 015;
      Page_Protection      AT 55 RANGE 0 .. 007;
   END RECORD;

   -- Parses the bootloader arguments and returns the first table. Will raise
   -- a panic if it's corrupt.
   FUNCTION Get_RSDP
      RETURN root_system_description_pointer
   WITH
      Volatile_Function => true,
      Global            => (Input    => ACPI_State,
                            Proof_In => UEFI.Bootloader_Arguments);

   -- Does the same as `Get_RSDP()`, but for the XSDT's SDT. The array of other
   -- tables must be aliased from the original XSDT address plus the SDT
   -- length. Will raise a panic if it's corrupt.
   FUNCTION Get_XSDT
      RETURN access_extended_system_description_table
   WITH
      Volatile_Function => true,
      Global            => (Input    => ACPI_State,
                            Proof_In => UEFI.Bootloader_Arguments),
      Post              => Get_XSDT'result /= NULL;

   -- Returns accesses to the tables in the XSDT. They only point to the SDT of
   -- each table, but they can be imported into different record types.
   FUNCTION Get_XSDT_Tables
      RETURN system_description_tables
   WITH
      Volatile_Function => true,
      Global            => (Input => ACPI_State);

END HAVK_Kernel.ACPI;
