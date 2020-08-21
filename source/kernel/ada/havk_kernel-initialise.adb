-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-initialise.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.APIC,
   HAVK_Kernel.APIC.Timer,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Intrinsics.CPUID,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Tasking,
   HAVK_Kernel.Tasking.ELF,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Debug,
   HAVK_Kernel.Drive.GPT,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.PIT;

PACKAGE BODY HAVK_Kernel.Initialise
IS
   PROCEDURE Interrupt_Controllers
   IS
   BEGIN
      -- Remap and disable the LAPIC's PIC-compatible mode.
      Log("Disabling PICs.", Tag => Initialise_Tag);
      APIC.Remap_PICs;
      Log("PICs have been disabled.", Tag => Initialise_Tag);

      -- See what's in the ACPI MADT's APIC structure area.
      Log("Enumerating the ACPI MADT.", Tag => Initialise_Tag);
      APIC.Enumerate_MADT;
      Log("ACPI MADT enumerated.", Tag => Initialise_Tag);

      -- TODO: Check for the x2APIC bit in CPUID's output before enabling it.
      Log("Enabling x2APIC mode.", Tag => Initialise_Tag);
      APIC.x2APIC_Mode;
      Log("Enabled x2APIC mode.", Tag => Initialise_Tag);

      -- We need the PS/2 controller's interrupts.
      Log("Redirecting ISA IRQs to I/O APIC.", Tag => Initialise_Tag);
      APIC.Set_IO_APIC_Redirects;
      Log("I/O APIC configured.", Tag => Initialise_Tag);

      Log("Interrupt controllers have been set up.", Tag => Initialise_Tag);
      Log("Detected " & Image(APIC.CPU_Cores) & " logical CPU cores.",
         Tag => Initialise_Tag);

      Intrinsics.Enable_Interrupts;
      Log("Interrupts enabled (APIC).", Tag => Initialise_Tag);
   END Interrupt_Controllers;

   PROCEDURE Timers
   IS
   BEGIN
      -- We will actually always have a PIT (to my knowledge). I'm not sure
      -- if it's emulated by a present HPET or something along those lines.
      PIT.Setup;

      -- Start the LAPIC timer. It will be calibrated using the PIT.
      APIC.Timer.Setup;
   END Timers;

   PROCEDURE Default_Page_Layout
   IS
      USE
         Memory;
      USE TYPE
         UEFI.access_memory_descriptor,
         UEFI.memory_type;
   BEGIN
      -- Map the virtual null address to the physical null address.
      Paging.Kernel_Map_Address
        (address(0),
         address(0));

      -- Mark the text section as read-only, but executable.
      Paging.Kernel_Map_Address_Range
        (Kernel_Text_Base,
         Kernel_Virtual_To_Physical(Kernel_Text_Base),
         Kernel_Text_Size,
         Write_Access => false,
         No_Execution => false);

      -- Mark the read-only data section as read-only... obviously.
      Paging.Kernel_Map_Address_Range
        (Kernel_RO_Data_Base,
         Kernel_Virtual_To_Physical(Kernel_RO_Data_Base),
         Kernel_RO_Data_Size,
         Write_Access => false,
         No_Execution =>  true);

      -- Mark the data section as modifiable, but also not executable.
      Paging.Kernel_Map_Address_Range
        (Kernel_Data_Base,
         Kernel_Virtual_To_Physical(Kernel_Data_Base),
         Kernel_Data_Size,
         Write_Access => true,
         No_Execution => true);

      -- Mark the BSS section as modifiable, but also not executable.
      Paging.Kernel_Map_Address_Range
        (Kernel_BSS_Base,
         Kernel_Virtual_To_Physical(Kernel_BSS_Base),
         Kernel_BSS_Size,
         Write_Access => true,
         No_Execution => true);

      -- The same as the text section, but shared with other page layouts.
      Paging.Kernel_Map_Address_Range
        (Kernel_Isolated_Text_Base,
         Kernel_Virtual_To_Physical(Kernel_Isolated_Text_Base),
         Kernel_Isolated_Text_Size,
         Write_Access => false,
         No_Execution => false);

      -- The same as the data section, but shared with other page layouts.
      Paging.Kernel_Map_Address_Range
        (Kernel_Isolated_Data_Base,
         Kernel_Virtual_To_Physical(Kernel_Isolated_Data_Base),
         Kernel_Isolated_Data_Size,
         Write_Access => true,
         No_Execution => true);

      -- The same as the BSS section, but shared with other page layouts.
      Paging.Kernel_Map_Address_Range
        (Kernel_Isolated_BSS_Base,
         Kernel_Virtual_To_Physical(Kernel_Isolated_BSS_Base),
         Kernel_Isolated_BSS_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the framebuffer address space.
      Paging.Kernel_Map_Address_Range
        (UEFI.Bootloader_Arguments.Framebuffer_Address,
         UEFI.Bootloader_Arguments.Framebuffer_Address,
         UEFI.Bootloader_Arguments.Framebuffer_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the system heap. Don't put it in the higher-half space.
      Paging.Kernel_Map_Address_Range
        (Kernel_Heap_Base,
         Kernel_Heap_Base,
         Kernel_Heap_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the loader and ACPI regions sent to us by the UEFI
      -- bootloader. One of the MMIO regions is basically guaranteed to be just
      -- below 4 GiB, as it is the APICs. For the local APIC, I access it
      -- through the modern x2APIC way (MSR and not MMIO), whereas I map the
      -- I/O APIC separately in the APIC package.
      FOR
         Region OF UEFI.Memory_Map
      LOOP
         EXIT WHEN Region = NULL;

         IF -- TODO: Not sure if the ACPI firmware is vital to map.
            Region.Memory_Region_Type = UEFI.loader_data OR ELSE
            Region.Memory_Region_Type = UEFI.ACPI_table_data
         THEN
            Paging.Kernel_Map_Address_Range
              (Region.Start_Address_Physical,
               Region.Start_Address_Physical,
               Region.Number_Of_Pages * Paging.Page,
               Write_Access => false,
               No_Execution =>  true);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
      Log("Switching to the kernel's page layout.", Tag => Initialise_Tag);
      Paging.Load_Kernel_Page_Layout;
      Log("Kernel page layout has been loaded.", Tag => Initialise_Tag);
   END Default_Page_Layout;

   PROCEDURE Check_Entry
   IS
      -- The magic number passed to us by my UEFI bootloader.
      Magic : CONSTANT number
      WITH
         Import     => true,
         Convention => Assembler,
         Link_Name  => "global__bootloader_magic";
   BEGIN
      IF -- Inform if the magic number is wrong or corrupted.
         Magic = 16#55_45_46_49#
      THEN
         Log("Successful UEFI boot.", Tag => Initialise_Tag);
      ELSIF
         Magic = 16#42_49_4F_53#
      THEN
         Log("Successful BIOS boot.", Tag => Initialise_Tag);
      ELSE
         Log("Kernel entry via unknown method.", Tag => Initialise_Tag);

         RAISE Panic
         WITH
            Source_Location & " - The bootloader's magic number is corrupt.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "If the bootloader's magic number is incorrect, then panic.");
      END IF;
   END Check_Entry;

   FUNCTION HAVK_Build_Datetime
      RETURN datetime_string
   IS
      SUBTYPE date_string IS string(1 .. 10);
      SUBTYPE time_string IS string(1 .. 08);

      FUNCTION Compilation_ISO_Date -- Format: "YYYY-MM-DD"
         RETURN date_string
      WITH
         Import     => true,
         Convention => Intrinsic;

      FUNCTION Compilation_Time     -- Format:   "HH:MM:SS"
         RETURN time_string
      WITH
         Import     => true,
         Convention => Intrinsic;
   BEGIN
      RETURN (Compilation_ISO_Date & 'T' & Compilation_Time); -- 19 characters.
   END HAVK_Build_Datetime;

   PROCEDURE Memory_Map_Info
   IS
   BEGIN
      Log("Total usable system memory: " & Image(Memory.System_Limit / MiB) &
         " mebibytes.", Tag => Initialise_Tag);
   END Memory_Map_Info;

   PROCEDURE Debugger
   IS
   BEGIN
      Exceptions.Elaborated := true; -- Should silence an "unused" warning.
      Debug.Activate;
   END Debugger;

   PROCEDURE CPU_Feature_Check
   IS
      USE
         Intrinsics.CPUID;

      CPU_Identity  : CONSTANT highest_function_parameter_leaf_format :=
         Get_Highest_Function_Parameter;
      CPU_Frequency : CONSTANT processor_frequency_leaf_format :=
         Get_Processor_Frequency;
   BEGIN
      Log("CPU ID string: """ & CPU_Identity.CPU_Identity_1 &
         CPU_Identity.CPU_Identity_2 & CPU_Identity.CPU_Identity_3 & """.",
         Tag => Initialise_Tag);
      Log("CPU maximum frequency: " & Image(CPU_Frequency.Max_Frequency_MHz) &
         " MHz.", Tag => Initialise_Tag);
   END CPU_Feature_Check;

   PROCEDURE Boot_Partition_Check
     (EFI_File_System : OUT Drive.FAT.file_system)
   IS
      EFI_Boot_Partition : Drive.GPT.partition;
   BEGIN
      FOR -- Check both buses and both drives for the (U)EFI boot partition.
         I IN 1 .. 4
      LOOP
         Drive.GPT.Get_Partition(EFI_Boot_Partition, "EFI",
            Secondary_Bus => (I IN 3 | 4), Secondary_Drive => (I IN 2 | 4));
         EXIT WHEN EFI_Boot_Partition.Present;
      END LOOP;

      IF
         NOT EFI_Boot_Partition.Present
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Could not find the EFI/UEFI boot partition.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We cannot continue if we don't have access to other files.");
      END IF;

      Drive.FAT.Get_File_System(EFI_File_System, EFI_Boot_Partition);

      IF
         Drive.FAT.Get_FAT_Version(EFI_File_System) /= Drive.FAT.FAT16
      THEN
         RAISE Panic
         WITH
            Source_Location & " - EFI/UEFI boot partition is not FAT16.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "It must be formatted as FAT16 for now, not i.e. FAT12.");
      END IF;
   END Boot_Partition_Check;

   -- TODO: For now, this just loads a few ELF files. I really need to add more
   -- worthwhile system calls.
   PROCEDURE Begin_Tasking
   IS
      Error_Check     : error;
      EFI_File_System : Drive.FAT.file_system;
   BEGIN
      Boot_Partition_Check(EFI_File_System);

      Tasking.ELF.Load(EFI_File_System, Initialiser_Path, Initialiser_Name,
         Error_Check);

      IF
         Error_Check /= no_error
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Failed to load the initialiser program.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We cannot continue if it fails to load. External error.");
      END IF;

      Log("Now beginning multi-tasking environment.", Tag => Initialise_Tag);
      Tasking.Start;
   END Begin_Tasking;

END HAVK_Kernel.Initialise;
