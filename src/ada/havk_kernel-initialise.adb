-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-initialise.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Phase_II,
   HAVK_Kernel.User_Input,
   HAVK_Kernel.APIC,
   HAVK_Kernel.APIC.Timer,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Intrinsics.CPUID,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Tasking,
   HAVK_Kernel.Debug,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.ACPI,
   HAVK_Kernel.PS2,
   HAVK_Kernel.PIT;
USE
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Initialise
IS
   PROCEDURE Interrupt_Controllers
   IS
   BEGIN
      IF
         NOT ACPI.Valid_Implementation
      THEN
         Log("Skipping ACPI tables due to corruption.", warning);
         APIC.Remap_PICs(Disable_Emulation => false);
         Enable_Interrupts;
         Log("Interrupts enabled (PIC).");
         RETURN;
      ELSE
         Log("ACPI tables RSDP and XSDT are valid.");
      END IF;

      -- Disable the LAPIC's PIC-compatible mode.
      Log("Disabling PICs.");
      APIC.Remap_PICs(Disable_Emulation => true);
      Log("PICs have been disabled.");

      -- See what's in the ACPI MADT's APIC structure area.
      Log("Enumerating the ACPI MADT.");
      APIC.Enumerate_MADT(Kernel_Paging_Layout);
      Log("ACPI MADT enumerated.");

      -- TODO: Check for the x2APIC bit in CPUID's output before enabling it.
      Log("Enabling x2APIC mode.");
      APIC.x2APIC_Mode;
      Log("Enabled x2APIC mode.", nominal);

      -- We need the PS/2 controller's interrupts.
      Log("Redirecting ISA IRQs to IO/APIC.");
      APIC.Set_IO_APIC_Redirects;
      Log("IO/APIC configured.", nominal);

      Log("Interrupt controllers have been set up.", nominal);
      Log("Detected" & number'image(APIC.CPU_Cores) & " CPU cores.", nominal);

      Enable_Interrupts;
      Log("Interrupts enabled (APIC).");
   END Interrupt_Controllers;

   PROCEDURE Timers
   IS
   BEGIN
      -- We will actually always have a PIT (to my knowledge). I'm not sure
      -- if it's emulated by a present HPET or something along those lines.
      PIT.Setup;

      IF
         ACPI.Valid_Implementation
      THEN
         -- Start the LAPIC timer. It will be calibrated using the PIT.
         APIC.Timer.Setup;
      END IF;
   END Timers;

   PROCEDURE Default_Page_Layout
   IS
      USE
         HAVK_Kernel.UEFI,
         HAVK_Kernel.Paging,
         HAVK_Kernel.Memory;

      Bootloader : CONSTANT arguments  := Get_Arguments;
      Map        : CONSTANT memory_map := Get_Memory_Map;
   BEGIN
      -- Map the virtual null address to the physical null address.
      Kernel_Paging_Layout.Map_Address
        (address(0),
         address(0));

      -- Mark the text section as read-only, but executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_Text_Base,
         Kernel_Virtual_To_Physical(Kernel_Text_Base),
         Kernel_Text_Size,
         Write_Access => false,
         No_Execution => false);

      -- Mark the read-only data section as read-only... obviously.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_RO_Data_Base,
         Kernel_Virtual_To_Physical(Kernel_RO_Data_Base),
         Kernel_RO_Data_Size,
         Write_Access => false,
         No_Execution =>  true);

      -- Mark the data section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_Data_Base,
         Kernel_Virtual_To_Physical(Kernel_Data_Base),
         Kernel_Data_Size,
         Write_Access => true,
         No_Execution => true);

      -- Mark the BSS section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_BSS_Base,
         Kernel_Virtual_To_Physical(Kernel_BSS_Base),
         Kernel_BSS_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the framebuffer address space.
      Kernel_Paging_Layout.Map_Address_Range
        (Bootloader.Framebuffer_Address,
         Bootloader.Framebuffer_Address,
         Bootloader.Framebuffer_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the system heap. Don't put it in the higher-half space.
      Kernel_Paging_Layout.Map_Address_Range
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
         Region OF Map
      LOOP
         IF -- TODO: Not sure if the ACPI firmware is vital to map.
            Region.Memory_Region_Type = loader_data OR ELSE
            Region.Memory_Region_Type = ACPI_table_data
         THEN
            Kernel_Paging_Layout.Map_Address_Range
              (Region.Start_Address_Physical,
               Region.Start_Address_Physical,
               Region.Number_Of_Pages * Page,
               Write_Access => false,
               No_Execution =>  true);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
      Log("Switching to default page layout.");
      Kernel_Paging_Layout.Load;
      Log("Self-described page directories loaded.", nominal);
   END Default_Page_Layout;

   PROCEDURE Grid_Test
     (Display  : IN view;
      Colour   : IN pixel)
   IS
      Box_Size : CONSTANT number := 20; -- Hardcoded box size.
   BEGIN
      -- Clear the screen before we draw anything new.
      Display.Draw_Fill(0, Display.Framebuffer_Elements, 0);

      -- Draw the boxes so a grid is shown across the screen in some shape.
      FOR
         Y IN number RANGE 0 .. (Display.Screen_Height / Box_Size) - 1
      LOOP
         FOR
            X IN number RANGE 0 .. (Display.Screen_Width / Box_Size) - 1
         LOOP
            -- Range checks. Avoid drawing a square if it doesn't fit.
            EXIT WHEN Box_Size * X > Display.Screen_Width;
            EXIT WHEN Box_Size * Y > Display.Screen_Height;
            EXIT WHEN Box_Size - 1 > Display.Framebuffer_Elements;
            EXIT WHEN Display.Calculate_Pixel(Box_Size * X, Box_Size * Y) +
               (Box_Size - 1) > Display.Framebuffer_Elements;
            EXIT WHEN Display.Calculate_Pixel(Box_Size * X, Box_Size * Y) +
               Display.Screen_Width * (Box_Size - 1) >
               Display.Framebuffer_Elements;

            Display.Draw_Box(
               Display.Calculate_Pixel(Box_Size * X, Box_Size * Y),
               Colour,
               Box_Size - 1,
               Box_Size - 1);
         END LOOP;
      END LOOP;
   END Grid_Test;

   PROCEDURE See_Magic
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      IF -- Inform if the magic number is wrong or corrupted.
         Magic = 16#55_45_46_49#
      THEN
         Terminal.Print("Boot method: UEFI.");
         Log("Successful UEFI boot.");
      ELSIF
         Magic = 16#42_49_4F_53#
      THEN
         Terminal.Print("Boot method: BIOS."); -- If I bother with it.
         Log("Successful BIOS boot.");
      ELSE
         Terminal.Print("Boot method: Unknown.");
         Log("Unsuccessful boot via unknown method.");
      END IF;
   END See_Magic;

   PROCEDURE Font_Test
     (Terminal       : IN OUT textbox)
   IS
      Uppercase      : CONSTANT string := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      Lowercase      : CONSTANT string := "abcdefghijklmnopqrstuvwxyz";
      Numeric_Digits : CONSTANT string := "0123456789";
      Common_Symbols : CONSTANT string := "!@#$%^&*()_-+=[]{}\|;:`~'"",<.>/?";
   BEGIN
      Terminal.Print("Font test:");
      Terminal.Print("   Uppercase: " & Uppercase);
      Terminal.Print("   Lowercase: ", Next_Line => false);
      Terminal.Print(Lowercase, Uppercase => false); -- TODO: No support yet.
      Terminal.Print("   Numbers:   " & Numeric_Digits);
      Terminal.Print("   Symbols:   " & Common_Symbols);
   END Font_Test;

   FUNCTION HAVK_Build_Datetime
      RETURN string
   IS
      FUNCTION Compilation_ISO_Date -- Format: "YYYY-MM-DD"
         RETURN string
      WITH
         Import     => true,
         Convention => Intrinsic,
         Post       => Compilation_ISO_Date'result'first =  1 AND THEN
                       Compilation_ISO_Date'result'last  = 10;

      FUNCTION Compilation_Time     -- Format:   "HH:MM:SS"
         RETURN string
      WITH
         Import     => true,
         Convention => Intrinsic,
         Post       => Compilation_Time'result'first = 1 AND THEN
                       Compilation_Time'result'last  = 8;
   BEGIN
      RETURN (Compilation_ISO_Date & 'T' & Compilation_Time); -- 19 characters.
   END HAVK_Build_Datetime;

   PROCEDURE PS2_Input
   IS
      USE
         HAVK_Kernel.PS2;
   BEGIN
      Log("Attempting to initialise PS/2 controller.");
      Setup;

      IF
         Check_Condition /= functional
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Non-working PS/2 controller detected.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "HAVK needs a PS/2 controller for any and all input as of now.");
      ELSE
         Log("PS/2 controller is initialised.", nominal);
      END IF;
   END PS2_Input;

   PROCEDURE Input_Key_Test
     (Terminal : IN OUT textbox;
      Display  : IN view)
   IS
      Formatted_Key_Name : key_string;
   BEGIN
      User_Input.Invalidate_Key_State;
      Terminal.Print("PS/2 input test, press enter to exit:");
      Log("PS/2 key test has started.");

      WHILE
         Get_Key /= character'val(10)
      LOOP
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Clear_Line(Terminal.Current_Y_Index);

         -- Avoid a race condition of the key state changing during print.
         Disable_Interrupts;
         Formatted_Key_Name := Get_Key_Name;

         FOR -- Change all nulls to spaces so it draws over previous text.
            ASCII OF Formatted_Key_Name
         LOOP
            ASCII := (IF ASCII = character'val(0) THEN ' ' ELSE ASCII);
         END LOOP;

         PRAGMA Warnings(GNATprove, off,
            "attribute ""Image"" has an implementation-defined length",
            Reason => "The textbox object can handle whatever string length.");

         Terminal.Print("   " & character'image(Get_Key) & " - " &
            Formatted_Key_Name, Next_Line => false);

         -- Enable interrupts again for the next iteration or exit.
         Enable_Interrupts;
         Halt; -- Slow down the processor to limit heat and power just in case.

         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Current_X_Index := Terminal.Data'first(2);
      Terminal.Print("   Enter key successfully pressed.");
      Terminal.Newline;
      Terminal.Draw_On(Display);

      Log("PS/2 key test ended.");
   END Input_Key_Test;

   PROCEDURE Seconds_Count
     (Terminal      : IN OUT textbox;
      Display       : IN view)
   IS
      Previous_Line : number;
   BEGIN
      User_Input.Invalidate_Key_State;
      Log("Seconds count beginning.");
      Terminal.Print("Inaccurate seconds count, press enter to exit:");

      Previous_Line := Terminal.Current_Y_Index;

      WHILE
         Get_Key /= character'val(10)
      LOOP
         Halt; -- Don't burn the CPU.
         Terminal.Current_X_Index := Terminal.Data'first(2);

         Terminal.Print("LAPIC timer:" & -- TODO: Presume the tick rate.
            number'image(APIC.Timer.Ticks / 100));

         Terminal.Print("PIT        :" &
            number'image(PIT.Ticks / PIT.Tick_Rate),
            Next_Line => false);

         Terminal.Current_Y_Index := Previous_Line;

         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Newline(2);
   END Seconds_Count;

   PROCEDURE Memory_Map_Info
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      Terminal.Print("Total usable system memory:" &
         number'image(Memory.System_Limit / MiB) & " mebibytes.");
   END Memory_Map_Info;

   PROCEDURE Debugger
     (Terminal : IN OUT textbox;
      Printing : IN boolean)
   IS
   BEGIN
      Exceptions.Elaborated := true; -- Should silence an "unused" warning.
      Debug.Initialise(Terminal, Printing);
   END Debugger;

   PROCEDURE Tests
     (Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      Terminal.Newline;
      Terminal.Print("Press space to initialise all tests.");
      Terminal.Print("Press escape to skip all tests.");
      Terminal.Newline;
      Terminal.Draw_On(Display);
      User_Input.Invalidate_Key_State;

      LOOP
         IF
            User_Input.Get_Key_Name(1 .. 5) = "SPACE"
         THEN
            -- Test the PS/2 input.
            Initialise.Input_Key_Test(Terminal, Display);
            -- Count seconds until the user exits it.
            Initialise.Seconds_Count(Terminal,  Display);
            EXIT WHEN true;
         ELSIF
            User_Input.Get_Key_Name(1 .. 6) = "ESCAPE"
         THEN
            Terminal.Print("Tests have been skipped.");
            Terminal.Draw_On(Display);
            EXIT WHEN true;
         END IF;
      END LOOP;
   END Tests;

   PROCEDURE CPU_Feature_Check
     (Terminal : IN OUT textbox)
   IS
      USE
         HAVK_Kernel.Intrinsics.CPUID;

      CPU_Identity : CONSTANT highest_function_parameter_leaf_format :=
         Get_Highest_Function_Parameter(highest_function_parameter_leaf);
   BEGIN
      Terminal.Print("CPU ID: " & CPU_Identity.CPU_Identity_1 &
         CPU_Identity.CPU_Identity_2 & CPU_Identity.CPU_Identity_3 & '.');
   END CPU_Feature_Check;

   PROCEDURE Enter_Phase_II
   WITH
      SPARK_Mode => off -- Address attribute is used to create the task.
   IS
   BEGIN
      -- TODO: Maybe it would be better if this procedure took in an access
      -- type to another procedure instead of using its address directly.
      Tasking.Create("HAVK Phase II", HAVK_Phase_II'address,
         User_Task => false);

      Log("Now beginning multi-tasking environment.", nominal);
      Tasking.Start; -- If this somehow returns, then something is wrong.

      RAISE Panic
      WITH
         Source_Location & " - Failed to enter Phase II.";
   END Enter_Phase_II;

END HAVK_Kernel.Initialise;
