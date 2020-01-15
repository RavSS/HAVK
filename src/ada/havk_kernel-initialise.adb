-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-initialise.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Phase_II,
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Interrupts.PIC,
   HAVK_Kernel.Interrupts.APIC,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Tasking,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Debug,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.ACPI,
   HAVK_Kernel.PS2;
USE
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Initialise
IS
   PROCEDURE Descriptor_Tables
   IS
   BEGIN
      Disable_Interrupts;
      Interrupts.Prepare_GDT;
      Interrupts.Prepare_IDT;
      Log("Descriptor tables prepared.", nominal);
      Enable_Interrupts;
      Log("Interrupts enabled.");
   END Descriptor_Tables;

   PROCEDURE Interrupt_Controllers
   IS
   BEGIN
      -- Remap the PIC's interrupt vector so no interrupts overlap.
      Interrupts.PIC.Remap;

      -- See what's in the ACPI MADT's APIC structure area.
      Interrupts.APIC.Enumerate_MADT;

      Log("Interrupt controllers have been set up.", nominal);

      Log("Detected" & number'image(Interrupts.APIC.CPU_Cores) & " CPU cores.",
         nominal);
   END Interrupt_Controllers;

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
      Kernel_Paging_Layout.Map_Address(0, 0);

      -- Mark the text section as read-only, but executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_Text_Base,
         Kernel_Text_Base - Kernel_Virtual_Base,
         Kernel_Text_Size,
         Write_Access => false,
         No_Execution => false);

      -- Mark the read-only data section as read-only... obviously.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_RO_Data_Base,
         Kernel_RO_Data_Base - Kernel_Virtual_Base,
         Kernel_RO_Data_Size,
         Write_Access => false,
         No_Execution =>  true);

      -- Mark the data section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_Data_Base,
         Kernel_Data_Base - Kernel_Virtual_Base,
         Kernel_Data_Size,
         Write_Access => true,
         No_Execution => true);

      -- Mark the BSS section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_BSS_Base,
         Kernel_BSS_Base - Kernel_Virtual_Base,
         Kernel_BSS_Size,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the framebuffer address space.
      Kernel_Paging_Layout.Map_Address_Range
        (Address_Value(Bootloader.Framebuffer_Address),
         Address_Value(Bootloader.Framebuffer_Address),
         Bootloader.Framebuffer_Size,
         Page_Size    => Huge_Page,
         Write_Access =>      true,
         No_Execution =>      true);

      -- Map the heap.
      Kernel_Paging_Layout.Map_Address_Range
        (Kernel_Heap_Base,
         Kernel_Heap_Base - Kernel_Virtual_Base,
         Kernel_Heap_Size,
         Page_Size    => Huge_Page,
         Write_Access =>      true,
         No_Execution =>      true);

      -- Identity-map the loader and ACPI regions sent to us by the UEFI
      -- bootloader. One of the MMIO regions is basically guaranteed to be
      -- just below 4 GiB, as it is the APIC. I will be accessing it through
      -- the modern x2APIC way (MSR and not MMIO), so I will not map it here.
      FOR
         Region OF Map
      LOOP
         IF -- TODO: Not sure if the ACPI firmware is vital to map.
            Region.Memory_Region_Type = loader_data OR ELSE
            Region.Memory_Region_Type = ACPI_table_data
         THEN
            Kernel_Paging_Layout.Map_Address_Range
              (Align(Region.Start_Address_Physical, Huge_Page),
               Align(Region.Start_Address_Physical, Huge_Page),
               Region.Number_Of_Pages * Page,
               Page_Size => Huge_Page);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
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
         Terminal.Print("BOOT METHOD: UEFI");
         Log("Successful UEFI boot.");
      ELSIF
         Magic = 16#42_49_4F_53#
      THEN
         Terminal.Print("BOOT METHOD: BIOS"); -- If I bother with it.
         Log("Successful BIOS boot.");
      ELSE
         Terminal.Print("BOOT METHOD: UNKNOWN");
         Log("Unsuccessful boot via unknown method.");
      END IF;
   END See_Magic;

   PROCEDURE Font_Test
     (Terminal       : IN OUT textbox)
   IS
      Uppercase      : CONSTANT string := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      Lowercase      : CONSTANT string := "abcdefghijklmnopqrstuvwxyz";
      Numeric_Digits : CONSTANT string := "0123456789";
      Common_Symbols : CONSTANT string := "!@#$%^&*()_-+=[]{}\|;:'"",<.>/?";
   BEGIN
      Terminal.Print("FONT TEST:");
      Terminal.Newline;
      Terminal.Print("   " & Uppercase);
      Terminal.Newline;
      Terminal.Print("   " & Lowercase);
      Terminal.Newline;
      Terminal.Print("   " & Numeric_Digits);
      Terminal.Newline;
      Terminal.Print("   " & Common_Symbols);
      Terminal.Newline;
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

   PROCEDURE Wait_For_New_Key
     (Terminal : IN OUT textbox;
      Display  : IN view;
      Old_Key  : IN character;
      New_Key  : IN character;
      Message  : IN string)
   IS
   BEGIN
      IF
         Get_Key = Old_Key AND THEN Get_Key /= New_Key
      THEN
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Print(Message);
         Terminal.Draw_On(Display);

         -- Now wait for it to change.
         WHILE
            Get_Key /= New_Key
         LOOP
            Halt;
         END LOOP;

         -- TODO: Not sure how to make `gnatprove` understand that `Get_Key()`
         -- can return anything depending on the keyboard.
         PRAGMA Warnings(GNATprove, off, "unreachable code",
            Reason => "After the key is changed, this is indeed reached.");

         Terminal.Newline;
      END IF;
   END Wait_For_New_Key;

   PROCEDURE Input_Key_Test
     (Terminal : IN OUT textbox;
      Display  : IN view)
   IS
      Formatted_Key      : character;
      Formatted_Key_Name : key_string;
   BEGIN
      -- If the last key pressed was enter, then wait for the user to start it.
      Wait_For_New_Key(Terminal, Display, character'val(10), character'val(20),
         "PRESS SPACE TO BEGIN THE PS/2 INPUT TEST");

      Terminal.Print("PS/2 INPUT TEST, PRESS ENTER TO EXIT:");
      Terminal.Newline;
      Log("PS/2 key test has started.");

      WHILE
         Get_Key /= character'val(10)
      LOOP
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Clear_Line(Terminal.Current_Y_Index);

         -- Avoid a race condition of the key state changing during print.
         Disable_Interrupts;

         -- Change null to a space so it draws.
         Formatted_Key      := (IF Key_Is_Visible THEN Get_Key ELSE ' ');
         Formatted_Key_Name := Get_Key_Name;

         FOR
            I IN Formatted_Key_Name'range
         LOOP
            IF
               Formatted_Key_Name(I)  = character'val(0)
            THEN
               Formatted_Key_Name(I) := ' '; -- Change nulls to spaces again.
            END IF;
         END LOOP;

         Terminal.Print("   " & Formatted_Key & " - " & Formatted_Key_Name);

         -- Enable interrupts again for the next iteration or exit.
         Enable_Interrupts;
         Halt; -- Slow down the processor to limit heat and power just in case.

         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Current_X_Index := Terminal.Data'first(2);
      Terminal.Print("   ENTER KEY SUCCESSFULLY PRESSED.");
      Terminal.Newline(2);
      Terminal.Draw_On(Display);

      Log("PS/2 key test ended.");
   END Input_Key_Test;

   PROCEDURE Seconds_Count
     (Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      -- If the last key pressed was enter, then wait for the user to start it.
      Wait_For_New_Key(Terminal, Display, character'val(10), character'val(20),
         "PRESS SPACE TO COUNT SECONDS");

      Log("Seconds count beginning.");
      Terminal.Print("INACCURATE SECONDS COUNT, PRESS ENTER TO EXIT:");
      Terminal.Newline;

      WHILE
         Get_Key /= character'val(10)
      LOOP -- Loop showcasing interrupts.
         Halt; -- Don't burn the CPU.
         Terminal.Current_X_Index := 1;

         -- This will count seconds, but if I remember correctly it depends on
         -- the timer's frequency, which I have not retrieved from the UEFI
         -- runtime service function `GetTime()`'s capabilities structure nor
         -- have I programmed the legacy PIT to my settings.
         Terminal.Print(number'image(Interrupts.Ticker / 100));
         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Newline;
   END Seconds_Count;

   PROCEDURE Memory_Map_Info
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      Terminal.Print("TOTAL USABLE SYSTEM MEMORY:" &
         number'image(Memory.System_Limit / MiB) & " MEBIBYTES");
      Terminal.Newline;
   END Memory_Map_Info;

   PROCEDURE Parse_ACPI_Tables
   IS
   BEGIN
      IF
         ACPI.Valid_Implementation
      THEN
         Log("ACPI tables RSDP and XSDT are valid.");
      ELSE
         Log("Skipping ACPI tables due to corruption.", warning);
      END IF;
   END Parse_ACPI_Tables;

   PROCEDURE Debugger
   IS
   BEGIN
      Exceptions.Elaborated := true; -- Should silence an "unused" warning.
      PRAGMA Debug(Debug.Initialise);
   END Debugger;

   PROCEDURE Tests
     (Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      Terminal.Print("PRESS ENTER TO INITIALISE ALL TESTS.");
      Terminal.Newline;
      Terminal.Print("PRESS ESCAPE TO SKIP ALL TESTS.");
      Terminal.Newline(2);
      Terminal.Draw_On(Display);

      LOOP
         IF
            User_Input.Get_Key_Name(1 .. 5) = "ENTER"
         THEN
            -- Test the PS/2 input.
            Initialise.Input_Key_Test(Terminal, Display);
            -- Count seconds until the user exits it.
            Initialise.Seconds_Count(Terminal,  Display);
            EXIT WHEN true;
         ELSIF
            User_Input.Get_Key_Name(1 .. 6) = "ESCAPE"
         THEN
            Terminal.Print("TESTS HAVE BEEN SKIPPED.");
            Terminal.Newline;
            Terminal.Draw_On(Display);
            EXIT WHEN true;
         END IF;
      END LOOP;
   END Tests;

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
