-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-initialise.adb                             --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Debug,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.PS2;
USE
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Initialise
IS
   PROCEDURE Descriptor_Tables
   IS
   BEGIN
      Interrupts.Prepare_GDT;
      Interrupts.Prepare_IDT;
      Log("Descriptor tables prepared.", nominal);
      STI;
      Log("Interrupts enabled.");
   END Descriptor_Tables;

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
      Kernel_Paging_Layout.Map_Address(0, 0, No_Execution => false);

      -- This mapping is just so the higher level page structures have a set
      -- permission to them, the later section specific mappings will not
      -- cascade their privilege settings down all the page levels below L3.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Kernel_Base,          Huge_Page),
         Align(Kernel_Physical_Base, Huge_Page),
         Kernel_Size,
         Page_Size          => Huge_Page,
         Cascade_Privileges =>      true,
         Cascade_Presence   =>     false,
         Write_Access       =>      true,
         No_Execution       =>     false);

      -- Mark the text section as read-only, but executable.
      Kernel_Paging_Layout.Map_Address_Range(
         Kernel_Text_Base,
         Kernel_Text_Base - Kernel_Virtual_Base,
         Kernel_Text_Size,
         Page_Size    =>  Page,
         Write_Access => false,
         No_Execution => false);

      -- Mark the read-only data section as read-only... obviously.
      Kernel_Paging_Layout.Map_Address_Range(
         Kernel_RO_Data_Base,
         Kernel_RO_Data_Base - Kernel_Virtual_Base,
         Kernel_RO_Data_Size,
         Page_Size    =>  Page,
         Write_Access => false,
         No_Execution =>  true);

      -- Mark the data section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range(
         Kernel_Data_Base,
         Kernel_Data_Base - Kernel_Virtual_Base,
         Kernel_Data_Size,
         Page_Size    => Page,
         Write_Access => true,
         No_Execution => true);

      -- Mark the BSS section as modifiable, but also not executable.
      Kernel_Paging_Layout.Map_Address_Range(
         Kernel_BSS_Base,
         Kernel_BSS_Base - Kernel_Virtual_Base,
         Kernel_BSS_Size,
         Page_Size    => Page,
         Write_Access => true,
         No_Execution => true);

      -- Identity-map the framebuffer address space.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Bootloader.Framebuffer_Address, Huge_Page),
         Align(Bootloader.Framebuffer_Address, Huge_Page),
         Bootloader.Framebuffer_Size,
         Page_Size          => Huge_Page,
         Cascade_Privileges =>      true,
         Write_Access       =>      true,
         No_Execution       =>      true);

      -- Identity-map the loader and ACPI regions sent to us by the UEFI
      -- bootloader. One of the MMIO regions is basically guaranteed to be
      -- just below 4 GiB, as it is the APIC. I will be accessing it through
      -- the modern x2APIC way (MSR and not MMIO), so I will not map it here.
      FOR I IN Map'range LOOP
         IF
            Map(I).Memory_Region_Type = loader_data OR
            ELSE Map(I).Memory_Region_Type = ACPI_table_data OR
            ELSE Map(I).Memory_Region_Type = ACPI_firmware_data
         THEN
            Kernel_Paging_Layout.Map_Address_Range(
               Align(Map(I).Start_Address_Physical, Huge_Page),
               Align(Map(I).Start_Address_Physical, Huge_Page,
                  Round_Up => false),
               Map(I).Number_Of_Pages * Page,
               Page_Size          => Huge_Page,
               Cascade_Privileges =>      true,
               Write_Access       =>     false,
               No_Execution       =>      true);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
      Kernel_Paging_Layout.Load;
      Log("Self-described page directories loaded.", nominal);

      -- LOOP NULL; END LOOP;
   END Default_Page_Layout;

   PROCEDURE Grid_Test(
      Display  : IN view;
      Colour   : IN pixel)
   IS
      Box_Size : CONSTANT num := 20; -- Hardcoded box size.
   BEGIN
      -- Clear the screen before we draw anything new.
      Display.Draw_Fill(0, Display.Framebuffer_Elements, 0);

      -- Draw the boxes so a grid is shown across the screen in some shape.
      FOR Y IN num RANGE 0 .. (Display.Screen_Height / Box_Size) - 1 LOOP
         FOR X IN num RANGE 0 .. (Display.Screen_Width / Box_Size) - 1 LOOP
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

      Log("Grid test drawn to the main framebuffer.");
   END Grid_Test;

   PROCEDURE See_Magic(
      Terminal : IN OUT textbox)
   IS
   BEGIN
      -- Inform if the magic number is wrong or corrupted.
      IF    Magic = 16#55_45_46_49# THEN
         Terminal.Print("BOOT METHOD: UEFI");
         Log("Successful UEFI boot.");
      ELSIF Magic = 16#42_49_4F_53# THEN
         Terminal.Print("BOOT METHOD: BIOS"); -- If I bother with it.
         Log("Successful BIOS boot.");
      ELSE
         Terminal.Print("BOOT METHOD: UNKNOWN");
         Log("Unsuccessful boot via unknown method.");
      END IF;
   END See_Magic;

   PROCEDURE Font_Test(
      Terminal       : IN OUT textbox)
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

      IF Check_Condition /= functional THEN
         Log("PS/2 controller is inoperable.", fatal);

         Exceptions.Tears_In_Rain("Non-working PS/2 controller detected",
            Debug.File, Debug.Line);
      ELSE
         Log("PS/2 controller is initialised.", nominal);
      END IF;
   END PS2_Input;

   PROCEDURE Wait_For_New_Key(
      Terminal : IN OUT textbox;
      Display  : IN view;
      Old_Key  : IN character;
      New_Key  : IN character;
      Message  : IN string)
   IS
   BEGIN
      IF Get_Key = Old_Key AND THEN Get_Key /= New_Key THEN
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Print(Message);
         Terminal.Draw_On(Display);

         -- Now wait for it to change.
         WHILE Get_Key /= New_Key LOOP
            HLT;
         END LOOP;

         -- TODO: Not sure how to make `gnatprove` understand that `Get_Key()`
         -- can return anything depending on the keyboard.
         PRAGMA Warnings(GNATprove, off, "unreachable code",
            Reason => "After the key is changed, this is indeed reached.");

         Terminal.Newline;
      END IF;
   END Wait_For_New_Key;

   PROCEDURE Input_Key_Test(
      Terminal : IN OUT textbox;
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

      WHILE Get_Key /= character'val(10) LOOP
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Clear_Line(Terminal.Current_Y_Index);

         CLI; -- Avoid a race condition of the key state changing during print.

         -- Change null to a space so it draws.
         Formatted_Key      := (IF Key_Is_Visible THEN Get_Key ELSE ' ');
         Formatted_Key_Name := Get_Key_Name;

         FOR I IN Formatted_Key_Name'range LOOP
            IF Formatted_Key_Name(I)  = character'val(0) THEN
               Formatted_Key_Name(I) := ' '; -- Change nulls to spaces again.
            END IF;
         END LOOP;

         Terminal.Print("   " & Formatted_Key & " - " & Formatted_Key_Name);

         STI; -- Enable interrupts again for the next iteration or exit.
         HLT; -- Slow down the processor to limit heat and power just in case.

         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Current_X_Index := Terminal.Data'first(2);
      Terminal.Print("   ENTER KEY SUCCESSFULLY PRESSED.");
      Terminal.Newline(2);
      Terminal.Draw_On(Display);

      Log("PS/2 key test ended.");
   END Input_Key_Test;

   PROCEDURE Seconds_Count(
      Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      -- If the last key pressed was enter, then wait for the user to start it.
      Wait_For_New_Key(Terminal, Display, character'val(10), character'val(20),
         "PRESS SPACE TO COUNT SECONDS");

      Log("Seconds count beginning.");
      Terminal.Print("INACCURATE SECONDS COUNT, PRESS ENTER TO EXIT:");
      Terminal.Newline;

      WHILE Get_Key /= character'val(10) LOOP -- Loop showcasing interrupts.
         HLT; -- Don't burn the CPU.
         Terminal.Current_X_Index := 1;

         -- This will count seconds, but if I remember correctly it depends on
         -- the timer's frequency, which I have not retrieved from the UEFI
         -- runtime service function `GetTime()`'s capabilities structure nor
         -- have I programmed the legacy PIT to my settings.
         Terminal.Print(num'image(Interrupts.Ticker / 100));
         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Newline;
   END Seconds_Count;

   PROCEDURE Memory_Map_Info(
      Terminal : IN OUT textbox)
   IS
      USE
         HAVK_Kernel.Memory;

      Map : CONSTANT UEFI.memory_map := UEFI.Get_Memory_Map;
   BEGIN
      -- TODO: Needs work.
      Terminal.Print("MEMORY MAP ENUMERATION:");
      Terminal.Newline;

      Terminal.Print("   MEMORY DESCRIPTORS:" & num'image(Map'length));
      Terminal.Newline;

      Terminal.Print("   TOTAL USABLE MEMORY:" &
         num'image(System_Memory_Limit / MiB) & " MEBIBYTES");
      Terminal.Newline;
   END Memory_Map_Info;

   PROCEDURE Debugger
   IS
   BEGIN
      PRAGMA Debug(Debug.Initialise);
   END Debugger;

END HAVK_Kernel.Initialise;
