WITH
   HAVK_Kernel.Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Debug,
   HAVK_Kernel.PS2;
USE
   HAVK_Kernel.Input,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Initialise
IS
   PROCEDURE Descriptor_Tables
   IS
   BEGIN
      Interrupts.Prepare_GDT;
      Interrupts.Prepare_IDT;
      PRAGMA Debug(Debug_Message("Descriptor tables prepared."));
      STI;
      PRAGMA Debug(Debug_Message("Interrupts enabled."));
   END Descriptor_Tables;

   PROCEDURE Default_Page_Layout(
      Bootloader : IN UEFI.arguments;
      Map        : IN UEFI.memory_map)
   IS
      USE
         HAVK_Kernel.Paging,
         HAVK_Kernel.UEFI;
   BEGIN
      PRAGMA Debug(Debug_Message("Paging structure size with padding:" &
         num'image(Kernel_Paging_Layout'size / 8192) & " KiB"));

      -- TODO: Identity map the first page. Don't know why is this needed, but
      -- the CPU goes haywire without it.
      Kernel_Paging_Layout.Map_Address(
         0,
         0,
         Page_Size    => huge_page,
         Write_Access =>      true,
         NX           =>    false);

      -- Map the higher-half virtual address to the physical one.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Kernel_Base,          huge_page),
         Align(Kernel_Physical_Base, huge_page),
         Kernel_Size,
         Page_Size    => huge_page,
         Write_Access =>      true,
         NX           =>    false);

      -- Identity-map the framebuffer address space.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Bootloader.Framebuffer_Address, huge_page),
         Align(Bootloader.Framebuffer_Address, huge_page),
         Bootloader.Framebuffer_Size,
         Page_Size    => huge_page,
         Write_Access =>      true,
         NX           =>    false);

      -- Identity-map the loader and MMIO regions sent to us by the UEFI
      -- bootloader. One of the MMIO regions is basically guaranteed to be
      -- just below 4 GiB, as it is the APIC. I will be accessing it through
      -- the modern x2APIC way (MSR and not MMIO), so I will not map it here.
      -- TODO: It may be easier to copy the map's data onto our local stack...
      FOR I IN Map'range LOOP
         IF Map(I).Memory_Region_Type = loader_data THEN
            Kernel_Paging_Layout.Map_Address_Range(
               Align(Map(I).Start_Address_Physical, huge_page),
               Align(Map(I).Start_Address_Physical, huge_page),
               Map(I).Number_Of_Pages * page'enum_rep,
               Page_Size    => huge_page,
               Write_Access =>      true,
               NX           =>    false);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
      Kernel_Paging_Layout.Load;
      PRAGMA Debug(Debug_Message("Self-described page directories loaded."));
   END Default_Page_Layout;

   PROCEDURE Grid_Test(
      Display  : IN OUT view;
      Colour   : IN pixel)
   IS
      Box_Size : CONSTANT num := 20; -- Hardcoded box size.
   BEGIN
      -- Clear the screen before we draw anything new.
      Display.Draw_Fill(0, Display.Framebuffer_Elements, 0);

      -- Draw the boxes so a grid is shown across the screen in some shape.
      FOR Y IN num RANGE 0 .. (Display.Screen_Height / Box_Size) - 1 LOOP
         FOR X IN num RANGE 0 .. (Display.Screen_Width / Box_Size) - 1 LOOP
            Display.Draw_Box(
               Display.Calculate_Pixel(Box_Size * X, Box_Size * Y),
               Colour,
               Box_Size - 1,
               Box_Size - 1);
         END LOOP;
      END LOOP;

      PRAGMA Debug(Debug_Message("Grid test drawn to the main framebuffer."));
   END Grid_Test;

   PROCEDURE See_Magic(
      Terminal : IN OUT textbox)
   IS
   BEGIN
      -- Inform if the magic number is wrong or corrupted.
      IF    Magic = 16#55_45_46_49# THEN
         Terminal.Print("BOOT METHOD: UEFI");
         PRAGMA Debug(Debug_Message("Successful UEFI boot."));
      ELSIF Magic = 16#42_49_4F_53# THEN
         Terminal.Print("BOOT METHOD: BIOS"); -- If I bother with it.
         PRAGMA Debug(Debug_Message("Successful BIOS boot."));
      ELSE
         Terminal.Print("BOOT METHOD: UNKNOWN");
         PRAGMA Debug(Debug_Message("Unsuccessful boot via unknown method."));
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
         Convention => Intrinsic;

      FUNCTION Compilation_Time     -- Format:   "HH:MM:SS"
      RETURN string
      WITH
         Import     => true,
         Convention => Intrinsic;
   BEGIN
      RETURN (Compilation_ISO_Date & "T" & Compilation_Time);
   END HAVK_Build_Datetime;

   PROCEDURE PS2_Input
   IS
      USE
         HAVK_Kernel.PS2;
   BEGIN
      PRAGMA Debug(Debug_Message("Attempting to initialise PS/2 controller."));
      Setup;

      IF Check_Condition /= functional THEN
         PRAGMA Debug(Debug_Message("PS/2 controller is inoperable -" &
            controller_condition'image(Check_Condition) & "."));

         Exceptions.Tears_In_Rain("Non-working PS/2 controller detected",
            Debug.File, Debug.Line);
      ELSE
         PRAGMA Debug(Debug_Message("PS/2 controller is initialised."));
      END IF;
   END PS2_Input;

   PROCEDURE PS2_Scancode_Test(
      Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      LOOP
         HLT;
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Print("     "); -- Clear the first 5 characters.
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Print(num'image(INB(16#60#)));
         Terminal.Draw_On(Display);
      END LOOP;
   END PS2_Scancode_Test;

   PROCEDURE Input_Key_Test(
      Terminal : IN OUT textbox;
      Display  : IN view)
   IS
      -- TODO: Store the key state here, don't fetch the latest one on each
      -- of the `Get_*` functions, or else a race occurs and there can be
      -- an erroneous difference in the output if the user is fast enough
      -- or their system is slow enough.
   BEGIN
      Terminal.Print("PS/2 KEYPRESS TEST, PRESS ENTER TO EXIT:");
      Terminal.Newline;
      PRAGMA Debug(Debug_Message("PS/2 key test has started."));

      WHILE Get_Key /= character'val(10) LOOP
         Terminal.Clear_Line(Terminal.Current_Y_Index);
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Print("   " & Get_Key & " - " & Get_Key_Name);
         Terminal.Draw_On(Display);
      END LOOP;

      Terminal.Current_X_Index := Terminal.Data'first(2);
      Terminal.Print("   ENTER KEY SUCCESSFULLY PRESSED.");
      Terminal.Newline(2);
      Terminal.Draw_On(Display);

      PRAGMA Debug(Debug_Message("PS/2 key test ended."));
   END Input_Key_Test;

   PROCEDURE Seconds_Count(
      Terminal : IN OUT textbox;
      Display  : IN view)
   IS
   BEGIN
      Terminal.Print("INACCURATE SECONDS COUNT: ");
      Terminal.Newline;
      PRAGMA Debug(Debug_Message("Endless seconds count beginning."));

      LOOP -- Endless loop showcasing interrupts.
         HLT; -- Don't burn the CPU.
         Terminal.Current_X_Index := 1;

         -- This will count seconds, but if I remember correctly it depends on
         -- the timer's frequency, which I have not retrieved from the UEFI
         -- runtime service function `GetTime()`'s capabilities structure nor
         -- have I programmed the legacy PIT to my settings.
         Terminal.Print(num'image(Interrupts.Ticker / 100));
         Terminal.Draw_On(Display);
      END LOOP;
   END Seconds_Count;
END HAVK_Kernel.Initialise;