WITH
   HAVK_Kernel.User_Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Debug,
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
         HAVK_Kernel.Paging;

      Bootloader : CONSTANT UEFI.arguments  := Get_Arguments;
      Map        : CONSTANT UEFI.memory_map := Get_Memory_Map;
   BEGIN
      -- Map the virtual null address to the physical null address.
      Kernel_Paging_Layout.Map_Address(
         Align(0, Page), -- Help `gnatprove` out.
         Align(0, Page),
         Page_Size    => Page,
         Write_Access =>      true,
         NX           =>    false);

      -- Map the higher-half virtual address to the physical one.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Kernel_Base,          Huge_Page),
         Align(Kernel_Physical_Base, Huge_Page),
         Kernel_Size,
         Page_Size    => Huge_Page,
         Write_Access =>      true,
         NX           =>    false);

      -- Identity-map the framebuffer address space.
      Kernel_Paging_Layout.Map_Address_Range(
         Align(Bootloader.Framebuffer_Address, Huge_Page),
         Align(Bootloader.Framebuffer_Address, Huge_Page),
         Bootloader.Framebuffer_Size,
         Page_Size    => Huge_Page,
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
               Align(Map(I).Start_Address_Physical, Huge_Page),
               Align(Map(I).Start_Address_Physical, Huge_Page),
               Map(I).Number_Of_Pages * Page,
               Page_Size    => Huge_Page,
               Write_Access =>      true,
               NX           =>    false);
         END IF;
      END LOOP;

      -- Finally, load the CR3 register with the highest level directory.
      Kernel_Paging_Layout.Load;
      Log("Self-described page directories loaded.", nominal);
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
      Map : CONSTANT UEFI.memory_map := Get_Memory_Map;
   BEGIN
      -- TODO: Needs work.
      Terminal.Print("MEMORY MAP ENUMERATION:");
      Terminal.Newline;
      Terminal.Print("   MEMORY DESCRIPTORS:" & num'image(Map'length));
      Terminal.Newline;
   END Memory_Map_Info;

   PROCEDURE Debugger
   IS
   BEGIN
      PRAGMA Debug(Debug.Initialise);
   END Debugger;

   FUNCTION Get_Arguments
   RETURN arguments          IS
      -- The bootloader will never pass a null pointer, so this type is just
      -- here to inform `gnatprove` of it without an explicit pragma.
      TYPE access_arguments  IS NOT NULL ACCESS arguments;
      Bootloader_Arguments : CONSTANT access_arguments
      WITH
         Import     => true,
         Convention => NASM,
         Link_Name  => "bootloader.arguments";
   BEGIN
      -- It's not physically possible for a scanline to have less pixels than
      -- the scanline's width, so the UEFI GOP implementation will never return
      -- anything to the contrary.
      PRAGMA Assume(Bootloader_Arguments.Pixels_Per_Scanline >=
         Bootloader_Arguments.Horizontal_Resolution);

      RETURN Bootloader_Arguments.ALL;
   END Get_Arguments;

   FUNCTION Get_Memory_Map
   RETURN memory_map    IS
      Bootloader      : CONSTANT arguments := Get_Arguments;
      Map             : CONSTANT memory_map(0 .. Bootloader.Memory_Map_Size /
         Bootloader.Memory_Map_Descriptor_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Bootloader.Memory_Map_Address;
   BEGIN
      -- I doubt UEFI will give back a memory map with more than a few hundred
      -- memory map descriptors. To make it easier on `gnatprove`, I've assumed
      -- the memory map to just be 10000 descriptors long. There does not seem
      -- to be an actual limit on it in the UEFI specification as of 2.8.
      PRAGMA Assume(Map'first = 0 AND THEN Map'last < 10000);

      RETURN Map;
   END Get_Memory_Map;
END HAVK_Kernel.Initialise;
