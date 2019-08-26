WITH
   HAVK_Kernel.Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.PS2;
USE
   HAVK_Kernel.Input,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.PS2;

PACKAGE BODY HAVK_Kernel.Initialise IS
   PROCEDURE Descriptor_Tables
   IS
   BEGIN
      Prepare_GDT;
      Prepare_IDT;
      PRAGMA Debug(Debug_Message("Descriptor tables prepared."));
      STI;
      PRAGMA Debug(Debug_Message("Interrupts enabled."));
   END Descriptor_Tables;

   PROCEDURE Default_Page_Layout(
      Display : IN view)
   IS
      Aligned_Kernel_Base      : CONSTANT num :=
         Paging.Align_Huge(Paging.Kernel_Base);
      Aligned_Framebuffer_Base : CONSTANT num :=
         Paging.Align_Huge(Display.Framebuffer_Address);
   BEGIN
      PRAGMA Debug(Debug_Message("Paging structure size:" &
         num'image(Kernel_Paging_Layout'size / 8) & " KiB"));

      -- TODO: Identity map the first 2 MiB page, can't remember why it is
      -- needed. Something to do with the UEFI bootloader's memory allocations?
      Kernel_Paging_Layout.Map_Address(0, 0);

      -- Identity map the kernel.
      Kernel_Paging_Layout.Map_Linear_Address_Range(
         Aligned_Kernel_Base, Aligned_Kernel_Base,
         Paging.Kernel_Size);

      -- Identity map the framebuffer address space.
      Kernel_Paging_Layout.Map_Linear_Address_Range(
         Aligned_Framebuffer_Base, Aligned_Framebuffer_Base,
         Display.Framebuffer_Size * 4);

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
      Display.Draw_Fill(0, Display.Framebuffer_Size, 0);
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
      Terminal.Next_Line;
      Terminal.Print("   " & Uppercase);
      Terminal.Next_Line;
      Terminal.Print("   " & Lowercase);
      Terminal.Next_Line;
      Terminal.Print("   " & Numeric_Digits);
      Terminal.Next_Line;
      Terminal.Print("   " & Common_Symbols);
      Terminal.Next_Line;
   END Font_Test;

   PROCEDURE PS2_Input
   IS
   BEGIN
      PRAGMA Debug(Debug_Message("Attempting to initialise PS/2 controller."));
      PS2.Controller_Initialise;

      IF PS2.Controller_State /= functional THEN
         PRAGMA Debug(Debug_Message("PS/2 controller is inoperable - """ &
            PS2.Controller_State'img & ""));
      ELSE
         PRAGMA Debug(Debug_Message("PS/2 controller is initialised."));
      END IF;
   END PS2_Input;

   PROCEDURE PS2_Scancode_Test(
      Terminal : IN OUT textbox)
   IS
   BEGIN
      LOOP
         HLT;
         Terminal.Current_X_Index := 1;
         Terminal.Print("     "); -- Clear the first 5 characters.
         Terminal.Current_X_Index := 1;
         Terminal.Print(num'image(INB(16#60#)));
      END LOOP;
   END PS2_Scancode_Test;

   PROCEDURE Input_Key_Test(
      Terminal : IN OUT textbox)
   IS
      Key      : character := '-';
   BEGIN
      Terminal.Print("PS/2 KEYPRESS TEST, PRESS ENTER TO EXIT: ");
      Terminal.Next_Line;
      PRAGMA Debug(Debug_Message("PS/2 key test has started."));

      WHILE Key /= character'val(10) LOOP
         Key := Get_Key;
         Terminal.Current_X_Index := 1;
         Terminal.Print(Key & "");
         Terminal.Draw;
         HLT;
      END LOOP;

      Terminal.Current_X_Index := 1;
      Terminal.Print("ENTER KEY SUCCESSFULLY PRESSED.");
      Terminal.Next_Line;
      Terminal.Next_Line;

      PRAGMA Debug(Debug_Message("PS/2 key test ended."));
   END Input_Key_Test;

   PROCEDURE Seconds_Count(
      Terminal : IN OUT textbox)
   IS
   BEGIN
      Terminal.Print("INACCURATE SECONDS COUNT: ");
      Terminal.Next_Line;
      PRAGMA Debug(Debug_Message("Endless seconds count beginning."));

      LOOP -- Endless loop showcasing interrupts.
         HLT; -- Don't burn the CPU.
         Terminal.Current_X_Index := 1;

         -- This will count seconds, but if I remember correctly it depends on
         -- the timer's frequency, which I have not retrieved from the UEFI
         -- runtime service function `GetTime()`'s capabilities structure nor
         -- have I programmed the legacy PIT to my settings.
         Terminal.Print(u64'image(Ticker / 100));
         Terminal.Draw;
      END LOOP;
   END Seconds_Count;
END HAVK_Kernel.Initialise;