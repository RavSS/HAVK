WITH
   -- System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Input,
   HAVK_Kernel.PS2,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   -- System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Input,
   HAVK_Kernel.PS2,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

PROCEDURE HAVK WITH No_Return
IS
   -- Access the arguments passed by HAVK's UEFI bootloader.
   Bootloader : access_UEFI_arguments
   WITH
      Import        =>  true,
      Convention    =>  C,
      External_Name => "bootloader";

   -- Bootloader magic number.
   Magic : CONSTANT num
   WITH
      Import        =>  true,
      Convention    =>  C,
      External_Name => "magic";

   -- Access the framebuffer that we can hopefully manipulate.
   -- 4 bytes make up a pixel, so 32 bit pixels.
   Screen : ALIASED framebuffer(0 .. Bootloader.Framebuffer_Size / 4)
   WITH
      Import        =>  true,
      Convention    =>  C,
      Address       =>  Bootloader.Framebuffer_Address;

   -- The UEFI memory map. Assumes the descriptor size has been "fixed".
   Map    : ALIASED memory_map(0  .. Bootloader.Memory_Map_Size /
      Bootloader.Memory_Map_Descriptor_Size)
   WITH
      Import        =>  true,
      Convention    =>  C,
      Address       =>  Bootloader.Memory_Map_Address;

   -- The terminal where I will display things to the user concerning the
   -- current system like warnings, errors, info, etc.
   -- Font width and height are both 8 pixels, so divide by 10 the
   -- horizontal resolution (2 for default kerning) and divide by 11
   -- the vertical resolution (3 for default line separation). A small
   -- border is placed around the terminal.
   Terminal_Border : CONSTANT num := Bootloader.Horizontal_Resolution / 4;
   Terminal_Start  : CONSTANT num := Calculate_Pixel(Terminal_Border / 2,
      Terminal_Border / 2);
   Terminal : textbox((Bootloader.Horizontal_Resolution - Terminal_Border) /
      10, (Bootloader.Vertical_Resolution - Terminal_Border) / 11);

   -- As an early test, I'll draw a grid to the screen.
   Box_Size        : CONSTANT num := 20;
   -- The grid's colour. I set it after I obtain the pixel format.
   Grid_Colour     :        pixel :=  0;

   PROCEDURE Grid_Test;
   PROCEDURE Grid_Test
   IS
   BEGIN
      -- Draw the boxes so a grid is shown across the screen in some shape.
      FOR Y IN num RANGE 0 .. (Screen_Height / Box_Size) - 1 LOOP
         FOR X IN num RANGE 0 .. (Screen_Width / Box_Size) - 1 LOOP
            Draw_Box(
               Screen,
               Calculate_Pixel(Box_Size * X, Box_Size * Y),
               Grid_Colour,
               Box_Size - 1,
               Box_Size - 1);
         END LOOP;
      END LOOP;
   END Grid_Test;

   Welcome : CONSTANT string := "WELCOME TO HAVK";
   Scratch : string(1 .. 10) := (OTHERS => character'val(0));
BEGIN
   -- Set up the graphics package variables.
   Screen_Width  := Bootloader.Horizontal_Resolution;
   Screen_Height := Bootloader.Vertical_Resolution;
   Pixel_Size    := Bootloader.Pixels_Per_Scanline / Screen_Width;
   Pixel_Format  := Bootloader.Pixel_Format;

   -- Clear the screen and draw the testing grid.
   Draw_Fill(Screen, Screen'first, Screen'last, 0);
   Grid_Colour := Create_Pixel(70, 10, 10);
   Grid_Test;

   -- Prepare the descriptor tables.
   Prepare_GDT;
   Prepare_IDT;

   Terminal.Clear_All; -- Set to all null characters.
   Terminal.Background_Colour := Create_Pixel(0, 0, 0);
   Terminal.Foreground_Colour := Create_Pixel(255, 55, 0);

   -- Print the welcome message.
   Terminal.Current_X_Index := Terminal.Data'last(2) / 2 - Welcome'length / 2;
   Terminal.Print(Welcome);
   Terminal.Next_Line;
   Terminal.Next_Line;

   -- Inform if the magic number is wrong or corrupted.
   IF    Magic = 16#55_45_46_49# THEN
      Terminal.Print("BOOT METHOD: UEFI");
   ELSIF Magic = 16#42_49_4F_53# THEN
      Terminal.Print("BOOT METHOD: BIOS"); -- If I bother with it.
   ELSE
      Terminal.Print("BOOT METHOD: UNKNOWN");
   END IF;

   Terminal.Next_Line;

   -- Print the font test.
   Terminal.Print("FONT TEST:");
   Terminal.Next_Line;
   Terminal.Print("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
   Terminal.Next_Line;
   Terminal.Print("1234567890");
   Terminal.Next_Line;
   Terminal.Print("!@#$%^&*()_-+=[]{}\|;:'"",<.>/?");
   Terminal.Next_Line;
   Terminal.Next_Line;

   -- TODO: Needs work.
   Terminal.Print("MEMORY MAP ENUMERATION:");
   Terminal.Next_Line;
   Terminal.Print("MEMORY DESCRIPTORS: " & num'image(Map'length));
   Terminal.Next_Line;
   Terminal.Next_Line;

   Terminal.Draw(Screen, Terminal_Start);

   -- Initialize the PS2 controller for input purposes.
   PS2.Controller_Initialize;
   STI; -- Enable interrupts.

   IF PS2.Controller_State /= functional THEN
      Terminal.Print("YOUR SYSTEM DOES NOT EMULATE A PS2 CONTROLLER.");
      Terminal.Next_Line;
      Terminal.Print("MEANINGFULLY CONTINUING FURTHER IS IMPOSSIBLE.");
      Terminal.Draw(Screen, Terminal_Start);
   ELSE
      Terminal.Print("TESTING KEYBOARD INPUT. EXIT BY PRESSING ENTER: ");
      Terminal.Next_Line;

      LOOP -- Keyboard test.
         HLT;
         Terminal.Current_X_Index := 1;

         Scratch(1) := Get_Key;
         EXIT WHEN Scratch(1) = character'val(10);

         Terminal.Print(Scratch);
         Terminal.Draw(Screen, Terminal_Start);
      END LOOP;

      -- LOOP -- Keyboard scancode test.
         -- Asm("HLT;", Volatile => true);
         -- Terminal.Print("     "); -- Clear the first 5 characters.
         -- Terminal.Draw(Screen, Terminal_Start);
         -- Terminal.Current_X_Index := 1;

         -- Terminal.Print(num'image(INB(16#60#)));
         -- Terminal.Draw(Screen, Terminal_Start);
      -- END LOOP;
   END IF;

   Terminal.Next_Line;
   Terminal.Print("INACCURATE SECONDS COUNT: ");
   Terminal.Next_Line;

   LOOP -- Endless loop showcasing interrupts.
      HLT; -- Don't burn the CPU.

      -- This will count seconds, but if I remember correctly it depends on
      -- the timer's frequency, which I have not retrieved from the UEFI
      -- runtime service function `GetTime()`'s capabilities structure.
      Terminal.Current_X_Index := 1;
      Terminal.Print(u64'image(Ticker / 100));
      Terminal.Draw(Screen, Terminal_Start);
   END LOOP;
END HAVK;
