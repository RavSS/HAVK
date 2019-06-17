WITH
   System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

PROCEDURE HAVK WITH No_Return
IS
   -- Access the UEFI Bootloader passed by HAVK's UEFI bootloader.
   Bootloader : access_UEFI_arguments
   WITH
      Import        =>  true,
      Convention    =>  C,
      External_Name => "bootloader";

   -- Access the framebuffer that we can hopefully manipulate.
   Screen : -- 4 bytes make up a pixel, so 32 bit pixels.
      ALIASED framebuffer(0 .. SHR(Bootloader.Framebuffer_Size - 1, 2))
   WITH
      Import     => true,
      Convention => Ada,
      Address    => Bootloader.Framebuffer_Address;

   -- As an early test, I'll draw a grid to the screen.
   Box_Size    : CONSTANT num := 20;
   Grid_Colour :        pixel :=  0;

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

   Welcome : CONSTANT string := "WELCOME TO HAVK";
BEGIN
   -- Set up the graphics package variables.
   Screen_Width  := Bootloader.Horizontal_Resolution;
   Screen_Height := Bootloader.Vertical_Resolution;
   Pixel_Size    := Bootloader.Pixels_Per_Scanline / Screen_Width;
   Pixel_Format  := Bootloader.Pixel_Format;

   -- Clear the screen.
   Draw_Fill(Screen, Screen'first, Screen'last, 0);

   -- Set a colour for the grid. I've set it to "red", so that the user
   -- can confirm if the pixel format is recognized properly.
   Grid_Colour := Create_Pixel(80, 10, 10);

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

   -- Prepare the descriptor tables.
   Prepare_GDT;
   Prepare_IDT;
   Asm("STI;", Volatile => true); -- Enable interrupts.

   Clear_Textbox(Terminal); -- Set to all null characters.
   Terminal.Background_Colour := Create_Pixel(0, 0, 0);
   Terminal.Foreground_Colour := Create_Pixel(255, 55, 0);

   Terminal.Current_X_Index := Terminal.Data'last(2) / 2 - Welcome'length / 2;
   Print(Terminal, Welcome);
   Next_Line(Terminal);

   Draw_Textbox(
      Screen,
      Terminal,
      Terminal_Start);

   Terminal.Current_X_Index := Terminal.Data'last(2) / 2 - 37 / 2;
   Print(Terminal, "ABCDEFGHIJKLMNOPQRSTUVWXYZ 1234567890");
   Next_Line(Terminal);
   Next_Line(Terminal);

   Print(Terminal, "INACCURATE SECONDS COUNT: ");
   Next_Line(Terminal);

   LOOP -- Endless loop showcasing interrupts.
      Asm("HLT;", Volatile => true);

      -- This will count seconds, but if I remember correctly it depends on
      -- the timer's frequency, which I have not retrieved from the UEFI
      -- runtime service function `GetTime()`'s capabilities structure.
      IF Ticker MOD 100 = 0 THEN
         Print(Terminal, "X");
      END IF;

      Draw_Textbox(
         Screen,
         Terminal,
         Terminal_Start);
   END LOOP;
END HAVK;
