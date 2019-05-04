WITH
   System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.Bitwise,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE
   System.Machine_Code,
   HAVK_Kernel,
   HAVK_Kernel.Bitwise,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

PROCEDURE HAVK
IS
   -- Access the UEFI arguments passed by HAVK's UEFI bootloader.
   Arguments : Access_UEFI_Arguments;
   PRAGMA Import(C, Arguments);

   -- Access the framebuffer that we can hopefully manipulate.
   Screen : -- 4 bytes make up a pixel, so 32 bit pixels.
      framebuffer(0 .. Rsh64(Arguments.Framebuffer_Size - 1, 2));
   FOR Screen'address USE Arguments.Framebuffer_Address;
   PRAGMA Import(Ada, Screen);

   -- As an early test, I'll draw a grid to the screen.
   Box_Size : CONSTANT u32 := 20;
   Grid_Colour : u32;

   -- The terminal where I will display things to the user concerning the
   -- current system like warnings, errors, info, etc.
   -- Font width and height are both 8 pixels, so divide by 10 the
   -- horizontal resolution (2 for default kerning) and divide by 11
   -- the vertical resolution (3 for default line separation). Minus 100 off
   -- both resolutions for a 100 pixel visual border effect.
   Terminal_Border : CONSTANT u32 := 100;
   Terminal_Start : CONSTANT u64 := Calculate_Pixel(Terminal_Border / 2,
      Terminal_Border / 2);
   Terminal : textbox((Arguments.Horizontal_Resolution - Terminal_Border) /
      10, (Arguments.Vertical_Resolution - Terminal_Border) / 11);

   Welcome : CONSTANT str := "WELCOME TO HAVK";
BEGIN
   -- Set up the graphics package variables.
   Screen_Width := Arguments.Horizontal_Resolution;
   Screen_Height := Arguments.Vertical_Resolution;
   Pixel_Size := Arguments.Pixels_Per_Scanline / Screen_Width;
   Pixel_Format := Arguments.Pixel_Format;

   -- Clear the screen.
   Draw_Fill(Screen, Screen'first, Screen'last, 0);

   -- Set a colour for the grid. I've set it to "red", so that the user
   -- can confirm if the pixel format is recognized properly.
   Grid_Colour := Create_Pixel(30, 10, 10);

   -- Draw the boxes so a grid is shown across the screen in some shape.
   FOR Y IN u32 RANGE 0 .. (Screen_Height / Box_Size) - 1 LOOP
      FOR X IN u32 RANGE 0 .. (Screen_Width / Box_Size) - 1 LOOP
         Draw_Box(
            Screen,
            Calculate_Pixel(Box_Size * X, Box_Size * Y),
            Grid_Colour,
            Box_Size - 1,
            Box_Size - 1);
      END LOOP;
   END LOOP;

   Terminal.Background_Colour := Create_Pixel(0, 0, 0);
   Terminal.Foreground_Colour := Create_Pixel(255, 55, 0);

   Terminal.Current_X_Index := Terminal.Data'last(2) / 2 - Welcome'length / 2;
   Print(Terminal, Welcome);
   Next_Line(Terminal);

   Draw_Textbox(
      Screen,
      Terminal,
      Terminal_Start);

   Asm("HLT", Volatile => True);
END HAVK;

