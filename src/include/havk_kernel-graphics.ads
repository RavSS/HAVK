WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI;
USE
   HAVK_Kernel,
   HAVK_Kernel.UEFI;

PACKAGE HAVK_Kernel.Graphics
IS
   SUBTYPE framebuffer IS u32s;

   -- Global variables so I don't need to keep passing them everywhere.
   -- Bare minimum values as default for screen properties.
   -- Will (must) be changed by the kernel startup.
   Screen_Width : u32 := 400;
   Screen_Height : u32 := 200;
   Pixel_Size : u32 := 1;
   Pixel_Format : Pixel_Formats := Max;

   -- All drawing procedures ignore the constraint error and just
   -- silently return. It does not matter to the stability of
   -- the kernel and anything "off-screen" is just not displayed.

   -- Creates a pixel by taking in RGB values which are then shifted
   -- and OR'd into a single value according to the pixel format.
   FUNCTION Create_Pixel(
      Red : IN u8;
      Green : IN u8;
      Blue : IN u8)
   RETURN u32;

   -- Turns a screen resolution position into a framebuffer position.
   FUNCTION Calculate_Pixel(
      Width : IN u32;
      Height : IN u32)
   RETURN u64;

   -- Fills the entire framebuffer with a colour. Can be used for
   -- clearing out the framebuffer fast.
   PROCEDURE Draw_Fill(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_End : IN u64;
      Pixel_Colour : IN u32);

   -- Simply draws a pixel on the screen.
   PROCEDURE Draw_Pixel(
      Buffer : IN OUT framebuffer;
      Pixel : IN u64;
      Pixel_Colour : IN u32);

   -- Draws a horizontal line with a specific line size.
   PROCEDURE Draw_Horizontal_Line(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Line_Size : IN u64);

   -- Draws a vertical line with a specific line size.
   PROCEDURE Draw_Vertical_Line(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Line_Size : IN u64);

   -- Draws a box with a box width and box height in pixel size.
   PROCEDURE Draw_Box(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Box_Width : IN u32;
      Box_Height : IN u32);
END HAVK_Kernel.Graphics;
