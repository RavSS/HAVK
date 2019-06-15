WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI;
USE
   HAVK_Kernel,
   HAVK_Kernel.UEFI;

PACKAGE HAVK_Kernel.Graphics WITH
   SPARK_Mode
IS
   TYPE pixel IS MOD 2 ** 32;
   TYPE framebuffer IS ARRAY(num RANGE <>) OF pixel;
   FOR  framebuffer'component_size USE 32;
   FOR  framebuffer'alignment      USE  4;

   -- Global variables so I don't need to keep passing them everywhere.
   -- Bare minimum values as default for screen properties.
   -- Will (must) be changed by the kernel startup.
   Screen_Width  :           num := 400;
   Screen_Height :           num := 200;
   Pixel_Size    :           num :=   1;
   Pixel_Format  : pixel_formats := Max;

   -- All drawing procedures ignore the constraint error and just
   -- silently return. It does not matter to the stability of
   -- the kernel and anything "off-screen" is just not displayed.

   -- Creates a pixel by taking in RGB values which are then shifted
   -- and OR'd into a single value according to the pixel format.
   FUNCTION Create_Pixel(
      Red   : IN u8;
      Green : IN u8;
      Blue  : IN u8)
   RETURN pixel;

   -- Turns a screen resolution position into a framebuffer position.
   FUNCTION Calculate_Pixel(
      Width  : IN num;
      Height : IN num)
   RETURN num;

   -- Fills the entire framebuffer with a colour. Can be used for
   -- clearing out the framebuffer fast.
   PROCEDURE Draw_Fill(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_End    : IN num;
      Pixel_Colour : IN pixel);

   -- Draws a horizontal line with a specific line size.
   PROCEDURE Draw_Horizontal_Line(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num);

   -- Draws a vertical line with a specific line size.
   PROCEDURE Draw_Vertical_Line(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num);

   -- Draws a box with a box width and box height in pixel size.
   PROCEDURE Draw_Box(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Box_Width    : IN num;
      Box_Height   : IN num);
END HAVK_Kernel.Graphics;
