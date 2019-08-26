WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI;
USE
   HAVK_Kernel,
   HAVK_Kernel.UEFI;

PACKAGE HAVK_Kernel.Graphics
IS
   TYPE pixel       IS MOD 2 ** 32;
   TYPE framebuffer IS ARRAY(num RANGE <>) OF ALIASED pixel
   WITH
      Component_Size => 32,
      Alignment      => 4;

   TYPE view IS TAGGED RECORD
      PRAGMA Warnings(off, "record layout may cause performance issues");
      -- PRAGMA Warnings(off, "*length depends on a discriminant");
      PRAGMA Warnings(off, "comes too early and was moved down");
      -- The framebuffer MMIO address or size passed by the bootloader.
      Framebuffer_Address : num;
      Framebuffer_Size    : num;
      -- Details the width or height of the monitor or screen.
      Screen_Width        : num;
      Screen_Height       : num;
      -- The size of each pixel on the screen. Sometimes, a pixel may be more
      -- than a single pixel unit, which is common on very dense screens.
      Pixel_Size          : num;
      -- The pixel format does not have to always be in a static format like
      -- RGB, it can be BGR or a form of masks that show a pixel's layout.
      Pixel_Format        : pixel_formats;
   END RECORD;

   FUNCTION Create_View(
      Configuration : IN UEFI_arguments)
   RETURN view;

   -- TODO: Accesses the framebuffer dynamically. This is where my current
   -- knowledge of Ada falters, I have no idea how to add an imported and
   -- overlayed array into a (tagged) record, nor can I do it with an access.
   -- Last time I checked, this is going to add seriously awful overhead, as
   -- a procedure is called for every single pixel update. I can't apply
   -- the GNAT specific "Inline_Always" pragma either to primitive operations.
   PROCEDURE Screen(
      Object : IN view;
      Index  : IN num;
      Data   : IN pixel)
   WITH
      Inline => true,
      Pre    => Index <= Object.Framebuffer_Size;

   -- Creates a pixel by taking in RGB values which are then shifted
   -- and OR'd into a single value according to the pixel format.
   FUNCTION Create_Pixel(
      Object : IN view;
      Red    : IN u8;
      Green  : IN u8;
      Blue   : IN u8)
   RETURN pixel;

   -- Turns a screen resolution position into a framebuffer position.
   FUNCTION Calculate_Pixel(
      Object : IN view;
      Width  : IN num;
      Height : IN num)
   RETURN num;

   -- Fills the entire framebuffer with a colour. Can be used for
   -- clearing out the framebuffer fast.
   PROCEDURE Draw_Fill(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_End    : IN num;
      Pixel_Colour : IN pixel);

   -- Draws a horizontal line with a specific line size.
   PROCEDURE Draw_Horizontal_Line(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num);

   -- Draws a vertical line with a specific line size.
   PROCEDURE Draw_Vertical_Line(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num);

   -- Draws a box with a box width and box height in pixel size.
   PROCEDURE Draw_Box(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Box_Width    : IN num;
      Box_Height   : IN num);
END HAVK_Kernel.Graphics;
