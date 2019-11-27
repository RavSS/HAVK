WITH
   HAVK_Kernel.UEFI;
USE
   HAVK_Kernel.UEFI;

-- This package is where graphics are rendered onto the display and it is
-- the only package which should do anything with video.
PACKAGE HAVK_Kernel.Graphics
IS
   -- UEFI GOP defines pixels as 32-bits, so we will as well.
   TYPE pixel IS MOD 2 ** 32
   WITH
      Size => 32;

   -- The low-level framebuffer type, which is an array of pixels.
   TYPE framebuffer IS ARRAY(num RANGE <>) OF ALIASED pixel
   WITH
      Component_Size => 32,
      Alignment      =>  4;

   -- This class type composes a few abstractions for the framebuffer itself.
   -- It has a few line drawing functionalities and allows text to be drawn.
   TYPE view IS TAGGED RECORD
      -- The framebuffer MMIO address passed by the bootloader.
      Framebuffer_Address  : num RANGE 1 .. num'last;
      -- The number of 32-bit pixels in the framebuffer's address space.
      Framebuffer_Elements : num RANGE 1 .. num'last;
      -- Details the width or height of the monitor or screen.
      Screen_Width         : num RANGE 1 .. num'last;
      Screen_Height        : num RANGE 1 .. num'last;
      -- The size of each pixel on the screen. Sometimes, a pixel may be more
      -- than a single pixel unit, which is common on very dense screens.
      Pixel_Size           : num RANGE 1 .. num'last;
      -- The pixel format does not have to always be in a static format like
      -- RGB, it can be BGR or a form of masks that show a pixel's layout.
      Pixel_Format         : pixel_formats RANGE RGB .. bitmask;
   END RECORD;

   -- Uses the UEFI arguments passed by the bootloader to return a view type.
   FUNCTION Get_Display(
      Configuration : IN UEFI.arguments)
   RETURN view
   WITH
      Pre'class => Configuration.Pixels_Per_Scanline >=
                   Configuration.Horizontal_Resolution;

   -- TODO: Accesses the framebuffer dynamically. This is where my current
   -- knowledge of Ada falters, I have no idea how to add an imported and
   -- overlayed array into a (tagged) record, nor can I do it with an access.
   -- Last time I checked, this is going to add seriously awful overhead, as
   -- a procedure is called for every single pixel update. Inlining doesn't
   -- solve it.
   PROCEDURE Screen(
      Object        : IN view'class;
      Index         : IN num;
      Data          : IN pixel)
   WITH
      Inline    => true,
      Pre       => Index <= Object.Framebuffer_Elements;

   -- TODO: This function does not support the pixel bitmasks as of now.
   -- Creates a pixel by taking in RGB values which are then shifted
   -- and OR'd into a single value according to the pixel format.
   FUNCTION Create_Pixel(
      Object        : IN view;
      Red           : IN num;
      Green         : IN num;
      Blue          : IN num)
   RETURN pixel
   WITH
      Inline    => true,
      Pre'class => Red   <= 16#FF#          AND THEN
                   Green <= 16#FF#          AND THEN
                   Blue  <= 16#FF#          AND THEN
                  (Object.Pixel_Format = RGB OR ELSE
                   Object.Pixel_Format = BGR);

   -- Turns a screen resolution position into a framebuffer position.
   FUNCTION Calculate_Pixel(
      Object        : IN view'class;
      Width         : IN num;
      Height        : IN num)
   RETURN num
   WITH
      Inline    => true,
      Pre       => Width  <= Object.Screen_Width AND THEN
                   Height <= Object.Screen_Height,
      Post      => Calculate_Pixel'result <= Object.Framebuffer_Elements;

   -- Fills the framebuffer with a colour in a linear fashion at any points.
   -- Can be used for clearing out the entire framebuffer fast.
   PROCEDURE Draw_Fill(
      Object        : IN view;
      Pixel_Start   : IN num;
      Pixel_End     : IN num;
      Pixel_Colour  : IN pixel)
   WITH
      Pre'class => Pixel_Start <= Object.Framebuffer_Elements AND THEN
                   Pixel_End   <= Object.Framebuffer_Elements;

   -- Draws a horizontal line with a specific line size.
   PROCEDURE Draw_Horizontal_Line(
      Object        : IN view;
      Pixel_Start   : IN num;
      Pixel_Colour  : IN pixel;
      Line_Size     : IN num)
   WITH
      Pre'class => Pixel_Start + Line_Size <= Object.Framebuffer_Elements;

   -- Draws a vertical line with a specific line size.
   PROCEDURE Draw_Vertical_Line(
      Object        : IN view;
      Pixel_Start   : IN num;
      Pixel_Colour  : IN pixel;
      Line_Size     : IN num)
   WITH
      Pre'class => Pixel_Start + Line_Size * Object.Screen_Width <=
                   Object.Framebuffer_Elements;

   -- Draws a box with a box width and box height in pixel size.
   PROCEDURE Draw_Box(
      Object        : IN view;
      Pixel_Start   : IN num;
      Pixel_Colour  : IN pixel;
      Box_Width     : IN num;
      Box_Height    : IN num)
   WITH
      Pre'class => Box_Width <= Object.Framebuffer_Elements  AND THEN
                   Box_Height <= Object.Framebuffer_Elements AND THEN
                   Pixel_Start + Box_Width <= Object.Framebuffer_Elements AND
                   THEN Pixel_Start + Object.Screen_Width * Box_Height <=
                   Object.Framebuffer_Elements;

END HAVK_Kernel.Graphics;
