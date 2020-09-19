-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Terminal                         --
-- Filename        -- havk_terminal-graphics.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Terminal.Graphics
WITH
   Abstract_State =>
   (  -- Writing a pixel to the framebuffer will cause the GPU to read it.
      Graphics_Output_Protocol_State
      WITH
         External => (Async_Readers, Effective_Writes)
   )
IS
   -- UEFI GOP defines pixels as 32 bits, so we will as well. The upper 8 bits
   -- are usually empty/reserved, but they are too included just in case.
   TYPE pixel IS MOD 2**32
   WITH
      Size        => 32,
      Object_Size => 32;

   -- See the UEFI specifications for more about the types of pixel
   -- formats and bitmasks.
   TYPE pixel_formats IS
     (RGB,     -- PixelRedGreenBlueReserved8BitPerColor.
      BGR,     -- PixelBlueGreenRedReserved8BitPerColor.
      bitmask, -- PixelBitMask.
      BLT,     -- PixelBltOnly (no framebuffer access = failure).
      max)     -- PixelFormatMax (should never get this from boot).
   WITH
      Size       => 32,
      Convention => C;

   -- The low-level framebuffer type, which is an array of pixels. Ada's fat
   -- pointers are sort of odd, so I've just made it unconstrained. Don't use
   -- the length attributes on this to get the size of the array i.e. "last".
   TYPE framebuffer_area IS ARRAY(number) OF ALIASED pixel
   WITH
      Component_Size => 32;
   TYPE access_framebuffer_area IS ACCESS framebuffer_area;

   -- The framebuffer area. UEFI GOP can only grant a single framebuffer, so
   -- I've removed the old tagged record way of handling it.
   Framebuffer          : access_framebuffer_area;
   -- The framebuffer MMIO address passed by the kernel.
   Framebuffer_Address  : address RANGE 1 .. address'last;
   -- The number of 32-bit pixels in the framebuffer's address space. This
   -- should count from zero. Use this to understand the range of the
   -- framebuffer.
   Framebuffer_Elements : number;
   -- Details the width or height of the monitor or screen.
   Screen_Width         : number  RANGE 1 ..  number'last;
   Screen_Height        : number  RANGE 1 ..  number'last;
   -- The size of each pixel on the screen. Sometimes, a pixel may be more
   -- than a single pixel unit, which is common on very dense screens.
   Pixel_Size           : number  RANGE 1 ..  number'last;
   -- The pixel format does not have to always be in a static format like
   -- RGB, it can be BGR or a form of masks that show a pixel's layout.
   Pixel_Format         : pixel_formats;

   -- TODO: This function does not support the pixel bitmasks as of now.
   -- Creates a pixel by taking in RGB values which are then shifted
   -- and OR'd into a single value according to the pixel format.
   FUNCTION Create_Pixel
     (Red   : IN number;
      Green : IN number;
      Blue  : IN number)
      RETURN pixel
   WITH
      Pre => Red   <= 16#FF# AND THEN
             Green <= 16#FF# AND THEN
             Blue  <= 16#FF#;

   -- Turns a screen resolution position into a framebuffer position.
   FUNCTION Calculate_Pixel
     (Width  : IN number;
      Height : IN number)
      RETURN number
   WITH
      Pre  => Width  <= Screen_Width AND THEN
              Height <= Screen_Height,
      Post => Calculate_Pixel'result <= Framebuffer_Elements;

   -- Fills the framebuffer with a colour in a linear fashion at any points.
   -- Can be used for clearing out the entire framebuffer fast.
   PROCEDURE Draw_Fill
     (Pixel_Start  : IN number;
      Pixel_End    : IN number;
      Pixel_Colour : IN pixel)
   WITH
      Global => (Output => Graphics_Output_Protocol_State),
      Pre    => Pixel_Start <= Framebuffer_Elements AND THEN
                Pixel_End   <= Framebuffer_Elements;

   -- Draws a horizontal line with a specific line size.
   PROCEDURE Draw_Horizontal_Line
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Line_Size    : IN number)
   WITH
      Global => (Output => Graphics_Output_Protocol_State),
      Pre    => Pixel_Start + Line_Size <= Framebuffer_Elements;

   -- Draws a vertical line with a specific line size.
   PROCEDURE Draw_Vertical_Line
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Line_Size    : IN number)
   WITH
      Global => (Output => Graphics_Output_Protocol_State),
      Pre    => Pixel_Start + Line_Size * Screen_Width <= Framebuffer_Elements;

   -- Draws a box with a box width and box height in pixel size.
   PROCEDURE Draw_Box
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Box_Width    : IN number;
      Box_Height   : IN number)
   WITH
      Global => (Output => Graphics_Output_Protocol_State),
      Pre    => Box_Width <= Framebuffer_Elements               AND THEN
                Box_Height <= Framebuffer_Elements              AND THEN
                Pixel_Start + Box_Width <= Framebuffer_Elements AND THEN
                Pixel_Start + Screen_Width * Box_Height <=
                   Framebuffer_Elements;

PRIVATE
   FUNCTION To_Pointer
     (Passed_Framebuffer_Address : IN address)
      RETURN access_framebuffer_area
   WITH
      Import     => true,
      Convention => Intrinsic,
      Pre        => Passed_Framebuffer_Address /= 0,
      Post       => To_Pointer'result /= NULL;

END HAVK_Terminal.Graphics;
