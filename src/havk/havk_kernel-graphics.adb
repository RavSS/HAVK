WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics
IS
   -- TODO: This doesn't handle pixel bitmasks and only does
   -- RGB and BGR for now.
   FUNCTION Create_Pixel(
      Object   : IN view;
      Red      : IN num;
      Green    : IN num;
      Blue     : IN num)
   RETURN pixel  IS
      -- Green will always be in-between red and blue (for now).
      Mid_Byte : CONSTANT num RANGE 0 .. 16#FF00# := SHL(Green, 8) AND
         16#FF00#;
      Value    : num := Mid_Byte; -- Green on error.
   BEGIN
      IF    Object.Pixel_Format = BGR THEN
         Value := SHL(Red,  16) OR Mid_Byte OR Blue;
      ELSIF Object.Pixel_Format = RGB THEN
         Value := SHL(Blue, 16) OR Mid_Byte OR Red;
      END IF;

      IF Value >= num(pixel'last) THEN -- Validate the range.
         RETURN pixel'first; -- A black pixel is returned on an invalid value.
      ELSE
         RETURN pixel(Value);
      END IF;
   END Create_Pixel;

   PROCEDURE Draw_Fill(
      Object       : IN view;
      Pixel_Start  : IN num;
      Pixel_End    : IN num;
      Pixel_Colour : IN pixel)
   IS
   BEGIN
      FOR I IN num RANGE Pixel_Start .. Pixel_End LOOP
         Object.Screen(I, Pixel_Colour);
      END LOOP;
   END Draw_Fill;

   PROCEDURE Draw_Horizontal_Line(
      Object       : IN view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
      Line_Length  : CONSTANT num := Pixel_Start + Line_Size;
   BEGIN
      FOR I IN num   RANGE Pixel_Start .. Line_Length LOOP
         IF I <= Object.Framebuffer_Elements THEN
            Object.Screen(I,  Pixel_Colour);
         ELSE
            RETURN;
         END IF;
      END LOOP;
   END Draw_Horizontal_Line;

   PROCEDURE Draw_Vertical_Line(
      Object       : IN view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
      Index        : num;
   BEGIN
      FOR I IN num   RANGE 1 .. Line_Size LOOP
         Index := Pixel_Start + I * Object.Screen_Width;

         IF Index <= Object.Framebuffer_Elements THEN
            Object.Screen(Index, Pixel_Colour);
         ELSE
            RETURN;
         END IF;
      END LOOP;
   END Draw_Vertical_Line;

   PROCEDURE Draw_Box(
      Object       : IN view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Box_Width    : IN num;
      Box_Height   : IN num)
   IS
      Top_Line_End      : CONSTANT num := Pixel_Start + Box_Width;
      Bottom_Line_Start : CONSTANT num := Pixel_Start + Object.Screen_Width *
         Box_Height;
   BEGIN
      IF -- Top left corner towards down.
         Pixel_Start + Box_Height * Object.Screen_Width <=
            Object.Framebuffer_Elements
      THEN
         Object.Draw_Vertical_Line(
            Pixel_Start,
            Pixel_Colour,
            Box_Height);
      END IF;

      IF -- Top left corner to across.
         Pixel_Start + Box_Width <= Object.Framebuffer_Elements
      THEN
         Object.Draw_Horizontal_Line(
            Pixel_Start,
            Pixel_Colour,
            Box_Width);
      END IF;

      IF -- Top right corner towards down.
         Top_Line_End + Box_Height * Object.Screen_Width <=
            Object.Framebuffer_Elements
      THEN
         Object.Draw_Vertical_Line(
            Top_Line_End,
            Pixel_Colour,
            Box_Height);
      END IF;

      IF -- Bottom left corner to across.
         Bottom_Line_Start + Box_Width <= Object.Framebuffer_Elements
      THEN
         Object.Draw_Horizontal_Line(
            Bottom_Line_Start,
            Pixel_Colour,
            Box_Width);
      END IF;
   END Draw_Box;

   FUNCTION Calculate_Pixel(
      Object : IN view'class;
      Width  : IN num;
      Height : IN num)
   RETURN num  IS
      Index  : CONSTANT num := Width * Object.Pixel_Size +
         Height * Object.Screen_Width;
   BEGIN
      IF Index >= Object.Framebuffer_Elements THEN
         RETURN Object.Framebuffer_Elements; -- Last element if out-of-bounds.
      ELSE
         RETURN Index;
      END IF;
   END Calculate_Pixel;

   PROCEDURE Screen(
      Object : IN view'class;
      Index  : IN num;
      Data   : IN pixel)
   WITH
      SPARK_Mode => off -- SPARK doesn't let me declare the array as volatile.
   IS
      -- The assembly generated does not seem to be too different if I replace
      -- this with a single pixel variable overlayed to the index/address
      -- instead of an entire array overlayed to the base index/address.
      Buffer : ALIASED framebuffer(0 .. Object.Framebuffer_Elements)
      WITH
         Import        => true,
         Volatile      => true,
         Async_Readers => true, -- Technically useless, but here for clarity.
         Convention    => C,
         Address       => Object.Framebuffer_Address;
   BEGIN
      Buffer(Index) := Data; -- TODO: Hopefully a temporary slow solution.
   END Screen;

   FUNCTION Get_Display(
      Configuration : IN UEFI.arguments)
   RETURN view        IS
      -- I'll assume a few things about what the UEFI GOP implementation
      -- returned to HAVK. Most of them are fairly sensible, while others
      -- are limitations.

      -- TODO: For now, we only support these two pixel formats.
      PRAGMA Assume(Configuration.Pixel_Format = RGB OR
         ELSE Configuration.Pixel_Format = BGR);

      New_View      : CONSTANT view :=
      (
         Framebuffer_Address  => Configuration.Framebuffer_Address,
         Framebuffer_Elements => Configuration.Framebuffer_Size / 4,
         Screen_Width         => Configuration.Horizontal_Resolution,
         Screen_Height        => Configuration.Vertical_Resolution,
         Pixel_Size           => Configuration.Pixels_Per_Scanline /
            Configuration.Horizontal_Resolution,
         Pixel_Format         => Configuration.Pixel_Format
      );
   BEGIN
      RETURN New_View;
   END Get_Display;
END HAVK_Kernel.Graphics;
