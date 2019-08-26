WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics
IS
   -- TODO: Make a new type of 24 bits for a pixel that goes from
   -- 0x0 to 0xFFFFFF. This may be a bad idea if the current system uses
   -- specific bit masks for the pixel that don't end at those values.

   -- TODO: This doesn't handle pixel bit masks and only does
   -- RGB and BGR for now.
   FUNCTION Create_Pixel(
      Object   : IN view;
      Red      : IN u8;
      Green    : IN u8;
      Blue     : IN u8)
   RETURN pixel  IS
      -- Green will always be in-between red and blue.
      Mid_Byte : CONSTANT num := SHL(num(Green), 8) AND 16#00FF00#;
      Value    : pixel;
   BEGIN
      IF    Object.Pixel_Format = BGR THEN
         Value := pixel(SHL(num(Red),  16) OR Mid_Byte OR num(Blue));
      ELSIF Object.Pixel_Format = RGB THEN
         Value := pixel(SHL(num(Blue), 16) OR Mid_Byte OR num(Red));
      ELSE
         Value := 16#DEADC0DE#; -- Incorrect format error.
      END IF;

      RETURN Value;
   END Create_Pixel;

   PROCEDURE Draw_Fill(
      Object       : IN OUT view;
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
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
      Line_Length  : CONSTANT num := Pixel_Start + Line_Size;
   BEGIN
      FOR I IN num   RANGE Pixel_Start .. Line_Length LOOP
         Object.Screen(I,  Pixel_Colour);
      END LOOP;
   END Draw_Horizontal_Line;

   PROCEDURE Draw_Vertical_Line(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
   BEGIN
      FOR I IN num RANGE 1 .. Line_Size LOOP
         Object.Screen(Pixel_Start + I * Object.Screen_Width, Pixel_Colour);
      END LOOP;
   END Draw_Vertical_Line;

   PROCEDURE Draw_Box(
      Object       : IN OUT view;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Box_Width    : IN num;
      Box_Height   : IN num)
   IS
      Top_Line_End      : CONSTANT num := Pixel_Start + Box_Width;
      Bottom_Line_Start : CONSTANT num := Pixel_Start + Object.Screen_Width *
         Box_Height;
      Bottom_Line_End   : CONSTANT num := Bottom_Line_Start + Box_Width;
   BEGIN
      -- Top left down.
      FOR I IN num RANGE 1 .. Box_Height LOOP
         Object.Screen(Pixel_Start + I * Object.Screen_Width, Pixel_Colour);
      END LOOP;

      -- Top left across.
      FOR I IN num RANGE Pixel_Start .. Top_Line_End LOOP
         Object.Screen(I, Pixel_Colour);
      END LOOP;

      -- Top right down.
      FOR I IN num RANGE 1 .. Box_Height LOOP
         Object.Screen(Top_Line_End + I * Object.Screen_Width, Pixel_Colour);
      END LOOP;

      -- Bottom left across.
      FOR I IN num RANGE Bottom_Line_Start .. Bottom_Line_End LOOP
         Object.Screen(I, Pixel_Colour);
      END LOOP;
   END Draw_Box;

   FUNCTION Calculate_Pixel(
      Object : IN view;
      Width  : IN num;
      Height : IN num)
   RETURN num  IS
      Index  : CONSTANT num := Width * Object.Pixel_Size + Height *
         Object.Screen_Width;
   BEGIN
      RETURN Index;
   END Calculate_Pixel;

   FUNCTION Create_View(
      Configuration : IN UEFI_arguments)
   RETURN view        IS
      New_View      : CONSTANT view :=
      (
         Framebuffer_Address => Configuration.Framebuffer_Address,
         -- TODO: Convert size to 32 bit indices. Do this elsewhere.
         Framebuffer_Size    => Configuration.Framebuffer_Size / 4,
         Screen_Width        => Configuration.Horizontal_Resolution,
         Screen_Height       => Configuration.Vertical_Resolution,
         Pixel_Size          => Configuration.Pixels_Per_Scanline /
            Configuration.Horizontal_Resolution,
         Pixel_Format        => Configuration.Pixel_Format
      );
   BEGIN
      RETURN New_View;
   END Create_View;

   PROCEDURE Screen(
      Object : IN view;
      Index  : IN num;
      Data   : IN pixel)
   IS
      Buffer : ALIASED framebuffer(0 .. Object.Framebuffer_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Object.Framebuffer_Address;
   BEGIN
      Buffer(Index) := Data; -- TODO: Hopefully a temporary slow solution.
   END Screen;
END HAVK_Kernel.Graphics;
