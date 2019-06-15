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
      Red      : IN u8;
      Green    : IN u8;
      Blue     : IN u8)
   RETURN pixel  IS
      -- Green will always be in-between red and blue.
      Mid_Byte : CONSTANT num := SHL(num(Green), 8);
      Value    : pixel;
   BEGIN
      IF    Pixel_Format = BGR THEN
         Value := pixel(SHL(num(Red),  16) OR Mid_Byte OR num(Blue));
      ELSIF Pixel_Format = RGB THEN
         Value := pixel(SHL(num(Blue), 16) OR Mid_Byte OR num(Red));
      ELSE
         Value := 16#DEADC0DE#; -- Incorrect format error.
      END IF;

      RETURN Value;
   END Create_Pixel;

   FUNCTION Calculate_Pixel(
      Width   : IN num;
      Height  : IN num)
   RETURN num   IS
      Index   : CONSTANT num := Width * Pixel_Size + Height * Screen_Width;
   BEGIN
      RETURN Index;
   END Calculate_Pixel;

   PROCEDURE Draw_Fill(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_End    : IN num;
      Pixel_Colour : IN pixel)
   IS
   BEGIN
      FOR I IN num RANGE Pixel_Start .. Pixel_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   END Draw_Fill;

   PROCEDURE Draw_Horizontal_Line(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
      Line_Length  : CONSTANT num := Pixel_Start + Line_Size;
   BEGIN
      FOR I IN num   RANGE Pixel_Start .. Line_Length LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   END Draw_Horizontal_Line;

   PROCEDURE Draw_Vertical_Line(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Line_Size    : IN num)
   IS
   BEGIN
      FOR I IN num   RANGE 1 .. Line_Size LOOP
         Buffer(Pixel_Start + I * Screen_Width) := Pixel_Colour;
      END LOOP;
   END Draw_Vertical_Line;

   PROCEDURE Draw_Box(
      Buffer       : IN OUT framebuffer;
      Pixel_Start  : IN num;
      Pixel_Colour : IN pixel;
      Box_Width    : IN num;
      Box_Height   : IN num)
   IS
      Top_Line_End      : CONSTANT num := Pixel_Start + Box_Width;
      Bottom_Line_Start : CONSTANT num := Pixel_Start + Screen_Width *
         Box_Height;
      Bottom_Line_End   : CONSTANT num := Bottom_Line_Start + Box_Width;
   BEGIN
      -- Top left down.
      FOR I IN num RANGE 1 .. Box_Height LOOP
         Buffer(Pixel_Start + I * Screen_Width) := Pixel_Colour;
      END LOOP;

      -- Top left across.
      FOR I IN num RANGE Pixel_Start .. Top_Line_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;

      -- Top right down.
      FOR I IN num RANGE 1 .. Box_Height LOOP
         Buffer(Top_Line_End + I * Screen_Width) := Pixel_Colour;
      END LOOP;

      -- Bottom left across.
      FOR I IN num RANGE Bottom_Line_Start .. Bottom_Line_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   END Draw_Box;
END HAVK_Kernel.Graphics;

